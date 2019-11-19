(ns stenojure.plover
  (:require [clojure.data.json :as json]
            [clojure.set :as set]
            [clojure.string :as str]
            [meander.epsilon :as m]
            [medley.core :as medley]
            [stenojure.predicates :as pred]))


(defn replace-numbers [stroke]
  (if (pred/includes-number? stroke)
    (str "#" (-> stroke
                 (str/replace "#" "")
                 (str/replace "1" "S")
                 (str/replace "2" "T")
                 (str/replace "3" "P")
                 (str/replace "4" "H")
                 (str/replace "5" "A")
                 (str/replace "0" "O")
                 (str/replace "6" "F")
                 (str/replace "7" "P")
                 (str/replace "8" "L")
                 (str/replace "9" "T")))
    stroke))


(defn split-words [translation]
  (let [words (str/split translation #"\W")
        separators (into [] (remove empty? (str/split translation #"\w")))]
    (if (and (= 1 (count words))
             (= 0 (count separators)))
      [{:word translation}]
      (let [partitioned-word-seq (partition-all 2 (medley/interleave-all words separators))]
        (mapv #(hash-map :word (first %)
                         :separator (second %)) partitioned-word-seq)))))


(defn make-vowels [inner]
  (cond
    (empty? inner) ""
    (= "-" (first inner)) ""
    :else (first inner)))


(defn split-stroke [stroke]
  (let [outer (str/split stroke #"\*|-|A|O|E|U")
        inner (str/split (str/replace stroke #"\*" "") #"S|T|K|P|W|R|H|R|F|B|L|G|D|Z")
        asterisk? (str/includes? stroke "*")
        number? (str/includes? stroke "#")
        symbols (cond
                  (and asterisk? number?) "#*"
                  asterisk? "*"
                  number? "#"
                  :else "")
        left (if (not (empty? (first outer)))
               (first outer)
               "")
        right (if (< 1 (count outer))
                (last outer)
                "")
        vowels (->> inner
                    (remove #(empty? %))
                    make-vowels)]
    {:left       left
     :vowels     vowels
     :right      right
     :symbols    symbols
     :stroke-set (set/union (into #{} (mapv #(keyword (str % "-")) left))
                            (into #{} (mapv #(keyword (str %)) vowels))
                            (into #{} (mapv #(keyword (str "-" %)) right))
                            (into #{} (mapv #(keyword (str %)) symbols)))}))


(defn get-plover-dictionary [file-path]
  (->> (slurp file-path)
       (json/read-str)
       (mapv #(hash-map :stroke (str/split (replace-numbers (first %)) #"/")
                        :translation (last %)
                        :words (split-words (str/lower-case (last %)))))
       ))

(defn group-by-word [ms]
  (-> (group-by :word ms)
      (m/search
        (m/scan [?id [{:strokes !s} ...]])
        {:word    ?id
         :strokes !s})))


(defn filter-entries-containing-only-stroke-set [stroke-set combined-dict]
  (->> combined-dict
       (mapv #(assoc % :stroke-sets (mapv (fn [x]
                                            (mapv (fn [y]
                                                    (:stroke-set (split-stroke y)))
                                                  x))
                                          (:strokes %))))
       (mapv #(assoc % :stroke-sets (mapv (fn [x] (apply set/union x)) (:stroke-sets %))))
       (mapv #(assoc % :filtered-strokes (filter (fn [x] (set/superset? stroke-set x)) (:stroke-sets %))))
       (filter #(not (empty? (:filtered-strokes %))))))



(defn make-single-word-plover-dictionary [plover-dict]
  (->> (m/search plover-dict
                 (m/scan {:stroke !stroke
                          :words  [{:word ?word}]})
                 (m/subst {:word    ?word
                           :strokes !stroke}))
       group-by-word))

(defn combine-cmu-freq-dictionaries [cmu-dict freq-dict]
  (set/join cmu-dict freq-dict {:word :word}))


(defn combine-plover-cmu-freq-dictionaries [plover-dict cmu-dict freq-dict]
  (set/join (set/join cmu-dict freq-dict {:word :word})
            (make-single-word-plover-dictionary plover-dict) {:word :word}))
