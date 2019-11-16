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


(defn get-plover-dictionary [file-path]
  (->> (slurp file-path)
       (json/read-str)
       (map #(hash-map :stroke (str/split (replace-numbers (first %)) #"/")
                       :translation (last %)
                       :words (split-words (last %))))))

(defn group-by-word [ms]
  (-> (group-by :word ms)
      (m/search
        (m/scan [?id [{:strokes !s} ...]])
        {:word    ?id
         :strokes !s})))


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
            (make-single-word-plover-dictionary plover-dict)))
