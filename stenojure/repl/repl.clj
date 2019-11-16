(ns stenojure.repl
  (:require [clojure.data.json :as json]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [stenojure.cmudict :as cmudict]
            [stenojure.orthographic :as orthographic]
            [stenojure.phonetic :as phonetic]
            [stenojure.predicates :as predicates]))



(def cmu-dict-7b
  (cmudict/load-cmudict "resources/cmudict/cmudict-0.7b"))





(defrecord Word [word
                 chunks
                 phonemes])


(defn make-word [word phonemes]
  (->Word word
          (orthographic/split-clusters word)
          phonemes))


(defn split-words [translation]
  (when-not (predicates/includes-number? translation)
    (str/split translation #" |-")))


(defn replace-numbers [stroke]
  (if (predicates/includes-number? stroke)
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



(defn make-vowels [inner]
  (cond
    (empty? inner) ""
    (= "-" (first inner)) ""
    :else (first inner)))


(defn split-stroke [stroke]
  (let [stroke (if (predicates/includes-number? stroke) (replace-numbers stroke) stroke)
        outer (str/split stroke #"\*|-|A|O|E|U")
        inner (str/split (str/replace stroke #"\*" "") #"S|T|K|P|W|R|H|R|F|B|L|G|D|Z")
        asterisk? (str/includes? stroke "*")
        left (if (not (empty? (first outer)))
               (first outer)
               "")
        right (if (< 1 (count outer))
                (last outer)
                "")
        vowels (->> inner
                    (remove #(empty? %))
                    make-vowels)]
    {:stroke    stroke
     :left      left
     :vowels    vowels
     :right     right
     :asterisk? asterisk?}))


(defn split-strokes [outline]
  (->> (str/split outline #"\/")
       (map split-stroke)))


(defn make-entry [source [outline translation]]
  (let [strokes (split-strokes outline)
        words (split-words translation)]
    {:outline      outline
     :phonemes     (get cmu-dict-7b translation)
     :strokes      strokes
     :stroke-count (count strokes)
     :translation  translation
     :words        words
     :chunks       (into [] (mapcat split-clusters words))
     :clusters     (into [] (mapcat split-clusters words))
     :numbers?     (includes-number? outline)
     :source-dict  source}
    )
  )


(defn read-steno-dictionary [file-path]
  (->> (slurp file-path)
       (json/read-str)
       (map (partial make-entry file-path))))


(defn chord-shapes [s]
  (if (= "-" s) [""]
                (->> s
                     combo/subsets
                     (map #(apply str %))
                     (into #{}))))


(defn single-stroke-match-xf [left vowels right]
  (let [left (chord-shapes left)
        vowels (chord-shapes vowels)
        right (chord-shapes right)]

    (comp (filter #(left (:left (first (:strokes %)))))
          (filter #(vowels (:vowels (first (:strokes %)))))
          (filter #(right (:right (first (:strokes %))))))))


(defn single-stroke-match-left-xf [left]
  (let [left (chord-shapes left)]
    (filter #(left (:left (first (:strokes %)))))))

