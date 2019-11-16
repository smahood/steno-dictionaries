(require '[clojure.math.combinatorics :as combo]
         '[clojure.data :as data]
         '[clojure.set :as set]
         '[clojure.string :as str]
         '[stenojure.cmudict :as cmu]
         '[stenojure.frequency :as freq]
         '[stenojure.plover :as plover]
         '[meander.epsilon :as m]
         '[medley.core :as medley])


(def norvig-freq-dict (freq/load-normalized-norvig-frequencies))


(def cmu-dict (cmu/load-cmudict))


(def plover-main-dict (plover/get-plover-dictionary "resources/plover/main.json"))


(defn reformat-simple-plover-dict [dict]
  (->> (m/search dict
                 (m/scan {:stroke !stroke
                          :words  [{:word ?word}]})
                 (m/subst {:word    ?word
                           :strokes !stroke}))
       (mapv #(hash-map (:word %) [(:strokes %)]))
       (apply merge-with (fn [& x] (into [] (apply concat x))))
       (mapv #(hash-map :word (first %) :strokes (second %)))))


(def simple-plover-dict (reformat-simple-plover-dict plover-main-dict))

(def combined-dict (plover/combine-plover-cmu-freq-dictionaries cmu-dict norvig-freq-dict simple-plover-dict))



(defn update-values [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(defn diff-strings [s1 s2]
  (let [d (data/diff (mapv str s1)
                     (mapv str s2))
        common (->> (last d)
                    (partition-by some?)
                    (mapv str/join)
                    (remove empty?)
                    (into []))
        different (->> (butlast d)
                       (mapv #(->> %
                                   (partition-by some?)
                                   (mapv str/join)))
                       (apply zipmap)
                       (remove #(empty? (key %)))
                       distinct
                       (into []))]
    {:common        common
     :different     different
     :different-set (mapv #(into #{} %) different)}))


(defn diff-phonetics [phonetics]
  (->> (combo/combinations phonetics 2)
       (mapv #(apply diff-strings %))
       (mapv #(hash-map (:common %) (:different-set %)))
       (apply merge-with concat)
       (mapv #(hash-map (key %) (into #{} (val %))))
       (apply merge)
       vals
       (apply concat)
       (into #{})))





(comment (->> combined-dict
              (mapv #(diff-phonetics (:phonetic %)))
              (remove empty?)
              (apply concat)
              distinct
              (mapv #(hash-map (first %) (second %)))
              (apply merge)))