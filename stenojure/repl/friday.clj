(require '[clojure.math.combinatorics :as combo]
         '[clojure.data :as data]
         '[clojure.set :as set]
         '[clojure.string :as str]
         '[stenojure.cmudict :as cmu]
         '[stenojure.dictionaries.canonical :as canonical]
         '[stenojure.frequency :as freq]
         '[stenojure.orthographic :as orthographic]
         '[stenojure.phonetic :as phonetic]
         '[stenojure.plover :as plover]
         '[meander.epsilon :as m]
         '[medley.core :as medley])

(def norvig-freq-dict (freq/load-normalized-norvig-frequencies))

(def cmu-dict (cmu/load-cmudict))

(def plover-main-dict (plover/get-plover-dictionary "resources/plover/main.json"))

(def combined-dict (-> (plover/make-single-word-plover-dictionary plover-main-dict)
                       (set/join cmu-dict {:word :word})
                       (set/join norvig-freq-dict {:word :word})))




(->> canonical/my-learned-definitions
     (mapv #(assoc % :orthographic-clusters (orthographic/split-clusters (:word %))
                     :phonetic-clusters (phonetic/split-phoneme-clusters (:canonical-phonetic %))
                     :stroke-parts (mapv plover/split-stroke (:canonical-stroke %)))))






(let [learned-definitions (->> canonical/my-learned-definitions
                               (mapv :word)
                               (into #{}))]

  (->> combined-dict
       (remove #(canonical/ignored-words (:word %)))
       (remove #(learned-definitions (:word %)))
       (sort-by :frequency)
       reverse
       (mapv #(select-keys % [:word :strokes :phonetic]))
       (mapv #(set/rename-keys % {:word     :word
                                  :strokes  :canonical-stroke
                                  :phonetic :canonical-phonetic}))
       (filter (str/starts-with? ""))
       (take 10)))

#_(->> plover-main-dict
       (filter #(#{"num"} (:translation %)))
       #_(filter #(#{"UPB"} (first (:stroke %)))))

#_(count canonical/my-learned-definitions)