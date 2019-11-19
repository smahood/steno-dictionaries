(ns stenojure.steno-in-small-pieces.main
  (:require
    [clojure.set :as set]
    [stenojure.cmudict :as cmu]
    [stenojure.dictionaries.canonical :as canonical]
    [stenojure.frequency :as freq]
    [stenojure.plover :as plover]
    [stenojure.steno-in-small-pieces.chapters.01 :as c01]))



(def combined-dict (-> (plover/make-single-word-plover-dictionary (plover/get-plover-dictionary "resources/plover/main.json"))
                       (set/join (cmu/load-cmudict) {:word :word})
                       (set/join (freq/load-normalized-norvig-frequencies) {:word :word})))


(defn left-hand-concepts [dict]
  (let [filter-dict (fn [xs] (plover/filter-entries-containing-only-stroke-set xs dict))]
    {:S-   (filter-dict #{:S-})
     :T-   (filter-dict #{:T-})
     :ST-  (filter-dict #{:S- :T-})
     :STK- (filter-dict #{:S- :T- :K-})
     })

  )


(defn concepts-by-frequency [dict]
  (let [filter-dict (fn [xs] (plover/filter-entries-containing-only-stroke-set xs dict))]
    [(filter-dict #{:-T})
     (filter-dict #{:-T :-F})
     (filter-dict #{:O :-T :-F})
     (filter-dict #{:S- :O :-T :-F})
     (filter-dict #{:S- :A :O :-T :-F})


     ]))

(mapv #(mapv :word %) (concepts-by-frequency combined-dict))


(mapv #(set/join % canonical/my-learned-definitions {:word :word}) (concepts-by-frequency combined-dict))