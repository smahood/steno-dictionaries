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

(def combined-dict (plover/combine-plover-cmu-freq-dictionaries plover-main-dict cmu-dict norvig-freq-dict))

(->> combined-dict
     (sort-by :frequency)
     reverse
     (take 10))



