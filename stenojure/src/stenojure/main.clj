(ns stenojure.main
  (:require [clojure.data.json :as json]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :as str]
            [stenojure.dictionaries.clojure-core]))


(defn format-custom-lesson
  "Format dictionary for a custom typey-type lesson"
  [dict]
  (->> dict
       (remove #(empty? (key %)))
       (remove #(empty? (val %)))
       (map #(str/join "\t" %))
       (str/join "\n")))


(defn format-dictionary
  "Format dictionary to writable JSON string for use in Plover"
  [dict]
  (let [splitter #(str/split % (re-pattern "\",\""))
        filtered-dict (->> dict
                           (remove #(empty? (key %)))
                           (remove #(empty? (val %)))
                           (map reverse)

                           (into (priority-map)))]

    (->> (json/write-str filtered-dict :escape-slash false)
         splitter
         (str/join "\",\n\""))))


(spit "clojure-core.txt" (format-custom-lesson stenojure.dictionaries.clojure-core/clojure-core-dictionary))

(spit "clojure-core.json" (format-dictionary stenojure.dictionaries.clojure-core/clojure-core-dictionary))

