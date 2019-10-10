(ns stenojure.cmudict
  (:require [clojure.string :as str]
            [stenojure.phonetic :as phonetic]))


(def cmudict-xf
  (comp
    (remove #(str/starts-with? % ";;;"))
    (filter #(re-matches #"[A-Z]+.*" %))
    (map #(str/split % #" "))
    (map #(remove empty? %))
    (map #(vector (str/replace (first %) #"\([0-9]\)" "")
                  (into [] (rest %))))
    (remove #(re-matches #".*[0-9]+.*" (first %)))
    (remove #(str/includes? % "."))
    (remove #(str/includes? % "'"))
    (map #(hash-map (str/lower-case (first %))
                    (mapv (fn [x] (phonetic/convert-to-ipa x)) (rest %))))))


(defn load-cmudict [file-path]
  (->> (slurp file-path)
       str/split-lines
       (transduce cmudict-xf conj)
       (apply merge-with concat)))