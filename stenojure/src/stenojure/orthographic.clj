(ns stenojure.orthographic)


(defn split-words [phrase]
  (re-seq #"\w+" phrase))


(defn split-clusters [word]
  "Splits a word into vowel and consonant chunks (only handles lowercase simple words)"
  (->> word
       (re-seq #"[aeiou]+|[bcdfghjklmnpqrstvwxz]+|[y]+")
       (into [])))
