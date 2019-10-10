(ns stenojure.frequency
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [stenojure.steno-order :as order]
            [stenojure.predicates :as pred]
            [clojure.set :as set]))
(comment (spit "resources/norvig/common-words.txt" (slurp "http://norvig.com/google-books-common-words.txt")))


(defn replace-numbers [stroke]
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
               (str/replace "9" "T"))))


(defn make-vowels [inner]
  (cond
    (empty? inner) ""
    (= "-" (first inner)) ""
    :else (first inner)))





(defn split-stroke [stroke]
  (let [stroke (if (pred/includes-number? stroke) (replace-numbers stroke) stroke)
        outer (str/split stroke #"\*|-|A|O|E|U")
        inner (str/split (str/replace stroke #"\*" "") #"S|T|K|P|W|R|H|R|F|B|L|G|D|Z")
        asterisk? (str/includes? stroke "*")
        number? (str/includes? stroke "#")
        left (str/replace (if (not (empty? (first outer)))
                            (first outer)

                            "") #"\#" "")
        right (str/replace (if (< 1 (count outer))
                             (last outer)
                             "") #"\#" "")
        vowels (str/replace (->> inner
                                 (remove #(empty? %))
                                 make-vowels)
                            #"\#" "")]
    {:stroke    stroke
     :left      left
     :vowels    vowels
     :right     right
     :asterisk? asterisk?
     :number?   number?
     }))

(defn key-count [stroke]
  {:S-  (if (str/includes? (:left stroke) "S") 1 0)
   :T-  (if (str/includes? (:left stroke) "T") 1 0)
   :K-  (if (str/includes? (:left stroke) "K") 1 0)
   :P-  (if (str/includes? (:left stroke) "P") 1 0)
   :W-  (if (str/includes? (:left stroke) "W") 1 0)
   :H-  (if (str/includes? (:left stroke) "H") 1 0)
   :R-  (if (str/includes? (:left stroke) "R") 1 0)
   :A   (if (str/includes? (:vowels stroke) "A") 1 0)
   :O   (if (str/includes? (:vowels stroke) "O") 1 0)
   :E   (if (str/includes? (:vowels stroke) "E") 1 0)
   :U   (if (str/includes? (:vowels stroke) "U") 1 0)
   :AO  (if (and (str/includes? (:vowels stroke) "A")
                 (str/includes? (:vowels stroke) "O")) 1 0)
   :EU  (if (and (str/includes? (:vowels stroke) "E")
                 (str/includes? (:vowels stroke) "U")) 1 0)
   :S-F (if (and (str/includes? (:left stroke) "S")
                 (str/includes? (:right stroke) "F")) 1 0)
   :S-R (if (and (str/includes? (:left stroke) "S")
                 (str/includes? (:right stroke) "R")) 1 0)
   :-FR (if (and (str/includes? (:right stroke) "F")
                 (str/includes? (:right stroke) "R")) 1 0)
   :SH  (if (and (str/includes? (:left stroke) "S")
                 (str/includes? (:left stroke) "H")) 1 0)
   :SR  (if (and (str/includes? (:left stroke) "S")
                 (str/includes? (:left stroke) "R")) 1 0)

   :PH  (if (and (str/includes? (:left stroke) "P")
                 (str/includes? (:left stroke) "H")) 1 0)

   :PR  (if (and (str/includes? (:left stroke) "P")
                 (str/includes? (:left stroke) "R")) 1 0)

   :WH  (if (and (str/includes? (:left stroke) "W")
                 (str/includes? (:left stroke) "H")) 1 0)

   :WR  (if (and (str/includes? (:left stroke) "W")
                 (str/includes? (:left stroke) "R")) 1 0)

   :TH  (if (and (str/includes? (:left stroke) "T")
                 (str/includes? (:left stroke) "H")) 1 0)


   :*   (if (:asterisk? stroke) 1 0)
   :#   (if (:number? stroke) 1 0)
   :-F  (if (str/includes? (:right stroke) "F") 1 0)
   :-R  (if (str/includes? (:right stroke) "R") 1 0)
   :-P  (if (str/includes? (:right stroke) "P") 1 0)
   :-B  (if (str/includes? (:right stroke) "B") 1 0)
   :-L  (if (str/includes? (:right stroke) "L") 1 0)
   :-G  (if (str/includes? (:right stroke) "G") 1 0)
   :-T  (if (str/includes? (:right stroke) "T") 1 0)
   :-S  (if (str/includes? (:right stroke) "S") 1 0)
   :-D  (if (str/includes? (:right stroke) "D") 1 0)
   :-Z  (if (str/includes? (:right stroke) "Z") 1 0)
   :H*  (if (and (:asterisk? stroke)
                 (str/includes? (:left stroke) "H")) 1 0)
   :R*  (if (and (:asterisk? stroke)
                 (str/includes? (:left stroke) "R")) 1 0)

   :*F  (if (and (:asterisk? stroke)
                 (str/includes? (:right stroke) "F")) 1 0)
   :*R  (if (and (:asterisk? stroke)
                 (str/includes? (:right stroke) "R")) 1 0)})


(defn definition-key-count [dict-entry]
  (vector (first dict-entry)
          (last dict-entry)
          (apply merge-with + (map key-count (first dict-entry)))))



(defn split-strokes [outline]
  (->> (str/split outline #"\/")
       (mapv split-stroke)))


(defn read-steno-dictionary [file-path]
  (->> (slurp file-path)
       (json/read-str)
       (mapv #(hash-map (str/lower-case (last %)) (vector (first %))))
       (apply merge-with concat)
       (mapv #(hash-map (str/lower-case (key %))
                        {:outlines (vector (val %))}))
       (into #{})))


(defn read-word-frequency [file-path]
  (->> (slurp file-path)
       (str/split-lines)
       (mapv #(str/split % #"\t"))
       (mapv #(hash-map (str/lower-case (first %))
                        {:frequency (Long/parseLong (last %))}))
       (into #{})))

(def main-dict (read-steno-dictionary "resources/plover/main.json"))

(def word-frequency (read-word-frequency "resources/norvig/common-words.txt"))


(merge-with conj [{"b" {:b 1}}] [{"b" {:c 2}}])

(def combined-lists
  (merge word-frequency main-dict)
  )

(->> combined-lists (take 100))

(->> word-frequency
     (take 2))

(->> main-dict (take 2))

(def key-count-sum (apply merge-with + (->> main-dict
                                            #_(take 2)
                                            (mapv #(vector (split-strokes (first %)) (last %)))
                                            (mapv definition-key-count)
                                            (mapv last)
                                            )))

(->> key-count-sum
     (mapv #(vector (first %) (quot (last %) 100)))
     )