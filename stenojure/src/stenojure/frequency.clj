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


(defn key-count [stroke freq]
  {
   :S- (if (str/includes? (:left stroke) "S") freq 0)
   :T- (if (str/includes? (:left stroke) "T") freq 0)
   :K- (if (str/includes? (:left stroke) "K") freq 0)
   :P- (if (str/includes? (:left stroke) "P") freq 0)
   :W- (if (str/includes? (:left stroke) "W") freq 0)
   :H- (if (str/includes? (:left stroke) "H") freq 0)
   :R- (if (str/includes? (:left stroke) "R") freq 0)
   :A  (if (str/includes? (:vowels stroke) "A") freq 0)
   :O  (if (str/includes? (:vowels stroke) "O") freq 0)
   :E  (if (str/includes? (:vowels stroke) "E") freq 0)
   :U  (if (str/includes? (:vowels stroke) "U") freq 0)


   :*  (if (:asterisk? stroke) freq 0)
   :#  (if (:number? stroke) freq 0)
   :-F (if (str/includes? (:right stroke) "F") freq 0)
   :-R (if (str/includes? (:right stroke) "R") freq 0)
   :-P (if (str/includes? (:right stroke) "P") freq 0)
   :-B (if (str/includes? (:right stroke) "B") freq 0)
   :-L (if (str/includes? (:right stroke) "L") freq 0)
   :-G (if (str/includes? (:right stroke) "G") freq 0)
   :-T (if (str/includes? (:right stroke) "T") freq 0)
   :-S (if (str/includes? (:right stroke) "S") freq 0)
   :-D (if (str/includes? (:right stroke) "D") freq 0)
   :-Z (if (str/includes? (:right stroke) "Z") freq 0)
   :ST (if (and (str/includes? (:left stroke) "S")
                (str/includes? (:left stroke) "T")) freq 0)
   :SK (if (and (str/includes? (:left stroke) "S")
                (str/includes? (:left stroke) "K")) freq 0)
   :SP (if (and (str/includes? (:left stroke) "S")
                (str/includes? (:left stroke) "P")) freq 0)
   :SW (if (and (str/includes? (:left stroke) "S")
                (str/includes? (:left stroke) "W")) freq 0)
   :SH (if (and (str/includes? (:left stroke) "S")
                (str/includes? (:left stroke) "H")) freq 0)
   :SR (if (and (str/includes? (:left stroke) "S")
                (str/includes? (:left stroke) "R")) freq 0)
   :S* (if (and (str/includes? (:left stroke) "S")
                (:asterisk? stroke)) freq 0)

   :TK (if (and (str/includes? (:left stroke) "T")
                (str/includes? (:left stroke) "K")) freq 0)
   :TP (if (and (str/includes? (:left stroke) "T")
                (str/includes? (:left stroke) "P")) freq 0)
   :TW (if (and (str/includes? (:left stroke) "T")
                (str/includes? (:left stroke) "W")) freq 0)
   :TH (if (and (str/includes? (:left stroke) "T")
                (str/includes? (:left stroke) "H")) freq 0)
   :TR (if (and (str/includes? (:left stroke) "T")
                (str/includes? (:left stroke) "R")) freq 0)
   :T* (if (and (str/includes? (:left stroke) "T")
                (:asterisk? stroke)) freq 0)

   :KP (if (and (str/includes? (:left stroke) "K")
                (str/includes? (:left stroke) "P")) freq 0)
   :KW (if (and (str/includes? (:left stroke) "K")
                (str/includes? (:left stroke) "W")) freq 0)
   :KH (if (and (str/includes? (:left stroke) "K")
                (str/includes? (:left stroke) "H")) freq 0)
   :KR (if (and (str/includes? (:left stroke) "K")
                (str/includes? (:left stroke) "R")) freq 0)
   :K* (if (and (str/includes? (:left stroke) "K")
                (:asterisk? stroke)) freq 0)

   :PW (if (and (str/includes? (:left stroke) "P")
                (str/includes? (:left stroke) "W")) freq 0)
   :PH (if (and (str/includes? (:left stroke) "P")
                (str/includes? (:left stroke) "H")) freq 0)
   :PR (if (and (str/includes? (:left stroke) "P")
                (str/includes? (:left stroke) "R")) freq 0)
   :P* (if (and (str/includes? (:left stroke) "P")
                (:asterisk? stroke)) freq 0)

   :WH (if (and (str/includes? (:left stroke) "W")
                (str/includes? (:left stroke) "H")) freq 0)
   :WR (if (and (str/includes? (:left stroke) "W")
                (str/includes? (:left stroke) "R")) freq 0)
   :W* (if (and (str/includes? (:left stroke) "W")
                (:asterisk? stroke)) freq 0)

   :HR (if (and (str/includes? (:left stroke) "H")
                (str/includes? (:left stroke) "R")) freq 0)
   :H* (if (and (str/includes? (:left stroke) "H")
                (:asterisk? stroke)) freq 0)

   :R* (if (and (str/includes? (:left stroke) "R")
                (:asterisk? stroke)) freq 0)











   ;
   ;
   ;:H*  (if (and (:asterisk? stroke)
   ;              (str/includes? (:left stroke) "H")) freq 0)
   ;:R*  (if (and (:asterisk? stroke)
   ;              (str/includes? (:left stroke) "R")) freq 0)
   ;
   ;:*F  (if (and (:asterisk? stroke)
   ;              (str/includes? (:right stroke) "F")) freq 0)
   ;:*R  (if (and (:asterisk? stroke)
   ;              (str/includes? (:right stroke) "R")) freq 0)
   ;:AO  (if (and (str/includes? (:vowels stroke) "A")
   ;              (str/includes? (:vowels stroke) "O")) freq 0)
   ;:EU  (if (and (str/includes? (:vowels stroke) "E")
   ;              (str/includes? (:vowels stroke) "U")) freq 0)
   ;:S-F (if (and (str/includes? (:left stroke) "S")
   ;              (str/includes? (:right stroke) "F")) freq 0)
   ;:S-R (if (and (str/includes? (:left stroke) "S")
   ;              (str/includes? (:right stroke) "R")) freq 0)
   ;:-FR (if (and (str/includes? (:right stroke) "F")
   ;              (str/includes? (:right stroke) "R")) freq 0)
   ;:SH  (if (and (str/includes? (:left stroke) "S")
   ;              (str/includes? (:left stroke) "H")) freq 0)
   ;:SR  (if (and (str/includes? (:left stroke) "S")
   ;              (str/includes? (:left stroke) "R")) freq 0)
   ;
   ;:PH  (if (and (str/includes? (:left stroke) "P")
   ;              (str/includes? (:left stroke) "H")) freq 0)
   ;
   ;:PR  (if (and (str/includes? (:left stroke) "P")
   ;              (str/includes? (:left stroke) "R")) freq 0)
   ;
   ;:WH  (if (and (str/includes? (:left stroke) "W")
   ;              (str/includes? (:left stroke) "H")) freq 0)
   ;
   ;:WR  (if (and (str/includes? (:left stroke) "W")
   ;              (str/includes? (:left stroke) "R")) freq 0)
   ;
   ;:TH  (if (and (str/includes? (:left stroke) "T")
   ;              (str/includes? (:left stroke) "H")) freq 0)

   })




(defn definition-key-count [dict-entry]
  (vector (first dict-entry)
          (last dict-entry)
          (apply merge-with + (map key-count (first dict-entry)))))



(defn split-strokes [outline]
  (->> (str/split outline #"\/")
       (mapv split-stroke)))


(defn shortest-outline [outlines]
  (->> outlines
       (map #(hash-map :outline % :length (count %)))
       (apply min-key :length)
       :outline))

(defn read-steno-dictionary [file-path]
  (->> (slurp file-path)
       (json/read-str)
       (mapv #(hash-map (str/lower-case (last %)) (vector (first %))))
       (apply merge-with into)
       (mapv #(hash-map :word (str/lower-case (key %))
                        :outline (shortest-outline (val %))))
       (into #{})))


(defn read-word-frequency [file-path]
  (->> (slurp file-path)
       (str/split-lines)
       (mapv #(str/split % #"\t"))
       (mapv #(hash-map (str/lower-case (first %))
                        (Long/parseLong (last %))))
       (apply merge)))



(defn combine-word [plover-definition frequencies]
  (let [word (:word plover-definition)
        freq (get frequencies word)
        strokes (split-strokes (:outline plover-definition))]
    (when-not (nil? freq)
      (let [key-counts (map #(key-count % freq) strokes)]
        (assoc plover-definition
          :frequency freq
          :strokes strokes
          :key-counts key-counts
          :sum-key-counts (apply merge-with + key-counts))))))


(def main-dict (read-steno-dictionary "resources/plover/main.json"))
(def word-frequencies (read-word-frequency "resources/norvig/common-words.txt"))

(defn combine-lists [steno-dict frequencies]
  (->> steno-dict
       (mapv #(combine-word % frequencies))
       (remove nil?)))

(defn update-vals [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

#_(let [combined-lists (combine-lists main-dict word-frequencies)

        ]
    (->> combined-lists
         (mapcat :strokes)
         (map :left)
         (distinct)
         (filter #(= 2 (count %)))
         (map keyword)))



(let [combined-lists (combine-lists main-dict word-frequencies)
      key-frequency-sum (->> combined-lists
                             (mapv :sum-key-counts)
                             (apply merge-with +))
      min-sum 1337087500 #_(apply min (vals key-frequency-sum))
      adjusted-freq (update-vals key-frequency-sum #(quot % (quot min-sum 10)))
      ]

  {:min-sum     min-sum
   :frequencies (->> adjusted-freq
                     (filter #(#{:S-
                                 :SH
                                 :SW
                                 :ST
                                 :SK
                                 :SP
                                 :SR
                                 :S*

                                 :T-
                                 :TK
                                 :TP
                                 :TW
                                 :TH
                                 :TR
                                 :T*

                                 :K-
                                 :KH
                                 :KP
                                 :KW
                                 :KR
                                 :K*

                                 :P-
                                 :PW
                                 :PH
                                 :PR
                                 :P*

                                 :W-
                                 :WH
                                 :WR
                                 :W*

                                 :H-
                                 :HR
                                 :H*

                                 :R-
                                 :R*

                                 :*}
                               (first %)))
                     (sort-by key)
                     )}

  )

(let [shortest (->> [[:* 273]
                     [:H* 63]
                     [:H- 1427]
                     [:HR 352]
                     [:K* 49]
                     [:K- 893]
                     [:KH 91]
                     [:KP 111]
                     [:KR 232]
                     [:KW 191]
                     [:P* 75]
                     [:P- 1499]
                     [:PH 648]
                     [:PR 261]
                     [:PW 280]
                     [:R* 65]
                     [:R- 1108]
                     [:S* 43]
                     [:S- 895]
                     [:SH 78]
                     [:SK 252]
                     [:SP 95]
                     [:SR 187]
                     [:ST 269]
                     [:SW 74]
                     [:T* 60]
                     [:T- 1458]
                     [:TH 471]
                     [:TK 393]
                     [:TP 604]
                     [:TR 179]
                     [:TW 108]
                     [:W* 38]
                     [:W- 770]
                     [:WH 55]
                     [:WR 192]]
                    (mapv #(hash-map (first %) {:shortest-outline (last %)})))
      longest (->> [[:* 923]
                    [:H* 187]
                    [:H- 1884]
                    [:HR 610]
                    [:K* 101]
                    [:K- 1110]
                    [:KH 105]
                    [:KP 150]
                    [:KR 461]
                    [:KW 452]
                    [:P* 226]
                    [:P- 2170]
                    [:PH 892]
                    [:PR 423]
                    [:PW 379]
                    [:R* 104]
                    [:R- 1609]
                    [:S* 78]
                    [:S- 1073]
                    [:SH 95]
                    [:SK 140]
                    [:SP 232]
                    [:SR 259]
                    [:ST 128]
                    [:SW 96]
                    [:T* 182]
                    [:T- 2017]
                    [:TH 780]
                    [:TK 376]
                    [:TP 1105]
                    [:TR 360]
                    [:TW 154]
                    [:W* 113]
                    [:W- 1080]
                    [:WH 152]
                    [:WR 420]]
                   (mapv #(hash-map (first %) {:longest-outline (last %)})))]
  (apply merge-with conj (concat shortest longest))

  )




(comment
  "based on min-key"
  {:min-sum 1337087500, :frequencies ([:* 273]
                                      [:H* 63]
                                      [:H- 1427]
                                      [:HR 352]
                                      [:K* 49]
                                      [:K- 893]
                                      [:KH 91]
                                      [:KP 111]
                                      [:KR 232]
                                      [:KW 191]
                                      [:P* 75]
                                      [:P- 1499]
                                      [:PH 648]
                                      [:PR 261]
                                      [:PW 280]
                                      [:R* 65]
                                      [:R- 1108]
                                      [:S* 43]
                                      [:S- 895]
                                      [:SH 78]
                                      [:SK 252]
                                      [:SP 95]
                                      [:SR 187]
                                      [:ST 269]
                                      [:SW 74]
                                      [:T* 60]
                                      [:T- 1458]
                                      [:TH 471]
                                      [:TK 393]
                                      [:TP 604]
                                      [:TR 179]
                                      [:TW 108]
                                      [:W* 38]
                                      [:W- 770]
                                      [:WH 55]
                                      [:WR 192])}
  "based on max-key"
  {:min-sum 1337087500, :frequencies ([:* 923]
                                      [:H* 187]
                                      [:H- 1884]
                                      [:HR 610]
                                      [:K* 101]
                                      [:K- 1110]
                                      [:KH 105]
                                      [:KP 150]
                                      [:KR 461]
                                      [:KW 452]
                                      [:P* 226]
                                      [:P- 2170]
                                      [:PH 892]
                                      [:PR 423]
                                      [:PW 379]
                                      [:R* 104]
                                      [:R- 1609]
                                      [:S* 78]
                                      [:S- 1073]
                                      [:SH 95]
                                      [:SK 140]
                                      [:SP 232]
                                      [:SR 259]
                                      [:ST 128]
                                      [:SW 96]
                                      [:T* 182]
                                      [:T- 2017]
                                      [:TH 780]
                                      [:TK 376]
                                      [:TP 1105]
                                      [:TR 360]
                                      [:TW 154]
                                      [:W* 113]
                                      [:W- 1080]
                                      [:WH 152]
                                      [:WR 420])}


  {:KW {:shortest-outline 191 :longest-outline 452}
   :TP {:shortest-outline 604 :longest-outline 1105}
   :T* {:shortest-outline 60 :longest-outline 182}
   :SK {:shortest-outline 252 :longest-outline 140}
   :SH {:shortest-outline 78 :longest-outline 95}
   :*  {:shortest-outline 273 :longest-outline 923}
   :HR {:shortest-outline 352 :longest-outline 610}
   :K* {:shortest-outline 49 :longest-outline 101}
   :PR {:shortest-outline 261 :longest-outline 423}
   :WR {:shortest-outline 192 :longest-outline 420}
   :ST {:shortest-outline 269 :longest-outline 128}
   :KP {:shortest-outline 111 :longest-outline 150}
   :KH {:shortest-outline 91 :longest-outline 105}
   :W* {:shortest-outline 38 :longest-outline 113}
   :SP {:shortest-outline 95 :longest-outline 232}
   :W- {:shortest-outline 770 :longest-outline 1080}
   :SW {:shortest-outline 74 :longest-outline 96}
   :R* {:shortest-outline 65 :longest-outline 104}
   :P* {:shortest-outline 75 :longest-outline 226}
   :SR {:shortest-outline 187 :longest-outline 259}
   :H* {:shortest-outline 63 :longest-outline 187}
   :TH {:shortest-outline 471 :longest-outline 780}
   :WH {:shortest-outline 55 :longest-outline 152}
   :TR {:shortest-outline 179 :longest-outline 360}
   :PW {:shortest-outline 280 :longest-outline 379}
   :TW {:shortest-outline 108 :longest-outline 154}
   :S* {:shortest-outline 43 :longest-outline 78}
   :S- {:shortest-outline 895 :longest-outline 1073}
   :K- {:shortest-outline 893 :longest-outline 1110}
   :TK {:shortest-outline 393 :longest-outline 376}
   :H- {:shortest-outline 1427 :longest-outline 1884}
   :T- {:shortest-outline 1458 :longest-outline 2017}
   :P- {:shortest-outline 1499 :longest-outline 2170}
   :R- {:shortest-outline 1108 :longest-outline 1609}
   :KR {:shortest-outline 232 :longest-outline 461}
   :PH {:shortest-outline 648 :longest-outline 892}}
  )



:S- {:shortest-outline 895 :longest-outline 1073}
:T- {:shortest-outline 1458 :longest-outline 2017}
:SK {:shortest-outline 252 :longest-outline 140}
:SH {:shortest-outline 78 :longest-outline 95}