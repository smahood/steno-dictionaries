(ns stenojure.phonetic
  (:require [clojure.string :as str]))


(def arpabet-vowels
  #{"AA"
    "AE"
    "AH"
    "AO"
    "AW"
    "AX"
    "AXR"
    "AY"
    "EH"
    "ER"
    "EY"
    "IH"
    "IX"
    "IY"
    "OW"
    "OY"
    "UH"
    "UW"
    "UX"
    "a"
    "A"
    "c"
    "x"
    "Y"
    "E"
    "e"
    "I"
    "X"
    "i"
    "o"
    "O"
    "U"
    "u"})


(def arpabet-consonants
  #{"B"
    "CH"
    "D"
    "DH"
    "DX"
    "EL"
    "EM"
    "EN"
    "F"
    "G"
    "HH"
    "H"
    "JH"
    "K"
    "L"
    "M"
    "N"
    "NG"
    "NX"
    "P"
    "Q"
    "R"
    "S"
    "SH"
    "T"
    "TH"
    "V"
    "W"
    "WH"
    "Y"
    "Z"
    "ZH"})


(def ipa-vowels
  (->> ["ɑ" "æ" "ʌ" "ɔ" "aʊ" "ə" "ɚ" "aɪ" "ɛ" "ɝ" "eɪ"
        "ɪ" "ɨ" "i" "oʊ" "ɔɪ" "ʊ" "u" "ʉ" "j"]
       (str/join)
       (partition-all 1)
       (map str/join)
       (into #{})))


(def arpabet->ipa
  {"AA"  "ɑ"
   "AE"  "æ"
   "AH"  "ʌ"
   "AO"  "ɔ"
   "AW"  "aʊ"
   "AX"  "ə"
   "AXR" "ɚ"
   "AY"  "aɪ"
   "EH"  "ɛ"
   "ER"  "ɝ"
   "EY"  "eɪ"
   "IH"  "ɪ"
   "IX"  "ɨ"
   "IY"  "i"
   "OW"  "oʊ"
   "OY"  "ɔɪ"
   "UH"  "ʊ"
   "UW"  "u"
   "UX"  "ʉ"
   "B"   "b"
   "CH"  "tʃ"
   "D"   "d"
   "DH"  "ð"
   "DX"  "ɾ"
   "EL"  "l̩"
   "EM"  "m̩"
   "EN"  "n̩"
   "F"   "f"
   "G"   "ɡ"
   "HH"  "h"
   "H"   "h"
   "JH"  "dʒ"
   "K"   "k"
   "L"   "l"
   "M"   "m"
   "N"   "n"
   "NG"  "ŋ"
   "NX"  "ɾ̃"
   "P"   "p"
   "Q"   "ʔ"
   "R"   "ɹ"
   "S"   "s"
   "SH"  "ʃ"
   "T"   "t"
   "TH"  "θ"
   "V"   "v"
   "W"   "w"
   "WH"  "ʍ"
   "Y"   "j"
   "Z"   "z"
   "ZH"  "ʒ"})


(defn arpabet-sounds [s]
  (if (some? (arpabet-vowels (str s)))
    :vowel
    :consonant))


(defn ipa-sounds [s]
  (if (some? (ipa-vowels (str s)))
    :vowel
    :consonant))


(defn arpabet-phoneme-clusters [arpabet-phonemes]
  (->> arpabet-phonemes
       (partition-by #(arpabet-sounds %))
       (mapv str/join)))


(defn ipa-phoneme-clusters [ipa-phonemes]
  (->> ipa-phonemes
       (partition-by #(ipa-sounds %))
       (map str/join)))


(defn convert-to-ipa [arpabet]
  (let [unstressed (mapv #(str/replace % #"[012]" "") arpabet)
        ipa (str/join (mapv #(get arpabet->ipa %) unstressed))]
    ipa))