(ns stenojure.predicates
  (:require [clojure.string :as str]))


(defn includes-number? [s]
  (let [includes? (partial str/includes? s)]
    (or (includes? "#")
        (includes? "1")
        (includes? "2")
        (includes? "3")
        (includes? "4")
        (includes? "5")
        (includes? "6")
        (includes? "7")
        (includes? "8")
        (includes? "9")
        (includes? "0"))))


(defn all-lowercase? [s]
  (= s (str/lower-case s)))


(defn includes-plover-attachment? [s]
  (or (str/includes? s "{^")
      (str/includes? s "^}")))


(defn includes-plover-command? [s]
  (and (str/includes? s "{")
       (str/includes? s "}")))


(defn includes-symbols? [s]
  (or (str/includes? s "`")
      (str/includes? s "~")
      (str/includes? s "!")
      (str/includes? s "@")
      (str/includes? s "#")
      (str/includes? s "$")
      (str/includes? s "%")
      (str/includes? s "^")
      (str/includes? s "&")
      (str/includes? s "*")
      (str/includes? s "(")
      (str/includes? s ")")
      (str/includes? s "-")
      (str/includes? s "_")
      (str/includes? s "=")
      (str/includes? s "+")
      (str/includes? s "[")
      (str/includes? s "{")
      (str/includes? s "]")
      (str/includes? s "}")
      (str/includes? s "\\")
      (str/includes? s "|")
      (str/includes? s ";")
      (str/includes? s ":")
      (str/includes? s "'")
      (str/includes? s "\"")
      (str/includes? s ",")
      (str/includes? s "<")
      (str/includes? s ".")
      (str/includes? s ">")
      (str/includes? s "/")
      (str/includes? s "?")
      (str/includes? s "÷")
      (str/includes? s "–")
      (str/includes? s "—")))
