(ns stenojure.predicates
  (:require [clojure.string :as str]))


(defn includes-number? [s]
  (some? (re-find #"[0-9#]" s)))


(defn all-lowercase? [s]
  (= s (str/lower-case s)))


(defn includes-plover-command? [s]
  (and (str/includes? s "{")
       (str/includes? s "}")))


(defn includes-plover-attachment? [s]
  (or (and (str/includes? s "{^") (str/includes? s "}"))
      (and (str/includes? s "{") (str/includes? s "^}"))))


(defn multiple-strokes? [s]
  (str/includes? s "/"))


(defn includes-plover-glue-command? [s]
  (or
    (and (str/includes? s "{&") (str/includes? s "}"))
    (and (str/includes? s "{") (str/includes? s "&}"))))


(defn includes-plover-capitalize-command? [s]
  (or
    (and
      (str/includes? s "{")
      (str/includes? s "~|")
      (str/includes? s "}"))
    (and
      (str/includes? s "{")
      (str/includes? s "-|")
      (str/includes? s "}"))))


(defn includes-plover-uppercase-command? [s]
  (str/includes? s "{<}"))


(defn includes-plover-lowercase-command? [s]
  (str/includes? s "{>}"))


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


(defn includes-uppercase-letters? [s]
  (some? (re-find #"[A-Z]" s)))