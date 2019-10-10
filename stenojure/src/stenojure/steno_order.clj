(ns stenojure.steno-order)


(defn steno-order-left [c]
  (condp = c
    \S 1
    \T 2
    \K 3
    \P 4
    \W 5
    \H 6
    \R 7
    8))


(defn steno-order-vowels [c]
  (condp = c
    \A 8
    \O 9
    \* 10
    \E 12
    \U 13
    14))


(defn steno-order-right [c]
  (condp = c
    \- 11
    \F 14
    \R 15
    \P 16
    \B 17
    \L 18
    \G 19
    \T 20
    \S 21
    \D 22
    \Z 23
    \/ 24
    25))


(defn steno-comparator-left-val [x y]
  (cond
    (identical? (val x) (val y)) 0
    (nil? (val x)) -1
    (empty? (val x)) -1
    (nil? (val y)) 1
    (empty? (val y)) 1

    (< (steno-order-left (first (val x))) (steno-order-left (first (val y)))) -1
    (> (steno-order-left (first (val x))) (steno-order-left (first (val y)))) 1
    :else (steno-comparator-left-val (rest (val x)) (rest (val y)))))


(defn steno-comparator-left [x y]
  (cond
    (identical? x y) 0
    (nil? x) -1
    (empty? x) -1
    (nil? y) 1
    (empty? y) 1

    (< (steno-order-left (first x)) (steno-order-left (first y))) -1
    (> (steno-order-left (first x)) (steno-order-left (first y))) 1
    :else (steno-comparator-left (rest x) (rest y))))


(defn steno-comparator-vowels [x y]
  (cond
    (identical? x y) 0
    (nil? x) -1
    (empty? x) -1
    (nil? y) 1
    (empty? y) 1

    (< (steno-order-vowels (first x)) (steno-order-vowels (first y))) -1
    (> (steno-order-vowels (first x)) (steno-order-vowels (first y))) 1
    :else (steno-comparator-vowels (rest x) (rest y))))


(defn steno-comparator-right [x y]
  (cond
    (identical? x y) 0
    (nil? x) -1
    (empty? x) -1
    (nil? y) 1
    (empty? y) 1

    (< (steno-order-right (first x)) (steno-order-right (first y))) -1
    (> (steno-order-right (first x)) (steno-order-right (first y))) 1
    :else (steno-comparator-right (rest x) (rest y))))