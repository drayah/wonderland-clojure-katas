(ns wonderland-number.finder)

(def candidates (range 100000 1000000))

(defn same
  "Given numbers return true if they have 
  the same digits irrespective of order"
  [& nums]
  (let [sets (map #(set (str %)) nums)]
    (apply = sets)))

(defn wonderland-number
  "Generate wonderland nubmer"
  []
  (let [number (for [n candidates
                     :let [n2 (* 2 n)
                           n3 (* 3 n)
                           n4 (* 4 n)
                           n5 (* 5 n)
                           n6 (* 6 n)]
                     :when (same n n2 n3 n4 n5 n6)]
                  n)]
    (first number)))