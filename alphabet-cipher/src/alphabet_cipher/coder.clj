(ns alphabet-cipher.coder
  (:require [clojure.string :as str]))

(def alphabet 
  "abcdefghijklmnopqrstuvwxyz")

(defn index-of
  "Returns index of character in alphabet"
  [character]
  (.indexOf alphabet character))

(defn rotate-alphabet
  "Rotate alphabet by n characters"
  [alphabet n]
  (let [length (count alphabet)]
    (->>
      (cycle alphabet)
      (drop n)
      (take length)
      (str/join))))

(defn encode-character
  "Returns an encoded character given 
  one character of a keyword and 
  one character of a message"
  [c1 c2]
  (let [index-c1 (index-of (str c1))
        index-c2 (index-of (str c2))]
    (->
      (rotate-alphabet alphabet index-c1)
      (rotate-alphabet index-c2)
      (first))))

(defn encode-sequence
  "Returns an encoded string given
  a sequence of interleaved characters"
  [coll]
  (let [pairs (partition 2 coll)]
    (->
      (map #(encode-character (first %) (second %)) pairs)
      (str/join))))

(defn encode [keyword message]
  "encodeme")

(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")

