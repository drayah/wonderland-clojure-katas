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

(defn encode [keyword message]
  "encodeme")

(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")

