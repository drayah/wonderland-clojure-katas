(ns alphabet-cipher.coder
  (:require [clojure.string :as str]))

(def alphabet 
  "abcdefghijklmnopqrstuvwxyz")

(defn index-of
  "Returns index of character in alphabet"
  [alphabet character]
  (.indexOf alphabet (str character)))

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
  (let [index-c1 (index-of alphabet c1)
        index-c2 (index-of alphabet c2)]
    (->
      (rotate-alphabet alphabet index-c1)
      (rotate-alphabet index-c2)
      (first))))

(defn decode-character
  "Returns a decoded character given
  one character of a keyword and
  one character of an encoded message"
  [c1 c2]
  (let [rotated (rotate-alphabet alphabet (index-of alphabet c1))
        index-c2 (index-of rotated c2)
        decoded (nth alphabet index-c2)]
    decoded))

(defn decipher-character
  "Returns a keyword character given
  one character of a ciphertext and
  one character of an original message"
  [cipher-char message-char]
  (let [rotated (rotate-alphabet alphabet (index-of alphabet message-char))
        index (index-of rotated cipher-char)]
    (nth alphabet index)))

(declare encode)

(defn extract-keyword
  "Extracts a keyword given a sequence of repeated
  keyword characters a cipher and a message"
  [characters cipher message]
  (reduce (fn [result character] 
            (let [keyword (:keyword result)
                  done (:done result)]
              (if done
                (reduced (str/join keyword))
                (if (= (first keyword) character) 
                  (let [test-keyword (str/join keyword)
                        encoded (encode test-keyword message)]
                    (if (= encoded cipher) 
                      (assoc result :done true)
                      (assoc result :keyword (conj keyword character))))
                  (assoc result :keyword (conj keyword character)))))) {:done false :keyword []} characters))

(defn encode-decode
  "Returns an encoded or decoded string given
  an encoder or decoder function and a sequence 
  of interleaved characters"
  [f coll]
  (let [pairs (partition 2 coll)]
    (->
      (map #(f (first %) (second %)) pairs)
      (str/join))))

(defn encode 
  "Encodes a message given a keyword"
  [keyword message]
  (->>
    (interleave (cycle keyword) message)
    (encode-decode encode-character)))

(defn decode 
  "Decodes a message given a keyword" 
  [keyword message]
  (->>
    (interleave (cycle keyword) message)
    (encode-decode decode-character)))

(defn decipher 
  "Decipher used keyword given ciphertext and original message"
  [cipher message]
  (let [cipher-seq (seq cipher)
        message-seq (seq message)
        keyword-seq (map decipher-character cipher-seq message-seq)]
    (extract-keyword keyword-seq cipher message)))