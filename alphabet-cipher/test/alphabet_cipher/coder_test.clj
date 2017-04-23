(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer :all]
            [alphabet-cipher.coder :refer :all]))

(deftest test-index-of
  (testing "should return index of character in alphabet"
    (is (= 0
           (index-of alphabet "a")))
    (is (= 1
           (index-of alphabet \b)))
    (is (= 18
           (index-of alphabet \s)))
    (is (= 25
           (index-of alphabet \z)))
    (is (= 25
           (index-of alphabet "z")))))

(deftest test-rotate-alphabet
  (testing "should slide an alphabet by a number of characters"
    (is (= "abcdefghijklmnopqrstuvwxyz"
           (rotate-alphabet alphabet 0)))
    (is (= "bcdefghijklmnopqrstuvwxyza"
           (rotate-alphabet alphabet 1)))
    (is (= "stuvwxyzabcdefghijklmnopqr" 
           (rotate-alphabet alphabet 18)))
    (is (= "zabcdefghijklmnopqrstuvwxy" 
           (rotate-alphabet alphabet 25)))
    (is (= "efghijklmnopqrstuvwxyzabcd"
           (rotate-alphabet "stuvwxyzabcdefghijklmnopqr" 12)))))

(deftest test-encode-character
  (testing "should encode a single character"
    (is (= \e
           (encode-character \s \m)))
    (is (= \g
           (encode-character \c \e)))
    (is (= \s
           (encode-character \o \e)))
    (is (= \g
           (encode-character \n \t)))))

(deftest test-decode-character
  (testing "should decode a single character"
    (is (= \m
           (decode-character \s \e)))
    (is (= \e
           (decode-character \c \g)))
    (is (= \e
           (decode-character \o \s)))))

(deftest test-decipher-character
  (testing "should decipher a single character"
    (is (= \s
           (decipher-character \e \m)))
    (is (= \c
           (decipher-character \g \e)))
    (is (= \o
           (decipher-character \s \e)))
    (is (= \n
           (decipher-character \g \t)))))

(deftest test-encode-decode
  (testing "should encode a sequence of interleaved characters"
    (is (= "egsg"
           (encode-decode encode-character [\s \m \c \e \o \e \n \t])))
    (is (= "meet"
           (encode-decode decode-character [\s \e \c \g \o \s \n \g])))))

(deftest test-encode
  (testing "can encode given a secret keyword"
    (is (= "hmkbxebpxpmyllyrxiiqtoltfgzzv"
          (encode "vigilance" "meetmeontuesdayeveningatseven")))
    (is (= "egsgqwtahuiljgs"
          (encode "scones" "meetmebythetree")))))

(deftest test-decode
  (testing "can decode an encrypted message given a secret keyword"
    (is (= "meetmeontuesdayeveningatseven"
          (decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv")))
    (is (= "meetmebythetree"
          (decode "scones" "egsgqwtahuiljgs")))))

(deftest test-extract-keyword
  (testing "should extract a keyword from a repeated keyword sequence"
    (is (= [\s \c \o \n \e \s]
           (:keyword (extract-keyword [\s \c \o \n \e \s \s \c] "egsgqwta" "meetmeby"))))))

(deftest test-decipher
  (testing "can extract the secret keyword given an encrypted message and the original message"
    (is (= "vigilance"
           (decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog")))
    (is (= "scones"
           (decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs")))))
