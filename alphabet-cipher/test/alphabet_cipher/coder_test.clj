(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer :all]
            [alphabet-cipher.coder :refer :all]))

(deftest test-index-of
  (testing "should return index of character in alphabet"
    (is (= 0
           (index-of "a")))
    (is (= 1
           (index-of "b")))
    (is (= 18
           (index-of "s")))
    (is (= 25
           (index-of "z")))))

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

(deftest test-encode-characters
  (testing "should encode a single character"
    (is (= \e
           (encode-character \s \m)))
    (is (= \g
           (encode-character \c \e)))
    (is (= \s
           (encode-character \o \e)))
    (is (= \g
           (encode-character \n \t)))))

(deftest test-encode-sequence
  (testing "should encode a sequence of interleaved characters"
    (is (= "egsg"
           (encode-sequence [\s \m \c \e \o \e \n \t])))))

(comment
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

  (deftest test-decipher
    (testing "can extract the secret keyword given an encrypted message and the original message"
      (is (= "vigilance"
            (decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog")))
      (is (= "scones"
            (decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs"))))))
