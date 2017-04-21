(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer :all]
            [alphabet-cipher.coder :refer :all]))

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
