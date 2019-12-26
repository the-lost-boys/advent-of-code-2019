(ns justin.core
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

;; * Part one

(def parse-integer #?(:clj #(java.lang.Integer/parseInt %)
                      :cljs #(js/parseInt %)))

(def input (vec (map parse-integer (re-seq #"\d+" "240298-784956"))))

(defn digits-seq
  [number]
  (map (comp parse-integer str) (seq (str number))))

(defn two-adjacent-digits-equal?
  [number]
  (some #(>= (count %) 2) (partition-by identity (digits-seq number))))

(deftest two-adjacent-digits-equal-test
  (is (two-adjacent-digits-equal? 111111))
  (is (two-adjacent-digits-equal? 223450))
  (is (not (two-adjacent-digits-equal? 123789))))

(defn increasing-digits?
  "True when the sequence of digits in `number` are monotonically increasing."
  [number]
  (every? #(apply <= %) (partition 2 1 (digits-seq number))))

(deftest increasing-digits-test
  (is (increasing-digits? 111111))
  (is (not (increasing-digits? 223450)))
  (is (increasing-digits? 123789)))

(def is-valid-number-for-part-one? (every-pred two-adjacent-digits-equal?
                                               increasing-digits?))

(defn valid-numbers-for-part-one
  "Generates a sequence of the valid numbers between
  the inclusive `lower` and `upper` bounds."
  [lower upper]
  (filter is-valid-number-for-part-one? (range lower (inc upper))))

(defn part-one-solution
  [input]
  (count (apply valid-numbers-for-part-one input)))

(deftest part-one-solution-test
  (is (= 1150 (part-one-solution input))))

;; * Part two

(defn exactly-two-adjacent-digits-equal?
  [number]
  (some #(= (count %) 2) (partition-by identity (digits-seq number))))

(deftest exactly-two-adjacent-digits-equal-test
  (is (exactly-two-adjacent-digits-equal? 112233))
  (is (not (exactly-two-adjacent-digits-equal? 123444)))
  (is (exactly-two-adjacent-digits-equal? 111122)))

(def is-valid-number-for-part-two? (every-pred exactly-two-adjacent-digits-equal?
                                               increasing-digits?))

(defn valid-numbers-for-part-two
  [lower upper]
  (filter is-valid-number-for-part-two? (range lower (inc upper))))

(defn part-two-solution
  [input]
  (count (apply valid-numbers-for-part-two input)))

(deftest part-two-solution-test
  (is (= 748 (part-two-solution input))))
