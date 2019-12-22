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

(def digit-pairs #(partition 2 1 (digits-seq %)))

(defn two-adjacent-digits-equal?
  [number]
  (some #(when (apply = %) %) (digit-pairs number)))

(deftest two-adjacent-digits-equal-test
  (is (two-adjacent-digits-equal? 111111))
  (is (two-adjacent-digits-equal? 223450))
  (is (not (two-adjacent-digits-equal? 123789))))

(defn increasing-digits?
  "True when the sequence of digits in `number` are monotonically increasing."
  [number]
  (every? #(apply <= %) (digit-pairs number)))

(deftest increasing-digits-test
  (is (increasing-digits? 111111))
  (is (not (increasing-digits? 223450)))
  (is (increasing-digits? 123789)))

(def is-valid-number? (every-pred two-adjacent-digits-equal?
                                  increasing-digits?))

(defn valid-numbers
  "Generates a sequence of the valid numbers between
  the inclusive `lower` and `upper` bounds."
  [lower upper]
  (let [inclusive (update input 1 inc)]
    (filter is-valid-number? (apply range input))))

(defn part-one-solution
  [input]
  (count (apply valid-numbers input)))
