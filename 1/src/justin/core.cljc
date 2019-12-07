(ns justin.core
  (:require #?(:clj [clojure.test :refer [is]]
               :cljs [cljs.test :refer-macros [is]])))

;; * Part one

(def input (->> "src/justin/input.txt"
                slurp
                clojure.string/split-lines
                (mapv #(java.lang.Integer/parseInt %))))

(defn fuel
  {:test (fn []
           (is (= 2 (fuel 12)))
           (is (= 2 (fuel 14)))
           (is (= 654 (fuel 1969)))
           (is (= 33583 (fuel 100756))))}
  [mass]
  (-> mass
      (/ 3)
      int
      (- 2)))

(defn total-fuel
  {:test (fn []
           (is (= 33583 (total-fuel [100756]))))}
  [modules]
  (reduce + (map fuel modules)))

;; * Part two

(defn fuel-bis
  {:test (fn []
           (is (= 2 (fuel-bis 14)))
           (is (= 966 (fuel-bis 1969)))
           (is (= 50346 (fuel-bis 100756))))}
  [mass]
  (->> mass
       (iterate fuel)
       (drop 1)
       (take-while #(> % 0))
       (reduce +)))

(defn total-fuel-bis
  [modules]
  (reduce + (map fuel-bis modules)))

(comment
  #_(clojure.test/run-tests 'justin.core)
  #_(cljs.test/run-tests 'justin.core)
  )
