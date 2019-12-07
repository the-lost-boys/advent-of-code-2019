(ns justin.core
  (:require #?(:clj [clojure.test :refer [is]]
               :cljs [cljs.test :refer-macros [is]])))

;; * Part one

(def safe-read-string #?(:clj #(clojure.edn/read-string %)
                         :cljs #(cljs.reader/read-string %)))

(def input (safe-read-string (str "[" (slurp "src/justin/input.txt") "]")))

(defn- take-four
  [start v]
  (mapv #(nth v %)
        (range start
               (+ start 4))))

(defn execute-once
  {:test (fn []
           (is (= [1 9 10 70
                   2 3 11 0
                   99
                   30 40 50]
                  (execute-once
                   0
                   [1 9 10 3
                    2 3 11 0
                    99
                    30 40 50])))
           (is (= [3500 9 10 70
                   2 3 11 0
                   99
                   30 40 50]
                  (execute-once
                   4
                   [1 9 10 70
                    2 3 11 0
                    99
                    30 40 50]))))}
  [index program]
  (let [[opcode index1 index2 out :as taken] (take-four index program)
        operator                             (case opcode
                                               1 +
                                               2 *
                                               (throw (ex-info "HCF" {})))
        [input1 input2 :as inputs]           (mapv #(nth program %)
                                                   [index1 index2])]
    (assoc program
           out
           (operator
            input1
            input2))))

(defn execute-all
  {:test (fn []
           (is (= [2 0 0 0 99]
                  (execute-all [1 0 0 0 99])))
           (is (= [2 3 0 6 99]
                  (execute-all [2 3 0 3 99])))
           (is (= [2 4 4 5 99 9801]
                  (execute-all [2 4 4 5 99 0])))
           (is (= [30 1 1 4 2 5 6 0 99]
                  (execute-all [1 1 1 4 99 5 6 0 99]))))}
  [program]
  (loop [current program
         index   0]
    (let [halt (= 99 (nth current index))]
      (if halt
        current
        (recur (execute-once index current) (+ index 4))))))

(defn part-one-solution
  [program]
  (nth (execute-all (assoc program
                           1 12
                           2 2))
       0))
