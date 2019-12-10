(ns justin.core
  (:require #?(:clj [clojure.test :refer [is]]
               :cljs [cljs.test :refer-macros [is]])))

;; * Part one

(def safe-read-string #?(:clj #(clojure.edn/read-string %)
                         :cljs #(cljs.reader/read-string %)))

(def input (safe-read-string (str "[" (slurp "src/justin/input.txt") "]")))

(defn- slice
  "Starting at `start`, take until `end` or the sequence ends, using `s`."
  {:test (fn []
           (is (= '(6 7 8)
                  (slice 5 8 [1 2 3 4 5 6 7 8])))
           (is (= '(6 7 8 9)
                  (slice 5 8 [1 2 3 4 5 6 7 8 9])))
           (is (= '(6 7 8 9)
                  (slice 5 8 [1 2 3 4 5 6 7 8 9 10]))))}
  [start end s]
  (take (inc (- end start)) (drop start s)))

(defn execute-once
  "Evaluates to a tuple of next index and updated program state.
  Index is `nil` when the program is finished executing."
  {:test (fn []
           (is (= [4 [1 9 10 70
                      2 3 11 0
                      99
                      30 40 50]]
                  (execute-once
                   0
                   [1 9 10 3
                    2 3 11 0
                    99
                    30 40 50])))
           (is (= [8 [3500 9 10 70
                      2 3 11 0
                      99
                      30 40 50]]
                  (execute-once
                   4
                   [1 9 10 70
                    2 3 11 0
                    99
                    30 40 50]))))}
  [index program]
  (let [operation (slice index (+ index 3) program)
        opcode    (first operation)]
    (if (= 99 opcode)
      [nil program]
      (let [[index1 index2 out] (rest operation)
            operator            (case opcode
                                  1 +
                                  2 *)
            [input1 input2]     (mapv #(nth program %)
                                      [index1 index2])
            next-index          (+ index 4)]
        [next-index (assoc program
                           out
                           (operator
                            input1
                            input2))]))))

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
  (loop [index   0
         current program]
    (let [halt (nil? index)]
      (if halt
        current
        (let [[next-index next-program] (execute-once index current)]
          (recur next-index next-program))))))

(defn part-one-solution
  ([program]
   (part-one-solution program 12 2))
  ([program input1 input2]
   (nth (execute-all (assoc program
                            1 input1
                            2 input2))
        0)))

;; * Part two

(defn brute-force-find-inputs
  "Naively tries every possible combination of inputs until the two
  that produce the desired output are found.
  Evaluates to a pair of such inputs."
  [program]
  (let [target-out 19690720
        inputs     (for [in1 (range 1 100)
                         in2 (range 1 100)]
                     [in1 in2])]
    (loop [remaining inputs]
      (let [[in1 in2 :as input] (first remaining)
            out                 (part-one-solution program in1 in2)]
        (if (= target-out out)
          input
          (recur (rest remaining)))))))

(defn part-two-solution
  [program]
  (let [[noun verb] (brute-force-find-inputs program)]
    (+ (* 100 noun) verb)))
