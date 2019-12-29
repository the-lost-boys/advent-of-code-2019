(ns justin.core
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

;; * Part one

;; ** Helper functions

;; *** Collections

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

;; *** Converting to and from numeric types

(def ^:private parse-integer #?(:clj #(java.lang.Integer/parseInt %)
                                :cljs #(js/parseInt %)))

(defn- digits-seq
  [number]
  (map (comp parse-integer str) (seq (str number))))

;; *** Reading from *in*

(def ^:private safe-read #?(:clj #(clojure.edn/read)
                            :cljs #(cljs.reader/read)))

(def ^:private safe-read-string #?(:clj #(clojure.edn/read-string %)
                                   :cljs #(cljs.reader/read-string %)))

;; ** Program execution

(def input (safe-read-string (str "[" (slurp "src/justin/input.txt") "]")))

(defn parse-operation
  [number]
  (let [opcode          (rem number 100)
        parameter-modes (drop 2 (reverse (digits-seq number)))]
    {:opcode          opcode
     :parameter-modes parameter-modes}))

(defn handle-param
  [mode param program]
  (case mode
    1   param
    0   (nth program param)))

(deftest handle-param-test
  (is (= 33 (handle-param 0
                          4
                          [2 4 3 4 33]))))

(defn binary-operation
  [{:keys [operator parameter-modes]} {:keys [index program]}]
  (let [[param-1 param-2 out] (slice (inc index) (+ index 3) program)
        [input-1 input-2]     (map (fn [[mode param]]
                                     (handle-param mode param program))
                                   (map (fn [[i param]]
                                          (let [mode (nth parameter-modes i 0)]
                                            [mode param]))
                                        (map-indexed vector
                                                     [param-1 param-2])))]
    {:index   (+ index 4)
     :program (assoc program
                     out
                     (operator
                      input-1
                      input-2))}))

(deftest binary-operation-test
  (is (= {:index   4
          :program [1002 4 3 4 99]}
         (binary-operation {:operator        *
                            :parameter-modes [0 1]}
                           {:index   0
                            :program [1002 4 3 4 33]}))))

(defn execute-once
  [{:keys [index program] :as state}]
  (let [operation                 (nth program index)
        {:keys [opcode
                parameter-modes]} (parse-operation operation)]
    (case opcode
      99 (dissoc state :index)
      1  (binary-operation {:operator        +
                            :parameter-modes parameter-modes}
                           state)
      2  (binary-operation {:operator        *
                            :parameter-modes parameter-modes}
                           state)
      3  (let [input (int (safe-read))
               out   (nth program (inc index))]
           {:index   (+ index 2)
            :program (assoc program out input)})
      4  (let [param-1 (nth program (inc index))
               input-1 (handle-param (nth parameter-modes 0 0) param-1 program)]
           (prn input-1)
           {:index   (+ index 2)
            :program program}))))

(defn execute-all
  "Executes the given `program` until a 'halt' (99) instruction is reached.
  Evaluates to the final state of the program.
  This means it is possible for a program to execute indefinitely."
  [program]
  (loop [state {:program program
                :index   0}]
    (let [halt (nil? (:index state))]
      (if halt
        (:program state)
        (let [next-state (execute-once state)]
          (recur next-state))))))

(deftest execute-all-test
  (is (= [1101 100 -1 4 99]
         (execute-all [1101 100 -1 4 0]))))

(defn part-one-solution
  "Executes the given program. The final output to `*out*` is the solution."
  [program]
  (execute-all program))
