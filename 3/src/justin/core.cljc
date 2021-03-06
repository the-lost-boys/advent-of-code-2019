(ns justin.core
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

;; * Part one

;; ** Math utilities

(def parse-integer #?(:clj #(java.lang.Integer/parseInt %)
                      :cljs #(js/parseInt %)))

(def sqrt #?(:clj #(java.lang.Math/sqrt %)
             :clsj #(js/Math.sqrt %)))

(def abs #?(:clj #(java.lang.Math/abs %)
            :cljs #(js/Math.abs %)))

(defn cross-product
  "See https://stackoverflow.com/a/565282"
  [v w]
  (let [[vx vy] v
        [wx wy] w]
    (- (* vx wy)
       (* vy wx))))

(defn take-pairs
  [s t]
  (if (or (empty? s) (empty? t))
    '()
    (cons [(first s) (first t)]
          (lazy-seq (take-pairs (rest s) (rest t))))))

(deftest take-pairs-test
  (is (= [[1 :a] [2 :b]]
         (take-pairs [1 2] [:a :b]))))

(defn vec-add
  [v w]
  (let [cv (count v)
        cw (count w)]
    (assert (= cv cw))
    (mapv #(+ (nth v %) (nth w %))
          (range cv))))

(defn vec-mul
  [k v]
  (mapv #(* k %) v))

(defn vec-sub
  [v w]
  (vec-add v (vec-mul -1 w)))

(defn dot-product
  [u v]
  (reduce + (map #(apply * %) (take-pairs u v))))

(defn vec-length
  [v]
  (sqrt (dot-product v v)))

(deftest vec-length-test
  (is (= 1.0 (vec-length [0 1])))
  (is (= 1.0 (vec-length [1 0])))
  (is (= 0.0 (vec-length [0 0]))))

;; ** Solution

(def input (clojure.string/split-lines (slurp "src/justin/input.txt")))

(defn command-to-point
  "Provides the next point as a result of transforming the previous one
  using the `command`."
  [prev {:keys [dir mag]
         :as   _command}]
  (let [op   (cond
               (contains? #{"D" "L"} dir) -
               (contains? #{"U" "R"} dir) +)
        axis (cond
               (contains? #{"U" "D"} dir) 1
               (contains? #{"R" "L"} dir) 0)
        next (update prev axis op mag)]
    next))

(deftest command-to-point-test
  (is (= [8 0]
         (command-to-point [0 0] {:dir "R" :mag 8}))))

(defn parse-commands
  [commands]
  (letfn [(parse-command [command]
            (let [[_ dir mag] (re-matches #"(\w)(\d+)" command)]
              {:dir dir
               :mag (parse-integer mag)}))]
    (map parse-command
         (clojure.string/split commands #","))))

(deftest parse-commands-test
  (is (= [{:dir "R"
           :mag 8}
          {:dir "U"
           :mag 5}]
         (parse-commands "R8,U5"))))

(defn commands-to-line-segments
  [commands]
  (loop [acc       '()
         remaining commands]
    (if (empty? remaining)
      (reverse acc)
      (let [p1      (or (second (first acc))
                        [0 0])
            p2      (command-to-point p1 (first remaining))
            updated (conj acc [p1 p2])]
        (recur updated (rest remaining))))))

(deftest commands-to-line-segments-test
  (is (= [#{[0 0] [8 0]}
          #{[8 0] [8 5]}]
         (mapv set             ;; order isn't so important
               (commands-to-line-segments [{:dir "R"
                                            :mag 8}
                                           {:dir "U"
                                            :mag 5}])))))

(defn intersection-point
  "Gets the single intersection point of line segments `a` and `b`,
  or else `nil`.
  Only provides integer values in the intersection point,
  since this puzzle involves Manhattan/Taxicab geometry.
  Follows https://stackoverflow.com/a/565282"
  [pr qs]
  (let [p  (first pr)
        r  (vec-mul -1 (vec-sub p (second pr)))
        q  (first qs)
        s  (vec-mul -1 (vec-sub q (second qs)))
        rs (cross-product r s)]
    (when (not= 0 rs)
      (let [t (/ (cross-product (vec-sub q p) s)
                 rs)
            u (/ (cross-product (vec-sub q p) r)
                 rs)]
        (when (and (<= 0 t 1)
                   (<= 0 u 1))
          (let [result (vec-add p (vec-mul t r))] ;; either pr or qs would work
            ;; integers only for this puzzle
            (mapv int result)))))))

(deftest intersection-point-test
  (is (= [3 3] (intersection-point [[3 5] [3 2]]
                                   [[6 3] [2 3]]))))

(defn intersections-for-two
  "Provides the sequence of distinct nonzero intersection points
  for the two given 'wires'.
  Each wire is itself a sequence of line segments.
  Each line segment includes a start point and an end point."
  [wire-a wire-b]
  (for [l-a   wire-a
        l-b   wire-b
        :let  [p (intersection-point l-a l-b)]
        :when (some? p)]
    p))

(deftest intersections-for-two-test
  (is (= [[3 3]]
         (intersections-for-two [[[3 5] [3 2]]]
                                [[[6 3] [2 3]]]))))

(defn combinations-of-two
  "Creates tuples of each item in `s` with another."
  [s]
  (if (empty? s)
    '()
    (let [head      (first s)
          remaining (rest s)]
      (lazy-cat (map #(vector head %) remaining)
                (combinations-of-two remaining)))))

(defn intersections
  "Accepts a sequence of `wires`,
  in which each wire is a sequence of line segments with a start and end,
  and evaluates to the intersecting points among the wires.
  Doesn't include intersections of a wire with itself."
  [wires]
  (distinct
   (remove #(= [0 0] %)
           (mapcat #(apply intersections-for-two %)
                   (combinations-of-two wires)))))

(deftest intersections-test
  (is (= #{[3 3]
           [6 5]}
         (set (intersections
               '([[[0 0] [8 0]]
                  [[8 0] [8 5]]
                  [[8 5] [3 5]]
                  [[3 5] [3 2]]]
                 [[[0 0] [0 7]]
                  [[0 7] [6 7]]
                  [[6 7] [6 3]]
                  [[6 3] [2 3]]]))))))

(defn manhattan-distance
  "The Manhattan distance between two 2D points."
  [point1 point2]
  (let [[x1 y1] point1
        [x2 y2] point2]
    (+ (abs (- x2 x1))
       (abs (- y2 y1)))))

(deftest manhattan-distance-test
  (is (= 6 (manhattan-distance [0 0] [3 3]))))

(defn min-distance
  "Evaluates to the shortest distance between the origin and an intersection."
  [intersections]
  (let [origin [0 0]]
    (reduce min (map #(manhattan-distance origin %) intersections))))

(defn part-one-solution
  [input]
  (min-distance (intersections (map commands-to-line-segments
                                    (map parse-commands
                                         input)))))

;; * Part two

(defn point-on-segment?
  "True when the given `point` is on the line `segment`.
  See https://stackoverflow.com/a/328122"
  [point segment]
  (let [[a b] segment
        ba    (vec-sub b a)
        pa    (vec-sub point a)]
    (when (zero? (cross-product ba pa))
      (let [segment-dot (dot-product ba ba)
            point-dot   (dot-product ba pa)]
        (<= 0 point-dot segment-dot)))))

(deftest point-on-segment-test
  (is (point-on-segment? [3 3] [[3 5] [3 2]])))

(defn wire-up-to-point
  "Evaluates to the portion of the given `wire` trimmed to the given point on it."
  [point wire]
  (loop [remaining wire
         results   []]
    (let [current (first remaining)]
      (cond
        (empty? remaining)                results
        (point-on-segment? point current) (let [trimmed (assoc current 1 point)]
                                            (conj results trimmed))
        :otherwise                        (recur (rest remaining)
                                                 (conj results current))))))

(deftest wire-up-to-point-test
  (is (= [[[0 0] [8 0]]
          [[8 0] [8 5]]
          [[8 5] [3 5]]
          [[3 5] [3 3]]]
         (wire-up-to-point [3 3] [[[0 0] [8 0]]
                                  [[8 0] [8 5]]
                                  [[8 5] [3 5]]
                                  [[3 5] [3 2]]]))))

(defn segment-length
  "Evaluates to the length of a line segment."
  [segment]
  (let [[p1 p2] segment
        x       first
        y       second
        dx      (abs (- (x p2)
                        (x p1)))
        dy      (abs (- (y p2)
                        (y p1)))]
    (int (vec-length [dx dy]))))        ;; integers only for this puzzle

(deftest segment-length-test
  (is (= 1
         (segment-length [[0 0] [0 1]])))
  (is (= 1
         (segment-length [[0 0] [1 0]]))))

(defn wire-length
  "Calculates the length of a wire.
  The wire is expected to be a sequence of line segments.
  Each line segment is expected to be a tuple of 2D coordinates."
  [wire]
  (reduce + (map segment-length wire)))

(defn joined-lengths
  [wires]
  (mapcat identity
          (map (fn [pair]
                 (let [intersections (apply intersections-for-two pair)
                       trimmed-pairs (map #(mapv (partial wire-up-to-point %) pair)
                                          intersections)
                       length-pairs  (map #(mapv wire-length %) trimmed-pairs)
                       lengths       (map #(apply + %) length-pairs)]
                   lengths))
               (combinations-of-two wires))))

(defn part-two-solution
  [input]
  (reduce min (joined-lengths (map commands-to-line-segments
                                   (map parse-commands
                                        input)))))
