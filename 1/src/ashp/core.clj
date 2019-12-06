(ns ashp.core)

(def input
  '( 109506 140405 139135 110950 84296 123991 59438 85647 81214 100517 100910 57704 83368 50777 85523 95788 127699 138908 95502 81703 67317 108468 58394 72202 121580 86908 72705 86578 83714 114900 142915 51332 69054 97039 143539 61143 113534 98335 58533 83893 127138 50844 88397 133591 83563 52435 96342 109491 81148 127397 86200 92418 144842 120142 97531 54449 91004 129115 142487 68513 140405 80111 139359 57486 116973 135102 59737 144040 95483 134470 60473 113142 78189 53845 124139 78055 63791 99879 58630 111233 80544 76932 79644 116247 54646 85217 110795 142095 74492 93318 122300 82755 147407 98697 98105 132055 67856 109731 75747 135700 )) 

(def sample
  '(12 14 1969 100756))

(defn solve-part-1 [] 
  (->> input
       (map #(/ % 3))
       (map #(int (Math/floor %)))
       (map #(- % 2))
       (reduce +))) 

(defn fuel-for-mass
  [mass]
  (- (int (Math/floor (/ mass 3))) 2))

(defn fuel-calculator 
  [mass]
  (let [fuel (fuel-for-mass mass)]
    (if (< fuel 0)
      0
      (+ fuel (fuel-calculator fuel)))))

(defn fuel-calculator-recur
  ([mass]
   (fuel-calculator-recur mass 0))
  ([mass total-fuel]
   (let [added-fuel-mass (fuel-for-mass mass)]
     (if (< added-fuel-mass 0)
       total-fuel
       (recur added-fuel-mass (+ total-fuel added-fuel-mass))))))

(defn solve-part-2 []
  (->> input
       (map fuel-calculator-recur)
       (reduce +)))

{:1 3216744 
 :2 4822249}
