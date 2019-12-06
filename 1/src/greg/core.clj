(ns greg.core)

(def input [139936
            96114
            73984
            101283
            71668
            119408
            54029
            134890
            83831
            147043
            70059
            136810
            124397
            123543
            107793
            117051
            111300
            93214
            91691
            106815
            61783
            138531
            69277
            75307
            125517
            72622
            117648
            71161
            147510
            133560
            147273
            101023
            100171
            108241
            128962
            85755
            50371
            141491
            96585
            103280
            122493
            126025
            124114
            123153
            94956
            86491
            54630
            112399
            121515
            58560
            80211
            84893
            103375
            65563
            64408
            131671
            90149
            131040
            138115
            99987
            51281
            57641
            130018
            141946
            111726
            99761
            54792
            75213
            71352
            59004
            136500
            148962
            144283
            114983
            97115
            87136
            137860
            146991
            67090
            51705
            99242
            109796
            147943
            83255
            57070
            55343
            67854
            101564
            74996
            74542
            57494
            90227
            69965
            103978
            142175
            116700
            70493
            62383
            100870
            110806])

(defn fuel-per-module
  [module]
  (- (int (/ module 3)) 2))

(defn calculate-fuel-requirements
  [input-data]
  (let [input-data-fuel-requirements (map fuel-per-module input-data)
        summed-fuel-requirements (reduce + input-data-fuel-requirements)]
    summed-fuel-requirements))

(comment
  ;; ANSWER 
  (calculate-fuel-requirements input))


(defn calculate-additional-fuel-requirements
  [input-data]
  (loop [fuel (fuel-per-module input-data)
         fuel-amounts '()]
    (cond
      (> fuel 0) (recur (fuel-per-module fuel) (conj fuel-amounts fuel))
      :else fuel-amounts)))

(comment
  ;; ANSWER 
  (->> (map calculate-additional-fuel-requirements input)
       (flatten)
       (reduce +)))
