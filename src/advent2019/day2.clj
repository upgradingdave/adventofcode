(ns advent2019.day2)

(defn run [inputs noun verb]
  (loop [idx 0
         result (-> inputs
                    (assoc 1 noun)
                    (assoc 2 verb))]

    (let [[op x y z] (take 4 (drop idx result))
          a (get result x)
          b (get result y)]

      (cond

        ;; add
        (= op 1)
        (let [result (assoc result z (+ a b))]
          (recur (+ 4 idx) result))
        
        (= op 2)
        (let [result (assoc result z (* a b))]
          (recur (+ 4 idx) result))

        :else
        (first result)
        ))))

(def inputs1 [1,9,10,3,2,3,11,0,99,30,40,50])
(def inputs2 [1,0,0,0,99])
(def inputs3 [2,3,0,3,99])
(def inputs4 [2,4,4,5,99,0])
(def inputs5 [1,1,1,4,99,5,6,0,99])

(defn read-input [file-path]
  (into [] (map #(Integer/parseInt %1) (input-from-file file-path))))

(defn part2 [inputs expected]
  (let [possibilities (for [noun (range 0 99)
                            verb (range 0 99)]
                        [noun verb])]
    (loop [possibility (first possibilities)
           remaining (rest possibilities)]
      (let [actual (apply run (into [] (concat [inputs] possibility)))
            [noun verb] possibility]
        (if (or (nil? possibility)
                (= expected actual))
          (+ (* 100 noun) verb)
          (recur (first remaining)
                 (rest remaining)))))))

(comment
  (def sol1 (run (read-input "files/day2.in") 12 2))
  (def sol2 (part2 (read-input "files/day2.in") 19690720))
  )
