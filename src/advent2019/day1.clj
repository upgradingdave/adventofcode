(ns advent2019.day1)

(defn calc-fuel [mass]
  (let [result (-> (quot mass 3) (- 2))]
    (if (< result 0) 0 result)))

(defn calc-fuels [mass]
  (loop [fuel-weights [(calc-fuel mass)]]
    (if (> (last fuel-weights) 0)
      (recur (conj fuel-weights (calc-fuel (last fuel-weights))))
      (apply + fuel-weights))))

(defn part1 [path-to-file]
  (with-open [rdr (clojure.java.io/reader path-to-file)]
    (reduce (fn [f mass] (+ f (calc-fuel (Integer/parseInt mass))))
            0 (line-seq rdr))))

(defn part2 [path-to-file]
  (with-open [rdr (clojure.java.io/reader path-to-file)]
    (reduce (fn [f mass] (+ f (calc-fuels (Integer/parseInt mass))))
            0 (line-seq rdr))))

(comment
  (def sol1 (part1 "files/day1.in"))
  (def sol2 (part2 "files/day1.in")))
