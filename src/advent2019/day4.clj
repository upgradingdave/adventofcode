(ns advent2019.day4)

(defn six-digit? [digit]
  (= 6 (count (str digit))))

(defn two-adjacent? [digit]
  (loop [x (first (str digit))
         y (second (str digit))
         xs (rest (str digit))]
    (if (nil? y)
      false
      (if (= x y)
        true
        (recur (first xs) (second xs) (rest xs))))))

(defn only-two-adjacent? [digit]
  (loop [cnt 0
         last nil
         x (first (str digit))
         xs (rest (str digit))]
    (if (nil? x)
      (if (= cnt 1) true false)
      (cond

        (= cnt 1)
        (if (= x last)
          (recur (inc cnt) x (first xs) (rest xs))
          true)

        :else
        (if (= x last)
          (recur (inc cnt) x (first xs) (rest xs))
          (recur 0 x (first xs) (rest xs)))))))

(defn increasing? [digit]
  (loop [x (first (str digit))
         y (second (str digit))
         xs (rest (str digit))]
    (if (nil? y)
      true
      (if (>= (Integer/parseInt (str y)) (Integer/parseInt (str x)))
        (recur (first xs) (second xs) (rest xs))
        false))))

(defn part1 [start finish]
  (loop [total 0
         possible start]

    (if (> possible finish)
      total

      (if (and (six-digit? possible)
               (two-adjacent? possible)
               (increasing? possible))
        (recur (inc total) (inc possible))
        (recur total (inc possible))))))

(defn part2 [start finish]
  (loop [total 0
         possible start]

    (if (> possible finish)
      total

      (if (and (six-digit? possible)
               (only-two-adjacent? possible)
               (increasing? possible))
        (recur (inc total) (inc possible))
        (recur total (inc possible))))))

(comment
  (part1 240298 784956)
  (part2 240298 784956))
