(ns advent2019.day3
  (:require [upgrade.input :refer [read-lines]]))

(defn segment-to-coords [x y direction len]
  (cond

    (= \R direction)
    [(+ x len) y 
     (map (fn [n] (vector (+ x n) y)) (range 1 (inc len)))]

    (= \L direction)
    [(- x len) y
     (map (fn [n] (vector (- x n) y)) (range 1 (inc len)))]

    (= \U direction)
    [x (+ y len)
     (map (fn [n] (vector x (+ y n))) (range 1 (inc len)))]
    
    (= \D direction)
    [x (- y len)
     (map (fn [n] (vector x (- y n))) (range 1 (inc len)))]

    :else
    (throw (Exception. (str "Invalid Direction: " direction)))))

(defn path-to-coords
  [path]
  (loop [x 0
         y 0
         segment (first path)
         remain (rest path)
         result []]
    (if (nil? segment)
      result
      (let [dir (first segment)
            len (Integer/parseInt (apply str (rest segment)))
            [newx newy coords] (segment-to-coords x y dir len)
            new-result (concat result coords)
            ]
        (recur newx newy (first remain) (rest remain) new-result)))))

(defn manhattan-dist [[x1 y1] [x2 y2]]
  (+ (StrictMath/abs (- x2 x1))
     (StrictMath/abs (- y2 y1))))

(defn path-intersections [path1 path2]
  (let [coords1 (into #{} (path-to-coords path1))
        coords2 (into #{} (path-to-coords path2))
        intersections (clojure.set/intersection coords1 coords2)]
    intersections
    ))

(defn read-input [path-to-file]
  (map #(.split %1 ",")
       (.split (slurp path-to-file) "\n")))

(defn part1 [inputs]
  (first
   (reduce
    (fn [[min-dist coords] [dist intersect]]
      (if (< dist min-dist) [dist intersect] [min-dist coords]))
    (map #(vector (manhattan-dist [0 0] %1) %1) 
         (apply path-intersections inputs)))))

(defn len-to-coord
  [coord coords]
  (loop [pos 1
         x (first coords)
         xs (rest coords)]
    ;;(println "pos: " pos ", x: " x ", coord: " coord)
    (if (nil? x)
      nil
      (if (= x coord)
        pos
        (recur (inc pos) (first xs) (rest xs))))))

(defn part2 [inputs]
  (let [coords1 (path-to-coords (first inputs))
        coords2 (path-to-coords (last inputs))
        intersects (apply path-intersections inputs)]
    (reduce (fn [acc n]
              (if (< n acc) n acc))
            (map #(+ (len-to-coord %1 coords1)
                     (len-to-coord %1 coords2))
                 intersects))))

(comment

  (def input1 [["R8","U5","L5","D3"]
               ["U7","R6","D4","L4"]])

  (def input2 [["R75","D30","R83","U83","L12","D49","R71","U7","L72"]
               ["U62","R66","U55","R34","D71","R55","D58","R83"]])

  (def input3
    [["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"]
     ["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]])


  (path-to-coords (first input1))
  (apply path-intersections input2)
  (part1 input2)

  (apply path-intersections )
  (part1 (read-input "files/day3.in"))
  (part2 (read-input "files/day3.in"))
  )
