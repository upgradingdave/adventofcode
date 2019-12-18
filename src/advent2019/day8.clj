(ns advent2019.day8)

(defn parse-img-str [img-str width height]
  (map-indexed vector
               (partition height
                          (map #(apply str %) (partition width img-str)))))

(defn count-digits-in-layer [rows digit]
  (reduce (fn [acc row]
            (+ acc (count (filter (fn [n] (= digit n)) row))))
          0 rows))

(defn layer-with-fewest-zeros [layers]
  (reduce
   (fn [min-zero-layer layer]
     (let [[layer-idx rows] layer
           [_ _ min-zeros] min-zero-layer
           zeros (count-digits-in-layer rows \0)]
       (if (or (nil? min-zero-layer) (< zeros min-zeros))
         [layer-idx rows zeros]
         min-zero-layer)))
   nil
   layers))

(defn one-digits-times-two-digits [rows]
  (let [ones (count-digits-in-layer rows \1)
        twos (count-digits-in-layer rows \2)]
    (* ones twos)))

(defn part1 [img-str width height]
  (let [layer (second
               (layer-with-fewest-zeros (parse-img-str img-str width height)))]
    (one-digits-times-two-digits layer)))

(defn merge-rows [row1 row2]
  (apply str
         (for [x (range (count row1))]
           (let [c1 (nth row1 x)
                 c2 (nth row2 x)]
             (if (= \2 c1) c2 c1)))))

;; 0 - black
;; 1 - white
;; 2 - transparent
(defn render [layers]
  (reduce
   (fn [[_ layer1]  [_ layer2]]
     [0 
      (for [x (range (count layer1))]
        (let [row1 (nth layer1 x)
              row2 (nth layer2 x)]
          (merge-rows row1 row2)))])
   layers))

(defn part2 [input width height]
  (doseq [row (second (render (parse-img-str input width height)))]
    (println row)))

(def input1 "123456789012")
(def input2 "0222112222120000")
(def inputDay8 (slurp "files/day8.in"))

(comment


  (layer-with-fewest-zeros (parse-img-str input1 3 2)))
