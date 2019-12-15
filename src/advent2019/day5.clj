(ns advent2019.day5)

(defn make-opcode [opcode-input]
  (format "%05d" opcode-input))

(defn intcode [inputs]
  (loop [idx 0
         result inputs]

    ;; Read the operation and then we can determine next steps
    (let [cmd (make-opcode (nth result idx))
          opcode (Integer/parseInt (str (nth cmd 3) (nth cmd 4)))
          param1 (= (nth cmd 2) \0)
          param2 (= (nth cmd 1) \0)
          param3 (= (nth cmd 0) \0)]

      ;; (println "idx:" idx)
      ;; (println "cmd:" cmd)
      ;; (println "opcode:" opcode)
      ;; (println "parm1:" param1 ", param2:" param2 ", param3:" param3)
      ;; (println "result:")
      ;; (println result)

      (cond

        ;; add
        (= opcode 1)
        (let [[_ x y z] (take 4 (drop idx result))
              a (if param1 (get result x) x)
              b (if param2 (get result y) y)
              result (assoc result z (+ a b))
              ]
          ;; (println "x:" x "y:" y "z:" z)
          ;; (println "adding a:" a "plus b:" b "storing to z:" z)
          (recur (+ 4 idx) result))

        ;; multiply
        (= opcode 2)
        (let [[_ x y z] (take 4 (drop idx result))
              a (if param1 (get result x) x)
              b (if param2 (get result y) y)
              result (assoc result z (* a b))
              ]
          (recur (+ 4 idx) result))

        ;; store
        (= opcode 3)
        (let [[_ x] (take 2 (drop idx result))
              y (Integer/parseInt (read-line))
              result (assoc result x y)
              ]
          ;;(println "storing y:" y " in position: " x)
          (recur (+ 2 idx) result))

        ;; output
        (= opcode 4)
        (let [[_ x] (take 2 (drop idx result))]
          (println "DIAGNOSTIC CODE: " (nth result x))
          (recur (+ 2 idx) result))

        ;; jump-if-true
        (= opcode 5)
        (let [[_ x y] (take 3 (drop idx result))
              x (if param1 (get result x) x)
              y (if param2 (get result y) y)]
          (if (not (= 0 x))
            (recur y (assoc result idx y))
            (recur (+ 3 idx) result)))

        ;; jump-if-false
        (= opcode 6)
        (let [[_ x y] (take 3 (drop idx result))
              x (if param1 (get result x) x)
              y (if param2 (get result y) y)]
          ;; (println "x:" x "y:" y)
          ;; (println "jump to" y "if x is 0")
          (if (= 0 x)
            (recur y (assoc result idx y))
            (recur (+ 3 idx) result)))

        ;; less than
        (= opcode 7)
        (let [[_ x y z] (take 4 (drop idx result))
              x (if param1 (get result x) x)
              y (if param2 (get result y) y)
              v (if (< x y) 1 0)
              result (assoc result z v)]
          (recur (+ 4 idx) result))

        ;; equals
        (= opcode 8)
        (let [[_ x y z] (take 4 (drop idx result))
              x (if param1 (get result x) x)
              y (if param2 (get result y) y)
              v (if (= x y) 1 0)
              result (assoc result z v)]
          (recur (+ 4 idx) result))

        ;; halt
        :else
        (println "done!")))))

(defn read-input [path-to-file]
  (into [] (map #(Integer/parseInt %1) (.split (slurp path-to-file) ","))))

(def input1 [1002,4,3,4,33])
(def input2 (read-input "files/day5.in"))
(def inputEq8 [3,9,8,9,10,9,4,9,99,-1,8])
(def inputLt8 [3,9,7,9,10,9,4,9,99,-1,8])
(def inputEq8i [3,3,1108,-1,8,3,4,3,99])
(def inputLt8i [3,3,1107,-1,8,3,4,3,99])
(def inputNot0 [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9])
(def inputNot0i [3,3,1105,-1,9,1101,0,0,12,4,12,99,1])
(def input9 [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])

(defn part1 []
  (intcode input2))

(defn part2 []
  (intcode input2))

