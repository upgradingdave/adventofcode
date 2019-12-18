(ns advent2019.day7
  (:require [clojure.math.combinatorics :as combo]
            [clojure.core.async :as async :refer [go <! >! chan
                                                  <!! >!! go-loop
                                                  pipe alts!
                                                  alts!!
                                                  ]]))

(defn make-opcode [opcode-input]
  (format "%05d" opcode-input))

(defn intcode [program id]
  (let [inchan (chan)
        outchan (chan)
        resultchan (chan)]
    (go-loop [cnt 0
              idx 0
              result program]

      ;; Read the operation and then we can determine next steps
      (let [cmd (make-opcode (nth result idx))
            opcode (Integer/parseInt (str (nth cmd 3) (nth cmd 4)))
            param1 (= (nth cmd 2) \0)
            param2 (= (nth cmd 1) \0)
            param3 (= (nth cmd 0) \0)]

        ;;(>! resultchan (str id "[" cnt "]: CMD: " cmd ", opcode: " opcode))

        (cond

          ;; add
          (= opcode 1)
          (let [[_ x y z] (take 4 (drop idx result))
                a (if param1 (get result x) x)
                b (if param2 (get result y) y)
                c (+ a b)
                result (assoc result z c)
                ]
            (>! resultchan (str id "[" cnt "]: '" a "' + '" b
                                "' = '" c "' store in '" z "'"))
            (recur (inc cnt) (+ 4 idx) result))

          ;; multiply
          (= opcode 2)
          (let [[_ x y z] (take 4 (drop idx result))
                a (if param1 (get result x) x)
                b (if param2 (get result y) y)
                c (* a b)
                result (assoc result z c)
                ]
            (>! resultchan (str id "[" cnt "]: '" a "' * '" b
                                "' = '" c "' store in '" z "'"))
            (recur (inc cnt) (+ 4 idx) result))

          ;; read from input and store
          (= opcode 3)
          (let [[_ x] (take 2 (drop idx result))]
            (>! resultchan (str id "[" cnt "]: Waiting for input"))
            (let [y (<! inchan)
                  result (assoc result x y)]
              (>! resultchan (str id "[" cnt "]: Store '" y "' to '" x "'"))
              (recur (inc cnt) (+ 2 idx) result)))

          ;; output
          (= opcode 4)
          (let [[_ x] (take 2 (drop idx result))
                x (if param1 (get result x) x)
                ]
            (>! outchan x)
            (>! resultchan (str id "[" cnt "]: DIAGNOSTIC CODE: " x))
            (recur (inc cnt) (+ 2 idx) result)
            )

          ;; jump-if-true
          (= opcode 5)
          (let [[_ x y] (take 3 (drop idx result))
                x (if param1 (get result x) x)
                y (if param2 (get result y) y)]
            (>! resultchan (str id "[" cnt "]: Jump if true, x:" x ", y:" y))
            (if (not (= 0 x))
              ;;(recur (inc cnt) y (assoc result idx y))
              (recur (inc cnt) y result)
              (recur (inc cnt) (+ 3 idx) result)))

          ;; jump-if-false
          (= opcode 6)
          (let [[_ x y] (take 3 (drop idx result))
                x (if param1 (get result x) x)
                y (if param2 (get result y) y)]
            (>! resultchan (str id "[" cnt "]: Jump if false, x:" x ", y:" y))
            (if (= 0 x)
              ;;(recur (inc cnt) y (assoc result idx y))
              (recur (inc cnt) y result)
              (recur (inc cnt) (+ 3 idx) result)))

          ;; less than
          (= opcode 7)
          (let [[_ x y z] (take 4 (drop idx result))
                x (if param1 (get result x) x)
                y (if param2 (get result y) y)
                v (if (< x y) 1 0)
                result (assoc result z v)]
            (>! resultchan (str id "[" cnt "]: Is x:" x " less than y:" y
                                "? v: " v))
            (recur (inc cnt) (+ 4 idx) result))

          ;; equals
          (= opcode 8)
          (let [[_ x y z] (take 4 (drop idx result))
                x (if param1 (get result x) x)
                y (if param2 (get result y) y)
                v (if (= x y) 1 0)
                result (assoc result z v)]
            (>! resultchan (str id "[" cnt "]: Does x:" x " = y:" y
                                "? v: " v))
            (recur (inc cnt) (+ 4 idx) result))

          ;; halt
          :else
          (do
            ;;(println "HALT!")
            (>! resultchan "HALT"))
          )))
    [inchan outchan resultchan]
    ))

(defn parse-program [path-to-file]
  (into [] (map #(Integer/parseInt (.trim %1))
                (.split (slurp path-to-file) ","))))

(defn runSimple [program init-val desc]
  (let [[ic oc rc] (intcode program desc)]
    (go (while true (let [v (<! rc)] (println v))))
    (go (while true (let [v (<! oc)] (println v))))
    (go (>! ic init-val))))

;; Pass input 1 to this program to solve day 5 part 1
;; Pass input 5 to solve day 5 part 2
(def inputDay5 (parse-program "files/day5.in"))

;; this program should output whatever is input
(def inputValTest [3,0,4,0,99])

;; output 1 if input == 8
(def equal8Test [3,9,8,9,10,9,4,9,99,-1,8])

;; output 1 if input < 8
(def inputLTTest1 [3,9,7,9,10,9,4,9,99,-1,8])

;; is input == 8 using immediate mode
(def equal8Test2 [3,3,1108,-1,8,3,4,3,99])

;; is input < 8 using immediate mode
(def LT8Test2 [3,3,1107,-1,8,3,4,3,99])

;; output 0 if input was 0
(def jumpTest1 [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9])

;; output 0 if input was 0 (immediate mode)
(def jumpTest2 [3,3,1105,-1,9,1101,0,0,12,4,12,99,1])

;; output 999 if < 8, 1000 if = 8, 1001 if > 8
(def bigTest1 [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
               1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])


(defn thruster-signal1 [program phases]
  (let [[amp-a-in amp-a-out ra] (intcode program "amp-a")
        [amp-b-in amp-b-out rb] (intcode program "amp-b")
        [amp-c-in amp-c-out rc] (intcode program "amp-c")
        [amp-d-in amp-d-out rd] (intcode program "amp-d")
        [amp-e-in amp-e-out re] (intcode program "amp-e")]

    (pipe amp-a-out amp-b-in)
    (pipe amp-b-out amp-c-in)
    (pipe amp-c-out amp-d-in)
    (pipe amp-d-out amp-e-in)

    (go 
      (>! amp-a-in (nth phases 0))
      (>! amp-b-in (nth phases 1))
      (>! amp-c-in (nth phases 2))
      (>! amp-d-in (nth phases 3))
      (>! amp-e-in (nth phases 4))
      (>! amp-a-in 0))

    amp-e-out
    ))

(defn part1 [program]
  (let [results (map (fn [phases] (<!! (thruster-signal1 program phases)))
                     (combo/permutations [0 1 2 3 4]))]
    (apply max results)))

;; day 7 part 1: in: 4,3,2,1,0 out: 43210 
(def input1 [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])

;; day 7 part 1: in: 0,1,2,3,4 out: 54321
(def input2 [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,
             1,24,23,23,4,23,99,0,0])

;; day 7 part 1 in: 1,0,4,3,2 out: 65210
(def input3 [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
             1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])

(def inputDay7 (parse-program "files/day7.in"))

(defn thruster-signal2 [program phases]
  (let [[ina outa ra] (intcode program "amp-a")
        [inb outb rb] (intcode program "amp-b")
        [inc outc rc] (intcode program "amp-c")
        [ind outd rd] (intcode program "amp-d")
        [ine oute re] (intcode program "amp-e")

        ;;inchan (async/merge [ina inb inc ind ine])
        ;;outchan (async/merge [outa outb outc outd oute])
        ;;resultchan (async/merge [ra rb rc rd re])
        result (chan)
        ]

    (pipe outa inb)
    (pipe outb inc)
    (pipe outc ind)
    (pipe outd ine)
    (pipe oute ina)

    (go (while true
          (let [v (<! ra)]
            ;;(println v)
            )))
    (go (while true
          (let [v (<! rb)]
            ;;(println v)
            )))
    (go (while true
          (let [v (<! rc)]
            ;;(println v)
            )))
    (go (while true
          (let [v (<! rd)]
            ;;(println v)
            )))
    (go (while true
          (let [v (<! re)]
            (when (= v "HALT")
              (>! result (<! ina)))
            ;;(println v)
            )))

    (go
      (>! ina (nth phases 0))
      (>! ina 0)
      (>! inb (nth phases 1))
      (>! inc (nth phases 2))
      (>! ind (nth phases 3))
      (>! ine (nth phases 4)))

    result

    ))

(defn part2 [program]
  (let [results (map (fn [phases] (<!! (thruster-signal2 program phases)))
                     (combo/permutations [5 6 7 8 9]))]
    (apply max results)))


;; day 7 part 2, 9,8,7,6,5
(def input4 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
             27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])

;; day 7 part 2: 9,7,8,5,6
(def input5 [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
             -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
             53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10])

