(ns advent2019.day7
  (:require [clojure.math.combinatorics :as combo]
            [clojure.core.async :as async :refer [go <! >! chan
                                                  <!! >!! go-loop]]))

;; (def inputs (atom '()))

;; (defn read-next! []
;;   (let [val (first @inputs)]
;;     (reset! inputs (rest @inputs))
;;     (println "NEXT VAL: " val)
;;     val))

;; (defn write-next! [val]
;;   (swap! inputs conj val))

(defn make-opcode [opcode-input]
  (format "%05d" opcode-input))

(defn intcode [program inchan outchan]
  (go-loop [idx 0
            result program]

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

        ;; read from input and store
        (= opcode 3)
        (let [[_ x] (take 2 (drop idx result))]
          ;;(println "Waiting for input ...")
          (let [y (<! inchan)
                result (assoc result x y)]
            (recur (+ 2 idx) result)))

        ;; output
        (= opcode 4)
        (let [[_ x] (take 2 (drop idx result))
              x (if param1 (get result x) x)]
          (>! outchan x)
          ;;(println "DIAGNOSTIC CODE: " x)
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
        nil
        ;;(println "HALT")
        ))))

(defn parse-program [path-to-file]
  (into [] (map #(Integer/parseInt (.trim %1)) (.split (slurp path-to-file) ","))))

(def input1 [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
(def input2 [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,
             1,24,23,23,4,23,99,0,0])
(def input3 [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
             1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])
(def input4 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
             27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])

(def inputDay7 (parse-program "files/day7.in"))

(comment

  ;; provide phase first! and then provide input
  (def inchan (chan))
  (def outchan (chan))
  (go
    (intcode input1 inchan outchan)
    (>! inchan 4)
    (>! inchan 0))
  (<!! outchan)

  (<!! (thruster-signal input1 [4 3 2 1 0]))
  
  )

(defn thruster-signal [program phases]
  (let [inchan (chan)
        outchan (chan)
        resultchan (chan)]
    (go-loop [phase (first phases)
              phases (rest phases)]
      (if phase
        (do
          (intcode program inchan outchan)
          (>! inchan phase)
          (>! inchan (<! outchan))
          (recur (first phases) (rest phases)))
        (>! resultchan (<! outchan))))
    (>!! outchan 0)
    (<!! resultchan)))

(defn part1 [program]
  (let [results (map #(thruster-signal program %)
                 (combo/permutations [0 1 2 3 4]))]
    (apply max results)))

