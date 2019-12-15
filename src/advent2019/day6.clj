(ns advent2019.day5
  (:require [clojure.walk :refer [postwalk]]))

(defn parse-orbit [orbit]
  (.split orbit "\\)"))

(defn make-orbit-tree [input]
  (reduce
   (fn [acc [parent child]]
     (let [parent (keyword parent)
           child (keyword child)]
       (update acc parent (fn [s] (if s (conj s child) [child])))))
   {}
   (map parse-orbit input)))

(defn find-relative-orbiters [root orbits]
  (tree-seq (fn [n] (get orbits n)) (fn [n] (get orbits n)) root))

(defn count-relative-orbits [orbits]
  (reduce
   (fn [acc root]
     (+ acc (dec (count (find-relative-orbiters root orbits)))))
   0
   (keys orbits)))

(defn find-parent [node orbits]
  (first
   (filter (fn [[_ children]] (some #(= % node) children)) orbits)))

(defn is-grandchild? [node children]
  (reduce (fn [acc n]
            (if (some #(= % node) (find-relative-orbiters n orbits))
              n
              acc))
          nil children))

(defn read-input [path-to-file]
  (with-open [rdr (clojure.java.io/reader path-to-file)]
    (let [lines (line-seq rdr)]
      (make-orbit-tree lines))))

(defn part1 []
  (count-relative-orbits (read-input "files/day6.in")))

(defn part2 [orbits]
  (let [[parent children] (find-parent :YOU orbits)
        [santa-parent _] (find-parent :SAN orbits)]
    (loop [len 0
           parent parent
           children children]
      (let [santa-in-orbit? (some #(= %1 :SAN) children)
            next-child (is-grandchild? :SAN children)
            next-node (or next-child
                          (first (find-parent parent orbits)))
            next-children (get orbits next-node)]
        ;; (println "==============")
        ;; (println "len:" len)
        ;; (println "parent:" parent)
        ;; (println "children:" children)
        ;; (println "santa-in-orbit?:" santa-in-orbit?)
        ;; (if next-child
        ;;   (println "DOWN")
        ;;   (println "UP"))
        ;; (println "next-node:" next-node)
        (if (or santa-in-orbit?)
          len
          (recur (inc len) next-node next-children))))))

(def input1
  ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"])

(def input2
  ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"
   "K)YOU" "I)SAN"])

(comment
  (def orbits (make-orbit-tree input1))
  (find-relative-orbiters :COM orbits)
  (find-relative-orbiters :E orbits)
  (count-relative-orbits orbits)

  (defn orbits (read-input "files/day6.in"))
  (find-parent :YOU orbits)

  (def orbits (make-orbit-tree input2))
  ;; is F a grandchild of C ?
  (is-grandchild? :F (:C orbits))
  (is-grandchild? :L (:C orbits))
  (find-parent :YOU orbits)
  (find-parent :SAN orbits)

  (def orbits (read-input "files/day6.in"))
  (find-parent :SAN orbits)
  (find-parent :LVH orbits)
  (find-parent :42C orbits)

  (is-grandchild? :SAN (:JZW orbits))
  
  )

