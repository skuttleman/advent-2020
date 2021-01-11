(ns advent-of-code.day-12
  (:require [advent-of-code.utils.input :as in]))

(def ->instr
  {"F" :forward
   "L" :left
   "R" :right
   "N" :north
   "S" :south
   "E" :east
   "W" :west})

(defn parse-line [line]
  (let [[_ instr amt] (re-matches #"(\w)(\d+)" line)]
    [(->instr instr) (in/parse-long amt)]))

(defn rotate [[x y]]
  [(* -1 y) x])

(defn move [[x y] [x' y'] amt]
  [(+ x (* x' amt)) (+ y (* y' amt))])

(defn tracer [track-key]
  (fn [state [instr amt]]
    (case instr
      :forward (update state :pos move (:dir state) amt)
      :left (recur state [:right (- 360 amt)])
      :right (if (zero? amt)
               state
               (recur (update state :dir rotate) [:right (- amt 90)]))
      :north (update state track-key move [0 -1] amt)
      :south (update state track-key move [0 1] amt)
      :east (update state track-key move [1 0] amt)
      :west (update state track-key move [-1 0] amt))))

(defn manhattan-dist [[x y]]
  (+ (Math/abs (long x)) (Math/abs (long y))))

(defn solve [input reducer dir]
  (->> input
       (reduce reducer {:dir dir :pos [0 0]})
       :pos
       manhattan-dist))

(comment
  (let [input (in/line-parser (in/load-input 12) parse-line)]
    ;; part 1
    (solve input (tracer :pos) [1 0])

    ;; part 2
    (solve input (tracer :dir) [10 -1])))
