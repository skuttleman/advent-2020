(ns advent-of-code.day-8
  (:require [advent-of-code.utils.input :as in]))

(defn parse-line [line]
  (let [[_ op operand] (re-find #"(\w+) ([+\-]\d+)" line)]
    [(keyword op) (in/parse-long operand)]))

(defmulti step (fn [_ [op _]] op))

(defmethod step :nop
  [state _]
  (step state [:jmp 1]))

(defmethod step :acc
  [state [_ amt]]
  (-> state
      (update :acc + amt)
      (step [:jmp 1])))

(defmethod step :jmp
  [state [_ amt]]
  (update state :pos + amt))

(def swap-op
  {:jmp :nop
   :nop :jmp})

(defn solve [input]
  (loop [seen? #{} state {:acc 0 :pos 0}]
    (let [instr (get input (:pos state))]
      (cond
        (nil? instr) [(:acc state) true]
        (seen? (:pos state)) [(:acc state) false]
        :else (recur (conj seen? (:pos state))
                     (step state instr))))))

(defn moves [input]
  (->> input
       (map-indexed vector)
       (filter (comp swap-op first second))
       (map first)))

(defn find-terminal [input]
  (first (for [move (moves input)
               :let [[acc terminated?] (solve (update-in input [move 0] swap-op))]
               :when terminated?]
           acc)))

(comment
  (let [input (vec (in/line-parser (in/load-input 8) parse-line))]
    ;; part 1
    (first (solve input))

    ;; part 2
    (find-terminal input)))
