(ns advent-of-code.day-1
  (:require [advent-of-code.utils.input :as in]))

(defn find-pair [total xs]
  (loop [seen? #{} [x & more] xs]
    (let [target (- total x)]
      (cond
        (seen? target) [target x]
        (seq more) (recur (conj seen? x) more)))))

(defn find-trio [total xs]
  (loop [[x & more] xs]
    (if-let [operands (find-pair (- total x) xs)]
      (conj operands x)
      (when (seq more)
        (recur more)))))

(defn solve [f year xs]
  (when-let [operands (f year xs)]
    (apply * operands)))

(comment
  (let [input (in/line-parser (in/load-input 1) in/parse-long)]
    ;; part 1
    (solve find-pair 2020 input)

    ;; part 2
    (solve find-trio 2020 input)))
