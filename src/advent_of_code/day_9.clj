(ns advent-of-code.day-9
  (:require [advent-of-code.utils.input :as in]))

(defn totals? [window n]
  (loop [seen? #{} [x & more] window]
    (let [target (- n x)]
      (cond
        (and (not= target x) (seen? target))
        true

        (seq more)
        (recur (conj seen? x) more)))))

(defn find-weakness [input window-size]
  (let [window (take window-size input)
        n (first (drop window-size input))]
    (when n
      (if (totals? window n)
        (recur (rest input) window-size)
        n))))

(defn find-contiguous [input target]
  (loop [sum 0 operands [] input input [x & more] input]
    (cond
      (= sum target) operands
      (nil? x) nil
      (> sum target) (recur 0 [] (rest input) (rest input))
      :else (recur (+ sum x) (conj operands x) input more))))

(defn find-contiguous-sum [input target]
  (when-let [operands (find-contiguous input target)]
    (+ (apply min operands)
       (apply max operands))))

(comment
  (let [input (in/line-parser (in/load-input 9) in/parse-long)]
    ;; part 1
    (find-weakness input 25)

    ;; part 2
    (find-contiguous-sum input (find-weakness input 25))))
