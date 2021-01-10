(ns advent-of-code.day-3
  (:require [advent-of-code.utils.input :as in]))

(def tree? #{\#})

(defn ->slope [right down]
  (fn [input]
    (map (partial drop right) (drop down input))))

(defn solve* [advance input]
  (loop [input input trees 0]
    (if (empty? input)
      trees
      (recur (advance input) (cond-> trees (tree? (ffirst input)) inc)))))

(defn solve [input slopes]
  (->> slopes
       (map #(solve* % input))
       (reduce * 1)))

(comment
  (let [input (in/line-parser (in/load-input 3) cycle)]
    ;; part 1
    (solve input [(->slope 3 1)])

    ;; part 2
    (->> [[1 1]
          [3 1]
          [5 1]
          [7 1]
          [1 2]]
         (map (fn [[right down]]
                (->slope right down)))
         (solve input))))
