(ns advent-of-code.day-10
  (:require [advent-of-code.utils.input :as in]))

(defn use-all [input]
  (let [{one 1 three 3} (frequencies (map #(- %2 %1) (butlast input) (rest input)))]
    (* one three)))

(defn connectable? [x y]
  (>= 3 (Math/abs (long (- x y)))))

(def total-combinations
  (memoize (fn [[x & input]]
             (cond
               (empty? input) 0

               (empty? (rest input)) 1

               :else
               (let [targets (count (take-while (partial connectable? x) input))]
                 (reduce + 0 (map #(total-combinations (drop % input)) (range targets))))))))

(comment
  (let [input (in/line-parser (in/load-input 10) in/parse-long)
        mx (apply max input)
        input (sort (conj input (+ 3 mx) 0))]
    ;; part 1
    (use-all input)

    ;; part 2
    (total-combinations input)))
