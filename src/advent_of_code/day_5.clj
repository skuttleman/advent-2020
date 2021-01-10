(ns advent-of-code.day-5
  (:require [advent-of-code.utils.input :as in]))

(defn ->num
  ([mapper s]
   (->num mapper s (dec (count s))))
  ([mapper [l & more] len]
   (if-let [x (mapper l)]
     (+ (* x (long (Math/pow 2 len)))
        (->num mapper more (dec len)))
     0)))

(defn ->seat-id [code]
  (let [[r c] (split-at 7 code)
        row (->num {\F 0 \B 1} r)
        col (->num {\L 0 \R 1} c)]
    (+ (* 8 row) col)))

(defn find-seat [input]
  (let [[min max seats] (reduce (fn [[lowest highest seats] seat-id]
                                  [(min lowest seat-id)
                                   (max highest seat-id)
                                   (conj seats seat-id)])
                                [0 0 #{}]
                                input)]
    (->> (range (inc min) max)
         (filter (every-pred (complement seats)
                             (comp seats dec)
                             (comp seats inc)))
         first)))

(comment
  (let [input (in/line-parser (in/load-input 5) ->seat-id)]
    ;; part 1
    (apply max input)

    ;; part 2
    (find-seat input)))
