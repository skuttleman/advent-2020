(ns advent-of-code.day-15
  (:require [advent-of-code.utils.input :as in]))

(defn solve [input len]
  (loop [spoken (into {} (map-indexed #(vector %2 [(inc %1)]) input))
         turns (vec input)
         turn (inc (count turns))]
    (if (> turn len)
      (peek turns)
      (let [last-spoken (peek turns)
            last-spoken (peek (pop (get spoken last-spoken)))
            age (if last-spoken
                  (- turn last-spoken 1)
                  0)]
        (recur (update spoken age (fnil conj []) turn)
               (conj turns age)
               (inc turn))))))

(defn ->input []
  (in/split-parser (in/load-input 15) #"," in/parse-long))

(comment
  ;; part 1
  (solve (->input) 2020)

  ;; part 2
  (solve (->input) 30000000))
