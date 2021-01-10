(ns advent-of-code.day-2
  (:require [advent-of-code.utils.input :as in]))

(defn parse-line [line]
  (let [[_ start end letter password] (re-find #"(\d+)-(\d+) (\w): (.+)" line)]
    {:start    (in/parse-long start)
     :end      (in/parse-long end)
     :letter   (first letter)
     :password password}))

(defn valid-1? [{:keys [start end letter password]}]
  (when-let [freq (get (frequencies password) letter)]
    (<= start freq end)))

(defn valid-2? [{:keys [start end letter password]}]
  (not= (= (nth password (dec start) nil) letter)
        (= (nth password (dec end) nil) letter)))

(defn solve [input pred]
  (count (filter pred input)))

(comment
  (let [input (in/line-parser (in/load-input 2) parse-line)]
    ;; part 1
    (solve input valid-1?)

    ;; part 2
    (solve input valid-2?)))
