(ns advent-of-code.day-18
  (:require [advent-of-code.utils.input :as in]
            [clojure.edn :as edn]))

(declare eval-all)

(defn call [op val1 val2]
  (({'+ + '* *} op) val1 val2))

(defn ->instr [line]
  (edn/read-string (str "[" line "]")))

(defn evaluate-1 [[expr1 op expr2 & more]]
  (if op
    (recur (cons (call op
                       (cond-> expr1 (coll? expr1) evaluate-1)
                       (cond-> expr2 (coll? expr2) evaluate-1))
                 more))
    (cond-> expr1 (coll? expr1) evaluate-1)))

(defn evaluate-2 [[expr1 :as exprs]]
  (if (= 1 (count exprs))
    (cond-> expr1 (coll? expr1) evaluate-2)
    (->> exprs (eval-all '+) (eval-all '*) recur)))

(defn eval-all [op exprs]
  (loop [[expr1 op' expr2 & more] exprs output []]
    (cond
      (nil? op') (conj output expr1)
      (= op op') (let [result (call op
                                    (cond-> expr1 (coll? expr1) evaluate-2)
                                    (cond-> expr2 (coll? expr2) evaluate-2))]
                   (recur (cons result more)
                          output))
      :else (recur (cons expr2 more)
                   (conj output expr1 op')))))

(defn ->input []
  (in/line-parser (in/load-input 18) ->instr))

(defn solve [input evaluate]
  (reduce (fn [total instr]
            (+ total (evaluate instr)))
          0
          input))

(comment
  ;; part 1
  (solve (->input) evaluate-1)

  ;; part 2
  (solve (->input) evaluate-2))
