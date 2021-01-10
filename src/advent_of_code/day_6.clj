(ns advent-of-code.day-6
  (:require [advent-of-code.utils.input :as in]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn ->every-answer-set [line]
  (set (string/replace line #"\s" "")))

(defn ->every-common-answer-set [line]
  (reduce set/intersection (map set (string/split-lines line))))

(comment
  ;; part 1
  (transduce (map count) + 0 (in/group-parser (in/load-input 6) ->every-answer-set))

  ;; part 2
  (transduce (map count) + 0 (in/group-parser (in/load-input 6) ->every-common-answer-set)))
