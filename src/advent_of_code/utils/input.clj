(ns advent-of-code.utils.input
  (:require [clojure.java.io :as io]))

(defn load-input [day]
  (io/reader (io/resource (str "day-" day ".txt"))))

(defn line-parser [input f]
  (sequence (map f) (line-seq input)))

(defn parse-long [x]
  (Long/parseLong x))
