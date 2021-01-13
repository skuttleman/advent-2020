(ns advent-of-code.day-19
  (:require [advent-of-code.utils.input :as in]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(declare compile-rule)

(defn compile* [rules parts]
  (string/join (map (fn [part]
                      (let [conds (get rules part)]
                        (cond-> conds
                          (not (string? conds)) (as-> $
                                                      (str "("
                                                           (second (compile-rule rules [part $]))
                                                           ")")))))
                    parts)))

(def compile-rule
  (memoize (fn [rules [name conds]]
             [name (cond-> conds
                     (not (string? conds)) (->> (map (partial compile* rules))
                                                (string/join "|")))])))

(defn compile-rules [rules]
  (into {} (map (partial compile-rule rules)) rules))

(defn parse-rule [rule]
  (mapv (comp keyword str)
        (edn/read-string (str "[" rule "]"))))

(defn ->rule [line]
  (let [[_ name rule] (re-find #"([^:]+): (.+)" line)]
    [(keyword name)
     (if-let [[_ rule] (re-matches #"\"(\w)\"" rule)]
       rule
       (mapv parse-rule (string/split rule #"\|")))]))

(defn ->input []
  (let [[rules msgs] (string/split (slurp (in/load-input 19)) #"\n\n")
        rules' (into {} (map ->rule) (string/split-lines rules))]
    {:rules     (compile-rules rules')
     :raw-rules rules'
     :messages  (string/split-lines msgs)}))

(defn match? [rule val]
  (re-matches (re-pattern rule) val))

(defn solve [input]
  (->> input
       :messages
       (filter (partial match? (get-in input [:rules :0])))
       count))

(comment
  ;; part 1
  (solve (->input)))
