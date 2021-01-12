(ns advent-of-code.day-14
  (:require [advent-of-code.utils.input :as in]))

(defn compile-mask-1 [mask]
  (->> mask
       (map-indexed vector)
       (remove (comp #{\X} second))
       (map (fn [[idx val]]
              (let [val (in/parse-long val)]
                #(assoc % idx val))))
       (reduce comp identity)))

(defn compile-mask-2 [mask]
  (fn [bits]
    (loop [output [bits] [[idx c] & more] (map-indexed vector mask)]
      (case c
        nil output
        \0 (recur output more)
        \1 (recur (mapv #(assoc % idx 1) output)
                  more)
        \X (recur (into (mapv #(assoc % idx 0) output) (map #(assoc % idx 1)) output)
                  more)))))

(defn ->bits [num]
  (let [result (Long/toBinaryString num)]
    (-> (repeat (- 36 (count result)) 0)
        vec
        (into (map in/parse-long) result))))

(defn ->num [bits]
  (in/parse-long (apply str bits) 2))

(defn parse-line [compile line]
  (let [[_ mask] (re-find #"mask = (\w+)" line)
        [_ loc val] (re-find #"mem\[(\d+)\] = (\d+)" line)]
    (if mask
      [:mask (compile mask)]
      [:write (in/parse-long loc) (in/parse-long val)])))

(defn ->input [compile]
  (in/line-parser (in/load-input 14) (partial parse-line compile)))

(defmulti process-1 (fn [_ [type]] type))

(defmethod process-1 :mask
  [state [_ mask]]
  (assoc state :mask mask))

(defmethod process-1 :write
  [state [_ loc n]]
  (update state :mem assoc loc ((:mask state) (->bits n))))

(defmulti process-2 (fn [_ [type]] type))

(defmethod process-2 :mask
  [state [_ mask]]
  (assoc state :mask mask))

(defmethod process-2 :write
  [state [_ loc n]]
  (reduce (fn [state bits]
            (update state :mem assoc (->num bits) (->bits n)))
          state
          ((:mask state) (->bits loc))))

(defn solve [input process]
  (->> input
       (reduce process {:mask identity :mem {}})
       :mem
       vals
       (transduce (map ->num) + 0)))

(comment
  ;; part 1
  (solve (->input compile-mask-1) process-1)

  ;; part 2
  (solve (->input compile-mask-2) process-2))
