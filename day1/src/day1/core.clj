(ns day1.core)

(def to-ints
  (partial map #(Integer/parseInt %)))

; thread takes expressions, not just functions, as arguments
(defn read-input
  "Reads in an advent of code 2021 file and turns it in to a list of depths"
  [path]
  (-> path
      (slurp)
      (clojure.string/split-lines)
      (to-ints)))

(defn count-larger
  [inputs]
 
  (reduce is-deeper {:depth 0 :count -1} inputs))


(defn is-deeper
  [memo depth]
  (let [prev-depth (:depth memo)
        new-memo (assoc memo :depth depth)]
    ;(println memo depth)
    (if (> depth prev-depth)
      (assoc new-memo :count (+ (:count new-memo) 1))
      new-memo)))

(defn do-thing
  [path] 
  (let [input (read-input path)
        counts (count-larger input)]
    (println (count input))
    (println counts)))

(defn is-deeper-partition [memo next-partition]
  (let [prev-depth (:depth memo)
        depth (reduce + next-partition)
        new-memo (assoc memo :depth depth)]
    (if (> depth prev-depth)
      (assoc new-memo :count (+ (:count new-memo) 1))
      new-memo)))

(defn sliding-window [inputs]
  (reduce is-deeper-partition {:depth 0 :count -1} (partition 3 1 inputs)))