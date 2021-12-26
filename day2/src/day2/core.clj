(ns day2.core)

(defn parse-input
  "Read in the input file in the form of [direction] [magnitude] in to a list of maps"
  [path]
  (-> path
      (slurp)
      (clojure.string/split-lines)
      (to-directions))
  )

(defn to-direction [line]
  (let [[direction magnitude] (clojure.string/split line #" ")]
    {:direction direction :magnitude (Integer/parseInt magnitude)}
    ))

(defn to-directions [lines] (map to-direction lines))

(defn get-final-position [directions] 
  (reduce update-position {:depth 0 :horizontal 0} directions))

(defn update-position [position direction]
  (let [depth (:depth position) horizontal (:horizontal position)]
    (condp = 
           (:direction direction) 
           "forward" (assoc position :horizontal (+ horizontal (:magnitude direction)))
           "up" (assoc position :depth (- depth (:magnitude direction)))
           "down" (assoc position :depth (+ depth (:magnitude direction)))

      )))

(defn solve-pt-1 [path]
  (let [
        input (parse-input path)
        final-position (get-final-position input)
        ]
    (* (:depth final-position) (:horizontal final-position))))

(defn get-final-position-with-aim [directions]
  (reduce update-position-with-aim {:depth 0 :horizontal 0 :aim 0} directions))

(defn update-position-with-aim [{:keys [depth horizontal aim] :as position}
                                {direction :direction magnitude :magnitude}]
  (condp = direction
    "forward" (assoc position 
                     :horizontal (+ horizontal magnitude)
                     :depth (+ depth (* magnitude aim)))
    "up" (assoc position :aim (- aim magnitude))
    "down" (assoc position :aim (+ aim magnitude))))

(defn solve-pt-2 [path]
  (let [input (parse-input path)
        position (get-final-position-with-aim input)]
    (* (:depth position) (:horizontal position))))