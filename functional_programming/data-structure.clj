;; messyMedians

;; This prints for each query
(defn messyMedians1 [steps]
  (loop [lst [] steps steps]
    (if (empty? steps)
      lst
      (let [new-lst (process-step lst (first steps))]
        (println (median (first lst)))
        (recur new-lst (rest steps))))))

(defn messyMedians [steps]
  (loop [lst [] steps steps]
    (if (empty? steps)
      (map median lst)
      (recur (process-step lst (first steps)) (rest steps)))))


(messyMedians [1 5 -2 3 2 5 4 -7 2 -3])


(defn process-step [lst step]
  (if (pos? step)
    (vec (cons (cons step (first lst)) lst))
    (vec (cons (get lst (dec (* -1 step))) lst))))


(process-step ['(1 5) '(1)] -2)


;; possible improvements: instead of sorting every vector, place N in the right place
;; so that when it gets copied, it will not have to be sorted again
(defn median [lst]
  (get (vec (sort lst)) (dec (quot (inc (count lst)) 2))))

(median [3 4 2 1 5])


(println (clojure.string/join "\n" (reverse (messyMedians lst))))