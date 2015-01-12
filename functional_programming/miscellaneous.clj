;; lcm (bunny)
(defn gcd [a b]
  (if (= a b)
    a
    (if (> a b)
      (recur (- a b) b)
      (recur (- b a) a))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn multiple-lcm [numbers]
  (reduce lcm numbers))

(read-line)
(println (multiple-lcm (map read-string (clojure.string/split (read-line) #" "))))


;; rotate-string
(defn rotate-string [string]
  (loop [n (dec (count string)) lst [string]]
    (if (= n 0)
      lst
      (let [current (first lst)]
        (recur (dec n) (cons (clojure.string/join (conj (vec (rest current)) (first current))) lst))))))

(def n (read-string (read-line)))
(dotimes [i n]
  (let [rotated (rotate-string (read-line))
        original (last rotated)]
    (println (clojure.string/join " " (rest (reverse (cons original rotated)))))))


(rotate-string "abc")






















