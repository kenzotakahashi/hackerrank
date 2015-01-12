;list replication
(fn[num lst]
  (loop [x lst]
    (when-not (empty? x)
      (dotimes [n num]
        (println (first x)))
      (recur (rest x)))))


;filter array
(fn [delim lst]
  (loop [x lst y []]
    (if (empty? x)
      y
      (if (< (first x) delim)
        (recur (rest x) (conj y (first x)))
        (recur (rest x) y)))))

(a 5 [5 7 9 3 4 1 2 8])

;odd indices
(defn a[lst]
  (loop [odd false x lst y []]
    (if (empty? x)
      y
      (if (true? odd)
        (recur (not odd) (rest x) (conj y (first x)))
        (recur (not odd) (rest x) y)))))

(a [1 2 3 4 5])

;range
(defn a [n]
  (vec (range n)))

(a 4)

;reverse
(defn a [lst]
  (loop [x lst y '()]
    (if (empty? x)
      y
      (recur (rest x) (cons (first x) y)))))

(a '(1 2 3))

;sum of odd
(defn a [lst]
  (loop [x lst sum 0]
    (if (empty? x)
      sum
      (if (odd? (first x))
        (recur (rest x) (+ (first x) sum))
        (recur (rest x) sum)))))

(a [2 3 4 6 7])


;length
(defn a [lst]
  (loop [x lst n 0]
    (if (empty? x)
      n
      (recur (rest x) (inc n)))))

(a [1 2 3])

;absolute
(defn a [lst]
  (map #(if (pos? %)
          %
          (* % -1)
          ) lst))

(a [-2 3 -54])

;sequence
(defn a [lst]
  (loop [y lst z []]
    (if (empty? y)
      z
      (let [x (first y)]
        (recur (rest y)
               (conj z (loop [i 0 sum 0]
                (if (= i 10)
                  sum
                  (recur (inc i) (+ (let [exp (reduce * (repeat i x))
                                          fac (reduce * (range 1 (inc i)))]
                                           (/ exp fac)) sum))))))))))

(a [20.0000 5.0000])

(map #(Float. (read-line)) (range (Integer. (read-line))))

(for [i (Integer. (read-line)), x []]
  (if (= i 0)
    x
    (recur (dec i)
           (conj x (read-line))
    )
  )
)


(defn exp [x n]
  (reduce * (repeat n x)))

(exp 2 3)

(defn factorial [n]
  (reduce * (range 1 (inc n))))

(factorial 2)






;; Fibonacci memoization
(def n (read-string (read-line)))
(def lst (for [x (range 0 n)] (read-string (read-line))))

(def fib-seq
     (lazy-cat [0 1] (map + (rest fib-seq) fib-seq)))


(println (mod (last (take n fib-seq)) 100000007))

;;Pascal's triangle
