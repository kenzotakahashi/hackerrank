;GCD
(let [f     (fn [a b] (if (= a b)
                        a
                        (if (> a b)
                          (recur (- a b) b)
                          (recur (- b a) a))
                       ))
      [m n] (map read-string (re-seq #"\d+" (read-line)))]

(println (f m  n)))


; Fibonacci
(def fib-seq
     (lazy-cat [0 1] (map + (rest fib-seq) fib-seq)))


(last (take 2 fib-seq))


; string mingling
(let [f (fn [a b] (loop[a1 a b1 b x []]
                    (if (empty? a1)
                      (apply str x)
                      (recur (rest a1) (rest b1) (conj (conj x (first a1)) (first b1))))))
      [m n] [(read-line) (read-line)]]

(f m n))



; pascal
(defn pascal []
  (map #(clojure.string/join " " %)
       (iterate (fn [a]
                  (vec (map #(reduce + %) (partition 2 1 (cons 0 (conj a 0)))))) [1])))

(clojure.string/join "\n" (take 10 (pascal)))


(letfn [(pascal []
          (map #(clojure.string/join " " %)
               (iterate (fn [a]
                  (vec (map #(reduce + %) (partition 2 1 (cons 0 (conj a 0)))))) [1])))
        (join [n]
          (clojure.string/join "\n" (take n (pascal))))]
        (println (join (read-string (read-line)))))

; string-o-permute

(def n (read-string (read-line)))
(def lst (for [x (range 0 n)] (read-line)))

(defn string-o-permute [lst]
  (clojure.string/join "\n"
    (map (fn [x] (clojure.string/join (map (fn [[a b]] (apply str [b a])) (partition 2 x)))) lst)))

(string-o-permute ["ab" "abcd"])

(println (clojure.string/join "\n" (String-o-permute lst)))


;; string-compression
(defn separate [string]
  (drop 2 (reverse
           (reduce (fn [a b]
              (if (= (first a) b)
                (cons b a)
                (cons b (cons \@ a)))) [\@] string))))

(defn split-string [string]
  (clojure.string/split (apply str (separate string)) #"@"))


(defn string-compression [string]
  (apply str
    (map #(if (= (count %) 1)
          %
          (str (str (first %)) (str (count %)))) (split-string string))))

(separate "aaabccc")
(split-string "aaabccc")
(string-compression "aaabccc")

(println (string-compression (read-line)))

;; prefix-compression
(defn get-prefix [a b]
  (loop [lst (map vector a b) prefix ""]
    (let [[f s] (first lst)]
      (if (or (not (= f s)) (nil? f) (nil? s))
        prefix
        (recur (rest lst) (str prefix f))))))

(defn get-sufix [a b]
  (let [prefix (get-prefix a b)
        c (count prefix)
        p (fn [c x] (apply str (drop c x)))]
    [prefix (p c a) (p c b)]))

(defn count-and-prefix [string]
  (if (empty? string)
    "0"
    (str (count string) " " string)))

(defn prefix-compression [a b]
  (let [lst (get-sufix a b)]
    (clojure.string/join "\n" (map count-and-prefix lst))))

(get-prefix "kit" "kitcat")
(get-sufix "abcd" "a")
(prefix-compression "kit" "kitcat")


;; full-of-colors
(defn invalid? [dic]
  (or (> (Math/abs (- (:R dic) (:G dic))) 1)
      (> (Math/abs (- (:Y dic) (:B dic))) 1)))

(defn full-of-colors [string]
  (loop [dic {:R 0 :G 0 :Y 0 :B 0}, colors (map #(keyword (str %)) string)]
    (cond
     (invalid? dic) "False"
     (empty? colors) (if (and (= (:R dic) (:G dic)) (= (:Y dic) (:B dic)))
                       "True"
                       "False")
     :else (recur (assoc dic (first colors) (inc (dic (first colors)))) (rest colors)))))


(println (clojure.string/join "\n" (map full-of-colors lst)))


;; filter-elements
(defn count-elements [lst]
  (loop [dic {}, lst lst, order 0]
    (if (empty? lst)
      dic
      (let [x (first lst), y (rest lst)]
        (if (contains? dic x)
          (recur (assoc dic x [(inc (first (dic x))) (second (dic x))]) y order)
          (recur (assoc dic x [1 order]) y (inc order)))))))



(defn filter-elements [k lst]
  (let [counted  (count-elements lst)
        filtered (filter #(>= (first (val %)) k) counted)]
    (if (empty? filtered)
      "-1"
      (clojure.string/join " " (map key (sort-by #(second (val %)) filtered))))))


(dotimes [i (read-string (read-line))]
  (let [k   (second (read-string (read-line)))
        lst (read-string (read-line))]
    (println (filter-elements k lst))))

;; super digit
(defn super-digit [x]
  (if (< x 10)
    x
    (recur (reduce + (map read-string (rest (clojure.string/split (str x) #"")))))))


(super-digit (*' 861568688536788 100000))

(def i (map read-string (re-seq #"\d+" (read-line))))
(println (super-digit (* (first i) (second i))))

(super-digit 29)





















