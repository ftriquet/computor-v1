(ns computor-v1.polynom
  (:require [clojure.string]))

(defn sum [p1 p2]
  (merge-with + p1 p2))


(defn to-list [p]
  (list (get p 0 0) (get p 1 0) (get p 2 0)))

(defn delta [[c b a]]
  (- (* b b) (* 4 a c)))

(defn initial-guess [x]
  (loop [i 0.0]
	(if (= (* i i) x)
		i
		(if (> (* i i) x)
		i
		(recur (inc i))))))

(defn babylonian-sqrt [s]
  (let [x0 (initial-guess s) threshold 0.0001]
	(if (= (* x0 x0) s)
	  x0
	  (loop [xn x0]
		(if (< (- (* xn xn) s) threshold)
		  xn
		  (recur (* 0.5 (+ xn (/ s xn)))))))))

(defn solve [p]
  (let [d (delta p)
        [c b a] p]
    (if (or (= a 0) (= a 0.0))
      (list {:re (- (/ c b))})
      (if (>= d 0)
	  	(do
		  (println (str "Positive discrimant, real solution(s)"))
          (list {:re (/ (- (- b) (babylonian-sqrt d)) (* 2 a))}
                {:re (/ (+ (- b) (babylonian-sqrt d)) (* 2 a))}))
		(do
		  (println (str "Negative discrimant, complex solution(s)"))
          (list {:re (/ (- b) (* 2 a)) :im (/ (- (babylonian-sqrt (- d))) (* 2 a))}
                {:re (/ (- b) (* 2 a)) :im (/ (babylonian-sqrt (- d)) (* 2 a))}))
		))))


(defn split-terms [string]
  (letfn [(filter-empty-strings [strings]
            (filter #(not (= %1 "")) strings))
          (split-minus [terms]
            (map (partial str "-") (filter-empty-strings (clojure.string/split terms #"-")))) ]
    (map #(clojure.string/replace %1 #"-\+" "+")
      (mapcat split-minus
              (map (partial str "+")
                   (filter-empty-strings (clojure.string/split string #"\+")))))))

(defn extract-term [string]
  (let [s                   (clojure.string/replace string #" " "")
        number              (re-matches #"[-+]\d+\.?\d*" s)
        [match coef power]  (re-matches #"([-+]\d*\.?\d*)\*?X(\^\d+)?" s)]
    (if number
      (list (Float. number) 0)
      (if match
        (list (cond (= "+" coef) 1.0
                    (= "-" coef) -1.0
                    :else (Float. coef))
              (if (nil? power)
                1
                (Integer. (clojure.string/replace power #"\^" ""))))
        nil))))

(defn term->pol [[coef power :as term]]
  (when term (hash-map power coef)))

(defn reduce-terms [terms]
  "Merges a list of term into a single map representing a polynom"
  (reduce (fn [h v]
            (let [[power] (keys v)
                  [coef]  (vals v)]
              (if (h power)
                (assoc h power (+ (h power) coef))
                (assoc h power coef))))
          {}
          terms))

(defn substract [p1 p2]
  (loop [res p1
         [key & rest :as k] (keys p2)]
    (cond
      (empty? k) res
      (res key) (recur (assoc res key (- (res key) (p2 key))) rest)
      :else (recur (assoc res key (- (p2 key))) rest))))

(defn parse-polynom [string]
  "Takes the string form of a polynom and returns a hash mapping degrees to coefficients"
  (let [terms (->> (split-terms string)
                   (filter #(not= "+" %))
                   (filter #(not= "-" %))
                   (map extract-term)
                   (map term->pol))]
    (cond
      (some number? terms) (some #(when (number? %) %) terms)
      (some nil? terms) nil
      :else (reduce-terms terms))))

(defn parse-line [line]
  (let [s (clojure.string/replace line #" " "")
        [match left-term right-term] (re-matches #"(.+)=(.+)" s)]
    (when match
      (let [left  (parse-polynom left-term)
            right (parse-polynom right-term)]
        (if (or (nil? left) (nil? right))
          nil
          (substract left right))))))

(defn infinite-solutions [p]
  (every? (fn [coef] (= 0.0 coef)) (vals p)))

(defn nil-or-zero [v]
  (or (nil? v) (= 0 v) (= 0.0 v)))

(defn no-solutions [p]
  (and (nil-or-zero (p 1)) (nil-or-zero (p 2))
   (not= 0.0 (p 0))))
