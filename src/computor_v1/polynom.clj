(ns computor-v1.polynom
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string]))



(defn delta [[c b a]]
  (- (* b b) (* 4 a c)))

(defn solve [p]
  (let [d (delta p)
        [c b a] p]
    (if (> d 0)
      (list {:re (/ (- (- b) (math/sqrt d)) (* 2 a)) :im 0}
            {:re (/ (+ (- b) (math/sqrt d)) (* 2 a)) :im 0})
      (list {:re (/ (- b) (* 2 a)) :im (/ (- (math/sqrt (- d))) (* 2 a))}
            {:re (/ (- b) (* 2 a)) :im (/ (math/sqrt (- d)) (* 2 a))}))))


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
              (if (nil? power) 1
                (Integer. (clojure.string/replace power #"\^" "")) ))
        nil))))

(defn term->pol [[coef power :as term]]
  (println term)
  (cond
    (nil? term) nil
    (some #{0 1 2} [power]) (case power
                              0 (list coef 0 0)
                              1 (list 0 coef 0)
                              2 (list 0 0 coef))
    :else nil))

(defn parse-polynom [string]
  (let [terms (map term->pol (map extract-term (split-terms string)))]
    (if (some nil? terms) nil
      (apply (partial map +) terms))))
