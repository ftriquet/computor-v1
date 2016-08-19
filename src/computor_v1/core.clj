(ns computor-v1.core
  (:gen-class)
  (:require [computor-v1.polynom :refer :all]))

(defn fail [& messages]
  (println (apply str "Error: " messages))
  (System/exit 1))

(defn format-cmplx [{a :re b :im}]
  (cond
    (nil? b) (str a)
    (= 0 b) (str a)
    (> b 0) (str a " + " b "i")
    :else   (str a " - " (- b) "i")))

(defn print-reducted-form [p]
  (print "Reducted form: ")
  (let [sorted (into (sorted-map-by <) p)]
    (doseq [[power coef] (map identity sorted)]
      (if (= 0 power)
        (print (str coef "*X^" power " "))
        (cond (> coef 0.0) (print (str "+ " coef "*X^" power " "))
              (< coef 0.0) (print (str "- " (- coef) "*X^" power " "))
              )))
    (println "= 0")))


(defn -main
  [& args]
  (when (not= (count args) 1) (computor-v1.core/fail "Invalid number of arguments"))
  (let [p (parse-line (first args))]
    (if (infinite-solutions p)
      (println "This polynom has an infinite number of solutions")
      (cond
        (nil? p) (fail "Invalid parameter")
        (not-every? #(or (or (or (= % 0) (= % 1)) (= % 2)) (= ( p %) 0.0) ) (keys p)) (fail "Polynome degree is too high")
        :else (let [[x1 x2 :as roots] (->> p (to-list) (solve))]
                (print-reducted-form p)
                (if (nil? x2)
                  (println (str "One root: " (:re x1)))
                  (if (= x1 x2)
                    (println "One double root: " (:re x1))
                    (println "Two roots: " (format-cmplx x1) ", " (format-cmplx x2)))))))))
