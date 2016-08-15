(ns computor-v1.arithmetic-test
  (:require [clojure.test :refer :all]
            [computor-v1.arithmetic]))

(deftest test-add
  (testing "complex numbers addition"
    (let [x {:re 1 :im 3}
          y {:re 4 :im 0}
          sum (computor-v1.arithmetic/add x y)]
      (is (= (:re sum) 5))
      (is (= (:im sum) 3)))))

(deftest test-sub
  (testing "complex numbers addition"
    (let [x {:re 1 :im 3}
          y {:re 4 :im 0}
          sub (computor-v1.arithmetic/sub x y)]
      (is (= (:re sub) -3))
      (is (= (:im sub) 3)))))

(deftest test-mult
  (testing "complex numbers addition"
    (let [x {:re 1 :im 3}
          y {:re 4 :im 0}
          prod (computor-v1.arithmetic/mult x y)]
      (is (= (:re prod) 4))
      (is (= (:im prod) 12)))))
