(ns computor-v1.polynom-test
  (:require [clojure.test :refer :all]
            [computor-v1.polynom :refer :all]
            [clojure.pprint]))

(deftest parsing
  (testing "parsing"
    (testing "invalid polynoms"
      (testing "with empty string"
        (is (= (parse-line "") nil)))
      (testing "with invalid characters"
        (is (= (parse-line "-1 * X ^ 0 _ + 2 * X ^ 1 - 2 * X ^ 2 = 0") nil)))
      (testing "with no equals sign")
        (is (= (parse-line "-1 * X ^ 0 + 2 * X ^ 1 - 2 * X ^ 2") nil)))
      (testing "with empty terms"
        (is (= (parse-line "-1 * X ^ 0 + 2 * X ^ 1 - 2 * X ^ 2=") nil))
        (is (= (parse-line "=-1 * X ^ 0 + 2 * X ^ 1 - 2 * X ^ 2") nil)))
    (testing "complete polynoms"
        (is (not= (parse-line "-1 * X ^ 0 + 2 * X ^ 1 - 2 * X ^ 2= 1 * X^0 - 3 * X^1") nil))
        (is (not= (parse-line "-1*X^0+2*X^1-2*X^2=1*X^0-3*x^1+4*X^2") nil))
      )
    )
  )
