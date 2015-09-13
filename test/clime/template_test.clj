(ns clime.template-test
  (:require [clojure.test :refer :all]
            [clime.cota :refer :all]
            [clime.template :refer :all]))

(deftest template-test
  (testing "resolve"
    (let [context {"@" {"lines" [{"name" "l1"}] "name" "p1"}}]
      (is= (resolve "@name" context) "p1"))))
