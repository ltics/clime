(ns clime.template-test
  (:require [clojure.test :refer :all]
            [clime.cota :refer :all]
            [clime.template :refer :all])
  (:refer-clojure :exclude [resolve compile type]))

(deftest template-test
  (testing "fragment"
    (is= (type (clean-fragment (->Fragment "{% each vars %}"))) 1)
    (is= (type (clean-fragment (->Fragment "{% endeach %}"))) 2)
    (is= (type (clean-fragment (->Fragment "{{it}}"))) 0)
    (is= (type (clean-fragment (->Fragment "cleantha"))) 3))
  (testing "resolve"
    (let [context {"@" {"lines" [{"name" "l1"}] "name" "p1"}}]
      (is= (resolve "@name" context) "p1")))
  (testing "node"
    (is= (render (->Variable [] "@name") {"@" {"lines" [{"name" "l1"}] "name" "p1"}}) "p1")
    (is= (render-children (->Variable [] "@name") {"@" {"lines" [{"name" "l1"}] "name" "p1"}}) "")
    (is= (render (process_fragment (->Each []) "each vars") {"vars" ["a1" "b1"]}) "")))
