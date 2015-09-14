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
    (is= (render (->Root []) {"items" ["a1" "b1"]}) "")
    (is= (render (->Variable [] "@name") {"@" {"lines" [{"name" "l1"}] "name" "p1"}}) "p1")
    (is= (render-children (->Variable [] "@name") {"@" {"lines" [{"name" "l1"}] "name" "p1"}}) "")
    (is= (render (process-fragment (->Each [] "each vars")) {"vars" ["a1" "b1"]}) "")
    (let [ifnode (process-fragment (->If [] "if num > 5"))]
      (is= (:lhs ifnode) ["name" 'num])
      (is= (:op ifnode) ">")
      (is= (:rhs ifnode) ["literal" 5]))
    (let [ifnode (process-fragment (->If [] "if it"))]
      (is= (:lhs ifnode) ["name" 'it])))
  (testing "create-node"
    (let [fragments (map #(clean-fragment (->Fragment %)) (tokenizer "cleantha{% each items %}<div>{{it}}</div>{% end %}"))]
      (is= (:text (create-node (first fragments))) "cleantha")
      (is= (:fragment (create-node (second fragments))) "each items")
      (is= (:text (create-node (nth fragments 2))) "<div>")
      (is= (:name (create-node (nth fragments 3))) "it")
      (is= (:text (create-node (nth fragments 4))) "</div>")
      (is (nil? (create-node (last fragments))))))
  (testing "compile"
    (is= (compile "cleantha{% each items %}<div>{{item}}</div>{% end %}" {"items" ["a1" "b1"]}) "cleantha<div>a1</div><div>b1</div>")
    (is= (compile "{% each [1, 2, 3] %}<div>{{item}}</div>{% end %}" {}) "<div>1</div><div>2</div><div>3</div>")
    (is (compile* "{% if num > 5 %}<div>more than 5</div>{% end %}"))
    (is= (compile "{% if num > 5 %}<div>more than 5</div>{% end %}" {"num" 6}) "<div>more than 5</div>")
    (is= (compile "{% if num > 5 %}<div>more than 5</div>{% end %}" {"num" 5}) "")
    (is= (compile "{% if num > 5 %}<div>more than 5</div>{% else %}<div>less than or equal to 5</div>{% end %}" {"num" 6}) "<div>more than 5</div>")
    (is= (compile "{% if num > 5 %}<div>more than 5</div>{% else %}<div>less than or equal to 5</div>{% end %}" {"num" 5}) "<div>less than or equal to 5</div>")
    (is= (compile "{% if num > 5 %}{% each [1, 2] %}{{item}}{% end %}{% else %}{% each [3, 4] %}{{item}}{% end %}{% end %}" {"num" 5}) "34")
    (is= (compile "{% if num > 5 %}{% each [1, 2] %}{{item}}{% end %}{% else %}{% each [3, 4] %}{{item}}{% end %}{% end %}" {"num" 6}) "12")))
