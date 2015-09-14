(ns clime.sweet-test
  (:require [clojure.test :refer :all]
            [clime.cota :refer :all]
            [clime.sweet :refer :all])
  (:refer-clojure :exclude [resolve render type]))

(deftest each-test
  (testing "test-each-iterable-in-context"
    (is= (render "{% each items %}<div>{{item}}</div>{% end %}" {"items" ["alex" "maria"]})
         "<div>alex</div><div>maria</div>"))
  (testing "test-each-iterable-as-literal-list"
    (is= (render "{% each [1, 2, 3] %}<div>{{item}}</div>{% end %}")
         "<div>1</div><div>2</div><div>3</div>"))
  (testing "test-each-parent-context"
    (is= (render "{% each [1, 2, 3] %}<div>{{@name}}-{{item}}</div>{% end %}" {"name" "jon doe"})
         "<div>jon doe-1</div><div>jon doe-2</div><div>jon doe-3</div>"))
  (testing "test-each-space-issues"
    (is= (render "{% each [1,2, 3]%}<div>{{item}}</div>{%end%}") "<div>1</div><div>2</div><div>3</div>"))
  (testing "test-each-no-tags-inside"
    (is= (render "{% each [1,2,3] %}<br>{% end %}") "<br><br><br>"))
  (testing "test-nested-objects"
    (is= (render "<h1>{{name}}</h1>{% each lines %}<span class='{{@name}}-{{item.name}}'>{{item.name}}</span>{% end %}" {"lines" [{"name" "l1"}] "name" "p1"})
         "<h1>p1</h1><span class='p1-l1'>l1</span>"))
  (testing "test-nested-tag"
    (is= (render "{% each items %}{% if item %}yes{% end %}{% end %}" {"items" ["" nil "2"]}) "yes")))

(deftest if-test
  (testing "test-simple-if-is-true"
    (is= (render "{% if num > 5 %}<div>more than 5</div>{% end %}" {"num" 6})) "<div>more than 5</div>")
  (testing "test-simple-if-is-false"
    (is= (render "{% if num > 5 %}<div>more than 5</div>{% end %}" {"num" 4})) "")
  (testing "test-if-else-if-branch"
    (is= (render "{% if num > 5 %}<div>more than 5</div>{% else %}<div>less than 5</div>{% end %}" {"num" 6})
         "<div>more than 5</div>"))
  (testing "test-if-else-else-branch"
    (is= (render "{% if num > 5 %}<div>more than 5</div>{% else %}<div>less or equal to 5</div>{% end %}" {"num" 4})
         "<div>less or equal to 5</div>"))
  (testing "test-nested-if"
    (let [tpl "{% if num > 5 %}{% each [1, 2] %}{{item}}{% end %}{% else %}{% each [3, 4] %}{{item}}{% end %}{% end %}"]
      (is= (render tpl {"num" 6}) "12")
      (is= (render tpl {"num" 4}) "34")))
  (testing "test-truthy-thingy"
    (let [tpl "{% if items %}we have items{% end %}"]
      (is= (render tpl {"items" []}) "")
      (is= (render tpl {"items" nil}) "")
      (is= (render tpl {"items" ""}) "")
      (is= (render tpl {"items" [1]}) "we have items"))))

(deftest call-test
  (testing "test-no-args"
    (is= (render "{% call pow %}" {"pow" pow}) "4.0"))
  (testing "test-positional-args"
    (is= (render "{% call pow 3 %}" {"pow" pow}) "9.0")
    (is= (render "{% call pow 2 3 %}" {"pow" pow}) "8.0")))
