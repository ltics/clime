(ns clime.cota-test
  (:require [clojure.test :refer :all]
            [clime.cota :refer :all]))

(deftest cota-test
  (testing "tokenizer"
    (is= (tokenizer "cleantha{% each items %}<div>{{it}}</div>{% end %}")
         '("cleantha" "{% each items %}" "<div>" "{{it}}" "</div>" "{% end %}"))
    (is= (tokenizer "{% each vars %}<i>{{it}}</i>{% endeach %}")
         '("{% each vars %}" "<i>" "{{it}}" "</i>" "{% endeach %}"))
    (let [content "Hello, {{name}}!
                  {% if role == \"admin\" %}
                  <a href=\"/dashboard\">Dashboard</a>
                  {% end %}"]
      (is= (tokenizer content)
           '("Hello, " "{{name}}" "!\n                  " "{% if role == \"admin\" %}" "\n                  <a href=\"/dashboard\">Dashboard</a>\n                  " "{% end %}")))))
