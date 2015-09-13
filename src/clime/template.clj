(ns clime.template
  (:require [clojure.string :as cs]
            [clime.cota :refer :all])
  (:refer-clojure :exclude [resolve compile type]))

(defn- resolve-tokens
  [tokens context]
  (if (empty? tokens)
    context
    (let [token (first tokens)]
      (resolve-tokens (rest tokens) (context token)))))

(defn resolve
  [name context]
  (let [resolve' #(resolve-tokens (cs/split %1 #"\.") %2)]
    (if (.startsWith name "@")
      (let [context (context "@" {})
            name (subs name 1)]
        (resolve' name context))
      (resolve' name context))))

(declare render-children)

(defrecord Fragment [raw-text])

(defn clean-fragment
  [fragment]
  (let [raw (:raw-text fragment)
        clean (if (#{VAR_TOKEN_START BLOCK_TOKEN_START} (take-str raw 2))
                (-> raw
                    cs/trim
                    (drop-str 2)
                    (drop-last-str 2)
                    cs/trim)
                raw)]
    (assoc fragment :clean clean)))

(defn type
  [fragment]
  (let [raw-start (take-str (:raw-text fragment) 2)]
    (prn raw-start)
    (condp = raw-start
      VAR_TOKEN_START VAR_FRAGMENT
      BLOCK_TOKEN_START (if (= (take-str (:clean fragment) 3) "end")
                          CLOSE_BLOCK_FRAGMENT
                          OPEN_BLOCK_FRAGMENT)
      TEXT_FRAGMENT)))

(type (clean-fragment (->Fragment "{% each vars %}")))
(type (clean-fragment (->Fragment "{% endeach %}")))
(type (clean-fragment (->Fragment "{{it}}")))
(type (clean-fragment (->Fragment "cleantha")))

(defn eval-expression
  [node exp]
  (assoc node :it [(if (symbol? exp) "name" "literal") exp]))

(defprotocol Node
  (creates_scope [_])
  (process_fragment [self fragment])
  (render [self content]))

(defrecord Root [children]
  Node
  (render [self context]
    (render-children self context)))

(defrecord Variable [children name]
  Node
  (creates_scope [_] false)
  (render [self context]
    (resolve (:name self) context)))

(defrecord Each [children]
  Node
  (creates_scope [_] true)
  (process_fragment [self fragment]
    (let [it (-> (cs/split fragment WHITESPACE) second read-string)]
      (eval-expression self it)))
  (render [self context]
    (let [it (:it self)
          items (if (= (first it) "literal")
                  (second it)
                  (resolve (str (second it)) context))
          render-item (fn [item]
                        (render-children self {"@"    context
                                               "item" item}))]
      (cs/join "" (map render-item items)))))

(defn render-children
  ([node context]
   (render-children node context nil))
  ([node context children]
   (let [render-child (fn [child]
                        (let [child-html (render child context)]
                          (if child-html
                            (str child-html) "")))]
     (if (nil? children)
       (let [children (:children node)]
         (cs/join "" (map render-child children)))
       (cs/join "" (map render-child children))))))

(defn compile
  []
  (let [root (->Root [])
        scope-stack (atom [root])]
    ))

(render (->Variable [] "@name") {"@" {"lines" [{"name" "l1"}] "name" "p1"}})
(render-children (->Variable [] "@name") {"@" {"lines" [{"name" "l1"}] "name" "p1"}})

(render (process_fragment (->Each []) "each vars") {"vars" ["a1" "b1"]})
