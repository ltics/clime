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
    (condp = raw-start
      VAR_TOKEN_START VAR_FRAGMENT
      BLOCK_TOKEN_START (if (= (take-str (:clean fragment) 3) "end")
                          CLOSE_BLOCK_FRAGMENT
                          OPEN_BLOCK_FRAGMENT)
      TEXT_FRAGMENT)))

(defn eval-expression
  [node exp]
  (assoc node :it [(if (symbol? exp) "name" "literal") exp]))

(defprotocol Node
  (creates_scope [_])
  (enter_scope [_])
  (exit_scope [_])
  (process_fragment [self])
  (render [self content]))

(defrecord Root [children]
  Node
  (render [self context]
    (render-children self context)))

(defrecord Variable [children name]
  Node
  (creates_scope [_] false)
  (process_fragment [self] self)
  (render [self context]
    (resolve (:name self) context)))

(defrecord Text [children text]
  Node
  (creates_scope [_] false)
  (process_fragment [self] self)
  (render [self _] (:text self)))

(defrecord Each [children fragment]
  Node
  (creates_scope [_] true)
  (enter_scope [self] (do (prn "enter each scope") self))
  (exit_scope [self] (do (prn "exit each scope") self))
  (process_fragment [self]
    (let [it (-> (cs/split (:fragment self) WHITESPACE) second read-string)]
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

(def cmd-construct-map
  {"each" ->Each})

(defn create-node
  [fragment]
  (let [type (type fragment)
        clean (:clean fragment)
        cmd (first (cs/split clean WHITESPACE))
        construct-func (condp = type
                         TEXT_FRAGMENT ->Text
                         VAR_FRAGMENT ->Variable
                         (cmd-construct-map cmd))]
    (if construct-func
      (process_fragment
        (construct-func [] clean)))))

(defn deref-nested-atoms
  [atoms nested-key]
  (let [val @atoms
        nested (nested-key val)]
    (assoc val nested-key (mapv #(deref-nested-atoms % nested-key) nested))))

(defn compile
  [template-string]
  (let [root (atom (->Root []))]
    (loop [fragments (map #(clean-fragment (->Fragment %))
                          (tokenizer template-string))
           scope-stack [root]]
      (if ((complement empty?) fragments)
        (let [fragment (first fragments)]
          (if (empty? scope-stack)
            (throw (Exception. "nesting issues"))
            (let [parent-scope (last scope-stack)]
              (if (= (type fragment) CLOSE_BLOCK_FRAGMENT)
                (do (swap! parent-scope exit_scope)
                    (recur (rest fragments) (drop-last-v scope-stack)))
                (let [new-node (atom (create-node fragment))]
                  (if @new-node
                    (let [children (conj (:children @parent-scope) new-node)]
                      (swap! parent-scope assoc :children children)
                      (let [scope-stack (conj (drop-last-v scope-stack) parent-scope)]
                        (if (creates_scope @new-node)
                          (do (swap! new-node enter_scope)
                              (recur (rest fragments) (conj scope-stack new-node)))
                          (recur (rest fragments) scope-stack))))))))))
        (deref-nested-atoms
          (first scope-stack) :children)))))