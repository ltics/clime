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

(defn throw-template-error
  [msg]
  (throw (Exception. msg)))

(defn throw-template-syntax-error
  [error-syntax]
  (throw (Exception. (format "'%s' seems like invalid syntax" error-syntax))))

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
  [node exp key]
  (let [ast-eval (read-string exp)]
    (assoc node key [(if (symbol? ast-eval) "name" "literal") ast-eval])))

(defprotocol Node
  (creates-scope [_])
  (enter-scope [_])
  (exit-scope [_])
  (process-fragment [self])
  (render [self context]))

(defrecord Root [children]
  Node
  (render [self context]
    (render-children self context)))

(defrecord Variable [children name]
  Node
  (creates-scope [_] false)
  (process-fragment [self] self)
  (render [self context]
    (resolve (:name self) context)))

(defrecord Text [children text]
  Node
  (creates-scope [_] false)
  (process-fragment [self] self)
  (render [self _] (:text self)))

(defrecord Each [children fragment]
  Node
  (creates-scope [_] true)
  (enter-scope [self] (do (prn "enter each scope") self))
  (exit-scope [self] (do (prn "exit each scope") self))
  (process-fragment [self]
    (let [it (apply str (-> (cs/split (:fragment self) WHITESPACE) rest))]
      (eval-expression self it :it)))
  (render [self context]
    (let [it (:it self)
          items (if (= (first it) "literal")
                  (second it)
                  (resolve (str (second it)) context))
          render-item (fn [item]
                        (render-children self {"@"    context
                                               "item" item}))]
      (cs/join "" (map render-item items)))))

(defrecord Else [children fragment]
  Node
  (creates-scope [_] false)
  (process-fragment [self] self)
  (render [self _]))

(defn resolve-side
  [side context]
  (if (= (first side) "literal")
    (second side)
    (resolve (str (second side)) context)))

(defn split-children
  [node]
  (loop [children (:children node)
         curr :if-branch
         branchs {:if-branch   []
                  :else-branch []}]
    (let [child (first children)]
      (if ((complement empty?) children)
        (if (instance? Else @child)
          (recur (rest children) :else-branch branchs)
          (recur (rest children) curr (update-in branchs [curr] conj child)))
        (let [{:keys [if-branch else-branch]} branchs]
          {:if-branch   (mapv #(deref-nested-atoms % :children) if-branch)
           :else-branch (mapv #(deref-nested-atoms % :children) else-branch)})))))

(defrecord If [children fragment]
  Node
  (creates-scope [_] true)
  (enter-scope [self] (do (prn "enter if scope") self))
  (exit-scope [self] (do (prn "exit if scope")
                         (let [branches (split-children self)]
                           (merge self branches))))
  (process-fragment [self]
    (let [fragment (:fragment self)
          bits (drop-v 1 (cs/split fragment WHITESPACE))]
      (if (not (#{1 3} (count bits)))
        (throw-template-syntax-error fragment)
        (let [self-with-lhs (eval-expression self (nth bits 0) :lhs)]
          (if (= (count bits) 3)
            (eval-expression
              (assoc
                self-with-lhs :op (nth bits 1))
              (nth bits 2) :rhs)
            self-with-lhs)))))
  (render [self context]
    (let [lhs (resolve-side (:lhs self) context)
          exec-if-branch (if-let [op-name (:op self)]
                           (if-let [op (operator_lookup_table op-name)]
                             (let [rhs (resolve-side (:rhs self) context)]
                               (op lhs rhs))
                             (throw-template-syntax-error op-name))
                           (truth? lhs))]
      (render-children self context (if exec-if-branch
                                      (:if-branch self)
                                      (:else-branch self))))))

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
  {"each" ->Each
   "if"   ->If
   "else" ->Else})

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
      (process-fragment
        (construct-func [] clean)))))

(defn compile*
  [template-string]
  (let [root (atom (->Root []))]
    (loop [fragments (map #(clean-fragment (->Fragment %))
                          (tokenizer template-string))
           scope-stack [root]]
      (if ((complement empty?) fragments)
        (let [fragment (first fragments)]
          (if (empty? scope-stack)
            (throw-template-error "nesting issues")
            (let [parent-scope (last scope-stack)]
              (if (= (type fragment) CLOSE_BLOCK_FRAGMENT)
                (do (swap! parent-scope exit-scope)
                    (recur (rest fragments) (drop-last-v scope-stack)))
                (let [new-node (atom (create-node fragment))]
                  (if @new-node
                    (let [children (conj (:children @parent-scope) new-node)]
                      (swap! parent-scope assoc :children children)
                      (let [scope-stack (conj (drop-last-v scope-stack) parent-scope)]
                        (if (creates-scope @new-node)
                          (do (swap! new-node enter-scope)
                              (recur (rest fragments) (conj scope-stack new-node)))
                          (recur (rest fragments) scope-stack))))))))))
        (deref-nested-atoms
          (first scope-stack) :children)))))

(defn compile
  [template-string context]
  (render (compile* template-string) context))
