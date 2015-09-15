(ns clime.compile
  (:require [clojure.string :as cs])
  (:require [clime.cota :refer :all]
            [clime.node :refer :all]
            [clime.exception :refer :all])
  (:refer-clojure :exclude [resolve compile type]))

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

(def cmd-construct-map
  {"each" ->Each
   "if"   ->If
   "else" ->Else
   "call" ->Call})

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
