(ns clime.cota
  (:require [clojure.test :refer :all]))

(def VAR_FRAGMENT 0)
(def OPEN_BLOCK_FRAGMENT 1)
(def CLOSE_BLOCK_FRAGMENT 2)
(def TEXT_FRAGMENT 3)

(def VAR_TOKEN_START "\\{\\{")
(def VAR_TOKEN_END "\\}\\}")
(def BLOCK_TOKEN_START "\\{\\%")
(def BLOCK_TOKEN_END "\\%\\}")

(def TOK_REGEX (re-pattern (format "%s.*?%s|%s.*?%s" VAR_TOKEN_START VAR_TOKEN_END BLOCK_TOKEN_START BLOCK_TOKEN_END)))
(def WHITESPACE #"\s+")

(defn tokenizer
  [template-str]
  (let [special (re-seq TOK_REGEX template-str)
        normal (clojure.string/split template-str TOK_REGEX)]
    (->> (map (fn [s n]
                [n s]) special normal)
         (apply concat)
         (filter (complement empty?)))))

(def operator_lookup_table
  {"<"  <,
   ">"  >,
   "==" =,
   "!=" not=,
   "<=" <=,
   ">=" >=})

;;for test

(defmacro is= [& body]
  `(is (= ~@body)))

(defmacro isnot [& body]
  `(is (not ~@body)))