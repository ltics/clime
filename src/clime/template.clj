(ns clime.template
  (:refer-clojure :exclude [resolve]))

(defn- resolve-tokens
  [tokens context]
  (if (empty? tokens)
    context
    (let [token (first tokens)]
      (resolve-tokens (rest tokens) (context token)))))

(defn resolve
  [name context]
  (let [resolve' #(resolve-tokens (clojure.string/split %1 #"\.") %2)]
    (if (.startsWith name "@")
      (let [context (context "@" {})
            name (subs name 1)]
        (resolve' name context))
      (resolve' name context))))
