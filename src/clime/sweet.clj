(ns clime.sweet
  (:require [clime.compile :refer :all :exclude [render]])
  (:refer-clojure :exclude [resolve compile type]))

(defn render
  ([template-string]
   (render template-string {}))
  ([template-string context]
   (compile template-string context)))
