(ns clime.exception)

(defn throw-template-error
  [msg]
  (throw (Exception. msg)))

(defn throw-template-syntax-error
  [error-syntax]
  (throw (Exception. (format "'%s' seems like invalid syntax" error-syntax))))

