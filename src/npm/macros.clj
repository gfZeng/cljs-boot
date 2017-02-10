(ns npm.macros
  (:require [clojure.walk :as walk]))

(defmacro defn-async [name args & body]
  `(defn ~name [~@args ~'&async]
     ~@body))


(defn- async-expr [expr]
  (when (-> expr meta :async)
    expr))

(defn expand-async [bind expr & body]
  (if-let [async (some async-expr (tree-seq seq? seq expr))]
    (let [bind' (gensym "bind")
          expr (walk/prewalk
                (fn [x]
                  (if (async-expr x)
                    bind'
                    x))
                expr)]
      `(~@async (fn [~bind']
                  (let [~bind ~expr]
                    ~@body))))
    `(let [~bind ~expr]
       ~@body)))

(defmacro let-async [[bind expr & bindings] & body]
  (if-not expr
    `(do ~@body)
    (expand-async bind expr
                  `(let-async ~(vec bindings) ~@body))))
