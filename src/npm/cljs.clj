(ns npm.cljs)


(defmacro deftask [task bindings & body]
  `(defn ~task ~bindings
     (fn []
       ~@body)))

