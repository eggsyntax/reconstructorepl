(ns reconstructorepl.redef
  "An alternate strategy: instead of using a custom REPL, use custom versions
  of def and defn which record the defining form as metadata on the var."
  (:require [reconstructorepl.core :as r]
            [clojure.core :as c]))

(defmacro def
  "Identical to `def`, except that it adds the defining form
  to the var's metadata."
  [& args]
  (let [name# (first args)
        form# `(def ~@args)]
    ;; (println name# ":" form#)
    ;; (println (rest args))
    ;; TODO ensure correct return type
    `(alter-meta! (def ~name# ~@(rest args)) assoc :form '~form#)))


(defmacro defn'
  "Identical to `defn`, except that it adds the defining form
  to the var's metadata."
  [& args]
  (let [name# (first args)
        form# `(defn ~@args)]
    ;; (println "n:" name#)
    ;; (println "f:" form#)
    ;; TODO handle docstrings?
    ;; TODO maybe handle defns where an attr-map is *already* passed?
    `(defn ~name# {:form '~form#} ~@(rest args))
    ))
