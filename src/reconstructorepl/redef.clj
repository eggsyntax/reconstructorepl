(ns reconstructorepl.redef
  "An alternate strategy: instead of using a custom REPL, use custom versions
  of def and defn which record the defining form as metadata on the var."
  (:require [reconstructorepl.core :as r]
            [clojure.core :as c]))

;; If we're using def' and defn', forms are stored in metadata.
(alter-var-root (resolve `r/*form-storage*) (fn [& _] :metadata))

(defmacro def'
  "Identical to `def`, except that it adds the defining form
  to the var's metadata."
  [& args]
  (let [name# (first args)
        form# `(def ~@args)]
    ;; (println name# ":" form#)
    ;; (println "f:" form#)
    `(let [new-f# (def ~name# ~@(rest args))]
       (alter-meta! new-f# assoc :form '~form#)
       new-f#)))


;; TODO not working? throwing stack overflow on
;; (def m 3)
;; (def n (inc m))
;; (defn' h [c] (* c n))

(defmacro defn'
  "Identical to `defn`, except that it adds the defining form
  to the var's metadata."
  [& args]
  (let [name# (first args)
        ;; We have to handle form# differently than we did in def', because def
        ;; is a special form, but defn is a macro
        form# (cons 'defn args)]
    ;; (println "n:" name#)
    ;; (println "f:" form#)
    ;; TODO handle docstrings?
    ;; TODO maybe handle defns where an attr-map is *already* passed?
    `(defn ~name# {:form '~form#} ~@(rest args))
    ))
