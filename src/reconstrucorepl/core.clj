(ns reconstructorepl.core
  (:require [clojure.main :as main]))

"
* Alternate strategy: Store form as metadata.
* Is there a form keyword?
* Don't forget the resolve keyword.
* Clear user history at Rebel startup.
* It's conceivable that it's as simple as, if resolve resolves it, it's
  legit.
* Reconstructarepl
* Partial simplified solution: if we store the form as metadata, then we can
just check to see whether that metadata exists, if it does, we prepended to our
list of statements expressions, if it doesn't we return the symbol AS
entered.Not guaranteed to work, but nice and simple.
* The same strategy would work if we use a central registry instead of'q
metadata.
* Better version in the long run appends each statement to history and maybe
processes it as well, so it doesn't have to be recreated each time.
* Note that we can cache the histories.
"

(comment
  (require 'repl-exp-01.core :reload)
  (in-ns 'repl-exp-01.core)
  )

(comment
  (def a 1)
  (def b 2)
  (defn f [n] (inc n))
  (* (f b) 2)
  )

(comment
  ;; organized history looks like:
  {:defn  {f (defn f [x] (* x 2))
           g (defn g [x] (+ x 18))}
   :def   {a (def a 3)
           b (def b 4)}
   :other #{'(* (f b) 2)}
   }
  )

(defonce user-history (atom []))
(defonce defr-history (atom {}))

(defn add-to-history
  "Does a 2-level merge -- adds the current def/defn to the map of them stored
  in h"
  [h [typ [name exp]]]
  (let [exp-map (h typ)
        updated-exp-map (merge exp-map {name exp})]
    (merge h {typ updated-exp-map})))

(defn defr-organizer [expr]
  (let [name (second expr)]
    [name expr]))

(defn organize-defrs [h]
  (let [defrs (:defr h)]
    (into {}
          (map defr-organizer defrs))))

(defn clear-history [] (reset! user-history []))

(defn saving-read
  "Like main/repl-read, except saves history in user-history.
  `q` to exit"
  [request-prompt request-exit]
  (or ({:line-start request-prompt :stream-end request-exit}
       (main/skip-whitespace *in*))
      (let [input (read {:read-cond :allow} *in*)]
        (if (= input 'q)
          request-exit
          (do (swap! user-history conj input)
              (main/skip-if-eol *in*)
              input)))))

(defn- prompt []
  (print "***>"))

(defn saving-repl
  "Run a REPL which stores expressions so that other fns can organize it
  and build code from it."
  []
  (main/repl :read saving-read
             :prompt prompt))

(defn grouper
  "Identify the sort of expression which s is"
  [s]
  (case (first s)
    defn    :defr
    def     :defr
    require :reqr
    :othr))

;; Option 1: group by 1st element. Good if I ultimately need more
;; than just def and defn
;; (defn grouper
;;   "Identify whether s is a def, a defn, or something else"
;;   [s]
;;   (cond
;;     (contains? #{'def 'defn} (first s)) :defr
;;     (= (first s) 'require) :require
;;     :othr))

;; ;; Option 2: just filter to the ones I want
;; (defn is-defr?
;;   "Return just def and defn, ie the ones I might need as supporting
;;   players."
;;   [s]
;;   (contains? #{'def 'defn} (first s)))
;;
;; (defn just-defrs [coll]
;;   (filter is-defr? coll))

(defn organize-history
  "Examines history, and pulls out defs, defns, and requires."
  [h]
  ;; Option 1
  (->> h
       (group-by grouper))
  ;; Option 2
  #_(just-defrs h))

(defn process-history []
  (let [org-hx (organize-history @user-history)
        defr-hx (organize-defrs org-hx)]
    ;; (println "org-hx " org-hx)
    ;; (println "defr-hx" defr-hx)
    (reset! defr-history defr-hx)))

#_(sequential? typ)

(defn distinct-last
  "Just like `distinct` except that the *last* duplicated element
  is kept instead of the first."
  [coll]
  (reverse (distinct (reverse coll))))

;; TODO this is an experiment which maybe isn't going to work out.
;; Stuff that it doesn't take into account:
;; - parameters passed to a function are skippable
;; - let, for, and anything with that binding structure. A var
;;   defined in a let doesn't need to be found in the environment.
;;   The RHS of the binding could be put into the env, or we could
;;   maintain a set of skippables.
;; - what else?

;; ALSO this function probably wants to do an actual walk rather
;; than this ad hoc`for`/`filter`.
(defn needed-defrs
  "Return just the elements of defr which are symbols, & hence may
  need definitions. Ignore the first two elements, which are
  respectively def/defn and the name."
  [defr]
  (let [typ (first defr)
        candidates (condp = typ
                     'def  (drop 2 defr)
                     ;; TODO need to identify parameter in defn
                     'defn (drop 3 defr)
                     defr)]
    ;; (println candidates)
    (distinct-last
     (filter identity
             (flatten
              (for [el candidates]
                (cond (symbol? el) el
                      (sequential? el) (needed-defrs el)
                      )))))))

(defn build-code-for
  [sym]
  (if-let [sym-defr (@defr-history sym)])
  )
