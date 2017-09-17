(ns reconstructorepl.core
  (:require [clojure.main :as main]))

"
* Alternate strategy: Store form as metadata.
** ex: (alter-meta! (resolve 'f) assoc :source '(defn f [x] (inc x)))
* Is there a form keyword? If not, could write one. Like source, but pulls it
  from the metadata or registry (could fall back to source)
* Partial simplified solution: if we store the form as metadata, then we can
  just check to see whether that metadata exists. If it does, we prepend it to
  our list of expressions; if it doesn't we return the symbol as entered. Maybe
  not guaranteed to work, but nice and simple.
* The same strategy would work if we use a central registry instead of metadata.
* Another version would override def and defn (maybe others) to include the
  source form. That version wouldn't require a custom repl as long as you're
  willing to inject the custom def or defn, or require/refer them.
* Better version in the long run processes each expression as it's read, so it
  doesn't have to be recreated each time. Could then get rid of all but one
  registry (alhough there must be other things the history is good for)
* Note that we can cache the histories.
* TODO don't add exception-throwing expressions to history
* Known failure mode: if you def a as 1, then def b as (inc a), then redefine a
  as 100, and then (build-defs) and replay them in a new namespace, b will
  reflect the newest value of a, and hence differ from its previous value.
  This has the potential to suprprise, but I believe it matches actual repl use
  more closely; when you redefine a var in the repl, it's usually because you're
  changing it from an incorrect value to a correct value, and so other vars
  based on it should change as well. At some point I may consider making it
  possible to specify which behavior you prefer.

"

;; TODO set a dynamic var to control whether forms are pulled from
;; metadata or from a registry

;; Terms: by 'defr' I mean 'def or defn'
;;        by 'hx' I mean 'history'

(comment ; for pasting into saving-repl
  (def a 1)
  (def b (* a a))
  (defn f [n] (+ b n))
  (* (f b) 2)
  (defn g [n] (* a n))
  (defn h [a b] (+ (f a) (g b)))
  )

(comment
  ;; organized history looks like:
  {:defr  {f (defn f [x] (* x 2))
           g (defn g [x] (+ x 18))
           a (def a 3)
           b (def b 4)}
   :other #{'(* (f b) 2)}})

(defonce user-history (atom []))
(defonce defr-history (atom {}))

(defn form
  "Given a symbol (typically passed quoted), returns the form which
  defined it, if it can be found."
  [sym]
  (or (:form (meta (resolve sym)))
      (clojure.repl/source-fn sym)))

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

(defn read-with-save
  "Like main/repl-read, except saves history in user-history.
  `q` to exit (ctrl-d exits outer repl too)."
  [request-prompt request-exit]
  (or ({:line-start request-prompt :stream-end request-exit}
       (main/skip-whitespace *in*))
      (let [input (read {:read-cond :allow} *in*)]
        (println input)
        (if (= input 'q)
          request-exit
          (do (swap! user-history conj input)
              (main/skip-if-eol *in*)
              input)))))

(defn prompt []
  (print "***> "))

(defn expr-type
  "Identify the sort of expression which s is. 'defr' means def or defn."
  [s]
  (if (sequential? s)
    (case (first s)
      defn    :defr
      def     :defr
      require :reqr
      :othr)
    :othr))

;; Option 1: group by 1st element. Good if I ultimately need more
;; than just def and defn
;; (defn expr-type
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
       (group-by expr-type))
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

(defn needed-defrs
  "Return just the elements of defr which are symbols, & hence may
  need definitions. Ignore the first two elements of def , which are
  respectively 'def and the name. Ignore the first three elements of defn, which are
  respectively 'defn, the name, and the parameters."
  ([defr]
   (needed-defrs defr #{}))
  ([defr existing-params]
   (let [typ (first defr)
         params (if (= typ 'defn)
                  (into existing-params (nth defr 2))
                  ;; else def, has no new params
                  existing-params)
         is-param? #(contains? params %)
         candidates (condp = typ
                      'def  (drop 2 defr)
                      'defn (drop 3 defr)
                      defr)]
     (distinct-last
      (remove nil?
              (flatten
               (for [el candidates]
                 (cond (and (symbol? el) (not (is-param? el))) el
                       (sequential? el) (needed-defrs el params)))))))))

(declare build-code-for)

(defn local-definition [sym]
  (when-let [sym-defr (@defr-history sym)]
    (conj
     (apply concat ; Was removing nils from this, but maybe I don't need to?
            (for [subsym (needed-defrs sym-defr)]
              (build-code-for subsym)))
     sym-defr)))

(defn build-code-for
    "Build a list containing current and all children where children are the defrs
  that must be performed before current."
    [sym]
    (if-let [sym-defr (@defr-history sym)]
      ;; Locally defined:
      (local-definition sym)
      ;; Not locally defined:
      (if (or (resolve sym) (special-symbol? sym))
        ;; if we can resolve it or it's a special form, it's defined elsewhere and
        ;; we can just ignore it.
        nil
        ;; Otherwise it's undefined.
        (throw (Exception. (str sym " is not defined."))))))

(defn build-defs
  "Return a sequence of statements which, run in order, will let you define
  sym from scratch."
  [sym]
  (let [def-tree (build-code-for sym)]
    (distinct (reverse def-tree))))

(defn saving-repl
  "Run a REPL which stores expressions so that other fns can organize it
  and build code from it."
  ([]
   (clear-history)
   (saving-repl :no-clear))
  ([_] ; bonus arity to skip clearing (for dev)
   (main/repl :read read-with-save
              :prompt prompt)
   (process-history)))
