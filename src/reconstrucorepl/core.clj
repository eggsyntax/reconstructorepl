(ns reconstructorepl.core
  (:require [clojure.main :as main]))

"
* Alternate strategy: Store form as metadata.
* Is there a form keyword?
* Don't forget the resolve keyword.
* Clear user history at Rebel startup.
* It's conceivable that it's as simple as, if resolve resolves it, it's legit.
* Partial simplified solution: if we store the form as metadata, then we can
just check to see whether that metadata exists, if it does, we prepended to our
list of statements expressions, if it doesn't we return the symbol AS
entered.Not guaranteed to work, but nice and simple.
* The same strategy would work if we use a central registry instead of'q
metadata.
* Another version would override def and defn (maybe others) to include the
source form
* Better version in the long run appends each statement to history and maybe
processes it as well, so it doesn't have to be recreated each time.
* Note that we can cache the histories.
* TODO don't add exception-throwing expressions to history
* TODO allow defining multiple at once?
"

;; Note: by 'defr' I mean 'def or defn'
;;       by 'hx' I mean 'history'

(comment ; for pasting into saving-repl
  (def a 1)
  (def b (* a a))
  (defn f [n] (+ b n))
  (* (f b) 2)
  (defn g [n] (* a n))
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
        (if (= input 'q)
          request-exit
          (do (swap! user-history conj input)
              (main/skip-if-eol *in*)
              input)))))

(defn- prompt []
  (print "***>"))

(defn expr-type
  "Identify the sort of expression which s is"
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
   ;; (print defr ":")
   (let [typ (first defr)
         params (if (= typ 'defn)
                  (into existing-params (nth defr 2))
                  existing-params)
         ;; _ (println "params:" params)
         is-param? #(contains? params %)
         candidates (condp = typ
                      'def  (drop 2 defr)
                      ;; TODO need to identify parameter in defn
                      'defn (drop 3 defr)
                      defr)]
     ;; (println candidates)
     (distinct-last
      (remove nil?
              (flatten
               (for [el candidates]
                 (do #_(println ">>" el (contains? params el))
                     #_(println ">" el (is-param? el))
                     (cond (and (symbol? el) (not (is-param? el))) el
                           (sequential? el) (needed-defrs el params))))))))))

(defn build-code-for
  "Build a tree of the form [current (children)] where children are the defrs
  that must be performed before current."
  [sym]
  (if-let [sym-defr (@defr-history sym)]
    (do
      ;; #_(inspect sym-defr)
      (apply list
             sym-defr
             (remove nil?
                     (apply concat
                            (for [subsym (needed-defrs sym-defr)]
                              (build-code-for subsym))))))
    ;; Not locally defined:
    (if (or (resolve sym) (special-symbol? sym))
      ;; if we can resolve it or it's a special form, it's defined elsewhere and
      ;; we can just ignore it.
      nil ; alternate: return sym?
      ;; Otherwise it's undefined.))
      (str sym " undefined")
      #_(throw (Exception. (str sym " is not defined!"))))))

(defn build-defs
  "Return a sequence of statements which, run in order, will let you define
  sym from scratch."
  [sym]
  (let [def-tree (build-code-for sym)]
    (reverse def-tree)))

(defn saving-repl
  "Run a REPL which stores expressions so that other fns can organize it
  and build code from it."
  ([]
   (clear-history)
   (saving-repl :no-clear))
  ([_]
   (main/repl :read read-with-save
              :prompt prompt)
   (process-history)))
