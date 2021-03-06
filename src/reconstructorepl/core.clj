(ns reconstructorepl.core
  (:require [clojure.main :as main]))

"
Notes to self:

* TODO refactor so that method of storing data (repl or metadata) is separate
  from decision where to store the defining forms. Move REPL code into separate
  namespace.
* Is there a form keyword? If not, could write one. Like source, but pulls it
  from the metadata or registry (could fall back to source)
* Partial simplified solution: if we store the form as metadata, then we can
  just check to see whether that metadata exists. If it does, we prepend it to
  our list of expressions; if it doesn't we return the symbol as entered. Maybe
  not guaranteed to work, but nice and simple.
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

;; Set a dynamic var to control whether forms are pulled from metadata
;; or from a registry
(def ^:dynamic *form-storage* :registry)

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

(defonce form-registry (atom {}))
(assert (map? @form-registry))

(defn local-form
  "Given a quoted symbol, returns the *local* form which defined
  it, if it can be found in the of the var's metadata."
  [sym]
  (if (= :registry *form-storage*)
    (@form-registry sym)
    (:form (meta (resolve sym)))))

(defn clear-registry [] (reset! form-registry {}))

(defn prompt []
  (print "***> "))

(defn expr-type
  "Identify the sort of expression which s is. 'defr' means def or defn."
  [s]
  (if (sequential? s)
    (case (first s)
      defn    :defr
      def     :defr
      ;; require :reqr
      :othr)
    :othr))

(defn is-defr?
  "Return just def and defn, ie the ones I might need as supporting
  players."
  [s]
  (contains? #{'def 'defn} (first s)))

(defn add-to-registry [form]
  (when (is-defr? form)
    (let [name (second form)]
      (swap! form-registry assoc name form))))

(defn distinct-last
  "Just like `distinct` except that the *last* duplicated element
  is kept instead of the first."
  [coll]
  (reverse (distinct (reverse coll))))

(defn needed-defrs
  "Return just the elements of defr which are symbols, & hence may need
  definitions. Ignore the first two elements of def , which are respectively
  'def and the name. Ignore the first three elements of defn, which are
  respectively 'defn, the name, and the parameters."
  ([defr]
   (needed-defrs defr #{}))
  ([defr existing-params]
   (let [typ (first defr)
         ;; for/let/etc bindings are vectors of alternating lhs and rhs
         each-lhs #(map first (partition 2 %))
         is-binding-form? #(contains? #{'for 'let} %)
         params (cond
                  (= typ 'defn) (into existing-params (nth defr 2))
                  (is-binding-form? typ) (into existing-params
                                                (each-lhs (nth defr 1)))
                  :else existing-params)
         is-param? #(contains? params %)
         candidates (condp = typ
                      'def  (drop 2 defr)
                      'defn (drop 3 defr)
                      'for  (drop 2 defr)
                      'let  (drop 2 defr)
                      defr)]
     (distinct-last
      (remove nil?
              (flatten
               (for [el candidates]
                 (cond (and (symbol? el) (not (is-param? el))) el
                       (sequential? el) (needed-defrs el params)))))))))

(declare build-code-for)

(defn local-definition [sym-defr]
  (conj
   (apply concat ; Was removing nils from this, but maybe I don't need to?
          (for [subsym (needed-defrs sym-defr)]
            (build-code-for subsym)))
   sym-defr))

(defn get-form [sym]
  (case *form-storage*
    :registry (@form-registry sym)
    :metadata (local-form sym)
    (throw (IllegalStateException. "*form-storage* must be set to :registry or :metadata"))))

(defn build-code-for
  "Build a list containing current and all children where children are the defrs
  that must be performed before current."
  [sym]
  (if-let [sym-defr (get-form sym)]
      ;; Locally defined:
    (local-definition sym-defr)
      ;; Not locally defined:
    (if (or (resolve sym) (special-symbol? sym))
        ;; if we can resolve it or it's a special form, it's defined elsewhere and
        ;; we can just ignore it.
      nil
        ;; Otherwise it's undefined.
      (throw (Exception. (str sym " is not defined."))))))

(defn build-defs
  "Given the symbol for a var (eg `'foo`), return a sequence of statements
  which, run in order, will let you define sym from scratch."
  [^clojure.lang.Symbol sym]
  (let [def-tree (build-code-for sym)]
    (distinct (reverse def-tree))))

(defn read-with-save
  "Like main/repl-read, except saves history in user-history. `q` to exit
  (ctrl-d can't be used, as it may exit the outer repl too)."
  [request-prompt request-exit]
  (or ({:line-start request-prompt :stream-end request-exit}
       (main/skip-whitespace *in*))
      (let [input (read {:read-cond :allow} *in*)]
        ;; (println input)
        (if (= input 'q)
          request-exit
          (do
            (when (and (is-defr? input) (= :registry *form-storage*))
              (add-to-registry input))
            (main/skip-if-eol *in*)
            input)))))

(defn saving-repl
  "Run a REPL which stores expressions so that other fns can organize it
  and build code from it."
  ;; TODO - offer ability to use a custom registry?
  ([]
   (clear-registry)
   (saving-repl :no-clear))
  ([_] ; bonus arity to skip clearing (for dev)
   (main/repl :read read-with-save
              :prompt prompt)
   ;; (process-history) ; no longer needed
))
