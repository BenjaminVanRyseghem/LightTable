(ns lt.macros
  "Set of macros used in LightTable.
  Note that the macros are written in Clojure are the macro-expansion
  is done at compilation time (the unescaped sexp are in ClojureScript though)"
  (:require [clojure.walk :as walk]))

(defn- namify
  "Namify a KEYWORD by escaping the dots and prepending the TYPE"
  [type keyword]
  (symbol (str "__" type "__" (.replace (name keyword) "." "__DOT__"))))

(defn- ->params
  "Ensure a [params body] form from BODY"
  [body]
  (if (vector? (first body))
    [(first body) (rest body)]
    [[] body]))

(defmacro behavior [name & {:keys [reaction] :as r}]
  (if (and (seq? reaction) (= 'fn (first reaction)))
    (let [[_ args & body] reaction]
      `(do
         (defn- ~(namify "BEH" name) ~args ~@body)
         (lt.object/behavior* ~name ~@(apply concat (assoc r :reaction (namify "BEH" name))))))
    `(lt.object/behavior* ~name ~@(apply concat r))))

(defmacro defui [sym params hiccup & events]
  `(defn ~sym ~params
     (let [e# (crate.core/html ~hiccup)]
       (doseq [[ev# func#] (partition 2 ~(vec events))]
         (lt.util.dom/on e# ev# func#))
       e#)))

(defmacro timed [ev & body]
  `(let [start# (lighttable.util.js/now)
         res# (do ~@body)]
     (lighttable.components.logger/log ~ev (- (lighttable.util.js/now) start#))
     res#))

(defmacro on
  "Wrapper for the `lighttable.command/on` function"
  [name & body]
  `(lighttable.command/on ~name (fn ~@body)))

(defmacro in [ctx & body]
  (let [[params body] (->params body)]
    `(assoc ~ctx :in (fn ~params ~@body))))

(defmacro out [ctx & body]
  (let [[params body] (->params body)]
    `(assoc ~ctx :out (fn ~params ~@body))))

(defmacro extract [elem kvs & body]
  (let [defs (vec (apply concat (for [[k v] (partition 2 kvs)]
                                  `[~k (lt.util.dom/$ ~v ~elem)])))]
    `(let ~defs
       ~@body)))

(defmacro with-time [& body]
  (let [start (gensym "start")
        body (walk/postwalk-replace {'time (list '- '(.getTime (js/Date.)) start)} body)]
  `(let [~start (.getTime (js/Date.))]
     ~@body)))

(defmacro background [func]
  `(lt.objs.thread/thread*
    (fn ~(gensym "tfun") []
      (let [orig# (js/argsArray js/arguments)
            msg# (.shift orig#)
            args# (.map orig# cljs.reader/read-string)
            ~'raise (fn [obj# k# v#]
                     (js/_send obj# k# (pr-str v#) "clj"))]
        (.unshift args# (.-obj msg#))
        (.apply ~func nil args#)))))

(defmacro aloop [[var arr] & body]
  `(let [arr# ~arr]
     (loop [~var 0]
       (when (< ~var (.-length arr#))
         ~@body
         (recur (+ ~var 1))))))

(comment

  (worker (fn [v]
            (do-something v))
          :zomg (fn [r]
                  ))

  (defui cool [l]
    [:li (bound l)]

    :click (fn [e]
             (this-as me
                      ))
    :hover (fn [e]
             ))

  )
