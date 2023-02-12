(ns kokbok.latex.core
  (:require
   [kokbok.cl :refer [cl-aget cl-aget-all]]))

;;TODO: redo everything into functions that produces a vector (of deque?) of strings instead

(defn raw [& more]
  (doseq [m more] (print m)))

(defn rawln [& more]
  (apply raw more)
  (newline))

(defn text [& more]
  ;;TODO: escape & and pass to raw. Or have an escape function?
  ;; convert "" into ``''
  ;;TODO: convert ?hur mycket? into \unsure{hur mycket?} ?
  (apply raw more))

;; (defn surrounded [beg end functions]
;;   (let [functions (if (seqable? functions) functions (list functions))]
;;     (doseq [f functions]
;;       (raw beg)
;;       (f)
;;       (raw end))))

(defmacro surrounded [beg end & body]
  `(optional f# (do ~@body)
     (raw ~beg)
     (f#)
     (raw ~end)))

(defmacro environment
  {:style/indent :defn}
  [name & more]
  (let [body (cl-aget more :body)
        optargs (->> :optarg
                     (cl-aget-all more)
                     (map (fn [x] `(surrounded "[" "]" ~@x))))
        args (->> :arg
                  (cl-aget-all more)
                  (map (fn [x] `(surrounded "{" "}" ~@x))))]
    `(let [name# ~name]
       (raw "\\begin{" name# "}")
       ~@optargs
       ~@args
       (rawln)
       ~@body
       (rawln)
       (rawln "\\end{" name# "}"))))

;; (environment "hej"
;;   (arg (raw "arg"))
;;   (body (raw "hej"))
;;   (optarg (raw "opt1"))
;;   (optarg (raw "opt2")))

;; (defn environment [name & {:keys [args optargs body]}]
;;   (raw "\\begin{" name "}")
;;   (when (some? optargs)
;;     (surrounded "[" "]" optargs))
;;   (when (some? args)
;;     (surrounded "{" "}" args))
;;   (rawln)
;;   (body)
;;   (rawln "\\end{" name "}"))

(defmacro command
  {:style/indent :defn}
  [name & more]
  (let [optargs (->> :optarg
                     (cl-aget-all more)
                     (map (fn [x] `(surrounded "[" "]" ~@x))))
        args (->> :arg
                  (cl-aget-all more)
                  (map (fn [x] `(surrounded "{" "}" ~@x))))]
    `(do
       (raw "\\" ~name)
       ~@optargs
       ~@args
       nil)))

(defmacro statements [& more]
  `(do
     ~@(interleave more (repeat (list rawln)))))

(defmacro tabular-row [& columns]
  `(do
     ~@(interpose (list raw "&") columns)
     (raw "\\\\")))

;; (defn command [name & {:keys [args optargs]}]
;;   (raw "\\" name)
;;   (when (some? optargs)
;;     (surrounded "[" "]" optargs))
;;   (when (some? args)
;;     (surrounded "{" "}" args)))

(defmacro quotes [& body]
  `(surrounded "``" "''" ~@body))

(defmacro braced [& body]
  `(surrounded "{" "}" ~@body))

(defmacro math [& body]
  `(surrounded "$" "$" ~@body))

(defmacro optional-str
  {:style/indent :defn}
  [var expr & body]
  `(let [~var ~expr]
     (when (some? ~var)
       ~@body)))

(defmacro optional
  {:style/indent :defn}
  [var expr & body]
  ;;TODO: write to a stream or something instead of a string
  `(let [opt# (with-out-str ~expr)
         ~var (fn [] (raw opt#))
         non-empty# (seq opt#)]
     (when non-empty#
       ~@body)
     non-empty#))

(defmacro kv-pairs [& pairs]
  (when-some [[k v] (first pairs)]
    (let [printed (gensym "printed")]
      `(let [~printed (optional f# ~v
                        ~k
                        (raw "=")
                        (f#))]
         ~(when-some [kvs (next pairs)]
            `(optional g# (kv-pairs ~@kvs)
               (when ~printed
                 (raw ","))
               (g#)))
         nil))))

;; (kv-pairs [(raw "a") (optional it "a" (raw it))]
;;           [(raw "x") (braced (raw "y"))])

;; (defn kv-pairs
;;   ([] nil)
;;   ([k1 v1 & kvs]
;;    (assert (-> kvs count even?) "each key must have a value, i.e., even number of arguments")
;;    (k1)
;;    (raw "=")
;;    (v1)
;;    (doseq [[key value] (partition 2 kvs)]
;;      (raw ",")
;;      (key)
;;      (raw "=")
;;      (value))))

;; (defn comma-separated [& more]
;;   (doseq [f (interpose #(raw ",") more)]
;;     (f)))

;; (defn key-value [key value]
;;   (key)
;;   (raw "=")
;;   (value))

;; (defn table-row []
;;   ;;TODO: 
;;   )

;; (defmacro defer [& more]
;;   `(fn [] ~@more))
