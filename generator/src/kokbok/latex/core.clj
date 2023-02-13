(ns kokbok.latex.core
  (:require
   [kokbok.cl :refer [cl-aget cl-aget-all]]))

(defn as-string [body]
  (clojure.string/join body))

(defn as-print [body]
  (doseq [b body] (print b)))

(defn raw [& more]
  more)

(defn rawln [& more]
  (concat more (list "\n")))

(defn text [& more]
  ;;TODO: escape & and pass to raw. Or have an escape function?
  ;; convert "" into ``''
  ;;TODO: convert ?hur mycket? into \unsure{hur mycket?} ?
  (apply raw more))

(defn statements [& body]
  (->> (rawln)
       repeat
       (interleave body)
       (reduce concat)))

(defn surrounded [beg end body]
  (concat (list beg) body (list end)))

(defn environment [name optargs args & body]
  (concat
   (raw "\\begin{" name "}")
   (mapcat (partial surrounded "[" "]") optargs)
   (mapcat (partial surrounded "{" "}") args)
   (rawln)
   (apply statements body)
   (rawln "\\end{" name "}")))

(defn command [name optargs args]
  (concat
   (raw "\\" name)
   (mapcat (partial surrounded "[" "]") optargs)
   (mapcat (partial surrounded "{" "}") args)))

(defn tabular-row [& body]
  (concat
   (reduce concat (interpose (raw "&") body))
   (raw "\\\\")))

(defn quotes [body]
  (surrounded "``" "''" body))

(defn braced [body]
  (surrounded "{" "}" body))

(defn math [body]
  (surrounded "$" "$" body))

(defn kv-pairs [& pairs]
  (->> (for [[k v] pairs]
         (concat k (raw "=") v))
       (interpose (raw ","))
       (reduce concat)))
