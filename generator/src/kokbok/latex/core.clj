(ns kokbok.latex.core)

(defn as-string [body]
  (clojure.string/join body))

(defn as-print [body]
  (doseq [b body] (print b)))

(def conc concat)

(defn raw [& more]
  more)

(defn rawln [& more]
  (conc more (list "\n")))

(defn text [& more]
  ;;TODO: escape & and pass to raw. Or have an escape function?
  ;; convert "" into ``''
  ;;TODO: convert ?hur mycket? into \unsure{hur mycket?} ?
  (apply raw more))

(defmacro optional
  ([pred mapper value] `(let [v# ~value]
                          (when (~pred v#)
                            (~mapper v#))))
  ([pred value] `(let [v# ~value]
                   (when (~pred v#)
                     v#))))

(defn statements [body]
  (->> (rawln)
       repeat
       (interleave body)
       (reduce conc)))

(defn surrounded [beg end body]
  (conc (raw beg) body (raw end)))

(defmacro multify [plural singular]
  `(let [p# ~plural
         s# ~singular]
     (cond
       (some? p#) p#
       (some? s#) (list s#)
       :else nil)))

(defn environment [name & {:keys [optargs args optarg arg body]}]
  (conc
   (raw "\\begin{" name "}")
   (mapcat (partial surrounded "[" "]") (multify optargs optarg))
   (mapcat (partial surrounded "{" "}") (multify args arg))
   (rawln)
   (statements body)
   (rawln "\\end{" name "}")))

(defn command [name & {:keys [args optargs arg optarg]}]
  (conc
   (raw "\\" name)
   (mapcat (partial surrounded "[" "]") (multify optargs optarg))
   (mapcat (partial surrounded "{" "}") (multify args arg))))

(defn tabular-row [& columns]
  (conc
   (reduce conc (interpose (raw "&") columns))
   (raw "\\\\")))

(defn quotes [body]
  (surrounded "``" "''" body))

(defn braced [body]
  (surrounded "{" "}" body))

(defn math [body]
  (surrounded "$" "$" body))

(defn kv-pairs [pairs]
  (->> (for [[k v] pairs]
         (conc k (raw "=") v))
       (interpose (raw ","))
       (reduce conc)))
