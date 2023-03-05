(ns kokbok.latex.core
  (:require [clojure.string :as s]))

(defn as-string [body]
  (clojure.string/join body))

(defn as-print [body]
  (doseq [b body] (print b)))

(def conc concat)

(defn raw [& more]
  more)

(defn rawln [& more]
  (conc more (list "\n")))

(defn escape [string]
  (-> string
      (s/replace " - " " --- ")
      (s/replace "\\" "\\textbackslash{}")
      (s/replace "%" "\\%")
      (s/replace "{" "\\{")
      (s/replace "}" "\\}")
      (s/replace "&" "\\&")
      (s/replace "#" "\\#")
      (s/replace "<" "\\textless{}")
      (s/replace ">" "\\textgreater{}")
      (s/replace "_" "\\_")
      (s/replace "$" "\\$")))

(defn escape-quotes [string]
  (s/replace string #"\"(.*?)\"" "``$1''"))

(defn escape-unsure [string]
  (s/replace string #"[?](.*?)[?]" "\\\\unsure{$1\\?}"))

(defn text [& more]
  (->> more
       clojure.string/join
       escape
       escape-quotes
       escape-unsure
       raw))

(defmacro optional
  ([pred mapper value] `(let [v# ~value]
                          (when (~pred v#)
                            (~mapper v#))))
  ([pred value] `(let [v# ~value]
                   (when (~pred v#)
                     v#))))

(defn opt-text [string]
  (optional some? text string))

(defn opt-raw [string]
  (optional some? raw string))

(defn opt-body [body]
  (optional seq body))

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
