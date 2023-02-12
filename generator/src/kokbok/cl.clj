(ns kokbok.cl)

(defn cl-aget [alist key]
  (when-some [kv (->> alist
                      (filter (comp #(= key %) first))
                      first)]
    (rest kv)))

(defn cl-aget-two [alist key]
  (first (cl-aget alist key)))

(defn cl-aget-all [alist key]
  (->> alist
       (filter (comp #(= key %) first))
       (map next)))
