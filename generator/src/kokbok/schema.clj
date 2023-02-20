(ns kokbok.schema
  (:require
   [clojure.set :refer [difference intersection]]
   [clojure.string :refer [join starts-with?]]))

(defn- schema-key-optional [s]
  (starts-with? s "-"))

(defn- schema-key-name [s]
  (if (schema-key-optional s)
    (subs s 1)
    s))

(defn- path-of
  ([trail] (join "." trail))
  ([trail end] (path-of (conj trail end))))

(def schema-types {"string" (type "")
                   "number" (type 0)})

(defn- schema-compliant-impl [schema rec path]
  (cond
    (and (string? schema)
         (not= (get schema-types schema) (type rec)))
    {:different-types [{:path (path-of path)
                        :expected (get schema-types schema)
                        :actual (type rec)}]}

    (and (vector? schema)
         (vector? rec))
    (do
      (assert (== 1 (count schema)) "incorrect schema")
      (reduce #(merge-with into %1 %2)
              {}
              (map #(schema-compliant-impl (first schema) %2 (conj path %1))
                   (range)
                   rec)))

    (and (map? schema)
         (map? rec))
    (let [rec-keys (->> rec
                        keys
                        (map name)
                        set)
          schema-keys (->> schema
                           keys
                           (map schema-key-name)
                           set)
          extras (->> (difference rec-keys schema-keys)
                      (map #(path-of path %))
                      vec)
          required (->> schema
                        keys
                        (filter (complement schema-key-optional))
                        (map schema-key-name)
                        set)]
      (reduce #(merge-with into %1 %2)
              (cond-> {}
                (seq extras) (assoc :invalid-keys extras)
                (not= required (intersection required rec-keys)) (assoc :missing-keys
                                                                        (->>
                                                                         (difference required rec-keys)
                                                                         (map #(path-of path %))
                                                                         vec)))
              (for [k (intersection rec-keys schema-keys)]
                (schema-compliant-impl (or (get schema (str "-" k))
                                           (get schema k))
                                       (get rec (keyword k))
                                       (conj path k)))))
    :else {}))

(defn schema-compliant? [schema rec]
  (schema-compliant-impl schema rec []))
