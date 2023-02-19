(ns kokbok.generation
  (:require
   [clojure.java.io :refer [file]]
   [clojure.set :refer [difference intersection]]
   [clojure.string :refer [join starts-with?]]
   [kokbok.latex.recipe :as lr]
   [kokbok.latex.primitives :as lp]
   [toml.core :as toml]
   [kokbok.latex.core :as l]))

(defmacro defversion [name]
  `(do
     (defmulti ~name #(get-in % [:meta :version]))))

(defn- trim [str]
  (when (some? str)
    (clojure.string/trim str)))

(defversion portions)
(defmethod portions 1 [rec]
  (-> rec
      (get-in [:info :portions])))

(defversion bakingtime)
(defmethod bakingtime 1 [rec]
  (when-some [time (get-in rec [:info :bakingtime])]
    (if (map? time)
      time
      {:time time :unit "minute"})))

(defversion title)
(defmethod title 1 [rec]
  (-> rec
      (get-in [:info :title])
      trim))

(defversion picture)
(defmethod picture 1 [rec]
  (-> rec
      (get-in [:info :picture])))

(defversion introduction)
(defmethod introduction 1 [rec]
  (-> rec
      (get-in [:info :introduction])
      trim))

(defversion source-url)
(defmethod source-url 1 [rec]
  (-> rec
      (get-in [:info :source :url])))

(defversion source-book)
(defmethod source-book 1 [rec]
  (-> rec
      (get-in [:info :source :book])))

(defversion hint)
(defmethod hint 1 [rec]
  (-> rec
      (get-in [:info :hint])
      trim))

(defversion ingredients)
(defmethod ingredients 1 [rec]
  (-> rec
      (get-in [:preparation :ingredients])))

(defversion steps)
(defmethod steps 1 [rec]
  (when-some [s (get-in rec [:preparation :steps])]
    (map trim s)))

(defn- recipe-schema [version]
  (-> (format "recipe-schema-v%s.toml" version)
      clojure.java.io/resource
      slurp
      toml/read))

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

(defn- schema-compliant? [schema rec path]
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
              (map #(schema-compliant? (first schema) %2 (conj path %1))
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
                (schema-compliant? (or (get schema (str "-" k))
                                       (get schema k))
                                   (get rec (keyword k))
                                   (conj path k)))))
    :else {}))

(defversion typecheck)
(defmethod typecheck 1 [rec]
  (let [errors (schema-compliant? (recipe-schema 1)
                                  rec
                                  [])]
    (when (seq errors)
      (throw (ex-info "recipe does not follow schema v1" errors)))))

(defn read-toml [filepath]
  (-> filepath
      slurp
      (toml/read :keywordize)))

(defn toml->latex [rec books]
  (typecheck rec)
  (l/as-print (lp/newpage))
  (l/as-print
   (lr/recipe (l/text (title rec))
              :portions (l/text (portions rec))
              :bakingtime (when-some [{:keys [unit time]} (bakingtime rec)]
                            (lr/si (l/text time) unit))
              :picture (l/raw (picture rec))
              :introduction (l/text (introduction rec))
              :hint (l/text (hint rec))
              :preparation (map l/text (steps rec))
              :ingredients (map #(vector (lr/si (l/optional some? l/text (:amount %))
                                                (:unit %)
                                                (l/optional some? l/text (:repeat %)))
                                         (l/text (:name %)))
                                (ingredients rec)))))

(defn generate-recipes [recipe-path books-path]
  (let [books (read-toml books-path)]
    (doseq [p (-> recipe-path file .list)]
      (l/as-print (lp/part p))
      (let [subfolder (file recipe-path p)]
        (doseq [r (.list subfolder)]
          (toml->latex (read-toml (file subfolder r)) books))))))
