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

(defn- latex-bakingtime [rec]
  (when-some [{:keys [unit time]} (bakingtime rec)]
    (lr/si (l/text time) unit)))

(defn- latex-preparation [rec]
  (when-some [s (steps rec)]
    (map l/text s)))

(defn- latex-ingredients [rec]
  (when-some [ingds (ingredients rec)]
    (map #(vector (lr/si (l/opt-text (:amount %))
                         (:unit %)
                         (l/opt-text (:repeat %)))
                  (l/text (:name %)))
         ingds)))

(defn- year [date]
  (-> "yyyy"
   (java.text.SimpleDateFormat.)
   (.format date)))

(defn- latex-source [rec books]
  (let [url (source-url rec)
        book-key (source-book rec)]
    (when (and (some? url)
               (some? book-key))
      (throw (ex-info "not allowed to have both kinds of sources"
                      {:url url :book-key book-key})))
    (cond
      (some? url) (l/command "url" :arg (l/raw url))
      (some? book-key) (let [book (or (get books (keyword book-key))
                                      (throw (ex-info "book not found"
                                                      {:book-key book-key})))]
                         (l/conc
                          (l/text (:title book))
                          (l/raw " (" (year (:release book)) ")")))
      :else nil)))

(defn toml->latex [rec books]
  (typecheck rec)
  (l/as-print (lp/newpage))
  (l/as-print
   (lr/recipe (l/text (title rec))
              :portions (l/opt-text (portions rec))
              :bakingtime (latex-bakingtime rec)
              :picture (l/opt-raw (picture rec))
              :introduction (l/opt-text (introduction rec))
              :hint (l/opt-text (hint rec))
              :preparation (latex-preparation rec)
              :ingredients (latex-ingredients rec)
              :source (latex-source rec books))))

(defn generate-recipes [recipe-path books-path]
  (let [books (read-toml books-path)] ;;TODO: schema för books
    (doseq [p (-> recipe-path file .list)]
      (l/as-print (lp/part p))
      (let [subfolder (file recipe-path p)]
        (doseq [r (.list subfolder)]
          (toml->latex (read-toml (file subfolder r)) books))))))
