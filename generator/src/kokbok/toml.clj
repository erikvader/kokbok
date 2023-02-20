(ns kokbok.toml
  (:require
   [kokbok.schema :refer [schema-compliant?]]
   [toml.core :as toml]))

(defmacro defversion [name]
  `(defmulti ~name #(get-in % [:meta :version])))

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
      (get-in [:info :source :book])
      keyword))

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

(defn recipe-schema [version]
  (-> (format "recipe-schema-v%s.toml" version)
      clojure.java.io/resource
      slurp
      toml/read))

(defversion typecheck)
(defmethod typecheck 1 [rec]
  (let [errors (schema-compliant? (recipe-schema 1)
                                  rec)]
    (when (seq errors)
      (throw (ex-info "recipe does not follow schema v1" errors)))))

(defn read-toml [filepath]
  (-> filepath
      slurp
      (toml/read :keywordize)))
