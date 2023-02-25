(ns kokbok.core
  (:gen-class)
  (:require
   [io.aviso.exception :as aviso]
   [clojure.java.io :refer [file]]
   [kokbok.generation :refer [generate-recipes]]))

(defn inner-main [args]
  (when (not= 1 (count args))
    (throw (ex-info "invalid usage" {:args args})))

  (let [root (first args)
        recipes-path (file root "recipes")
        books-path (file root "books.toml")
        ingredients-path (file root "ingredients.toml")]
    (generate-recipes recipes-path books-path ingredients-path)))

(defn -main [& args]
  (try (inner-main args)
       (catch Exception e
         (binding [*out* *err*]
           (aviso/write-exception e))
         (System/exit 1))))
