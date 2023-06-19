(ns kokbok.toml
  (:require
   [toml.core :as toml]))

;; TODO: can decimal numbers get weird? Is the precision a problem? Should strings also be
;; accepted as numbers to combat this?
(defn read-toml [filepath]
  (-> filepath
      slurp
      (toml/read :keywordize)))

(defn toml-keyword? [string]
  (some? (re-matches #"[a-zA-Z0-9_-]+" string)))

(defn recipe-schema [version]
  (-> (format "recipe-schema-v%s.toml" version)
      clojure.java.io/resource
      slurp
      toml/read))
