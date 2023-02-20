(ns kokbok.generation
  (:require
   [clojure.java.io :refer [file]]
   [kokbok.latex.core :as l]
   [kokbok.latex.primitives :as lp]
   [kokbok.latex.recipe :as lr]
   [kokbok.toml :as t]
   [kokbok.recipe :as r]))

(defn- latex-bakingtime [rec]
  (when-some [{:keys [unit time]} (r/bakingtime rec)]
    (lr/si (l/text time) unit)))

(defn- latex-preparation [rec]
  (when-some [s (r/steps rec)]
    (map l/text s)))

(defn- latex-ingredients [rec]
  (when-some [ingds (r/ingredients rec)]
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
  (let [url (r/source-url rec)
        book-key (r/source-book rec)]
    (when (and (some? url)
               (some? book-key))
      (throw (ex-info "not allowed to have both kinds of sources"
                      {:url url :book-key book-key})))
    (cond
      (some? url) (l/command "url" :arg (l/raw url))
      (some? book-key) (let [book (or (get books book-key)
                                      (throw (ex-info "book not found"
                                                      {:book-key book-key})))]
                         (l/conc
                          (l/text (:title book))
                          (l/raw " (" (year (:release book)) ")")))
      :else nil)))

(defn toml->latex [rec books]
  (r/typecheck rec)
  (l/as-print (lp/newpage))
  (l/as-print
   (lr/recipe (l/text (r/title rec))
              :portions (l/opt-text (r/portions rec))
              :bakingtime (latex-bakingtime rec)
              :picture (l/opt-raw (r/picture rec))
              :introduction (l/opt-text (r/introduction rec))
              :hint (l/opt-text (r/hint rec))
              :preparation (latex-preparation rec)
              :ingredients (latex-ingredients rec)
              :source (latex-source rec books))))

(defn generate-recipes [recipe-path books-path]
  (let [books (t/read-toml books-path)] ;;TODO: schema för books
    (doseq [p (-> recipe-path file .list)]
      (l/as-print (lp/part p))
      (let [subfolder (file recipe-path p)]
        (doseq [r (.list subfolder)]
          (toml->latex (t/read-toml (file subfolder r)) books))))))
