(ns kokbok.generation
  (:require
   [clojure.java.io :refer [file]]
   [kokbok.latex.core :as l]
   [kokbok.latex.primitives :as lp]
   [kokbok.latex.recipe :as lr]
   [kokbok.toml :as t]
   [kokbok.recipe :as r]))

(defn- latex-time [unittime]
  (when-some [{:keys [unit time]} unittime]
    (lr/si (l/raw time) unit)))

(defn- latex-preparation [rec]
  (when-some [s (r/steps rec)]
    (map l/text s)))

(defn- latex-ingredients [ingds ingredients]
  (when (some? ingds)
    (map #(vector (lr/si (l/opt-raw (:amount %))
                         (:unit %)
                         (l/opt-raw (:repeat %)))
                  (l/text (or (->> % :name (get ingredients))
                              (throw (ex-info "unknown ingredient"
                                              {:name (:name %)})))))
         ingds)))

(defn- latex-temp [temp]
  (when (some? temp)
    (lr/si (l/raw temp) "celsius")))

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

(defn toml->latex [rec books ingredients]
  (r/typecheck rec)
  (l/as-print (lp/newpage))
  (l/as-print
   (lr/recipe (l/text (r/title rec))
              :portions (l/opt-raw (r/portions rec))
              :bakingtime (latex-time (r/bakingtime rec))
              :preparationtime (latex-time (r/preptime rec))
              :bakingtemperature (latex-temp (r/bakingtemp rec))
              :picture (l/opt-raw (r/picture rec))
              :introduction (l/opt-text (r/introduction rec))
              :hint (l/opt-text (r/hint rec))
              :preparation (latex-preparation rec)
              :ingredients (latex-ingredients (r/ingredients rec) ingredients)
              :optional-ingredients (latex-ingredients (r/optional-ingredients rec) ingredients)
              :source (latex-source rec books))))

(defn generate-recipes [recipe-path books-path ingredients-path]
  (let [books (t/read-toml books-path)
        ingredients (t/read-toml ingredients-path)]
    (doseq [p (-> recipe-path file .list sort)]
      (l/as-print (lp/newpage))
      (l/as-print (lp/part p))
      (let [subfolder (file recipe-path p)]
        (doseq [r (sort (.list subfolder))]
          (toml->latex (t/read-toml (file subfolder r))
                       books
                       ingredients))))))
