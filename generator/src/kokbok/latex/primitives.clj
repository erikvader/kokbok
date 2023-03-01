(ns kokbok.latex.primitives
  (:require [kokbok.latex.core :as l]))

(defn part [title]
  (l/conc
   (l/command "part" :args (list (l/text title)))
   (l/rawln)))

(defn section [title]
  (l/conc
   (l/command "section" :args (list (l/text title)))
   (l/rawln)))

(defn newpage []
  (l/conc
   (l/command "newpage")
   (l/rawln)))
