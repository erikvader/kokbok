(ns kokbok.latex.primitives
  (:require [kokbok.latex.core :as l]))

(defn part [title]
  (l/command "part"
    (:arg (l/text title)))
  (l/rawln))

(defn newpage []
  (l/command "newpage")
  (l/rawln))
