(ns kokbok.latex.recipe
  (:require
   [kokbok.latex.core :as l]))

(def minute "\\minute")
(def gram "g") ;;TODO: \gram
(def deciliter "dl") ;;TODO: \deci\liter ??
(def milliliter "ml") ;;TODO: \milli\liter ??
(def liter "\\liter")
(def units {"minute" minute
            "min" minute
            "g" gram
            "gram" gram
            "dl" deciliter
            "l" liter
            "liter" liter
            "ml" milliliter
            ;;TODO: DeclareSIUnit\tesked{tsk}
            ;;TODO: DeclareSIUnit\matsked{msk}
            })

(defn recipe [title & {:keys [portions
                              bakingtime
                              bakingtemperature
                              preparationtime
                              picture
                              introduction
                              ingredients
                              preparation
                              hint
                              source]}] ;;TODO: url och bok. Antingen en URL direkt i toml eller en lookup i en anna fil för saker som böcker
  (l/environment
   "recipe"
   :arg title
   :optarg (l/optional seq
            (l/kv-pairs
             (cond-> []
               (some? portions) (conj [(l/raw "portion")
                                       (l/braced
                                        (l/command "portion"
                                                   :arg portions))])
               (some? bakingtime) (conj [(l/raw "bakingtime")
                                         (l/braced bakingtime)])
               (some? bakingtemperature) (conj [(l/raw "bakingtemperature")
                                                (l/braced bakingtemperature)])
               (some? preparationtime) (conj [(l/raw "preparationtime")
                                              (l/braced preparationtime)])
               (some? source) (conj [(l/raw "source")
                                     (l/braced source)]))))
   :body (cond-> []
           (some? picture) (conj (l/command "graph"
                                            :arg (l/kv-pairs
                                                  (list [(l/raw "big")
                                                         (l/command "recipepicture"
                                                                    :arg picture)]))))
           (some? introduction) (conj (l/command "introduction"
                                                 :arg introduction))
           (some? ingredients) (conj (l/command "ingredients"
                                                :arg (l/statements
                                                      (for [[unit desc] ingredients]
                                                        ;;TODO: \optional?
                                                        (l/tabular-row unit desc)))))
           (some? preparation) (conj (l/command "preparation"
                                                :arg (l/statements
                                                      (for [step preparation]
                                                        (l/conc
                                                         (l/command "step")
                                                         (l/raw " ")
                                                         step)))))
           (some? hint) (conj (l/command "hint"
                                         :arg hint)))))

(defn si
  ([num] (si num nil nil))
  ([num unit] (si num unit nil))
  ([num unit multiplier]
   (cond-> []
     (some? multiplier) (l/conc multiplier
                                (l/math (l/command "times")))
     (some? unit) (l/conc (l/command
                           "SI"
                           :args (list num
                                       (l/raw (or (get units unit)
                                                  (throw (ex-info "unit does not exist"
                                                                  {:unit unit})))))))
     (nil? unit) (l/conc num))))

;; (l/as-string
;;  (recipe (l/text "carbonara")))
;; (l/as-print
;;  (recipe (l/text "carbonara")
;;          :portions (l/text "2")
;;          :bakingtime (si "30" "minute")
;;          :picture (l/raw "carbonara")
;;          :introduction (l/text "blah blah blah")
;;          :ingredients (list [(si 100 "g") (l/text "pasta")]
;;                             [(si 7) (l/text "ägg")])
;;          :preparation (list (l/text "Koka pasta")
;;                             (l/text "Kläck ägg")
;;                             (l/text "stek äggen med pastan"))
;;          :hint (l/text "Gör inte denna rätt")))
