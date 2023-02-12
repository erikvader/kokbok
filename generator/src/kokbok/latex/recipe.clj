(ns kokbok.latex.recipe
  (:require
   [kokbok.cl :refer [cl-aget-two]]
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
  (l/environment "recipe"
    (:arg (l/text title))
    (:optarg (l/kv-pairs [(l/raw "portion") (l/optional-str it portions
                                              (l/braced
                                               (l/command "portion"
                                                 (:arg (l/text it)))))]
                         [(l/raw "bakingtime") (l/optional-str it bakingtime
                                                 (l/braced (l/raw it)))]
                         [(l/raw "bakingtemperature") (l/optional-str it bakingtemperature
                                                        (l/braced (l/raw it)))]
                         [(l/raw "preparationtime") (l/optional-str it preparationtime
                                                      (l/braced (l/raw it)))]
                         [(l/raw "source") (l/optional-str it source
                                             (l/braced (l/raw it)))]))
    (:body (l/statements
            (l/optional-str it picture
              (l/command "graph"
                (:arg (l/kv-pairs [(l/raw "big") (l/command "recipepicture"
                                                   (:arg (l/raw it)))]))))
            (l/optional-str it introduction
              (l/command "introduction"
                (:arg (l/text it))))
            (l/optional-str it ingredients
              (l/command "ingredients"
                (:arg
                 (doseq [[unit desc] it]
                   ;;TODO: \optional ?
                   (l/tabular-row (l/raw unit) (l/text desc))))))
            (l/optional-str it preparation
              (l/command "preparation"
                (:arg
                 (doseq [step it]
                   (l/command "step")
                   (l/raw " ") ;;TODO: add \shortstep on every step?
                   (l/text step)))))
            (l/optional-str it hint
              (l/command "hint"
                (:arg (l/text it))))))))

(defmacro SI
  ([num] `(SI ~num nil nil))
  ([num unit] `(SI ~num ~unit nil))
  ([num unit multiplier]
   `(let [unit# ~unit
          num# ~num
          mult# ~multiplier]
      (when (some? mult#)
        (l/text mult#)
        (l/math (l/command "times")))
      (if (some? unit#)
        (l/command "SI"
          (:arg (l/text num#))
          (:arg (l/raw (or (get units unit#)
                           (throw (ex-info "Unit does not exist"
                                           {:unit unit#}))))))
        (l/text num#)))))

(defn si
  ([num] (si num nil nil))
  ([num unit] (si num unit nil))
  ([num unit multiplier]
   (with-out-str
     (SI num unit multiplier))))

;; (recipe "carbonara"
;;   (:portions "2")
;;   (:bakingtime (si "30" "minute"))
;;   (:picture "carbonara")
;;   (:introduction "blah blah blah")
;;   (:ingredients (list [(si 100 "g") "pasta"]
;;                       [(si 7) "ägg"]))
;;   (:preparation (list "Koka pasta"
;;                       "Kläck ägg"
;;                       "stek äggen med pastan"))
;;   (:hint "Gör inte denna rätt"))
