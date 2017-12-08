(ns backward-chaining.rules
  (:require [backward-chaining.utils :as utils]))

;; ----------------------
;; Modify initial working memory and rule base below (casted to ints implicitly):
;; ----------------------

;; define seq to store the rulebase
;; :ante - list of symbols as predicates
;; :cons - list of symbols that follow from predicates being true
;; :operator - relationship between 
(def base
  '(
    ;; UNCOMMENT TO DEMO ARBITRARY RULES CAPABILITY
    ; {:numb 100
    ;  :ante [[\x \x]]
    ;  :cons [\b \g]
    ;  :operator "∧"}
    ; {:numb 101
    ;  :ante [[\y \y]]
    ;  :cons [\x \x]
    ;  :operator "∧"}
    ; {:numb 102
    ;  :ante [[\z \z]]
    ;  :cons [\y \y]
     ; :operator "∧"}
    ;; END EXTRA DEMO

    {:numb 1
     :ante [[\f \h] [\a \c]]
     :cons [\b \g]
     :operator "∧"}
    {:numb 2
     :ante [[\n \s]]
     :cons [\e \m]
     :operator "∧"}
    {:numb 3
     :ante [[\r \t]]
     :cons [\p \q]
     :operator "∧"}
    {:numb 4
     :ante [[\d \j] [\e \m] [\k \i]]
     :cons [\a \c]
     :operator "∧"}
    {:numb 5
     :ante [[\p \q]]
     :cons [\n \s]
     :operator "∧"}
    {:numb 6
     :ante [[\u \v]]
     :cons [\k \i]
     :operator "∧"}))

;; initial working memory
(def wm
  [[\f \h]
   [\d \j]
   [\u \v]
   [\r \t]])

(def wm-as-int
  (partition 2 (map int (flatten wm))))

;; Parse rules as integers for more efficient comparison
(def as-ints (utils/cons-to-int
               (utils/ants-to-int base)))
