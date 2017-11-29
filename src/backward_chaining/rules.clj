(ns backward-chaining.rules)

;; ----------------------
;; Modify initial working memory and rule base below:
;; ----------------------

;; define seq to store the rulebase
;; :ante - list of symbols as predicates
;; :cons - list of symbols that follow from predicates being true
;; :operator - relationship between 
(def base
  '(
    ;; Test dead end rules
    ; {:numb 100
    ;  :ante ["x x" "l l"]
    ;  :cons ["b g"]
    ;  :operator or}
    ; {:numb 101
    ;  :ante ["y y"]
    ;  :cons ["x x"]
    ;  :operator or}
    ; {:numb 102
    ;  :ante ["z z"]
    ;  :cons ["y y"]
    ;  :operator or}
    ;; End Test dead end rules

    {:numb 1
     :ante [[\f \h] [\a \c]]
     :cons [\b \g]
     :operator or}
    {:numb 2
     :ante [[\n \s]]
     :cons [\e \m]
     :operator or}
    {:numb 3
     :ante [[\r \t]]
     :cons [\p \q]
     :operator or}
    {:numb 4
     :ante [[\d \j] [\e \m] [\k \i]]
     :cons [\a \c]
     :operator or}
    {:numb 5
     :ante [[\p \q]]
     :cons [\n \s]
     :operator or}
    {:numb 6
     :ante [[\u \v]]
     :cons [\k \i]
     :operator or}))

;; initial working memory
(def wm
  [[\f \h]
   [\d \j]
   [\u \v]
   [\r \t]])

(def wm-as-int
  (partition 2 (map int (flatten wm))))

;; maps consequents of rules to int format
;; :return: the rules
(defn cons-to-int
  [rs]
  (map (fn [rule]
         (update rule :cons (fn [c] (map int c)))
         ) rs))

;; maps antecedents of rules to int format
;; :return: the rules
(defn ants-to-int
  [rs]
  (map (fn [rule]
         (update rule :ante (fn [a] (partition 2 (map int (flatten a)))))
         ) rs))

;; Parse rules as integers for more efficient comparison
(def as-ints (cons-to-int
               (ants-to-int base)))
