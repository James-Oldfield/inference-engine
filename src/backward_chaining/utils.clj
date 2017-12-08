(ns backward-chaining.utils)

;; ----------------------
;; Helper functions to operate on rulebase + wm datastructures.
;; ----------------------
;; maps consequents of rules to int format
;; :return: the rules
(defn cons-to-int
  [rs]
  (map (fn [rule]
         (update rule :cons (fn [c] (map int c))))
       rs))

;; maps antecedents of rules to int format
;; :return: the rules
(defn ants-to-int
  [rs]
  (map (fn [rule]
         (update rule :ante (fn [a] (partition 2 (map int (flatten a))))))
       rs))


;; ----------------------
;; General utility functions
;; ----------------------
;; complement of empty
(def not-empty? (complement empty?))

;; parses ints as chars, useful when printing to display progress
(defn intvec-to-char
  [v]
  (partition 2 (map char (flatten v))))
