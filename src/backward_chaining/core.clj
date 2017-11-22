(ns backward-chaining.core
  (:require [backward-chaining.rules :as rules])
  (:gen-class))

; ----------------------
; Helper functions to operate on above datastructures
; ----------------------

;; :return: a lazySeq of rules whose consequents contain the goal symbol
(defn get-rules-by-cons
  [goal]
  (let [matching-rules
        (filter (fn [rule]
                  (contains? (get rule :cons) goal))
                rules/base)]
    (print "-----\n")
    (print "Matched rules for goal:" goal "-" (map :numb matching-rules))
    (print "\n-----\n")
    matching-rules))

;; :return: a boolean specifying whether the goal is in the working memory
(defn fact-in-wm?
  [fact]
  (contains? rules/wm fact))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print (map :ante rules/base)))
