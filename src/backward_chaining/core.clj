(ns backward-chaining.core
  (:require [backward-chaining.rules :as rules])
  (:gen-class))

;; ----------------------
;; Helper functions to operate on above datastructures
;; ----------------------

;; :return: a lazySeq of rules whose consequents contain the goal symbol
(defn get-rules-by-cons
  [goal]
  (let [matching-rules
        (filter (fn [rule]
                  (.contains (get rule :cons) goal))
                rules/base)]
    (print "\n-----\n")
    (print "Matched rules for goal:" goal "-" (map :numb matching-rules))
    (print "\n-----\n")
    matching-rules))

;; :return: a boolean specifying whether the goal is in the working memory
(defn fact-in-wm?
  [fact]
  (let [fact-in-wm (.contains rules/wm fact)]
    (print "Fact" fact "in memory? -" fact-in-wm)
    fact-in-wm))

(defn match
  [goal]
  (or (fact-in-wm? goal)
      (print "recur here")))

(defn -main
  "Takes a goal and runs it through inference engine"
  [& args]
  (let [goal "b g"]
    (fact-in-wm? goal)
    (get-rules-by-cons goal)))
