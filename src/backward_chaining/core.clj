(ns backward-chaining.core
  (:require [backward-chaining.rules :as rules])
  (:gen-class))

;; ----------------------
;; Helper functions to operate on above datastructures
;; ----------------------

;; :return: a lazySeq of rules whose consequents contain the goal symbol
;; N.B. each rule may contain many antecedents (i.e. nested seq)
(defn get-rules-by-cons
  [goal]
  (let [matching-rules
        (filter (fn [rule]
                  (.contains (get rule :cons) goal))
                rules/base)]
    (print "\n\nMatched rules for goal:" goal "-" (map :numb matching-rules))
    matching-rules))

;; :return: a boolean specifying whether the goal is in the working memory
(defn fact-in-wm?
  [fact]
  (let [fact-in-wm (.contains rules/wm fact)]
    (print "\nFact" fact "in memory? -" fact-in-wm)
    fact-in-wm))

(defn prove
  [goal]
  ;; traverse the tree in tail call-recursive manner
  ;; :subgoal - single symbol to be proven
  ;; :queue   - list of non-immediate symbols to be proven
  (loop [subgoal goal
         queue '()]

    ;; If current goal is found, recur with rest of goals
    (if (fact-in-wm? subgoal)
      (if (empty? queue)
        (print "\nLast goal proven -" subgoal "=>" goal)
        (recur (first queue) (rest queue)))

      ;; bind a single flattened vector of antecedents sufficient for current subgoal.
      ;; i.e. single vec of antecedents of every rule with subgoal as consequent.
      (let [subgoal-queue
            (flatten (map :ante (get-rules-by-cons subgoal)))]
        (print "\nNew subgoals -" subgoal-queue)

        ;; Recur with next subgoal, depth-first
        (recur (first subgoal-queue) (rest subgoal-queue))))))

(defn -main
  "Takes a goal and runs it through inference engine"
  [& args]
  (let [goal "b g"]
    (prove goal)))
