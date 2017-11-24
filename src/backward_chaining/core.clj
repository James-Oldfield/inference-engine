(ns backward-chaining.core
  (:require [backward-chaining.rules :as rules])
  (:gen-class))

;; ----------------------
;; Helper functions to operate on datastructures
;; ----------------------

(def not-empty? (complement empty?))

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
  [fact memory]
  (let [fact-in-wm (.contains memory fact)]
    (print "\nFact" fact "in memory? -" fact-in-wm)
    fact-in-wm))

(defn prove
  [goal]
  ;; traverse the tree in tail call-recursive manner
  ;; :subgoal  - single symbol to be proven
  ;; :frontier - list of non-immediate symbols to be proven
  ;; :prnts    - list of parent symbols which the subgoal implies
  ;; :wm       - current state of working memory
  (loop [subgoal goal
         frontier []
         prnts []
         wm rules/wm]

    ;; if frontier is empty, we add parents to working memory,
    ;; and reset parents' binding to empty list, for next rule
    (let [memory (if (and (empty? frontier)
                          (not-empty? prnts))
                   (concat wm prnts)
                   wm)
          prnts (if (empty? frontier)
                  []
                  prnts)]

      (print "\nWorking memory -" memory)
      (print "\nUnexpanded leaf nodes -" frontier)
      (print "\nparents -" prnts)

      ;; If current goal is found, recur with next goal in frontier
      (if (fact-in-wm? subgoal memory)
        (if (empty? frontier)
          (print "\nLast goal proven -" subgoal "=>" goal)
          (recur (first frontier)
                 (rest frontier)
                 (concat prnts subgoal)
                 memory))

        ;; bind a single flattened vector of antecedents sufficient for current subgoal.
        ;; i.e. single vec of antecedents of every rule with subgoal as consequent.
        (let [queue
              (flatten (map :ante (get-rules-by-cons subgoal)))]
          (print "\nNew subgoals -" queue)

          ;; Recur with next subgoal, depth-first, if we have children to prove
          (if (empty? queue)
            (print "\nFailed to find" subgoal "in any rule's consequents. Perhaps add more rules?")
            (recur (first queue)
                   ;; Append the non-expanded frontier (if not empty) to back of queue
                   (concat (rest queue)
                           (if (empty? frontier) nil frontier))
                   (concat prnts subgoal)
                   memory)))))))

(defn -main
  "Takes a goal and runs it through inference engine"
  [& args]
  (prove (str (first args))))
