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
  [fact memory]
  (let [fact-in-wm (.contains memory fact)]
    (print "\nFact" fact "in memory? -" fact-in-wm)
    fact-in-wm))

(defn prove
  [goal]
  ;; traverse the tree in tail call-recursive manner
  ;; :subgoal  - single symbol to be proven
  ;; :frontier - list of non-immediate symbols to be proven
  ;; :memory   - current state of working memory
  (loop [subgoal goal
         frontier '()
         memory rules/wm]

    ;; If current goal is found, recur with next goal in frontier
    (if (fact-in-wm? subgoal memory)
      (if (empty? frontier)
        (print "\nLast goal proven -" subgoal "=>" goal)
        (recur (first frontier)
               (rest frontier)
               rules/wm))

      ;; bind a single flattened vector of antecedents sufficient for current subgoal.
      ;; i.e. single vec of antecedents of every rule with subgoal as consequent.
      (let [queue
            (flatten (map :ante (get-rules-by-cons subgoal)))]
        (print "\nCurrent subgoals -" queue)
        (print "\nUnexpanded leaf nodes -" frontier)

        ;; Recur with next subgoal, depth-first, if we have children to prove
        (if (empty? queue)
          (print "\nFailed to find" subgoal "in any rule's consequents. Perhaps add more rules?")
          (recur (first queue)
                 ;; Append the non-expanded frontier (if not empty) to back of queue
                 (concat (if (empty? frontier) nil frontier)
                         (rest queue))
                 rules/wm))))))

(defn -main
  "Takes a goal and runs it through inference engine"
  [& args]
  (prove (str (first args))))
