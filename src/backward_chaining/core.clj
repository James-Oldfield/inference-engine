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
  [goal log?]
  (let [matching-rules
        (filter (fn [rule]
                  (.contains (get rule :cons) goal))
                rules/base)]
    (and log? (print "\nMatched rules for goal:" goal "-" (map :numb matching-rules)))
    matching-rules))

;; :return: a boolean specifying whether the goal is in the working memory
(defn fact-in-wm?
  [fact memory]
  (let [fact-in-wm (.contains memory fact)]
    (print "\nFact" fact "in memory? -" fact-in-wm "\n\n")
    fact-in-wm))

(defn check-for-goal
  [goal memory]
   (if (fact-in-wm? goal memory)
     (print "GOAL FOUND")))

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
         breadth 0
         memory rules/wm]

      (print "\n\nCurrent subgoal -" subgoal)
      (print "\nWorking memory -" memory)
      (print "\nUnexpanded leaf nodes -" frontier)
      (print "\nParent nodes -" prnts)
      (print "\nBreadth of search -" (inc breadth))

      ;; Have we cleared out a current branch?
      ;; if so, check if we've satisfied goal before we continue with more rules
      ; (if (and (every? empty? [prnts frontier])
      ;          (not= goal subgoal))
      ;   (check-for-goal goal memory)

      ;; If current goal is found, recur with next goal in frontier
      (if (fact-in-wm? subgoal memory)
        (recur (first frontier)
               (rest frontier)
               (conj prnts subgoal)
               breadth
               memory)

        ;; get a list of this breadth's antecedents for new subgoals
        (let [queue
              (let [rules (get-rules-by-cons subgoal true)]
                (if (< breadth (count rules))
                  (:ante (nth rules breadth))
                  []))]
          (print "\nNew subgoals -" queue)

          ;; Recur with next subgoal, depth-first, if we have children to prove
          (if (empty? queue)
            (if (> breadth (count (get-rules-by-cons subgoal false)))
              (print "\nFailed to find" subgoal "in any rule's consequents. Perhaps add more rules?")
              (recur goal [] [] (inc breadth) memory))
            (recur (first queue)
                   ;; Append the non-expanded frontier (if not empty) to back of queue
                   (concat (rest queue)
                           (if (empty? frontier) nil frontier))
                   (conj prnts subgoal)
                   breadth
                   memory))))))

(defn -main
  "Takes a goal and runs it through inference engine"
  [& args]
  (prove (str (first args))))

