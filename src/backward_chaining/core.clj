(ns backward-chaining.core
  (:require [backward-chaining.rules :as rules])
  (:require [backward-chaining.utils :as utils])
  (:gen-class))

;; :return: a lazySeq of rules whose consequents contain the goal symbol
;; N.B. each rule may contain many antecedents (i.e. nested seq)
(defn match
  [goal log?]
  (let [matching-rules
        (filter (fn [rule]
                  (= (get rule :cons) goal))
                rules/as-ints)]
    (and log? (print "\nMatched rules for goal:" (utils/intvec-to-char goal) "-" (map :numb matching-rules)))
    matching-rules))

;; Given matched rules, returns antecedents that need to be next proven
;; i.e. (promoted as subgoals)
(defn match-antes
  [rules]
  (apply concat (map :ante rules)))

;; filter the matched rules' antecedents if they're contained in `visited` collection
;; this becomes our new frontier we expand in depth-first manner
(defn select
  [antecedents visited]
  (filter (fn [a]
            (not (.contains visited a)))
          antecedents))

;; :return: a boolean specifying whether the goal is in the working memory
(defn fact-in-wm?
  [fact memory log?]
  (let [fact-in-wm (.contains memory fact)]
    (and log? (print "\nFact" (utils/intvec-to-char fact) "in memory? -" fact-in-wm))
    fact-in-wm))

(defn prove
  [goal]
  ;; traverse the tree in tail call-recursive manner
  ;; :subgoal   - single symbol to be proven
  ;; :frontier  - list of non-immediate symbols to be proven
  ;; :prnts     - list of parent symbols which the subgoal implies
  ;; :visited   - already-traversed nodes
  ;; :backtrack - boolean flag, are we backtracking?
  ;; :wm        - current state of working memory
  (loop [subgoal goal
         frontier []
         prnts []
         visited []
         backtrack false
         memory rules/wm-as-int]

    ;; LOG PROGRESS
    (if backtrack
      (print "\nBacktracking to" (utils/intvec-to-char subgoal) "...\n")
      (do
        (print "\n\nCurrent subgoal -" (utils/intvec-to-char subgoal))
        (print "\nWorking memory -" (set (utils/intvec-to-char memory)))
        (print "\nFrontier" (utils/intvec-to-char frontier))))
    ;; END LOG

    (let [rules (match subgoal (not backtrack)) ;; match all rules concerning this subgoal
          antecedents (match-antes rules)       ;; match the rules to relevant antecedents
          queue (select antecedents visited)    ;; promote new subgoals we haven't visited
          true-fact (fact-in-wm? subgoal memory (not backtrack))]

      (if (not backtrack) (print "\nNew queue for expanded leaf node -" (utils/intvec-to-char queue)))

      ;; check for end of branch
      (if (or true-fact (empty? queue))
        (if (= goal subgoal) ;; If we are at root node with no more rules (i.e. empty queue), test for success
          ;; Is every/any antecedent for goal node satisfied?
          (if (every? true? (map #(fact-in-wm? % memory true) (select antecedents '())))
            (print "\n\nRequisite antecedents satisfied." (utils/intvec-to-char goal) "is true.")
            (print "\n\nNo more rules found for goal =>" (utils/intvec-to-char goal) "is not true."))

          (recur (first prnts)          ;; Start backtracking—most recent parent
                 []                     ;; Reset frontier
                 (rest prnts)           ;; Remove new subgoal from parents
                 (conj visited subgoal) ;; we've also now visited this `subgoal`
                 true                   ;; flag that we're backtracking
                 ;; Child node being true => this node is true, provding queue is empty
                 (if (and backtrack (fact-in-wm? (last visited) memory (not backtrack)))
                   (concat (seq [subgoal]) memory)
                   memory)))

        ;; else carry on picking facts off this branch
        (recur (first queue)          ;; take top of queue as new subgoal
               (rest queue)           ;; push rest of queue to the frontier 
               (conj prnts subgoal)   ;; subgoal is now the most recent parent
               (conj visited subgoal) ;; we've also now visisted `subgoal`
               false                  ;; no backtracking this recur
               memory)))))            ;; memory remains unmodified

(defn -main
  "Takes a goal and runs it through inference engine"
  [& args]
  (prove (map int [\b \g])))
