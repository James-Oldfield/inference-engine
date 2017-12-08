(ns backward-chaining.core
  (:require [backward-chaining.rules :as rules])
  (:gen-class))

;; ----------------------
;; Helper functions to operate on datastructures
;; ----------------------

;; complement of empty
(def not-empty? (complement empty?))

;; parses ints as chars, useful when printing to display progress
(defn intvec-to-char
  [v]
  (partition 2 (map char (flatten v))))

;; :return: a lazySeq of rules whose consequents contain the goal symbol
;; N.B. each rule may contain many antecedents (i.e. nested seq)
(defn match
  [goal]
  (let [matching-rules
        (filter (fn [rule]
                  (= (get rule :cons) goal))
                rules/as-ints)]
    (print "\nMatched rules for goal:" (intvec-to-char goal) "-" (map :numb matching-rules))
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
  [fact memory]
  (let [fact-in-wm (.contains memory fact)]
    (print "\nFact" (intvec-to-char fact) "in memory? -" fact-in-wm)
    fact-in-wm))

(defn prove
  [goal]
  ;; traverse the tree in tail call-recursive manner
  ;; :subgoal  - single symbol to be proven
  ;; :frontier - list of non-immediate symbols to be proven
  ;; :prnts    - list of parent symbols which the subgoal implies
  ;; :visited  - already-traversed nodes
  ;; :wm       - current state of working memory
  (loop [subgoal goal
         frontier []
         prnts []
         visited []
         backtrack 0
         memory rules/wm-as-int]

    (print "\n\nCurrent subgoal -" (intvec-to-char subgoal))
    (print "\nWorking memory -" (set (intvec-to-char memory)))
    (print "\nParent nodes -" (intvec-to-char prnts))
    (print "\nFrontier" (intvec-to-char frontier))

    (if (fact-in-wm? subgoal memory)
      (recur (first prnts)           ;; Start backtracking—most recent parent
             []                     ;; Reset frontier
             (rest prnts)        ;; Remove new subgoal from parents
             (conj visited subgoal) ;; we've also now visited this `subgoal`
             1
             (if backtrack (concat (seq [subgoal]) memory) memory))

      (let [rules (match subgoal)               ;; match all rules concerning this subgoal
            antecedents (match-antes rules)     ;; match the rules to relevant antecedents
            queue (select antecedents visited)] ;; promote new subgoals we haven't visited

        (print "\nNew queue for expanded leaf node -" (intvec-to-char queue))

        (if (empty? queue)
          (if (= goal subgoal)                               ;; If we are at root node with no more rules, test for success
            (if (every? true? (map #(fact-in-wm? % memory) ;; Is every antecedent for goal node satisfied?
                                   (select antecedents '())))
              (print "\n\nAll antecedents satisfied." (intvec-to-char goal) "is true.")
              (print "\n\nNo more rules found for goal =>" (intvec-to-char goal) "is not true."))

            ;; If we're not at root node, backtrack to last element of parent nodes
            (recur (first prnts)           ;; Start backtracking—most recent parent
                   []                     ;; Reset frontier
                   (rest prnts)        ;; Remove new subgoal from parents
                   (conj visited subgoal) ;; we've also now visited this `subgoal`
                   1
                   (if backtrack (concat (seq [subgoal]) memory) memory)))

          ;; else carry on picking facts off this branch
          (recur (first queue)                  ;; take top of queue as new subgoal
                 (concat frontier (rest queue)) ;; queue takes priority over frontier, as depth first
                 (conj prnts subgoal)           ;; subgoal is now the most recent parent
                 (conj visited subgoal)         ;; we've also now visisted `subgoal`
                 0
                 memory))))))

(defn -main
  "Takes a goal and runs it through inference engine"
  [& args]
  (prove (map int [\b \g])))
