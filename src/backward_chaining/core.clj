(ns backward-chaining.core
  (:require [backward-chaining.rules :as rules])
  (:require [backward-chaining.utils :as utils])
  (:gen-class))

;; --------------
;; CORE FUNCTIONS
;; --------------

(defn match
  "Match supplied goal with rules whose consequents contain goal symbol.
  :param goal: sequence - target goal.
  :param log?: boolean - flag to print out info.
  :return: sequence - rules whose consequents contain the goal symbol."
  [goal log?]
  (let [matching-rules
        (filter (fn [rule]
                  (= (get rule :cons) goal))
                rules/as-ints)]
    (and log? (print "\nMatched rules for goal:" (utils/intvec-to-char goal) "-" (map :numb matching-rules)))
    matching-rules))

(defn match-in-wm?
  "Match goal with WM to check for its existence there.
  :param goal: sequence - target goal.
  :param memory: sequence - current state of working memory.
  :param log?: boolean - flag to print out info.
  :return: boolean - specify whether the goal is in the working memory."
  [goal memory log?]
  (let [goal-in-wm (.contains memory goal)]
    (and log? (print "\nFact" (utils/intvec-to-char goal) "in memory? -" goal-in-wm))
    goal-in-wm))

(defn select
  "Filter the matched rules' antecedents if we've already visited its nodes.
  :param antecedents: sequence - antecedents of a rule selected in DFS manner
  :param visited: sequence - already-visited antecedents.
  :return: sequence - non-visited antecedents of selected rule."
  [antecedents visited]
  (filter (fn [a]
            (not (.contains visited a)))
          antecedents))

(defn act
  "Adds/removes proven fact(s) to memory when rule fires
  :param fact: sequence - facts to be acted upon.
  :param old-memory: sequence - current state of working memory.
  :param add?: boolean - specifies whether to add or remove fact from wm.
  :return: sequence - new modified working memory with fact removed/added."
  [fact old-memory add?]
  (let [operator (if add? concat remove)
        new-memory (operator (seq [fact]) old-memory)]
    (print "\nActing... " (if add? "adding" "removing") " fact(s)" (utils/intvec-to-char fact) "to memory -"
           (utils/intvec-to-char new-memory))
    new-memory))

;; -------
;; GETTERS
;; -------

(defn get-antes
  "Given selected rules, returns antecedents that need to be next proven i.e. (promoted as subgoals).
  :param rules: sequence - currently selected rules
  :return: sequence - new modified working memory with fact removed  /added"
  [rules]
  (apply concat (map :ante rules)))

(defn get-operator
  ":param rule: map - rule to act upon.
  :return: function - the logical operator function equivalent in clojure for a rule."
  [rule]
  (let [operator (:operator rule)]
    (if (= operator "∧")
      every?
      some)))

(defn get-rule
  ":param antecedent: sequence - selected antecedent(s).
  :return: sequence - first rule for current subgoal, by antecedent."
  [antecedent]
  (first (filter (fn [rule]
                   (.contains (get rule :ante) antecedent))
                 rules/as-ints)))

(defn rule-proven?
  ":param goal: sequence - selected (sub)goal.
  :param memory: sequence - current state of wm.
  :return: boolean - has the rule been proven with current state of wm?"
  [goal memory]
  (let [rule (get-rule goal)
        operator (get-operator rule)                            ;; Get the logical operator (all/any)
        antes (map (fn [fact] (match-in-wm? fact memory false)) ;; Map antecedents to seq of bools specifying if they're in memory
                   (:ante rule))
        rule-proven (operator true? antes)]                     ;; Use the rule's logical operator to test contents of `antes`
    (if rule-proven (print "\n---- RULE" (:numb rule) "PROVEN ----"))
    rule-proven))

;; --------------
;; MAIN INTERFACE
;; --------------

(defn prove
  [goal]
  "Traverse the tree in tail call-recursive manner.
  :param subgoal:   - single symbol to be proven.
  :param frontier:  - list of non-immediate symbols to be proven.
  :param prnts:     - list of parent symbols which the subgoal implies.
  :param visited:   - already-traversed nodes.
  :param backtrack: - boolean flag, are we backtracking?
  :param wm:        - current state of working memory."
  (loop [subgoal goal
         frontier []
         prnts []
         visited []
         backtrack false
         memory rules/wm-as-int]

    ;; LOG PROGRESS
    (if backtrack
      (print "\nBacktracking to" (utils/intvec-to-char subgoal) "...")
      (do
        (print "\n\nCurrent subgoal -" (utils/intvec-to-char subgoal))
        (print "\nFrontier" (utils/intvec-to-char frontier))))
    ;; END LOG

    (let [rules (match subgoal (not backtrack)) ;; match all rules concerning this subgoal
          antecedents (get-antes rules)         ;; get antecedents of relevant rules
          queue (select antecedents visited)    ;; promote new subgoals we haven't visited
          true-fact (match-in-wm? subgoal memory (not backtrack))]

      (if (not backtrack) (print "\nNew queue for expanded leaf node -" (utils/intvec-to-char queue)))

      ;; check for end of branch
      (if (or true-fact (empty? queue))
        (let [rule-proven (rule-proven? (last visited) memory)]
          (if (= goal subgoal)
            (if rule-proven
              (do (act goal memory true) (print "\n\nRequisite antecedents satisfied." (utils/intvec-to-char goal) "is true.\n\n"))
              (print "\n\nNo more rules found for goal =>" (utils/intvec-to-char goal) "is not true.\n\n"))

            (recur (first prnts)          ;; Start backtracking—most recent parent
                   []                     ;; Reset frontier
                   (rest prnts)           ;; Remove new subgoal from parents
                   (conj visited subgoal) ;; we've also now visited this `subgoal`
                   true                   ;; flag that we're backtracking
                   ;; Child node being true => this node is true, provding queue is empty
                   (if (and backtrack (match-in-wm? (last visited) memory (not backtrack)))
                     (act subgoal memory true)
                     memory))))

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
