(ns backward-chaining.core
  (:require [backward-chaining.rules :as rules])
  (:gen-class))

;; ----------------------
;; Helper functions to operate on datastructures
;; ----------------------

(def not-empty? (complement empty?))

;; :return: a lazySeq of rules whose consequents contain the goal symbol,
;; and that aren't present in `visited` seq
;; N.B. each rule may contain many antecedents (i.e. nested seq)
(defn get-rules-by-cons
  [goal visited log?]
  (let [matching-rules
        (filter (fn [rule]
                  (and (.contains (get rule :cons) goal)
                       (not (.contains visited (get rule :numb)))))
                rules/base)]
    (and log? (print "\nMatched rules for goal:" goal "-" (map :numb matching-rules)))
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
  ;; :visited  - already-traversed rules
  ;; :wm       - current state of working memory
  (loop [subgoal goal
         frontier []
         prnts []
         visited []
         memory rules/wm]

    (print "\n\nCurrent subgoal -" subgoal)
    (print "\nWorking memory -" memory)
    (print "\nParent nodes -" prnts)
    (print "\nVisited -" visited)

    ;; If current goal is found, recur with next goal in frontier
    (if (fact-in-wm? subgoal memory)
      (if (= goal subgoal)
        (print "\n\n-----GOAL FOUND-----\n\n")

        ;; if frontier is empty, backtrack to root node
        (recur (if (empty? frontier)
                 goal
                 (first frontier))
               (rest frontier)
               (conj prnts subgoal)
               visited
               ;; last frontier element being true => all previous facts are true
               ;; so append them to the memory
               (if (empty? frontier)
                 (concat prnts memory)
                 memory)))

      (let [rules (get-rules-by-cons subgoal visited false)
            queue (flatten (conj (first (map :ante rules)) frontier))
            rule-number (first (map :numb rules))]

        (print "\nQueue -" queue)

        (if (empty? queue)
          (if (= goal subgoal)
            (print "\nNo more rules found for goal =>" goal "is not true.")
            ;; If we're not at root node, backtrack to last element of parent nodes
            (recur (last prnts)
                   []
                   (butlast prnts)
                   visited
                   memory))

          ;; else carry on picking facts off this branch
          (recur (first queue)
                 (rest queue)
                 (conj prnts subgoal)
                 (if (number? rule-number)
                   (conj visited rule-number)
                   visited)
                 memory))))))

(defn -main
  "Takes a goal and runs it through inference engine"
  [& args]
  (prove (str (first args))))

