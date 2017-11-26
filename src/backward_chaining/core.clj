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
  ;; :breadth  - breadth of root node search
  ;; :wm       - current state of working memory
  (loop [subgoal goal
         frontier []
         prnts []
         breadth 0
         memory rules/wm]

    (print "\n\nCurrent subgoal -" subgoal)
    (print "\nWorking memory -" memory)
    (print "\nFrontier -" frontier)
    (print "\nParent nodes -" prnts)
    (print "\nBreadth of search -" breadth)

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
               breadth
               ;; last frontier element being true => all previous facts are true
               ;; so append them to the memory
               (if (empty? frontier)
                 (concat prnts memory)
                 memory)))

      ;; get a list of this breadth's antecedents for new subgoals
      ;; or if we're not at root, use 0 (no sub-depths implenented (yet?))
      (let [breadth (if (= goal subgoal) breadth 0)
            queue (let [rules (get-rules-by-cons subgoal true)]
                    (if (< breadth (count rules))
                      (:ante (nth rules breadth))
                      []))]

        (print "\nNew subgoals -" queue)

        (if (empty? queue)
          (if (> breadth (count (get-rules-by-cons subgoal false)))
            ;; if we have more rules, recur, increasing the breadth (next rule from root)
            (print "\nFailed to find" subgoal "in any rule's consequents. Perhaps add more rules?")
            (recur goal [] [] (inc breadth) memory))

          ;; else carry on picking facts off this branch
          (recur (first queue)
                 (concat (rest queue) frontier)
                 (conj prnts subgoal)
                 breadth
                 memory))))))

(defn -main
  "Takes a goal and runs it through inference engine"
  [& args]
  (prove (str (first args))))

