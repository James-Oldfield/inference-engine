(ns backward-chaining.core
  (:require [backward-chaining.rules :as rules])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print (map :ante rules/base)))
