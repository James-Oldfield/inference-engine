(ns backward-chaining.rules)

; define seq to store the rulebase
(def base
  '({:numb 1
     :ante [\f \h \a \c]
     :cons [\b \g]}
    {:numb 2
     :ante [\n \s]
     :cons [\e \m]}
    {:numb 3
     :ante [\r \t]
     :cons [\p \q]}
    {:numb 4
     :ante [\d \j \e \m \k \i]
     :cons [\a \c]}
    {:numb 5
     :ante [\p \q]
     :cons [\n \s]}
    {:numb 6
     :ante [\u \v]
     :cons [\k \i]}))
