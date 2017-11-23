# inference-engine

A simple inference engine for logical implication written in clojure, for coursework.

Tests the truth value of some goal symbol using backward chaining.

Rule base and working memory can be modified arbitrarily.

## use

```bash
usage: lein run [goal]

where:
  goal - string - "b h"
```

e.g., prove `"b g"`:

```bash
lein run "b g"

# Fact b g in memory? - false
# 
# Matched rules for goal: b g - (1)
# Current subgoals - (f h a c)
# Unexpanded leaf nodes - ()
# Fact f h in memory? - true
# 
# ...
# 
# Matched rules for goal: k i - (6)
# Current subgoals - (u v)
# Unexpanded leaf nodes - ()
# Fact u v in memory? - true
# Last goal proven - u v => b g
```
