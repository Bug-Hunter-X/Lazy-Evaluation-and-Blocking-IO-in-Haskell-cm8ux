# Haskell Lazy Evaluation Bug

This repository demonstrates a subtle bug in Haskell related to lazy evaluation and the interaction of `IO` actions with infinite lists. The code appears to work correctly for the first part, but blocks indefinitely due to an infinite list's unexpected behavior when combined with a blocking IO operation.

The `bug.hs` file contains the buggy code.  The `bugSolution.hs` file presents the solution.