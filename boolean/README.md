# boolean-template
Template Repository for HW 1

## Task for HW 1

The input is a Boolean formula in 2-CNF, given as a string of symbols. A *literal* is a variable name 
(at least any small Latin letter should be a legal variable name) or its negation, denoted by `~`.
A *clause* is either a literal, or `(L1 \/ L2)`, or `(L1 -> L2)`, where `L1` and `L2` are literals.
Finally, the *2-CNF* is either one clause or a conjunction of two or more clauses, where conjunction is
denoted by `/\`.

Example: `p /\ (p -> q) /\ (p -> ~r) /\ (~r \/ ~s) /\ (s \/ ~q)`

The *first* task is to check whether the given 2-CNF is satisfiable. The *second* task is, given a 2-CNF,
report that it is not satisfiable or return one of its satisfying assignments. Solving only the first task
gives you a half grade (2 points out of 4). Solving both gives 4 points.

### Technical Details

In order for the automated test to work fine, please carefully adhere the following technical instructions.
Your solution should be implemented in Python3, in a file named `boolean.py`. For the first task, you should
implement a function called `is_satisfiable`, which takes the string with the 2-CNF as an argument and
returns either `True` (satisfiable) or `False` (not satisfiable). The second task should be implemented as
a function called `sat_assignment`. This function also takes one argument (string) and returns an associative
array (dictionary) with the satifying assignment (e.g., `{ 'p': True, 'q': False, 'r': True }`) or `None`,
if the 2-CNF is not satisfiable.

**Warning:** the automatic tests run several iterations of your functions, with different CNFs.
If you use global variables, please make sure that their values from previous iterations do not 
make the next one work incorrectly.
