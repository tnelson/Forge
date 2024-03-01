#lang forge
option run_sterling off
option test_keep last
option verbose 0 

-- Regression test to confirm that the "double-check" done in example tests 
-- isn't duplicating run names (and causing an error that prevents multiple
-- examples from running).

sig Node {}
pred someNode { some Node }
example e1 is {someNode} for { no Node } -- failure; do a double-check
example e2 is someNode for { no Node }   -- failure; do another double-check


