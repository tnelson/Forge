#lang forge

option run_sterling off

-- Error should arise from exceeding counting capacity (here, 7) when 
-- cardinality constraints are needed to express a scope.
sig P {}
sig Q, R extends P {}
run { #Q = 9 } for 10 P, 9 Q, 9 R
