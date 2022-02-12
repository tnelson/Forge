#lang forge

option problem_type temporal
option verbose 0

var sig A {}
var sig B {}

-- Check correctness in arbitrary traces via deferral to future-time whenever possible
-- These are direct invocations of the engine's AST nodes.
test expect { 
  before0: {prev_state some A} is unsat
  beforeOther: {(next_state {prev_state {some A}}) iff (some A)} is theorem
  onceTest1: {eventually (some A) iff eventually once (some A)} is theorem
  onceTest2: {next_state next_state (some A) implies next_state next_state next_state next_state once (some A)} is theorem
  historicallyTest: {always (some A) implies next_state next_state next_state next_state historically (some A)} is theorem
  sinceTest: {(no A) and next_state (some B and some A) and next_state always (some A) implies next_state next_state next_state next_state (some A since some B)} is theorem
  triggeredTest: {(some A since some B) implies (some A triggered some B)} is theorem  
}

test expect {
  before0: {prev_state true} is unsat
}
