#lang forge

option run_sterling off


option problem_type temporal
option verbose 0

var sig A {}
var sig B {}

-- Check correctness in arbitrary traces via deferral to future-time whenever possible
-- These are direct invocations of the engine's AST nodes.
test expect { 
  before0_someA: {prev_state some A} is unsat
  beforeOther: {(next_state {prev_state {some A}}) iff (some A)} is checked
  onceTest1: {eventually (some A) iff eventually once (some A)} is checked
  onceTest2: {next_state next_state (some A) implies next_state next_state next_state next_state once (some A)} is checked
  historicallyTest: {always (some A) implies next_state next_state next_state next_state historically (some A)} is checked
  sinceTest: {(no A) and next_state (some B and some A) and next_state always (some A) implies next_state next_state next_state next_state (some A since some B)} is checked
  triggeredTest: {(some A since some B) implies (some A triggered some B)} is checked  
}

test expect {
  before0_true: {prev_state true} is unsat
}
