#lang forge

option problem_type temporal
option verbose 0

var sig A {}
var sig B {}

-- Check correctness in arbitrary traces via deferral to future-time whenever possible
-- These are direct invocations of the engine's AST nodes.
test expect { 
  before0: {before some A} is unsat
  beforeOther: {(after {before {some A}}) iff (some A)} is theorem
  onceTest1: {eventually (some A) iff eventually once (some A)} is theorem
  onceTest2: {after after (some A) implies after after after after once (some A)} is theorem
  historicallyTest: {always (some A) implies after after after after historically (some A)} is theorem
  sinceTest: {(no A) and after (some B and some A) and after always (some A) implies after after after after (some A since some B)} is theorem
  triggeredTest: {(some A since some B) implies (some A triggered some B)} is theorem  
}

expect {
  -- "before true" is sat, even though before F is false canonically regardless of F
  before0: {before true} is unsat
}