#lang forge/temporal

-- Minimal temporal model for e2e testing of Sterling temporal controls
sig Process {}
var sig Active extends Process {}

pred init { some Active }
pred step { Active' != Active }

temporalRun: run {
  init
  always step
} for exactly 3 Process
