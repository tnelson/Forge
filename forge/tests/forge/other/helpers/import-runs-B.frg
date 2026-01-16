#lang forge
// Helper file for import-runs-A.rkt - tests that runs ARE inherited
option run_sterling off
sig Thing {}
myRun: run { some Thing }
