#lang forge
// Helper file for import-examples-A.frg - tests that examples are NOT inherited
option run_sterling off
sig A {}
// This example WOULD FAIL if it ran - used to verify examples aren't inherited on import
example badExample is {no A} for { A = A }
