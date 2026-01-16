#lang forge
// Helper file for import-asserts-A.frg - tests that assertions are NOT inherited
option run_sterling off
sig A {}
// This assertion WOULD FAIL if it ran - used to verify assertions aren't inherited on import
deliberate_fail: assert {no A and some A} is sat
