#lang forge
// Helper file for import-tests-A.frg - tests that test expect blocks are NOT inherited
option run_sterling off
sig A {}
// This test WOULD FAIL if it ran - used to verify tests aren't inherited on import
test expect { deliberate_fail: {no A and some A} is sat }
