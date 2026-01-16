#lang forge
// Test that test expect blocks are NOT inherited on import
// helpers/import-tests-B.frg has a deliberately failing test
// If tests were inherited, this file would fail
open "helpers/import-tests-B.frg"
option run_sterling off

// The fact that this test runs (and the file doesn't error) proves
// that B's failing test was not inherited
test expect { import_succeeded: {some A} is sat }
