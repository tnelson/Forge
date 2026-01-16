#lang forge
// Test that examples are NOT inherited on import
// helpers/import-examples-B.frg has a deliberately failing example
// If examples were inherited, this file would fail
open "helpers/import-examples-B.frg"
option run_sterling off

// The fact that this test runs (and the file doesn't error) proves
// that B's failing example was not inherited
test expect { import_succeeded: {some A} is sat }
