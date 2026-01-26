#lang forge
// Test that assertions (assert {...} is sat/unsat) are NOT inherited on import
// helpers/import-asserts-B.frg has a deliberately failing assertion
// If assertions were inherited, this file would fail
open "helpers/import-asserts-B.frg"
option run_sterling off

// The fact that this test runs (and the file doesn't error) proves
// that B's failing assertion was not inherited
test expect { import_succeeded: {some A} is sat }
