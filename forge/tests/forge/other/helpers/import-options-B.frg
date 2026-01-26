#lang forge
// Helper file for import-options-A.frg - tests that final option value is inherited
option run_sterling off
option no_overflow false   // first value
sig A {}
option no_overflow true    // final value - this should be inherited by importer
