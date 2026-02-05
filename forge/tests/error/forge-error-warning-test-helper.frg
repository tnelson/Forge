#lang forge

/* Helper file for test-forge-error-warning-regression.rkt
   This test expects a forge_error but none is produced,
   so it should FAIL (not produce a spurious "successful" warning) */

option run_sterling off
option test_keep last
option verbose 0

sig A {}

-- This test expects an error, but `some A` is perfectly valid
-- and produces no error. The test should FAIL.
test expect {
    should_fail_no_error: { some A }
        is forge_error
}
