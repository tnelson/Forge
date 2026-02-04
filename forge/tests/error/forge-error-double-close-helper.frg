#lang forge

/* Helper file for test-forge-error-double-close-regression.rkt

   This tests the scenario where multiple failing `is forge_error` tests
   (tests that expect an error but none is produced) are followed by a
   normal test. Previously this would crash Pardinus due to double-close.

   Expected behavior: t1 and t2 FAIL (no forge error produced),
   but t3 should still run and PASS. */

option test_keep last
option run_sterling off
option verbose 1

sig Person {}
sig Animal {}

-- Two failing forge_error tests (no error produced)
test expect {
    t1: { some Person } is forge_error
    t2: { some Animal } is forge_error
}

-- Then a normal test that should still work
test expect {
    t3: { some Person and some Animal } is sat
}
