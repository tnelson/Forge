#lang forge

-- Running with constraints only generates instances containing
-- atoms which are contained in some set referenced in the
-- constraints. The meta-universe is populated up to the bounds,
-- but the generated instances don't use unused sigs.

option verbosity 10

sig A {}
sig B {}
sig C {}

pred P {
    no A
}

-- Will only generate the empty instance, when
-- should generate instances with 0+ B and 0+ C
run P

-- The issue is not with bounds, as can be told from the
-- verbose output, but just to be safe, same thing happens here.
run P for 4 A, 4 B, 4 C
