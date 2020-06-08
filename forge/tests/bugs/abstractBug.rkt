#lang forge

-- According to the docs for Alloy, unextended abstract sigs
-- should be able to populate and ignore abstract.
-- Commented out tests in:
--   - forge-alloy/basic/abstractSigs.rkt

abstract sig Unextended {}

expect abstractBug {
    unextendedAbstractPopulates : { some Unextended } is sat
}