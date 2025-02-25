#lang forge

option backend smtlibtor
option verbose 0

sig Set {
	elements: set Element
}

sig Element {}

pred Closed {
	all s0, s1: Set |
		some s2: Set |
			s2.elements = s0.elements + s1.elements
	}

-- ALLOY-TO-FORGE NOTE: this was originally a failing `check`:
test expect {
    set_test : {not Closed} for 10 is sat
}