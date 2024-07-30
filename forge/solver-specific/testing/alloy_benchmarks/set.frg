#lang forge

option backend smtlibtor

sig Set {
	elements: set Element
}

sig Element {}

pred Closed {
	all s0, s1: Set |
		some s2: Set |
			s2.elements = s0.elements + s1.elements
	}


test expect {
    set_test : Closed for 10 is theorem
}