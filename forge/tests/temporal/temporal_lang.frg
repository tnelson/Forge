#lang forge/temporal
option verbose 0

-- Confirm that forge/temporal activates temporal solver; DO NOT activate it manually in this test.

sig Node {
	var edges: set Node
}

test expect {
    temporal_enabled_positive: {no edges and eventually some edges} is sat
	temporal_enabled_negative: {eventually false} is unsat	
}