#lang forge

sig A { ab: B }
sig B { ba: A }

fact ab: bij
fact ba: bij

run {} for exactly 2 A, exactly 2 B
