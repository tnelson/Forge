#lang forge

sig B { }
sig A { b: set B }


pred main { some A }

run main for exactly 4 A