#lang forge
option run_sterling off

sig A {}
sig B {}
abstract sig P {}
sig P1, P2 extends P {}

test expect { warns: { some A } for 3 A, 2 P1 is sat }
