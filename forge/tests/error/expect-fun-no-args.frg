#lang forge/bsl
option verbose 0 
option run_sterling off
abstract sig Player {}
one sig X, O extends Player {}
sig Board {board: pfunc Int -> Int -> Player}

fun xplayer: lone Player { X }

test expect { should_error: { xplayer[O] = X } is sat }

