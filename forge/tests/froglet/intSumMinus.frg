#lang forge/froglet
option run_sterling off

option verbose 0

sig Node {
    next: one Node,
    val: one Int
}

one sig A, B extends Node {}

pred intMinus {
   (subtract[sum[A.val],sum[B.val]]) > 0
}

test expect{ 
  canRun: {intMinus} is sat
}
