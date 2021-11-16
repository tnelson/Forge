#lang forge/bsl

sig Node {
    next: one Node,
    val: one Int
}

one sig A, B extends Node {}

pred intMinus {
   (subtract[sum[A.val],sum[B.val]]) > 0
}

run {intMinus}