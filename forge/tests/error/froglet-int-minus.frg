#lang froglet

sig Node {
    next: one Node,
    val: one Int
}

one sig A, B extends Node {}

pred intMinus {
   some  (A.val - B.val)
}

test expect{
    {intMinus} is sat
}
