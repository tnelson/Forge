#lang forge

sig Elem {}

pred main {
    some Elem * Elem -> Elem
}

run main for exactly 2 Elem