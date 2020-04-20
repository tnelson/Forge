#lang forge

option verbosity 10

sig A { f : set B }
sig B { g : set A, h : set C }
sig C {}

run for {
    f is func
    g is func
    h is func
}