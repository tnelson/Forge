#lang forge/froglet

sig A {}
one sig a extends A {}

pred foo {
  true && true
  a
  a
}

run { foo }