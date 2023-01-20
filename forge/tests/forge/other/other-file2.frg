#lang forge

option run_sterling off

sig C {}

run {
  true
}

test expect {
  true is sat
}
