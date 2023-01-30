#lang forge

option run_sterling off
option verbose 0
option problem_type temporal

one sig Illumination {}

sig Light {
  var on: set Illumination,
  next: one Light
}

pred lightRing {
  all l1, l2: Light | l2 in l1.^next
}

example foo is { lightRing } for {
  Light = `L0 + `L1
}

