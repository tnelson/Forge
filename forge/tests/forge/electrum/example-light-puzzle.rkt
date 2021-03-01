#lang forge

option problem_type temporal

one sig Illumination {}

sig Light {
  var on: set Illumination,
  next: one Light
}

pred lightRing {
  all l1, l2: Light | l2 in l1.^next
}

pred flip[l: Light] {
    l.on' = { some l.on => none else Illumination }
}

pred puzzle {
  one l: Light | {
    flip[l]
    flip[l.next]
    flip[l.~next]
    all l2: Light - l - l.next - l.~next | l2.on' = l2.on
  }
}

test expect {
    {
        lightRing -- next isn't var so we don't need "always"
        puzzle
        eventually { all l: Light | some l.on }
        #on = 3
    } for exactly 6 Light is sat
} 


