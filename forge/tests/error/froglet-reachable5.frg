#lang forge/froglet
option run_sterling off

-- error: cannot reach target because types don't line up
--  need NA -> NB -> NC -> NA but missing one link

sig Node {}
sig NA extends Node {
  na: lone NB
}
sig NB extends Node {
  nb: lone NC
}
sig NC extends Node {
  nc: lone NA
}

reach5: run {
  all n: NA | reachable[n, n.na, na, nb]
}

