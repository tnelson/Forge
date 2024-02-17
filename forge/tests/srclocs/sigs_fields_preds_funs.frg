#lang forge

-- For importing into forge/core and inspecting syntax locations of individual nodes. 

sig Node {
    edges1: set Node,
    edges2: set Node 
}
sig Providence extends Node {}

pred reachesEverything[city: Node] {
  Node in city.^(edges1 + edges2)
}
pred allRoadsLeadToProvidence {
  all n: Node | Providence in n.^(edges1 + edges2)
}

fun twoHopsAway[city: Node]: set Node {
  city.(edges1+edges2).(edges1+edges2)
}
fun reachableFromProvidence: set Node {
  Providence.^(edges1+edges2)
}





