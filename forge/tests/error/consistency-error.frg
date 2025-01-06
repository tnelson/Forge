#lang forge

option run_sterling off
option verbose 0

sig Node {edges: set Node}


pred q { no Node }

assert (some Node) is consistent with q

