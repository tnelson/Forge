#lang forge

option run_sterling off
option verbose 0

sig Node {edges: set Node}


pred q { no Node }

assert (no edges) is inconsistent with q



