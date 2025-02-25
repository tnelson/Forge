#lang forge

option run_sterling off
option verbose 0

sig Node {edges: set Node}




pred q { no Node }

// Assertions do not support is_forge_error
thisisatestname: assert (some Node) is consistent with q

