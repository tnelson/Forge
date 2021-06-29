#lang forge
option local_necessity on
option verbosity 2
sig Node {edges: set Node}
blar: run {~edges = edges and #edges <= 1}
