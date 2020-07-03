#lang forge

sig Node {
    edges: set Node
}

pred acyclic { no iden & ^edges }


