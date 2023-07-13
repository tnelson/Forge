#lang forge

sig Node {
    edges: set Node
}

inst myInst {
    Node = N1 + N2 + N3 + N4 + N5
    edges is linear
}

myRun : run {} for 5 Node for myInst
