#lang forge/check-ex-spec "sample"

sig Node {
    edges : set Node
}

example EG is hasSink for {
    Node = N1 + N2 + N3
    edges = (N1 + N2 + N3)->N2
}

example NEG is not hasSink for {
    Node = N1 + N2 + N3
    edges = (N1 + N3)->N2 + N2->N3
}
