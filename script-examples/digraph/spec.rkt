#lang forge

sig Vertex {
    edges: set Vertex
}
one sig Boston, Prov, Worc, NYC extends Vertex {}

pred source[v: Vertex] {
    all v2: Vertex | v2 in v.^edges
}