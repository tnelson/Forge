#lang forge
sig Node { edges: set Node }
pred connected { all n: Node | Node in n.*edges }
