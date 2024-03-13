#lang forge/temporal 

-- Test for srcloc preservation in if-then-else case

sig Node {edges: set Node}
one sig A, B, C extends Node {}

pred if_example {
     {some A.edges.edges} 
       =>   {some B.edges.edges} 
       else {C.edges.edges}
} 
test expect { {if_example} is sat}