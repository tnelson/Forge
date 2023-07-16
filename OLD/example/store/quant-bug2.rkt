#lang forge

sig A { friendA: set A }
sig B { friendB: set B }

run { 
    all a1, a2: A, b1, b2: B | a1->a2 in friendA and b1->b2 in friendB
    --some A
    --some B
}
