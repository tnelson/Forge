#lang forge


sig A { r: set B }
sig B {}

--test {
--    bind B = B0+B1 | r = A->B
--} expect sat

run {
    bind A = A1,
         B = B2+B3 | r = A->B
}
