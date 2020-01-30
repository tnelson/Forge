#lang forge

sig A {
    r: set A,
    s: set A
}
check {
--    A.(r+s) = A.r + A.s
--    A.(r-s) = A.r - A.s
    A.(r&s) = A.r & A.s
}