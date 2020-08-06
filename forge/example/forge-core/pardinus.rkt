#lang forge/core

(sig A)
(sig B)
(relation r (A B))

(run my-run)
(is-sat? my-run)

