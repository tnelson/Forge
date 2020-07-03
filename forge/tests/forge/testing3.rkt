#lang forge/core

(sig Node)
(relation edges (Node Node))

(pred acyclic (no (& iden (^ edges))))

