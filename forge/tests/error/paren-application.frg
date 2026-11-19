#lang forge
-- Trigger error with p(x), not p[x]
sig A {}
pred p[a: A] {}
test expect { t: {some x: A | p(x)} is sat }
