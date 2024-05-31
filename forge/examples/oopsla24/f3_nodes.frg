#lang forge
-- ^ For historical reasons, Relational Forge is denoted by #lang forge

sig Node { edges : set Node -> Int }
pred wellformed { all disj n1 , n2 : Node | lone n1 . edges [ n2 ] }
pred connected { all disj n1 , n2 : Node | n2 in n1 .^( edges . Int) }