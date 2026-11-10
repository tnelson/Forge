#lang forge
option run_sterling off
sig Replica {}
sig Disk { copies: set Replica }
pred stored { all r: Replica | some copies.r }
pred separated { all d: Disk | lone d.copies }
placement: run { stored separated } for exactly 3 Replica, exactly 2 Disk
safety: check { separated implies not stored } for exactly 3 Replica, exactly 2 Disk
independent: run { some Disk  no Disk  some Replica  no Replica }
ordinary: run { some Disk }

sig Node { next: set Node }
pred cycle { some n: Node | n in n.^next }
inst chain { next is linear }
bounded: run { cycle } for exactly 3 Node for chain
inlineBound: run { cycle } for exactly 3 Node for {next is linear}

// Preserve a user's atom identity, a piecewise lower bound, and Int bitwidth.
partial: run { max[Int] = 3  no next } for exactly 2 Node, 3 Int for {
    Node = `Alpha + `Beta
    `Alpha.next ni `Beta
}

sig Catalog { entries: set Replica -> Disk }
wide: run {
    entries = Catalog -> Replica -> Disk
    all c: Catalog | no c.entries
} for exactly 2 Catalog, exactly 3 Replica, exactly 3 Disk
