#lang forge

option backend smtlibtor
option verbose 0

/*
 * In his 1973 song, Paul Simon said "One Man's Ceiling Is Another Man's Floor".
 * Does it follow that "One Man's Floor Is Another Man's Ceiling"?
 *
 * To see why not, check the assertion BelowToo.
 *
 * Perhaps simply preventing man's own floor from being his ceiling is enough,
 * as is done in the Geometry constraint.  BelowToo' shows that there are still
 * cases where Geometry holds but the implication does not, although now
 * the smallest solution has 3 Men and 3 Platforms instead of just 2 of each.
 *
 * What if we instead prevent floors and ceilings from being shared,
 * as is done in the NoSharing constraint?  The assertion BelowToo''
 * has no counterexamples, demonstrating that the implication now
 * holds for all small examples.
 *
 * model author: Daniel Jackson (11/2001)
 * modified by Robert Seater (11/2004)
 */

sig Platform {}
sig Man {ceiling, floor: one Platform}

pred PaulSimon {all m: Man | some n: Man | Above[n, m]}

pred Above[m, n: Man] {m.floor = n.ceiling}

pred BelowToo { all m: Man | some n: Man | Above[m, n] }

test expect{
    cf_0: {PaulSimon => BelowToo} for 2 Platform, 2 Man is sat
} 
