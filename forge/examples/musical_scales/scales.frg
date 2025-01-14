#lang forge/froglet

/*
  Synthesis of (western) musical scales

  Nim Telson Spring 2021 Logic for Systems
  Updated to Froglet by Tim in Spring 2024

  ASSUMPTIONS
  -----------
  * standard western music-theory setting (unsure if we need to assume 12-TET?)
  * The intervals are all that matter, not the tones
  * intervals are always ascending, and are always of whole or half-step lengths

  WOULD LIKE TO
  -------------
  * explore microtonal intervals
  * optimization, but not really needed yet
*/

-- give more power to eliminate solutions that are symmetric to ones we've seen already
option sb 250 

-- pre-load the script
option run_sterling "scales.js"

-- An interval corresponds to a number of half-steps, and is followed by another Interval
abstract sig Interval {
  hs: one Int,
  next: one Interval
}
-- Intervals are partitioned into Whole and Half steps (provided the intervalSizes 
-- predicate is being included))
sig W extends Interval {}
sig H extends Interval {}

-- Where does the scale begin, and with which interval?
one sig Start {
  start: one Interval,
  offset: one Int
}

-- Well-formedness constraints
pred wellformed {
  -- Interval sizes are as expected
  all s: W | s.hs = 2
  all s: H | s.hs = 1
  -- Offsets are within one octave
  Start.offset >= 0
  Start.offset < 12
  -- Intervals partition the octave
  (sum s: Interval | s.hs) = 12
  all s1,s2: Interval | reachable[s1, s2, next] 
  all s: Interval | one next[s]  
}

-- Include this to force scales to be diatonic
pred diatonic {
  -- expected number of whole and half-steps
  -- (Not the most efficient way to phrase this...)
  #W = 5
  #H = 2  
  -- Half steps are separated
  -- We'll be able to express this much more consisely in Relational Forge,
  -- but for now, let's stick to Froglet. "No half step's successor or twice 
  -- successor is another half step"
  all h1: H, h2: H | h1.next != h2 and h1.next.next != h2
  
}

--------------------------------------------------------------------------------
-- Synthesize some scales (in the logical sense of synthesis)
--------------------------------------------------------------------------------

-- Make sure that Forge can count to 12! (not factorial)
-- bitwidth=3: 8 ints [-4, 3]
-- bitwidth=4: 16 ints [-8, 7]
-- bitwidth=5: 32 ints [-16, 15]

  -- Note well: currently it is *REQUIRED* to give the numeric scope for _all_ these sigs.
  -- Otherwise, the results may be inconsistent. Improvement forthcoming in Spring 2024.
run {
  wellformed
  diatonic
} for 7 Interval, 5 Int, 7 H, 7 W -- 7, 5, 2, 5 would be more efficient

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

-- This is a partial instance, used for optimization. Think of it like a re-usable 
-- example. Here, it describes an ionian scale (but without saying what the offset 
-- is, so expect there are 12 possible ways to match it)
inst ionian {
  H = `H2 + `H6
  W = `W0 + `W1  +  `W3 + `W4 + `W5
  Interval = `W0 + `W1 + `H2 + `W3 + `W4 + `W5 + `H6
  Start = `Start0
  start = `Start0->`W0
  next = `W0 -> `W1 +
         `W1 -> `H2 +
         `H2 -> `W3 +
         `W3 -> `W4 +
         `W4 -> `W5 +
         `W5 -> `H6 +
         `H6 -> `W0
}

test expect {
  ionianGenerated: {
    wellformed 
    diatonic 
  } for 
  -- 5 W, 2 H would be more efficient, but this is a *TEST*. Leave room for issues to arise.
  7 W, 7 H,
  7 Interval, 5 Int for ionian is sat
}