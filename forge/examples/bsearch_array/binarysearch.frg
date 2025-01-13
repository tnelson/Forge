#lang forge/bsl 

/*
  Rough model of binary search on an array of integers.  

  Recall: binary search (on an array) searches the array as if it embedded a tree; each step narrows
    the range-to-be-searched by half. 

  Tim Feb 2023/2024 
*/

-----------------------------------------------------------
-- Data definitions: integer arrays
-----------------------------------------------------------

// Model an array of integers as a partial function from ints to ints. 
// For ease of modeling, we'll also explicitly represent the length of the array.
// Also for ease of modeling, we'll just allow one array to exist in each instance.
one sig IntArray {
    elements: pfunc Int -> Int,
    lastIndex: one Int
}

-- An IntArray is well-formed if this predicate returns true for it. 
pred validArray[arr: IntArray] {
    -- no elements before index 0
    all i: Int | i < 0 implies no arr.elements[i]

    -- if there's an element, either i=0 or there's something at i=1
    -- also the array is sorted:
    all i: Int | some arr.elements[i] implies {
        i = 0 or some arr.elements[subtract[i, 1]]
        arr.elements[i] >= arr.elements[subtract[i, 1]]
    }
    -- lastIndex reflects actual size of array    
    all i: Int | (no arr.elements[i] and some arr.elements[subtract[i, 1]]) implies {
        arr.lastIndex = subtract[i, 1]
    }    
    {all i: Int | no arr.elements[i]} implies 
      {arr.lastIndex = -1}

}

-- Helper function to get the first index of the array; will be either 0 or -1.
fun firstIndex[arr: IntArray]: one Int {    
    arr.lastIndex = -1  => -1 else  0
}

-----------------------------------------------------------
-- Data definitions: Binary search
-----------------------------------------------------------

-- Model the current state of a binary-search run on the array: the area to be searched
-- is everything from index [low] to index [high], inclusive of both. The [target] is 
-- the value being sought, and [arr] is the entire array being searched.
sig SearchState {
    arr: one IntArray, -- never changes
    low: one Int,      -- changes as the search runs
    high: one Int,     -- changes as the search runs
    target: one Int    -- never changes
}

-----------------------------------------------------------

-- What is an initial state of the search? There are many, depending on the actual 
-- array and the actual target. But it should be a valid array, and it should start
-- with the obligation to search the entire array.
pred init[s: SearchState] {
    validArray[s.arr]
    s.low = firstIndex[s.arr]
    s.high = s.arr.lastIndex        
    -- Note: we have written no constraints on the target; it may be any value
}

-----------------------------------------------------------
-- Transition predicates
--   - stepNarrow (binary search narrows the [low, high] interval)
--   - stepSucceed (binary search finishes, finding the target)
--   - stepFailed (binary search finishes, not finding the target)
-----------------------------------------------------------

pred stepNarrow[pre: SearchState, post: SearchState] {    
    -- mid = (low+high)/2  (rounded down)
    let mid = divide[add[pre.low, pre.high], 2] | {
      -- GUARD: must continue searching, this isn't the target
      pre.arr.elements[mid] != pre.target
      -- ACTION: narrow left or right
      (pre.arr.elements[mid] < pre.target)
          => {
            -- need to go higher
            post.low = add[mid, 1]
            post.high = pre.high            
          }
          else {
            -- need to go lower
            post.low = pre.low
            post.high = subtract[mid, 1]
          }
      -- FRAME: the array and the target never change
      post.arr = pre.arr
      post.target = pre.target
    }
}

-----------------------------------------------------------

-- Termination condition for the search: if low > high, we should stop.
-- "Do nothing" now; the search is over.
pred stepFailed[pre: SearchState, post: SearchState] {    
    -- GUARD: low and high have crossed 
    pre.low > pre.high
    -- ACTION: no change in any field
    post.arr = pre.arr
    post.target = pre.target
    post.low = pre.low
    post.high = pre.high    
}
-- Termination condition for the search: if we found the element, stop.
-- "Do nothing" now; the search is over.
pred stepSucceed[pre: SearchState, post: SearchState] {
    -- GUARD: mid element = target    
    let mid = divide[add[pre.low, pre.high], 2] |
        pre.arr.elements[mid] = pre.target      
    -- ACTION: no change in any field
    post.arr = pre.arr
    post.target = pre.target
    post.low = pre.low
    post.high = pre.high    
}

-----------------------------------------------------------

-- Make it easier to reason about the system with an overall
-- "take a step" predicate.
pred anyTransition[pre: SearchState, post: SearchState] {
    stepNarrow[pre, post]      or
    stepFailed[pre, post]    or
    stepSucceed[pre, post]
}

-- We will discover that binary search breaks when the array is too big. What "too big" is
-- depends on the bitwidth Forge is using. E.g., the default of 4 bits means Forge can represent
-- the interval [-8, 7]. If low=3 and high=5, then (low+high) = (3+5) = 8, which is too big, so 
-- it wraps around to -8. 
-- This behavior lets us find a _real problem_. Binary search (not so) famously breaks if the 
-- array is too long. See: https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html)
pred safeArraySize[arr: IntArray] {            
    -- A bit conservative, but it works for the model
    arr.lastIndex < divide[max[Int], 2]
    -- Let's also assume the array is non-empty
    arr.lastIndex >= 0
}

---------------------------------------------------------------------------------------------
-- Some "is satisfiable" tests. These test for consistency, possibility, non-vacuity.
-- If we don't include tests like this, our verification will be worthless. To see an 
-- extreme example of why, consider: "A implies B" is always true if "A" is unsatisfiable,
-- regardless of what B is. 
---------------------------------------------------------------------------------------------

test expect {
    -- Check that it's possible to narrow on the first transition (this is the common case)
    narrowFirstPossible: {
        some s1,s2: SearchState | { 
            init[s1]
            safeArraySize[s1.arr]        
            stepNarrow[s1, s2]
        }
    } for exactly 1 IntArray, exactly 2 SearchState 
    is sat

    -- Check that we can succeed immediately (likely because the target is in the middle exactly)
    doneSucceedFirstPossible: {
        some s1,s2: SearchState | { 
            init[s1]
            safeArraySize[s1.arr]        
            stepSucceed[s1, s2]
        }
    } for exactly 1 IntArray, exactly 2 SearchState
    is sat
}

---------------------------------------------------------------------------------------------
-- Some assertions: these check that counterexamples don't exist, meaning they _cannot_ check 
-- for consistency as the tests above do.
---------------------------------------------------------------------------------------------

pred unsafeOrNotInit[s: SearchState] { 
    not init[s] or not safeArraySize[s.arr]
}
-- Check: Since we start with high >= low, the failure condition can't be reached in first state
--   unless the array is unsafe. 
assert all s1, s2: SearchState | stepFailed[s1, s2] is sufficient for unsafeOrNotInit[s1]
  for exactly 1 IntArray, exactly 2 SearchState

-- The central invariant of binary search: 
--   If the target is present, it's located between low and high
pred bsearchInvariant[s: SearchState] {
   
    all i: Int | {    
        s.arr.elements[i] = s.target => {
            -- This has the side effect of saying: if the target is there, we never see low>high
            s.low <= i
            s.high >= i

            -- This is an example of how we need to _enrich_ the invariant in order to verify the property.
            -- Counter-intuitively, it's easier for Forge to prove something stronger! The strengthening
            -- says: low and high must be "reasonable" at all times.
            s.low >= firstIndex[s.arr]
            s.low <= s.arr.lastIndex
            s.high >= firstIndex[s.arr]
            s.high <= s.arr.lastIndex
            
            -- Note: these _technically_ should apply if the item is missing, too. But we'd need to be 
            -- more careful, if we wanted to move these outside the implication, because a 1-element array 
            -- (low=0; high=1) would end up with low>high and depending on how we model that, high could 
            -- "unreasonably" become -1. (high := mid-1). So for brevity we put the enrichment here.

        }        
    }    

    -- To avoid the "array is too large" problem with binary search.
    safeArraySize[s.arr]
    
    -- validArray should be preserved as well
    validArray[s.arr]
}

----------------------------------------------------------------------------
-- Inductively verify that binary search preserves the (enriched) invariant.
--   Step 1: Initiation: check that init states must satisfy the invariant. 
--   Step 2: Consecution: legal transitions preserve the invariant.
----------------------------------------------------------------------------

-- Step 1
pred safeSizeInit[s: SearchState] {  init[s] and safeArraySize[s.arr] }
assert all s: SearchState | safeSizeInit[s] is sufficient for bsearchInvariant[s]
  for exactly 1 IntArray, exactly 1 SearchState

-- Step 2
pred stepFromGoodState[s1, s2: SearchState] {
    bsearchInvariant[s1]
    anyTransition[s1,s2]
}
assert all s1, s2: SearchState | stepFromGoodState[s1,s2] is sufficient for bsearchInvariant[s2]
  for exactly 1 IntArray, exactly 2 SearchState

-- These pass (but only after we add the enrichment). 
-- To see an example, see the run below.

option run_sterling "binarysearch.js"
example_first_transition: run {
    some disj s1,s2: SearchState | { 
        init[s1]
        validArray[s1.arr]
        safeArraySize[s1.arr]        
        anyTransition[s1, s2]
        -- To make things interesting, let's see a transition where the target is present
        some i: Int | s1.arr.elements[i] = s1.target 
    }
} for exactly 1 IntArray, exactly 2 SearchState

