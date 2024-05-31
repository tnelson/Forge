#lang forge/temporal

option max_tracelength 10

/*
  Water buckets puzzle:
    - you have a faucet of running water
    - you have empty 8, 5, and 3-liter buckets 
    - you must get exactly 4 liters of water into one of the buckets using these tools.

  We'll solve this puzzle with Forge. To do so, we'll define the *transition system* 
  of the puzzle, and ask Forge to search it for a solution! 

  Note that "find a sequence of operations such that <a state like this> is reached"
  is useful for software and hardware verification, as well...
*/

sig Bucket {
    capacity: one Int,
    var contents: one Int
}
one sig B8, B5, B3 extends Bucket {} 

pred initial {
    B8.capacity = 8 
    B5.capacity = 5 
    B3.capacity = 3
    B8.contents = 0
    B5.contents = 0
    B3.contents = 0
}

pred fill[b: Bucket] {
    b.contents' = b.capacity
    all b2: Bucket - b | b2.contents' = b2.contents
}

pred empty[b: Bucket] {
    b.contents' = 0
    all b2: Bucket - b | b2.contents' = b2.contents
}

pred pour[from, to: Bucket] {
    let moved = min[from.contents + subtract[to.capacity, to.contents]] | {
        to.contents' = add[to.contents, moved]
        from.contents' = subtract[from.contents, moved]
    }
    all b2: Bucket - (from + to) | b2.contents' = b2.contents
}

run {
    initial
    always {
        (some b: Bucket | fill[b])
        or
        (some b: Bucket | empty[b])
        or
        (some from, to: Bucket | pour[from, to])
    }
    eventually {some b: Bucket | b.contents = 4} 
} for 5 Int, -- bitwidth
  exactly 3 Bucket -- no other buckets in the world
