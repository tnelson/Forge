#lang forge

-- (set-verbosity 10)

-- (sig Index #:extends Int)

-- (inst IndexInst
--     (= Index (- Int (- Int (sing 0)))))

-- (run myRun
--      #:scope ([Int 6])
--      #:bounds [IndexInst])
-- (display myRun)

option verbosity 10

sig Index extends Int {}

inst IndexBound {
    Index = Int - (Int - sing[0] - sing[1])
}

myRun: run {} for 6 Int for IndexBound
