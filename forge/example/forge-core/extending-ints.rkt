#lang forge

option verbosity 10

sig Index extends Int {}

inst IndexInst {
    Index = sing[0] + sing[1] + sing[2]
}

myRun: run {} for 6 Int for IndexInst


-- (sig Index #:extends Int)

-- (inst IndexInst
--     (= Index (+ (sing -1)
--              (+ (sing 0)
--              (+ (sing 1)
--                 (sing 2))))))

-- (run myRun
--      #:scope ([Int 6])
--      #:bounds [IndexInst])
-- (display myRun)