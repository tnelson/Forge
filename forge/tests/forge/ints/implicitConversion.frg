#lang forge

option run_sterling off
option verbose 5

-- Check implicit coercion between intexpr and expr. 
-- In 2021, we needed to explicitly convert via sing[] and sum[]
-- Now intexpr->expr is done in AST-node creation automatically.
sig A {}
one sig Test {    
    r: set A -> Int,  
    num: one Int -- don't override the original "max" 
}

-- DISABLE TEMPORARY (TN)
test expect {    
    -- node/formula needing node/int->node/expr child conversion
    intToExpr1: {Test.num = max[Test.r[A]] iff 
                 Test.num = sing[max[Test.r[A]]]} is theorem
    -- node/expr (in definition of "max") needing node/int->node/expr child conversion
    intToExpr2: {max[1] = 1 iff 
                 max[sing[1]] = 1 } is theorem
    -- node/int needing node/int->node/expr child conversion
    intToExpr3: {sum[1] = 1 iff 
                 sum[sing[1]] = 1} is theorem
    -- ite (has its own macro)
    intToExpr4: {Test.num = {(some r) => -1 else 0} iff 
                 Test.num = {(some r) => sing[-1] else sing[0]}} is theorem

    -- node/int needing node/expr->node/int child conversion    
    --exprToInt1: {add[Test.num, 1] > 0 iff 
    --             add[sing[Test.num], 1] > 0} is theorem
    -- node/expr needing node/expr->node/int child conversion
    --exprToInt2: {sing[Test.num] = Test.num } is theorem
    -- sum aggregator (sum in "quantifier form") has its own macro
    --exprToInt3: {(sum a: A | Test.r[a]) = (sum a: A | sum[Test.r[a]]) } is theorem
}
