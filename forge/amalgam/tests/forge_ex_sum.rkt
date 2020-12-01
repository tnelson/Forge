#lang forge

sig Node {size: one Int}

pred sizeGreaterThanTwo {
    all n:Node | sum[n.size] > 2
}

-- Now defined in forge_ex_test.rkt
--udt: run isUndirectedTree for 7 Node