#lang forge

sig Node {size: int}

pred sizeGreaterThanTwo {
    all n:Node | n.size > 2
}

-- Now defined in forge_ex_test.rkt
--udt: run isUndirectedTree for 7 Node 