#lang forge

sig Tree {
    val: one Int,
    left, right: lone Tree
}

pred bst {
    all t: Tree | {
        t not in t.^(left+right)
        some t.left => t.left != t.right
        all t2: t.left.*(left+right) | t2.val <= t.val
        all t3: t.right.*(left+right) | t3.val > t.val        
    }
    one t: Tree | no t.~(left+right)
}

run {bst} for exactly 10 Tree