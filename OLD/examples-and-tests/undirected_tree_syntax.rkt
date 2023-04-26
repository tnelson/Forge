#lang forge

sig TreeNode { neighbors: set TreeNode }

pred symmetric { neighbors = ~neighbors }
pred connected { TreeNode -> TreeNode in *neighbors }
pred noniden { no iden & neighbors }
pred minimal {
    all t1: TreeNode |
        all t2: TreeNode |
            let e = t1->t2 + t2->t1 |
                e in neighbors implies e not in ^(neighbors - e)}

run { symmetric connected noniden minimal } for 6 TreeNode