#lang forge

sig A { r: set A }

inst myInst {
    r is ref
}

run {} for myInst

/*

total-bounds
sigs: remove Int
bounds-store: decrease depth
relations-store: remove succ
extensions-store

total-bounds: (#(struct:bound (relation 2 "r" (A A) A) () ((A0 A0) (A0 A1) (A0 A2) (A0 A3) (A1 A0) (A1 A1) (A1 A2) (A1 A3) (A2 A0) (A2 A1) (A2 A2) (A2 A3) (A3 A0) (A3 A1) (A3 A2) (A3 A3))) #(struct:bound (relation 1 "Int" (Int) univ) ((-8) (-7) (-6) (-5) (-4) (-3) (-2) (-1) (0) (1) (2) (3) (4) (5) (6) (7)) ((-8) (-7) (-6) (-5) (-4) (-3) (-2) (-1) (0) (1) (2) (3) (4) (5) (6) (7))) #(struct:bound (relation 1 "A" (A) univ) () ((A0) (A1) (A2) (A3))) #(struct:bound (relation 2 "succ" (Int Int) CharlieSaysWhatever) ((-8 -7) (-7 -6) (-6 -5) (-5 -4) (-4 -3) (-3 -2) (-2 -1) (-1 0) (0 1) (1 2) (2 3) (3 4) (4 5) (5 6) (6 7)) ((-8 -7) (-7 -6) (-6 -5) (-5 -4) (-4 -3) (-3 -2) (-2 -1) (-1 0) (0 1) (1 2) (2 3) (3 4) (4 5) (5 6) (6 7))))
sigs: ((relation 1 "A" (A) univ))
bounds-store: #hash(((relation 1 "A" (A) univ) . (A0 A1 A2 A3)) 
                    ((relation 1 "Int" (Int) univ) . (-8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7)))
relations-store: #hash(((relation 2 "r" (A A) A) . ((relation 1 "A" (A) univ) (relation 1 "A" (A) univ))))
extensions-store: #hash()

total-bounds: (#(struct:bound (relation 1 "Int" (Int) univ) ((-8) (-7) (-6) (-5) (-4) (-3) (-2) (-1) (0) (1) (2) (3) (4) (5) (6) (7)) ((-8) (-7) (-6) (-5) (-4) (-3) (-2) (-1) (0) (1) (2) (3) (4) (5) (6) (7))) #(struct:bound (relation 1 "A" (A) univ) () ((A0) (A1) (A2) (A3))) #(struct:bound (relation 2 "succ" (Int Int) CharlieSaysWhatever) ((-8 -7) (-7 -6) (-6 -5) (-5 -4) (-4 -3) (-3 -2) (-2 -1) (-1 0) (0 1) (1 2) (2 3) (3 4) (4 5) (5 6) (6 7)) ((-8 -7) (-7 -6) (-6 -5) (-5 -4) (-4 -3) (-3 -2) (-2 -1) (-1 0) (0 1) (1 2) (2 3) (3 4) (4 5) (5 6) (6 7))) #(struct:bound (relation 2 "R" (A A) A) () ((A0 A0) (A0 A1) (A0 A2) (A0 A3) (A1 A0) (A1 A1) (A1 A2) (A1 A3) (A2 A0) (A2 A1) (A2 A2) (A2 A3) (A3 A0) (A3 A1) (A3 A2) (A3 A3))))
sigs: ((relation 1 "Int" (Int) univ) (relation 1 "A" (A) univ))
bounds-store: #hash(((relation 1 "Int" (Int) univ) . ((-8) (-7) (-6) (-5) (-4) (-3) (-2) (-1) (0) (1) (2) (3) (4) (5) (6) (7))) 
                    ((relation 1 "A" (A) univ) . ((A0) (A1) (A2) (A3))))
relations-store: #hash(((relation 2 "R" (A A) A) . ((relation 1 "A" (A) univ) (relation 1 "A" (A) univ))) 
                       ((relation 2 "succ" (Int Int) CharlieSaysWhatever) . ((relation 1 "Int" (Int) univ) (relation 1 "Int" (Int) univ))))
extensions-store: #hash()

*/
