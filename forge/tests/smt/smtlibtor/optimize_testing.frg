#lang forge
option verbose 3
option backend smtlibtor

---------------------------------------------------------------------------------------------------

pred isPythagoreanTriple[x,y,z: Int] {
    x > 0 
    y > 0
    z > 0
    let xsq = multiply[x, x], ysq = multiply[y,y], zsq = multiply[z,z] | {
        add[xsq,ysq] = zsq        
    }
}

/**
  A triple is primitive if its elements have no common factors besides 1.
  (Note: the handout is somewhat ambiguous there. Is it pairwise coprime, or collective coprime?)
*/
pred isPrimitive[x, y, z: Int] {
    all d: Int | d > 1 => {
        remainder[x, d] != 0 or
        remainder[y, d] != 0 or
        remainder[z, d] != 0
    }
}

test expect {
    pt2: {
        some m, n: Int | let msq = multiply[m, m], nsq = multiply[n,n] | { 
            m > n
            n > 0             
            isPythagoreanTriple[subtract[msq, nsq], multiply[2, multiply[m, n]], add[msq, nsq]]
            not isPrimitive[subtract[msq, nsq], multiply[2, multiply[m, n]], add[msq, nsq]]
        }
    -- In normal Forge, this requires a high bitwidth, so that the squares etc. can be represented.
    -- In SMT, we shouldn't need to provide a bitwidth at all.
    } for 10 Int is sat
}