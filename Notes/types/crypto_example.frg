#lang forge

abstract sig Key {}
sig PrivateKey extends Key {}
sig PublicKey extends Key {}

one sig KeyPairs {
  pairs: set PrivateKey -> PublicKey
}

-- implicit "one" multiplicity on the argument
fun getInv[k: Key]: one Key {
  (k in PublicKey => ((KeyPairs.pairs).k) else (k.(KeyPairs.pairs)))
}

test expect {
  force_checking: {some someKey: Key | some getInv[someKey]} is sat
}


/*
-- This is the example of where narrowing would be useful; it currently causes
-- an error in last-checker (necessarily empty join on a side of an ITE that isn't
-- really used).
run {
  some pub: PublicKey | {
      some getInv[pub]
  }
}
*/
