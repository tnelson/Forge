#lang forge/froglet -- forge

abstract sig Key {}
sig Pubk, Privk extends Key {}

one sig Authority {
  pairs: func Pubk -> Privk
}

fun counterpart[k: one Key]: one Key {
  -- this produces an error, since k is known to be a Pubk
  /*
  join always results in an empty relation:
  Left argument of join "(Authority.pairs)" is in ((Pubk -> Privk))
  Right argument of join "k" is in ((Pubk))
  There is no possible join result
  in: "(Authority.pairs.k)"
  */
  Authority.pairs.k
}

