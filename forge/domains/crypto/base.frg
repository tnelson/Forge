#lang forge

/*
  Base model of strand space (style) crypto
  Tim Mia Abby
  Opting to build in normal Forge, not Electrum
*/

abstract sig mesg {} -- CPSA name for any term

abstract sig Key extends mesg {}
abstract sig akey extends Key {}
sig skey extends Key {}
sig PrivateKey extends akey {}
sig PublicKey extends akey {}

-- Cannot just write fun akey: set Key { PublicKey + PrivateKey }
--   since the macro expands variable declarations to field definitions,
--   and those need to be in terms of sigs

-- relation to match key pairs -- 

one sig KeyPairs {
  pairs: set PrivateKey -> PublicKey,
  owners: set PrivateKey -> name,
  ltks: set name -> name -> skey
}

fun getLTK[name_a: name, name_b: name]: lone skey {
  (KeyPairs.ltks)[name_a][name_b]
}

fun getInv[k: Key]: one Key {
  (k in PublicKey => ((KeyPairs.pairs).k) else (k.(KeyPairs.pairs)))
  +
  (k in skey => k else none)
}


-- t=0, t=1, ...
sig Timeslot {
  -- structure of time
  next: lone Timeslot,
  
  -- <=1 actual "message" per timeslot
  sender: one strand,
  receiver: one strand,  
  data: set mesg,

  -- relation is: Tick x Microtick x learned-mesg
  -- Only one agent per tick is receiving, so always know which agent's workspace it is
  workspace: set Timeslot -> mesg
}

-- As names are sent messagest, they learn pieces of data --
sig name extends mesg {
  learned_times: set mesg -> Timeslot,
  generated_times: set mesg -> Timeslot
}

-- every strand will be either a protocol role or the attacker/medium
abstract sig strand {
  -- the name associated with this strand
  agent: one name
}
one sig AttackerStrand extends strand {}

one sig Attacker extends name {}

sig Ciphertext extends mesg {
   encryptionKey: one Key,
   -- result in concating plaintexts
   plaintext: set mesg
}

-- Non-name base value (e.g., nonces)
sig text extends mesg {}

fun baseKnown[a: name]: set mesg {
    -- name knows all public keys
    PublicKey
    +
    -- name knows the private keys it owns
    (KeyPairs.owners).a
    +
    -- name knows long-term keys they are party to    
    {d : skey | some a2 : name - a | d in getLTK[a, a2] + getLTK[a2, a] }
    +
    -- names know their own names
    a
}

pred wellformed {
  -- Design choice: only one message event per timeslot;
  --   assume we have a shared notion of time
    
  -- You cannot send a message with no data
  all m: Timeslot | some m.data

  -- someone cannot send a message to themselves
  all m: Timeslot | m.sender not in m.receiver

  -- workspace: workaround to avoid cyclic justification within just deconstructions
  --  e.g., knowing or receiving enc(x, x)
  -- AGENT -> TICK -> MICRO-TICK LEARNED_SUBTERM
  all d: mesg | all t: Timeslot | all microt: Timeslot | let a = t.receiver.agent | d in (workspace[t])[microt] iff {
    -- received in the clear just now (base case)
    {d in t.data and no microt.~next}
    or
    -- breaking down a ciphertext we learned *previously*, or that we've produced from something larger this timeslot
    --     via a key we learned *previously*, or that we've produced from something larger in this timeslot
    --  Note use of "previously" by subtracting the *R*TC is crucial in preventing cyclic justification.
    -- the baseKnown function includes e.g. an agent's private key, otherwise "prior knowledge" is empty (even of their private key)
    { 
      
      --d not in ((a.workspace)[t])[Timeslot - microt.^next] and -- first time appearing
      {some superterm : Ciphertext | {      
      d in superterm.plaintext and     
      superterm in (a.learned_times).(Timeslot - t.*next) + workspace[t][Timeslot - microt.*next] + baseKnown[a] and
      getInv[superterm.encryptionKey] in (a.learned_times).(Timeslot - t.*next) + workspace[t][Timeslot - microt.*next] + baseKnown[a]
    }}}
  }
 
  -- names only learn information that associated strands are explicitly sent 
  all d: mesg | all t: Timeslot | all a: name | d->t in a.learned_times iff {
    -- they have not already learned this value
    {d not in (a.learned_times).(Timeslot - t.*next)} and 

    -- This base-case is handled in the workspace now
    -- They received a message directly containing d (may be a ciphertext)
    { --{some m: Message | {d in m.data and t = m.sendTime and m.receiver.agent = a}}
    --or
    
    -- deconstruct encrypted term 
    -- constrain time to reception to avoid cyclic justification of knowledge. e.g.,
    --    "I know enc(other-agent's-private-key, pubk(me)) [from below via construct]"
    --    "I know other-agent's-private-key [from above via deconstruct]""
    -- instead: separate the two temporally: deconstruct on recv, construct on non-reception?
    -- in that case, the cycle can't exist in the same timeslot
    -- might think to write an accessibleSubterms function as below, except:
    -- consider: (k1, enc(k2, enc(n1, invk(k2)), invk(k1)))
    -- or, worse: (k1, enc(x, invk(k3)), enc(k2, enc(k3, invk(k2)), invk(k1)))
    { t.receiver.agent = a
      d in workspace[t][Timeslot] -- derived in any micro-tick in this (reception) timeslot
    }   
    or 
    -- construct encrypted terms (only allow at NON-reception time; see above)
    -- NOTE WELL: if ever allow an agent to send/receive at same time, need rewrite 
    {d in Ciphertext and 
	   d.encryptionKey in (a.learned_times).(Timeslot - t.^next) and        
	   d.plaintext in (a.learned_times).(Timeslot - t.^next)
     {a not in t.receiver.agent} -- non-reception
    }
    or 

    {d in baseKnown[a]}

    or
    -- This was a value generated by the name in this timeslot
    {d in (a.generated_times).t}    
    }} -- end big disjunction for learned_times 
  
  -- If you generate something, you do it once only
  all a: name | all d: text | lone t: Timeslot | d in (a.generated_times).t

  -- Messages comprise only values known by the sender
  all m: Timeslot | m.data in (((m.sender).agent).learned_times).(Timeslot - m.^next) 
  -- Always send or receive to the adversary
  all m: Timeslot | m.sender = AttackerStrand or m.receiver = AttackerStrand 

  -- plaintext relation is acyclic  
  --  NOTE WELL: if ever add another type of mesg that contains data, + inside ^.
  all d: mesg | d not in d.^(plaintext)

  all c: Ciphertext | some c.plaintext

  (KeyPairs.pairs).PublicKey = PrivateKey -- total
  PrivateKey.(KeyPairs.pairs) = PublicKey -- total
  all privKey: PrivateKey | {one pubKey: PublicKey | privKey->pubKey in KeyPairs.pairs} -- uniqueness
  all priv1: PrivateKey | all priv2: PrivateKey - priv1 | all pub: PublicKey | priv1->pub in KeyPairs.pairs implies priv2->pub not in KeyPairs.pairs

  -- at most one long-term key per (ordered) pair of names
  all a:name, b:name | lone getLTK[a,b]
  
  -- assume long-term keys are used for only one agent pair (or unused)
  all k: skey | lone (KeyPairs.ltks).k

  -- Attacker's strand
  AttackerStrand.agent = Attacker

  -- generation only of text and keys, not complex terms
  --  furthermore, only generate if unknown
  all n: name | {
      n.generated_times.Timeslot in text+Key
      all t: Timeslot, d: mesg | {
          d in n.generated_times.t implies {
              all t2: t.~(^next) | { d not in n.learned_times.t2 }
              d not in baseKnown[n]              
          }
      }
  }
}

fun subterm[supers: set mesg]: set mesg {
  -- VITAL: if you add a new subterm relation, needs to be added here, too!
  supers +
  supers.^(plaintext) -- union on new subterm relations inside parens
}
-- Problem: (k1, enc(k2, enc(n1, invk(k2)), invk(k1)))
--  Problem: (k1, enc(x, invk(k3)), enc(k2, enc(k3, invk(k2)), invk(k1)))
--    needs knowledge to grow on the way through the tree, possibly sideways
-- so this approach won't work
/*fun accessibleSubterms[supers: set mesg, known: set mesg]: set mesg {
  -- VITAL: ditto above 
  -- plaintext: Ciphertext -> set mesg
  -- set of Ciphertexts this agent can now open: 
  let openable = {c: Ciphertext | getInv[c.encryptionKey] in known} |
    supers + 
    supers.^(plaintext & (openable -> mesg))
}*/

-- Note it's vital this definition is about strands, not names
pred originates[s: strand, d: mesg] {

  -- unsigned term t originates on n in N iff
  --   term(n) is positive and
  --   t subterm of term(n) and
  --   whenever n' precedes n on the same strand, t is not subterm of n'

  some m: sender.s | { -- messages sent by strand s (positive term)     
      d in subterm[m.data] -- d is a sub-term of m     
      all m2: (sender.s + receiver.s) - m | { -- everything else on the strand
          -- ASSUME: messages are sent/received in same timeslot
          {m2 in m.^(~(next))}
          implies          
          {d not in subterm[m2.data]}
      }
  }
}

pred generates[s: strand, d: mesg] {
  some ((s.agent).generated_times)[d]
}

pred temporary {
  -- upper bounds for one sig have size > 1 at the moment; fix
  one Attacker

  -- for checking, debugging:
  all a1, a2: name | { 
    -- If one has a key, keys are different
    (some KeyPairs.owners.a1 and a1 != a2) implies 
      (KeyPairs.owners.a1 != KeyPairs.owners.a2)
  }

  -- TODO move some of these into the wellformedness pred
  
  all p: PrivateKey | one p.(KeyPairs.owners) 

  agent.Attacker = AttackerStrand

  -- number of keys greater than 0
  #Key > 0

  -- abstractness performance experiment
  --all m : mesg | m in Key + name + Ciphertext + text
  --all k : Key | k in skey + akey
  --all ak : akey | ak in PrivateKey + PublicKey

}