#lang forge
option verbose 0 

sig NameSig {}
sig ForwardTable { frows: set NameSig -> NameSig  }
sig NetworkState {
    members: set NameSig,
    forwardingTables: set ForwardTable
}

// Decls are property carried forward into _domains_ of later quantifiers when checking
pred demo {    
    all thisState: NetworkState | 
    let members = thisState.members | 
    let forwardingTables = thisState.forwardingTables | 
    {        
        all m1: members, n: (forwardingTables.frows).m1 | m1 in NameSig and n in NameSig
    }
}

test expect {
    {demo} is sat
}