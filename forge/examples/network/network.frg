#lang forge 

/*
  Example of modeling basic network behavior in Relational Forge. 
  Concretely, we model destination-based layer-2 forwarding between
  hosts, which may be either endpoints or switches. 

  Some features that we do not model here:
    - source-aware forwarding;
    - NAT; 
    - access control as a separate filter from forwarding 
    - layers 3 and above; 
    - more complicated endpoint behavior; ...
*/

sig Address {}
abstract sig Host {
    // The topology; direct connections between hosts
    wires: set Host,
    // The forwarding policy installed. Even endpoint hosts must 
    // have a forwarding policy to ensure traffic reaches a switch.
    forwarding: set Address -> Host
}
sig Endpoint, Switch extends Host{}

pred wellformed {
    // No host will forward on a non-existent wire.
    all h: Host { h.forwarding[Address] in h.wires }
    // Assume that endpoints have only one interface, and "forward" by sending on it.
    all e: Endpoint | { one e.wires }
    all e: Endpoint, addr: Address | { e.forwarding[addr] = e.wires}
    // No host will directly connect to itself (longer cycles are allowed)
    all h: Host | { h not in h.wires }
}

/** Definition of reachability between hosts, for security purposes: a host 
    can reach any other host if there is some address that leads to a route 
    from the source host to the destination host. */
fun hostReachability[policy: set Address -> Host -> Host]: set Host -> Host {
    {s, d: Host | some a: Address | s->d in ^(a.policy)}
}

/** A helper like this is useful for converting between different forms of the same 
    data. Yet, Forge's typechecker will complain for this version, because "univ" is 
    not a type variable. I.e., Forge cannot reason that A->B->C becomes B->A->C. 
    
    Instead, we will do the conversion in-place below. */
fun flipColumns12Univ[R: univ -> univ -> univ]: set univ->univ->univ {
    {a,b,c: univ | b->a->c in R}
}

/** Requirement: it is impossible for 2 hosts to communicate if they are 
    physically separated.  */
physicalLayerObeyed: assert {
    wellformed
    hostReachability[{a: Address, s,d: Host | s->a->d in forwarding}] not in ^wires
} is unsat

exampleNetwork: run {
    wellformed
} for exactly 2 Endpoint, 4 Host
