# Crypto Domain Language

This Forge language echoes the description in [Prototyping Formal Methods Tools: A Protocol Analysis Case Study](https://cs.brown.edu/~tbn/publications/ssdnk-fest21-forge.pdf) by Siegel, et al. We suggest using `#lang forge` to write your queries, importing the requisite [CPSA](https://github.com/mitre/cpsa) definitions from a similarly-named `#lang forge/domains/crypto` file. You can see an example of how this works in [`examples/reflect.rkt`](examples/reflect.rkt) (the CSPA definitions) and [`examples/reflect.frg`](examples/reflect.frg) (the queries to be run). 

## CPSA 

This system accepts protocol definitions in CPSA's input language. As this effort was meant for teaching, we only support the "simple" term language at the moment, and some more advanced features may be elided. Concretely, there are two CPSA languages:
* `defprotocol` expressions, which define the flow of a protocol execution from the perspective of its various participants; and 
* `defskeleton` expressions, which define a specific point-of-view for the solver to focus on. 

We briefly sketch the languages here, but the full scope of CPSA is beyond what we can explain in this document. For more information on CPSA, see the [user guide](https://hackage.haskell.org/package/cpsa-4.4.3/src/doc/cpsauser.html). 

### Protocols

A protocol definition consists of a name, an algebra to use, and a collection of roles. Each role comprises a set of variables and a _trace_, which describes the message sequence that a participant in this role expects. For example, the protocol below (taken from CPSA's [test suite](https://github.com/mitre/cpsa/blob/master/tst/reflect.scm)) describes a reflection of messages:

```
(defprotocol reflect basic
  (defrole init
    (vars (a b akey))
    (trace
     (send (enc b (invk a)))
     (recv (enc a (invk b)))))
  (defrole resp
    (vars (a b akey))
    (trace
     (recv (enc b (invk a)))
     (send (enc a (invk b))))))
```

From the initiator's perspective, they know two asymmetric keys, `a` and `b`. Then:
  * They send key `b`, encrypted with `a`'s inverse key; 
  * They receive `a`, encrypted with `b`'s inverse key.
The responder also knows two asymmetric keys, which have the same names `a` and `b` but which may differ in value from the initiator's. Then:
  * They receive `b` encrypted with `a`'s inverse key; 
  * They send `a` encrypted with `b`'s inverse key. 

There is, however, no guarantee that the messages these participants send and receive are not tampered with. 

### Skeletons 

A skeleton definition defines a point of view for a query. For example, this skeleton describes a situation where a complete execution of a responder is witnessed, and where the inverses of keys `a` and `b` are uniquely originating (i.e., are not guessed by some other principal). Crucially, the variables of a skeleton are not necessarily bound to the same values as the variables of any specific role in the protocol.

```
(defskeleton reflect
  (vars (a b akey))
  (defstrand resp 1 (a a) (b b))
  (non-orig (invk a) (invk b)))
```

A query should involve only one skeleton. 

## Crypto in Forge 

Queries may be written in either the crypto language or in ordinary Relational Forge. We suggest the latter, as it is generally more concise to write specific queries in Forge rather than in the Racket library that the crypto language provides. Put another way, the domain-specificity of `#lang forge/domains/crypto` is particular to CPSA protocol and skeleton definitions; any additional constraints, bounds, etc. must be expressed separately. 

### Model Predicates

The following predicates are exposed to any `forge/domains/crypto` module, and to any `forge` module that imports a `forge/domains/crypto` module:
  * `wellformed`: defines a set of domain constraints for crypto (should always be included);
  * for each role type `r` in protocol `p`, a predicate `exec_p_r`, which asserts that there is some principle executing protocol `p` with role `r`; and
  * for each skeleton for protocol `p`, a predicate `constrain_skeleton_p_i`, where `i` is a natural number (in the order that the skeleton definitions appear&mdash;CPSA skeletons don't have names in the same way protocols do, so the language just uses indexing).

There are also a number of helper predicates made available:
  * `getInv[k: akey]` accepts an asymmetric key and produces that key's inverse, if it exists; 
  * `attacker_learns[s: strand, d: mesg]` accepts a strand and field name, and evaluates to true if and only if the attacker eventually learns the value of field `d` in strand `s`; ...

After asserting `wellformed` and the pertinent role and skeleton predicates, the query can also contain additional constraints. For example, the reflection example we provide gives:

```
  // Enforce different principals (avoid scenario where one agent talks to itself) 
  reflect_resp.agent != reflect_init.agent
```

### Bounds 

Forge does _bounded_ model finding; that is, it renders the search problem decidable by limiting the scope of each type in the given search problem. In this case, that means Forge must bound the number of time slots, terms, keys, etc. 

E.g., we might say that there are no more than `3` names in an execution of the reflection protocol: the initiator's name, the responder's name, and the attacker's name. (The attacker must be present, as the model follows Dolev-Yao is making them the medium by which principles communicate.)

### Visualizing Executions 

When Sterling opens, the default visualization will appear as a dense cluster of boxes and lines. To switch to the sequence diagram version, click on the 'Script' tab and paste in the contents of the [visualization script](./vis/crypto_viz.js). Then click 'Run'. 
