# Target-oriented model finding (TOMF) in Forge 

**ADVISORY: This feature of Forge is experimental.**

Forge uses a modified version of the [Pardinus model finder](https://github.com/haslab/Pardinus) as its back end. Pardinus supports a form of optimization that [_targets_](https://link.springer.com/content/pdf/10.1007/978-3-642-54804-8_2.pdf) a specific goal instance (which may or may not satisfy the given constraints). 

**TODO: examples**

## How to enable TOMF

To enable target-oriented mode, switch solvers to partial max-SAT and use the `target` problem type:
```
option problem_type target
option solver PMaxSAT4J
```
**This should only be done in Relational Forge (`#lang forge`).** Do not attempt to use `target` mode in Temporal Forge.

## Target mode

The `target_mode` option provides the global default for how the solver treats the target. There are 5 options currently available:
* `close_noretarget`: Get as close to the target as possible. When enumerating instances, keep targeting the original target.
* `far_noretarget`: Get as far from the target as possible (up to the given bounds). When enumerating instances, keep targeting the original target.
* `close_retarget`: Get as close to the target as possible. When enumerating instances, reset the target to the last produced instance. 
* `far_retarget`: Get as far from the target as possible (up to the given bounds). When enumerating instances, reset the target to the last produced instance.
* `hamming_cover`: View instances as vectors of boolean variables, where each variable corresponds to a potential tuple membership in a relation. Define the distance between two instances to be the Hamming distance between their boolean vectors. Enumerate instances that maximize the distance from previously produced instances. 

Absent an explicit target, the engine will target the first instance generated. Therefore, use the global option if you don't have a specific target in mind, but want to customize the enumeration strategy that the solver follows. 

If you do have a specific target in mind, give it as part of a `run` command, following the target. E.g.:

```
tomf_close_fixed: run {} for 3 Node 
  target_pi {no Node} close_noretarget 
```

## Targeting a partial instance 

Use the `target_pi` keyword in a `run` command to provide a partial instance target. The argument may be either be the name of a pre-defined `inst` or a `{...}` partial-instance block.

### Example use-case: minimizing or maximizing a set

To minimize the contents of a specific relation `R`, use the `close_noretarget` mode and provide an exactly-empty partial-instance bound for that relation: `no R`. To maximize, provide the same exactly-empty bound but use the `far_noretarget` mode. 

## Minimizing an integer expression 

It can sometimes be useful to minimize an _integer_ expression, rather than a set. To do this, use the `target_int` keyword, providing an integer-valued expression. E.g., to target the number of tuples in relation `R`, use `target_int #R`. This expression may use arbitrary integer operators; to (e.g.) sum over all edges in a weighted directed graph where `sig Node {edges: pfunc Node -> Int}`, use `target_int sum n: Node | sum m: Node | n.edges[m]`. 

To minimize, use `close_noretarget`. To maximize, use `far_noretarget`. Do not attempt to use the other options.

**TODO: confirm lack of braces**


**???** why `flatten` of the bounds list?