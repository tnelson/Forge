# Trace Documentation

This document supplements the [Basic Documentation](./basicForgeDocumentation.md) with additional information about states/transitions/traces.

The examples here assume a sig like:
```alloy
sig MyState {
    field1: ...
    field2: ...
}
```

## State Predicates

A state predicate parameterized over that sig like:
```alloy
state[MyState] myInitState[a: A] { 
    a in field1 
    no @field2 & iden
}
```

Turns into a normal predicate like:
```alloy
pred myInitState[this: MyState, a: A] { 
    a in this.field1
    no field2 & iden
}
```

Note that:
- A `this` argument is prepended to the list of arguments.
- Fields in the parameterized sig can be accessed directly without writing `this.`.
- Such fields can be references as is with the `@` symbol, so `field = this.@field` in state predicates.

Since `this` is just an argument, a state predicate can be used like this:
```alloy
state[MyState] notAnInitState { 
    no a: A | myInitState[this, a]
}
```

## Transition Predicates

Transition predicates work very similarly to state predicates, except that:
- Both a `this` and a `this'` argument are prepended to the predicate arguments, representing the pre and post states respectively.
- We have `field' = this'.@field`.

So for example:
```alloy
transition[MyState] addA[a: A] { 
    a not in field1
    field1' = field + a
    field2' = field2
}
```

Turns into:
```alloy
pred addA[this, this': MyState, a: A] { 
    a not in this.field1
    this'.field1 = this.field1 + a
    this'.field2 = this.field2
}
```

As with state predicates, this can be used like:
```alloy
transition[MyState] addOrRemoveA[a: A] { 
    addA[this, this', a] or removeA[this, this', a]
}
```

## Trace Predicates

A trace over `MyState` can be defined and run like:
```alloy
trace<|MyState, myInitState, myTransition, myFinalState, myInvariant|> MyTrace {}
run<|MyTrace|> { ... }
```

Note that at the moment of writing this, the 3 predicates `myInitState, myTransition, myFinalState, myInvariant` must take no arguments other than the implicit `this/this'`.

Under the hood, the `trace<|MyState, ...|> MyTrace {}` line creates 3 things: 
1. A new sig: `one sig MyTrace extends BaseTrace {}`
1. A predicate `MyTrace_fact`
1. An instance `MyState_inst` 

The contents of the trace declaration's block are added to the `MyTrace_fact` predicate. In that block, the symbols `this, init, tran, term` are bounded to the trace sig and it's fields respectively.

A command like `run<|MyState, ...|> {...} for {...}` turns into `run { MyTrace_fact and ... } for { MyTrace_inst, ... }`

A command like `check<|MyState, ...|> {...} for {...}` turns into `check { MyTrace_fact => ... } for { MyTrace_inst, ... }`

Traces are implemented using bounds-based symmetry breaking. 
This enables traces with many states by effectively hardcoding the transition relation, and also allows non-linear time traces.
For example, tree-like traces can be made declared `trace<|...|> MyTrace: tree {}`.

See the [Instance Documentation](./instances.md#strategies) for more strategy options, but some relevant options are:
- `plinear` : the default option, which enables linear traces of varying lengths
- `linear` : linear traces of an exact length (the upper bound on the number of states)
- `default` : use this to define your own transition relation


