#lang forge/froglet 

/*
  While syntactically, one can write recursive Forge predicates, this won't 
  actually have the desired meaning. Alloy has an "unroll depth" for these,
  but we would like to disable them entirely in this context.
*/

sig A {}

-- arity 0, immediate cycle
pred r { r }

// 1-call cycles, test direct and within boolean expression
pred r1[a: A] { r1[a] }
pred r2[a: A] { some a: A | {some a and r2[a]} }

// 2-call cycle
pred r3x[a: A] { r3y[a] }
pred r3y[a: A] { r3x[a] }

// 3-call cycle
pred r4x[a: A] { r4y[a] }
pred r4y[a: A] { r4z[a] }
pred r4z[a: A] { r4x[a] }

-- This is actually hard to manage. So why is pred so permissive?
-- fun f[a: A]: one A { f[a] }


option verbose 10
test expect {
    --try_recur_pred0: {r} is sat -- forge_error
    
    try_recur_pred1: {some a: A | r1[a] } is forge_error "r1 eventually called itself"
    try_recur_pred2: {some a: A | r2[a] } is forge_error "r2 eventually called itself"
    try_recur_pred3: {some a: A | r3x[a] } is forge_error "r3x eventually called itself"
    try_recur_pred4: {some a: A | r4x[a] } is forge_error "r4x eventually called itself"


    --try_recur_fun: {some f} is forge_error
}