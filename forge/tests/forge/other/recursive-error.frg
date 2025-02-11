#lang forge/froglet 
option verbose 0
option run_sterling off

/*
  While syntactically, one can write recursive Forge predicates, this won't 
  actually have the desired meaning. Alloy has an "unroll depth" for these,
  but we would like to disable them entirely in this context.
*/

sig A {}

-- arity 0, immediate cycle
pred r { r }
-- lasso with self-loop
pred q { r }
-- arity 0, delayed cycle 
pred p1 { p2 }
pred p2 { p1 }

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

// 1-arg pred invoking self-loop 0-arg pred
pred r5[a: A] { r }

// 2-arg pred invoking 1-arg pred in cycle 
pred r6x[a1: A, a2: A] { r6y[a1] }
pred r6y[a: A] { r6x[a, a] }

fun f[a: A]: one A { g[a] }
fun g[a: A]: one A { f[a] }

test expect {
    recur_pred_0arg_self:       {r} is forge_error "r eventually called itself"
    recur_pred_0arg_1lead_self: {q} is forge_error "r eventually called itself"
    recur_pred0_2loop:          {p1} is forge_error "p1 eventually called itself"

    recur_pred_1arg_self:      {some a: A | r1[a] } is forge_error "r1 eventually called itself"
    recur_pred_1arg_self_bool: {some a: A | r2[a] } is forge_error "r2 eventually called itself"
    recur_pred_1arg_2loop:     {some a: A | r3x[a] } is forge_error "r3x eventually called itself"
    recur_pred_1arg_3loop:     {some a: A | r4x[a] } is forge_error "r4x eventually called itself"

    recur_pred_1arg_0arg:       {some a: A | r5[a] } is forge_error "r eventually called itself"
    recur_pred_1arg_2arg_2loop: {some a: A | r6y[a] } is forge_error "r6y eventually called itself"
    recur_pred_2arg_1arg_2loop: {some a: A | r6x[a,a] } is forge_error "r6x eventually called itself"
    
    recur_fun_2loop: {some a: A | f[a]} is forge_error "f eventually called itself"
}