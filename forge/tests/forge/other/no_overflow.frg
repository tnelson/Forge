#lang forge
option no_overflow true
option run_sterling off
option verbose 0
one sig Counter { x: one Int }
-- Confirm that the no_overflow option excludes instances due to overflow
test expect { 
    overflow_add: {Counter.x = add[2, 2]} for 3 Int is unsat 
    overflow_sum: {Counter.x = sum[1+2+3]} for 3 Int is unsat 
    overflow_successor: {Counter.x = 3.succ} for 3 Int is unsat 
}