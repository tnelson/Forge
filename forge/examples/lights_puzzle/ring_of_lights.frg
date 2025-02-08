#lang forge/temporal

option run_sterling "lights.js"
option java_exe_location "C:/Program Files/Common Files/Oracle/Java/javapath_target_30678640/java.exe"


/*
  When using the custom vis script, remember to click "Run" after
  asking for a new trace or config! (the script doesn't re-run automatically)
*/

------------------------
-- Puzzle infrastructure
------------------------
abstract sig Light {
  left: one Light,  -- neighbor to left
  right: one Light  -- neighbor to right
}
var sig Lit, Unlit extends Light {}

pred ring {
  -- Connected
  all x1, x2: Light | x2 in x1.^left and x2 in x1.^right
  -- Opposites
  left = ~right
}

pred solved {
  Lit = Light
}
pred init {
    -- ONE specific init state, so we can sketch how to exclude loopback 
    --   to the very beginning
    no Lit
    Unlit = Light
}

pred flip[switch: Light] {
  -- Flip the switch for <switch>, which also flips the neighbors
  Lit' = Lit - {x: switch+switch.left+switch.right | x in Lit}
             + {x: switch+switch.left+switch.right | x not in Lit}
}

// Look for a solution to the puzzle that involves at *least* 5 states (4 transitions)
option min_tracelength 5
// We need more than 5 states to find a solution
option max_tracelength 10 

run {
    -- well-formedness constraints
    ring
    -- start in an initial state
    init 

    -- transition predicate: either flip a single light, or we're done
    always {        
        -- Do nothing for a solved board. Otherwise, flip a switch
        {            
            solved -- guard
            Lit = Lit' -- action
        } or {
            not solved -- guard
            one l: Light | flip[l] -- action
        }        
    }

    -- find a trace that leads to a solved state
    eventually solved

    -- If we wanted, we could exclude some unproductive behavior by adding, e.g.:
    -- Loopback can't be to the beginning
    --next_state { always { some Lit }}    
    
} 
for exactly 5 Light
