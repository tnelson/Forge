#lang forge/temporal 

sig Counter { var value : one Int }
pred someTrace { 
	Counter.value = 0 and
	always { Counter.value' = add [Counter.value, 1] } 
}

-- This validation was not present in the figure, but illustrates an important point 
-- about temporal Forge: like Alloy 6, only infinite traces are admitted, so we must 
-- ensure sufficient trace length (default 10). They also show the solver working.
test expect {
	-- 2^4 = 16 default counter values, no wraparound possible
	cannot_wrap: {someTrace} is unsat 
	-- 2^2 = 4 default counter values, wraparound fits
	can_wrap: {someTrace} for 2 Int is sat 
}

run	{someTrace} for 2 Int