#lang forge/temporal 

sig Counter { value : one Int }
pred someTrace { Counter . value = 0 and
always { Counter . value ' = add [ Counter . value , 1] } }