#lang forge
option run_sterling off

sig Person { age: one Int }

-- Parameter 'age' shadows field 'age'
fun shadowsField[age: Person]: one Person { age }
