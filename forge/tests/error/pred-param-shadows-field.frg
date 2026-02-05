#lang forge
option run_sterling off

sig Person { age: one Int }

-- Parameter 'age' shadows field 'age'
pred shadowsField[age: Person] { some age }
