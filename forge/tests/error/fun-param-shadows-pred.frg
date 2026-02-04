#lang forge
option run_sterling off

sig Person {}

pred helper { some Person }

-- Parameter 'helper' shadows predicate 'helper'
fun shadowsPred[helper: Person]: one Person { helper }
