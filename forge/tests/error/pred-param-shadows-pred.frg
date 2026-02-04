#lang forge
option run_sterling off

sig Person {}

pred helper { some Person }

-- Parameter 'helper' shadows predicate 'helper'
pred shadowsPred[helper: Person] { some helper }
