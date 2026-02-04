#lang forge
option run_sterling off

sig Person {}
sig Animal {}

-- Parameter 'Person' shadows sig 'Person'
pred shadowsSig[Person: Animal] { some Person }
