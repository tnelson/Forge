#lang forge
option run_sterling off

sig Person {}
sig Animal {}

-- Parameter 'Person' shadows sig 'Person'
fun shadowsSig[Person: Animal]: one Animal { Person }
