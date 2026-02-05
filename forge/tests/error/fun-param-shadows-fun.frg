#lang forge
option run_sterling off

sig Person {}

fun getP[p: Person]: one Person { p }

-- Parameter 'getP' shadows function 'getP'
fun shadowsFun[getP: Person]: one Person { getP }
