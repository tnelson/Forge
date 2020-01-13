#lang forge

sig A { r: A }

--fact r: tree --, cotree
fact r: func, cofunc

run {} for exactly 10 A