#lang forge

sig A { r: A }

fact r: tree --, cotree

run {} for exactly 4 A