#lang forge

-- univ seems to be containing more than just the defined sigs + Int,
-- but when viewed in visualizer/evaluator, no extra elements in univ.
-- Commented out tests in:
--   - forge-alloy/basic/basicSigs.rkt

sig A {}

test expect univBug { 
	-- So A and Int are always contained in the universe, but don't always full make it up

	univContains : { A + Int !in univ } is unsat -- passes
	--univEquals : { univ != A + Int } is unsat -- fails


	-- Given default bounds, there are 0 to 67 extra elements in univ unaccounted for

	--differenceSize1 : { #(univ - (A + Int)) > 67 } is unsat -- fails
	differenceSize2 : { #(univ - (A + Int)) > 68 } is unsat -- passes
	differenceSize3 : { univ = A + Int } is sat


	bounds1 : { #(univ - (A + Int)) > 67 } for 4 A, 3 Int is unsat -- passes
}