#lang forge 

// Minimal example and regression test for 2022 post #848 (exactly vs. no exactly)

sig Match {} -- original had abstract, but the issue applies without it
sig SingleEliminationMatch extends Match {}
sig GroupMatch extends Match {}

test expect {
    -- There is no explicit bound given for the parent (abstract) sig Match 
    -- Forge needs to infer potential contents; the bug caused Match to be populated
    -- by *only* the 4 atoms given to GroupMatch, with no extras.
    withoutExactly: {some SingleEliminationMatch} for 3 SingleEliminationMatch, exactly 4 GroupMatch is sat
}