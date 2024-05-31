#lang forge

/*
  (Last updated in May 2024)

  Example of unsat-core highlighting in Forge. It's important that you're running 
  Forge 3.4 or later with the latest VSCode extension to get highlighting. It's also 
  important to keep in mind that there are corner cases where the core will contain 
  formulas that are not mapped to a location in the model. This is rare, but happens 
  in Alloy, too. (Alloy will not give a core at all in these cases; Forge will print 
  a combination of:
    (1) locations in the source; AND 
    (2) constraints that aren't directly mapped back.)

  If you're using the VSCode extension, a core will be underlined in red. 
  If you're _not_ using the VSCode extension, you'll see the location info in the terminal. 
  
  NOTE WELL: if you're running in VSCode, you may need to click "continue" to see the core
    for a "run" command.
*/

----------------------------------------------------------------------------------
-- Manually enable core extraction (see docs for more information than below)
option solver MiniSatProver  -- the only solver we support that extracts cores
option logtranslation 2      -- enable translation logging (1 = faster, less fine-grained)
option coregranularity 2     -- how fine-grained cores should be (1 = faster, less fine-grained)
option core_minimization rce -- tell the solver which algorithm to use to reduce core size
                             --    valid values: hybrid (fast, not always minimal), 
                             --                  rce (slower, complete)
----------------------------------------------------------------------------------

sig Node {edges: set Node}
test expect {
    -- This run has multiple potential cores. At least two:
    --   Core A: the "no edges" mixed with the "n1->n2" 
    --   Core B: the "no edges" mixed with the "n2->n1".
    -- Both will also highlight the variables involved; notice that "n3" is not blamed.
    complexUnsat: {
        some n1, n2, n3: Node | {
            n1->n2 in edges
            n2->n1 in edges 
            n3 not in n3.edges
        }
        no edges
    } is unsat
}
