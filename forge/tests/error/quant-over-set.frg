#lang forge
-- Higher-order quantification is not supported (for now)
sig A {}
test expect { t: { some X: set A | #X > 1 } is sat }
