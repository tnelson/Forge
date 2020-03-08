#lang forge

--$ (pre-declare-sig A)
--$ (declare-sig A)
--$ (pred p (some (join A A)))
--$ (run "foo" (p))
