#lang forge

-- Model with multiple sig types suitable for projection testing in Sterling
sig Epoch {}
sig Process {
  action: set Epoch -> Process
}

projectionRun: run {
  some action
} for exactly 2 Process, exactly 3 Epoch
