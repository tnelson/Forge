#lang forge

-- Regression test: `open <path> as <id>` must expand properly
open "helpers/open-as-helper.frg" as K

option run_sterling off
option verbose 0

test expect {
  openAs: { some K/Node and K/connected } is sat
}
