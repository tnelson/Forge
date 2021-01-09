#lang forge

sig Apple {
    org : set Orange
}

sig Orange {}

pred progression {
    Orange' = Orange - Apple.org
}

inst OrangeChanges {
    Apple = A0 + A1 + A2 + A3
    Orange = O0 + O1 + O2
    org = A0 -> O0 + A1 -> A1 + A2 -> O2
}

inst OrgEmpty {
    Apple = A0 + A1 + A2
    Orange = O0 + O1
    no org
}

test expect OrangeNeverChanges {
    --Orange would have to change to get rid of all the Oranges in org
    orgMustBeEmpty : progression for OrangeChanges is unsat
    worksIfOrgEmpty : progression for OrgEmpty is sat
}