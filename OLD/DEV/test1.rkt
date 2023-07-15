#lang forge
------------------------- sig setup -----------------------
sig Person {
    pPrefs: set Company -> Company
}
fact pPrefs: linear
sig Company {
    cPrefs: set Person -> Person
}
fact cPrefs: linear
sig Match {
    person: one Person,
    company: one Company
}
pred matchSetup {
    #Match = #Person
    #Match = #Company
    Person = Match.person
    Company = Match.company
}
//run matchSetup for 4 Person, 4 Company, 4 Match
---------------------- pairing -------------------------
pred stableMatching {
    no m1: Match | no m2: Match - m1 | {
        -- company in m1 prefers person in m2
        m2.person in m1.company.^~(m1.company.cPrefs)
        -- person in m2 prefers company in m1
        m1.company in m2.person.^~(m2.person.pPrefs)
    }
}
pred initWithNoStableExists {
}
run stableMatching for 4 Person, 4 Company, 4 Match