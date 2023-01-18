#lang forge

option run_sterling off


option verbose 0
option problem_type temporal

sig Node {
    var edge: set Node,
    var fruit: set Node
}

pred F1 {
    (some fruit) implies (no edge)
}

pred F2 {
    all n1, n2 : Node | n1->n2 in (edge + fruit)
}

pred F3 {
    edge = ^fruit
}


test expect{
    NotUntil12: {(not F1 until F2) iff ((not F1) until F2)} is theorem
    NotUntil12: {not ((not F1 until F2) iff (not (F1 until F2)))} is sat
    NotUntil13: {(not F1 until F3) iff ((not F1) until F3)} is theorem
    NotUntil13: {not ((not F1 until F3) iff (not (F1 until F3)))} is sat
    NotUntil23: {(not F2 until F3) iff ((not F2) until F3)} is theorem
    NotUntil23: {not ((not F2 until F3) iff (not (F2 until F3)))} is sat

    AlwaysUntil12: {(always F1 until F2) iff ((always F1) until F2)} is theorem
    AlwaysUntil12: {always ((always F1 until F2) iff (always (F1 until F2)))} is sat
    AlwaysUntil13: {(always F1 until F3) iff ((always F1) until F3)} is theorem
    AlwaysUntil13: {always ((always F1 until F3) iff (always (F1 until F3)))} is sat
    AlwaysUntil23: {(always F2 until F3) iff ((always F2) until F3)} is theorem
    AlwaysUntil23: {always ((always F2 until F3) iff (always (F2 until F3)))} is sat
    
    EventuallyUntil12: {(eventually F1 until F2) iff ((eventually F1) until F2)} is theorem
    EventuallyUntil12: {eventually ((eventually F1 until F2) iff (eventually (F1 until F2)))} is sat
    EventuallyUntil13: {(eventually F1 until F3) iff ((eventually F1) until F3)} is theorem
    EventuallyUntil13: {eventually ((eventually F1 until F3) iff (eventually (F1 until F3)))} is sat
    EventuallyUntil23: {(eventually F2 until F3) iff ((eventually F2) until F3)} is theorem
    EventuallyUntil23: {eventually ((eventually F2 until F3) iff (eventually (F2 until F3)))} is sat

    Notreleases12: {(not F1 releases F2) iff ((not F1) releases F2)} is theorem
    Notreleases12: {not ((not F1 releases F2) iff (not (F1 releases F2)))} is sat
    Notreleases13: {(not F1 releases F3) iff ((not F1) releases F3)} is theorem
    Notreleases13: {not ((not F1 releases F3) iff (not (F1 releases F3)))} is sat
    Notreleases23: {(not F2 releases F3) iff ((not F2) releases F3)} is theorem
    Notreleases23: {not ((not F2 releases F3) iff (not (F2 releases F3)))} is sat


    UntilAnd: {(F1 and F2 until F3) iff (F1 and (F2 until F3))} is theorem
    UntilAnd: {not ((F1 and F2 until F3) iff ((F1 and F2) until F3))} is sat

    ImpliesIff: {(F1 => F2 <=> F3) iff ((F1 => F2) <=> F3)} is theorem
    ImpliesIff: {not ((F1 => F2 <=> F3) iff (F1 => (F2 <=> F3)))} is sat

    IffOr: {(F1 || F2 <=> F3) iff (F1 || (F2 <=> F3))} is theorem
    IffOr: {not ((F1 || F2 <=> F3) iff ((F1 || F2) <=> F3))} is sat
}