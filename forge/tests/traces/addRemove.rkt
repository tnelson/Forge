#lang forge

sig Thing {}
sig Bag { contents: set Thing }

state[Bag] Bag_init { no contents}
transition[Bag] Bag_add {
    some a: Thing {
        contents' = contents+a
        a not in contents
    }
}
--state[Bag] Bag_term { #Thing = #contents }

trace<|Bag, Bag_init, Bag_add, _|> T: plinear {}

--run<|T|> { some b: Bag | b.(T.tran).contents in b.contents }

test expect {
    <|T|> { some b: Bag | b.contents = Thing } is sat

    -- Note that if we leave out the "-T.term", the last bag's b.(T.tran).contents is empty.    
    <|T|> { some b: Bag-T.term | b.(T.tran).contents in b.contents } is unsat
    <|T|> { some b: Bag | b not in (T.init).^(T.tran) } is sat
}








