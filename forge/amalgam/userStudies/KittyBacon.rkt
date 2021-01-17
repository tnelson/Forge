#lang forge
sig Cat {
    friends : set Cat
}
--run {} -- example of unrestrained friendship
pred NoLonelyKittens {
    -- all cats have at least one friend
    all c: Cat | some c.friends
}
pred OutsideFriends {
    -- no self-friendship     
    no iden & friends
}
pred TwoWayStreet {
    -- friendship is symmetric
    friends = ~friends
}
pred friendship {
    NoLonelyKittens
    OutsideFriends
    TwoWayStreet
}
--run friendship -- example of friendship rules
one sig KittyBacon extends Cat {
    connectionsOf: set Cat
}
pred connectionsOfKittyBacon {
    friendship
    KittyBacon.connectionsOf = KittyBacon.friends + KittyBacon.friends.friends + KittyBacon.friends.friends.friends - KittyBacon
}
pred ConnectedKittyBacon {
    connectionsOfKittyBacon
    Cat - KittyBacon = KittyBacon.connectionsOf
}
--run ConnectedKittyBacon -- example of KB connections
pred SuperConnected {
    connectionsOfKittyBacon
    Cat - KittyBacon in KittyBacon.^friends
}
pred ConnectedKittyBacon_equals_SuperConnected {
    ConnectedKittyBacon iff SuperConnected
}
--check ConnectedKittyBacon_equals_SuperConnected for exactly 3 Cat -- no counterexample
--check ConnectedKittyBacon_equals_SuperConnected for exactly 4 Cat -- no counterexample
--check ConnectedKittyBacon_equals_SuperConnected for exactly 5 Cat -- gets counterexample!
-- Example Part 1 Comment
-- It's not possible to modify KittyBacon.connectionsOf so that it returns no counterexamples for any # of cats 
-- without using * or ^, since as more cats enter the equation, being friends of friends, etc, 
-- won't be enough to necessarily connect all cats to Kitty Bacon, unless the scope of 
-- Kitty Bacon.connectionsOf is adjusted according to the increase in number of cats. 
-- Even if it worked for a large number of cats, it wouldn't work for ANY number of cats.

-- CoolCatClub

one sig CCC  {
    members: set Cat
} 

pred CoolCatClub {
    ConnectedKittyBacon
    CCC.members = KittyBacon.connectionsOf + KittyBacon
}

pred KittyBaconIsCool {
    CoolCatClub implies KittyBacon in CCC.members
}

check KittyBaconIsCool for exactly 4 Cat -- no counterexample 

// Question from handout: Why is this failing? Well, KittyBacon is never part of
// her connections! So there is an overconstraint. Students need to modify the
// CoolCatClub predicate to define CCC.members = KittyBacon.connectionsOf + Kitty
// Bacon 