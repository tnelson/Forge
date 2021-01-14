#lang forge

sig Cat {
    friends: set Cat
}

one sig KittyBacon extends Cat {
    connectionsOf: set Cat
}

// Make sure that every cat has at least one friend 
pred NoLonelyKittens {
    no c: Cat | no c.friends 

}

// Cats are friends with other kittens
pred OutsideFriends {
    all c: Cat | c not in c.friends
}

// Two way street for friendships 
pred TwoWayStreet {
    all c1, c2: Cat | c1 in c2.friends iff c2 in c1.friends 
}

// Combination of previous predicates
pred friendship {
    NoLonelyKittens
    OutsideFriends
    TwoWayStreet
}

// Define the connections of Kitty Bacon 
pred connectionsOfKittyBacon {
    friendship
    KittyBacon.connectionsOf = KittyBacon.friends + KittyBacon.friends.friends + KittyBacon.friends.friends.friends - KittyBacon 
}

// Kitty Bacon's connections
pred ConnectedKittyBacon {
    connectionsOfKittyBacon
    Cat - KittyBacon = KittyBacon.connectionsOf 
}

// Equivalent predicate to ConnectedKittyBacon, except using ^
// instead of union of friends
pred SuperConnected {
    connectionsOfKittyBacon
    Cat - KittyBacon in KittyBacon.^friends 
}

// If two predicates are equivalent then there is a bi-implication between both of
// them. However, if you increase the scope of cat, they stop being equivalent. 
pred ConnectedKittyBacon_equals_SuperConnected {
    ConnectedKittyBacon iff SuperConnected
}

// This check breaks for 5 (unless we change the union pred) 
check ConnectedKittyBacon_equals_SuperConnected for exactly 4 Cat

run {ConnectedKittyBacon and connectionsOfKittyBacon}
run {friendship}

pred failure {
    not ConnectedKittyBacon_equals_SuperConnected
}

pred inexactReason{
    some c:Cat-KittyBacon | c not in KittyBacon.connectionsOf
}

run { failure and not inexactReason } for exactly 5 Cat

pred exactReason { /* FILL */ }

check { connectionsOfKittyBacon implies (failure iff exactReason) } for
exactly 5 Cat