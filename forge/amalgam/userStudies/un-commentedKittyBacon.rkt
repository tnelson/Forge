#lang forge
sig Cat {friends : set Cat}
pred NoLonelyKittens {all c: Cat | some c.friends}
pred OutsideFriends {no iden & friends}
pred TwoWayStreet {friends = ~friends}
pred friendship {
    NoLonelyKittens
    OutsideFriends
    TwoWayStreet
}

one sig KittyBacon extends Cat {connectionsOf: set Cat}
pred connectionsOfKittyBacon {
    friendship
    KittyBacon.connectionsOf = KittyBacon.friends + KittyBacon.friends.friends
    + KittyBacon.friends.friends.friends - KittyBacon
}
pred ConnectedKittyBacon {
    connectionsOfKittyBacon
    Cat - KittyBacon = KittyBacon.connectionsOf
}
pred SuperConnected {
    connectionsOfKittyBacon
    Cat - KittyBacon in KittyBacon.^friends
}
pred ConnectedKittyBacon_equals_SuperConnected {
    ConnectedKittyBacon iff SuperConnected
}
check ConnectedKittyBacon_equals_SuperConnected for exactly 3 Cat 
check ConnectedKittyBacon_equals_SuperConnected for exactly 4 Cat 
check ConnectedKittyBacon_equals_SuperConnected for exactly 5 Cat 

one sig FFF  {membersOfFoundation: set Cat} 
pred FancyFelineFoundation {
    ConnectedKittyBacon
    FFF.membersOfFoundation = KittyBacon.connectionsOf 
}
pred KittyBaconIsAFancyFeline {
    FancyFelineFoundation implies KittyBacon in FFF.membersOfFoundation
}

check KittyBaconIsAFancyFeline for exactly 4 Cat 