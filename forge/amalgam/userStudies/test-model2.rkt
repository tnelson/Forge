#lang forge

option local_necessity on 
sig WeHangUser {
    follows: set WeHangUser,
    isVerified: one Bool
    --hasProfilePic: one Bool
}

abstract sig Bool {} 
one sig Yes extends Bool {}
one sig No extends Bool {}

-- (1) if weHangUserA follows weHangUserB, then weHangUserB must follow weHangUserA 
pred friendship {
    all w1, w2: WeHangUser | w1 in w2.follows implies w2 in w1.follows
}

-- (2) weHang users can't follow themselves 
pred noSelfFollower {
    no iden & follows
}

-- (3) weHang users are verified if they have more than 2 followers
pred verifiedWeHangUser {
    --all w:WeHangUser | ((#(w.follows) > 2) and w.hasProfilePic = Yes) iff w.isVerified = Yes
     all w:WeHangUser | (#(w.follows) > 2) iff w.isVerified = Yes 
}

-- (4) If a user has a follow, then the user must have more than 1 follow
pred leastNumFriends {
    all w: WeHangUser | some w.follows iff #(w.follows) > 1
}

-- (5) users can have no followes (we are not constrainint that they need followers)

pred weHangSimulation {
    friendship
    noSelfFollower
    leastNumFriends
    verifiedWeHangUser
}

run weHangSimulation for exactly 4 WeHangUser

