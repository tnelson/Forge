#lang forge

option local_necessity on 
sig WeHangUser {
    follows: set WeHangUser,
    isVerified: one Bool,
    hasProfilePic: one Bool
}

abstract sig Bool {} 
one sig Yes extends Bool {}
one sig No extends Bool {}

-- if weHangUserA follows weHangUserB, then weHangUserB must follow weHangUserA 
pred friendship {
    all w1, w2: WeHangUser | w1 in w2.follows implies w2 in w1.follows
}

-- weHang users can't follow each other 
pred noSelfFollower {
    no iden & follows
}

-- weHang users are verified if they have more than 2 followers
-- because follows is the same as followers, we can use follows
-- weHang users can only be verified if they have a profile pic 

pred verifiedWeHangUser {
    all w:WeHangUser | ((#(w.follows) > 2) and w.hasProfilePic = Yes) iff w.isVerified = Yes 
}

-- users can have no followes (we are not constrainint that they need followers)

pred weHangSimulation {
    friendship
    noSelfFollower
    verifiedWeHangUser
}

run weHangSimulation for exactly 4 WeHangUser

