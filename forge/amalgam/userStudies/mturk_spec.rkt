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
one sig Owner extends WeHangUser {}

-- Properties that hold: 
-- (1) weHang users (excluding the owner) can follow nobody
-- (2) The Owner needs to follow all weHang users  
-- (3) weHang users can't follow themselves
-- (4) weHang users (excluding the owner) are verified if they have more than 2 followers
--     and have a profile pic
-- (5) The Owner is always verified
-- (6) The owner needs to have a profile picture 

-- Properties that don't hold:
-- (1) follows is a symmetric relation
-- (2) not all users can be verified 
-- (4) weHang users can have no followers

pred noSelfFollower {
    no iden & follows
}

pred verifiedWeHangUser {
    all w:WeHangUser - Owner | ((#(follows.w) > 2) and w.hasProfilePic = Yes) iff w.isVerified = Yes
}

pred OwnerRestriction {
    no follows.Owner
    Owner.isVerified = Yes
    Owner.hasProfilePic = Yes 
}

pred OwnerFollowsAll {
    all w:WeHangUser - Owner | w in Owner.follows
}

pred weHangSimulation {
    noSelfFollower
    verifiedWeHangUser
    OwnerFollowsAll
    OwnerRestriction
}

run weHangSimulation for exactly 4 WeHangUser

