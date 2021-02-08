#lang forge

option local_necessity on 
sig WeHangUser {
    follows: set WeHangUser, --the weHangUsers that A follows
    --followers: set WeHangUser, --the weHangUsers that follow A 
    isVerified: one Bool,
    hasProfilePic: one Bool
}

abstract sig Bool {} 
one sig Yes extends Bool {}
one sig No extends Bool {}

-- Properties that hold: 
-- (1) weHang users can have no follows
-- (2) weHang users can have no followers
-- (3) weHang users can't follow themselves
-- (4) weHang users are verified if they have more than 2 followers
--     and have a profile pic

-- Properties that don't hold:
-- (1) follows is a symmetric relation
-- (2) not all users can be verified 
-- (3) users can't be followed if they don't have a profile picture

pred noSelfFollower {
    no iden & follows
}

pred verifiedWeHangUser {
    all w:WeHangUser | ((#(follows.w) > 2) and w.hasProfilePic = Yes) iff w.isVerified = Yes
}

pred weHangSimulation {
    noSelfFollower
    verifiedWeHangUser
}

run weHangSimulation for exactly 4 WeHangUser

