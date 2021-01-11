#lang forge

option problem_type temporal

/*
Taken from https://github.com/haslab/Electrum2/wiki/Social-Network
Models a distributed social network
*/

sig User {}

sig Post {}

sig DistributedSN {
    servers : set Server,
    friends : set User->User
}

sig Server {
    posts : set User->Post,
    capacity : set Int //max number of posts in the Server
}

//Each post is owned by one user
pred oneUserPerPost {
    all p : Post | one Server.posts.p
}

pred validFriend {
    //A user cannot be their own friend
    all sn : DistributedSN | no (sn.friends && iden)
    //Friendship is symmetric
    all sn : DistributedSN | ~(sn.friends) in sn.friends
}

pred validServer {
    //Each Server has one Capacity
    all s : Server | one s.capacity
    //Each Server's Capacity is Positive
    all s : Server | all c : s.capacity | min[c] > 0
    //Servers cannot exceed their Capacity
    all s : Server | all c : s.capacity | #(User.(s.posts)) <= min[c]
}

pred addPost[sn : DistributedSN, u : User, p : Post] {
    u->p not in sn.servers.posts
    (sn.servers.posts)' = sn.servers.posts + u->p
}

pred delPost[sn : DistributedSN, u : User, p : Post] {
    u->p in sn.servers.posts
    (sn.servers.posts)' = sn.servers.posts - u->p
}

pred addFriend[sn : DistributedSN, u : User, v : User] {
    u->v not in sn.friends
    (sn.friends)' = sn.friends + u->v
}

pred delFriend[sn : DistributedSN, u : User, v : User] {
    u->v in sn.friends
    (sn.friends)' = sn.friends - u->v
}

pred addServer[sn : DistributedSN, s : Server] {
    s not in sn.servers
    (sn.servers)' = sn.servers + s
}

pred delServer[sn : DistributedSN, s : Server] {
    s in sn.servers
    (sn.servers)' = sn.servers - s
}

pred deleteVsAdd[sn : DistributedSN] {
    all u : User, p : Post | {
        addPost[sn, u, p] implies u->p in sn.servers.posts until delPost[sn, u, p]
        delPost[sn, u, p] implies u->p not in sn.servers.posts until addPost[sn, u, p]
        all v : User | {
            addFriend[sn, u, v] implies u->v in sn.friends until delFriend[sn, u, v]
            delFriend[sn, u, v] implies u->v not in sn.friends until delFriend[sn, u, v]
        }
    }
    all s : Server | {
        addServer[sn, s] implies sn->s in servers until delServer[sn, s]
        delServer[sn, s] implies sn->s not in servers until addServer[sn, s]
    }
}

pred behavior {
    oneUserPerPost
    validFriend
    validServer
    always (
    all sn : DistributedSN {
        (some u : User, p : Post | addPost[sn, u, p] or delPost[sn, u, p])
        or
        (some u : User, v : User | addFriend[sn, u, v] or delFriend[sn, u, v])
        or
        (some s : Server | addServer[sn, s] or delServer[sn, s])
    }
    )
}

test expect AddAndDeleteWork {
    delCanUndoAdd : {
        behavior
        all sn : DistributedSN | deleteVsAdd[sn]
    } is sat
    delMustUndoAdd : {
        behavior
        not (all sn : DistributedSN | deleteVsAdd[sn])
    } is unsat
}