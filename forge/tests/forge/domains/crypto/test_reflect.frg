#lang forge

/*
  CI regression test for the reflection protocol.
  A valid execution from the responder's POV should be satisfiable.
  Bounds taken from reflect.frg.
*/

open "../../../../domains/crypto/examples/reflect.rkt"

option run_sterling off

pred reflect_scenario {
    wellformed
    exec_reflect_init
    exec_reflect_resp
    constrain_skeleton_reflect_2
    reflect_resp.agent != reflect_init.agent
    reflect_resp.reflect_resp_a != getInv[reflect_resp.reflect_resp_b]
    reflect_resp.reflect_resp_b != getInv[reflect_resp.reflect_resp_a]
}

test expect {
    reflect_sat: { reflect_scenario }
        for exactly 4 Timeslot, 13 mesg,
            exactly 1 KeyPairs, exactly 6 Key, exactly 6 akey, 0 skey,
            exactly 3 PrivateKey, exactly 3 PublicKey,
            exactly 3 name, 0 text, exactly 4 Ciphertext,
            exactly 1 reflect_init, exactly 1 reflect_resp,
            1 Int
            for {next is linear}
        is sat
}
