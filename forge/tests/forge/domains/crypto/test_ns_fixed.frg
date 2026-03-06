#lang forge

/*
  CI regression test for the fixed Needham-Schroeder protocol.
  The fix prevents the man-in-the-middle attack — should be unsatisfiable.
  Same bounds as the vulnerable version for a direct comparison.
*/

open "../../../../domains/crypto/examples/ns_fixed.rkt"

option run_sterling off

pred ns_attack_exists {
    ns_init.ns_init_n1 + ns_init.ns_init_n2 in Attacker.learned_times.Timeslot
}
pred ns_success {
    (ns_init.ns_init_n1 + ns_init.ns_init_n2) in
    (ns_init.agent.learned_times.Timeslot & ns_resp.agent.learned_times.Timeslot)
}
pred ns_attack_conditions {
    ns_init.ns_init_n1 not in Attacker.generated_times.Timeslot
    ns_init.ns_init_n2 not in Attacker.generated_times.Timeslot
    ns_init.ns_init_a = ns_init.agent
    ns_resp.ns_resp_b = ns_resp.agent
    ns_init.ns_init_n1 != ns_init.ns_init_n2
    ns_init.agent != Attacker
    ns_resp.agent != Attacker
}
pred ns_scenario {
    wellformed
    ns_attack_conditions
    ns_success
    exec_ns_init
    exec_ns_resp
    constrain_skeleton_ns_1
    ns_attack_exists
}

test expect {
    ns_fixed_no_attack: { ns_scenario }
        for exactly 6 Timeslot, exactly 1 KeyPairs, exactly 6 Key, exactly 3 name, 16 mesg,
            exactly 2 text, exactly 5 Ciphertext, exactly 1 ns_init, exactly 1 ns_resp,
            exactly 3 PublicKey, exactly 3 PrivateKey, 0 skey, exactly 6 akey, exactly 3 strand,
            6 Int
            for {next is linear}
        is unsat
}
