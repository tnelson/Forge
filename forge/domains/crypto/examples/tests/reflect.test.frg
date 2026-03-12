#lang forge 

open "../../base.frg"
open "../reflect.rkt"
open "../reflect.frg"
option run_sterling "../../vis/crypto_viz.js"

// See reflect.frg for documentation.
TEST_reflect_responder_pov: assert {
  wellformed                   
  exec_reflect_init           
  exec_reflect_resp            
  constrain_skeleton_reflect_2 
  reflect_resp.agent != reflect_init.agent
  reflect_resp.reflect_resp_a != getInv[reflect_resp.reflect_resp_b]    
  reflect_resp.reflect_resp_b != getInv[reflect_resp.reflect_resp_a]
} is sat for 
      exactly 4 Timeslot, exactly 2 Microtick, 13 mesg,  
      exactly 1 KeyPairs, exactly 6 Key, exactly 6 akey, 0 skey, 
      exactly 3 PrivateKey, exactly 3 PublicKey,
      exactly 3 name, 0 text, exactly 4 Ciphertext,
      exactly 1 reflect_init, exactly 1 reflect_resp,
      1 Int
    for {next is linear 
        mt_next is linear} 
