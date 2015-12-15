Require Import Coqlib.
Require Import List. Import ListNotations.
Require Import sha.HMAC256_functional_prog.
Require Import DRBG_instantiate_function.
Require Import HMAC_DRBG_instantiate_algorithm.
Require Import DRBG_generate_function.
Require Import HMAC_DRBG_generate_algorithm.
Require Import DRBG_reseed_function.
Require Import HMAC_DRBG_reseed_algorithm.
Require Import HMAC_DRBG_update.

Definition HMAC256_DRBG_update := HMAC_DRBG_update HMAC256.

Definition HMAC256_DRBG_instantiate_algorithm := HMAC_DRBG_instantiate_algorithm HMAC256.

Definition HMAC256_DRBG_instantiate_function := DRBG_instantiate_function HMAC256_DRBG_instantiate_algorithm.

Definition HMAC256_DRBG_generate_algorithm := HMAC_DRBG_generate_algorithm HMAC256.

Definition HMAC256_DRBG_generate_function := DRBG_generate_function HMAC256_DRBG_generate_algorithm.

Definition HMAC256_DRBG_reseed_algorithm := HMAC_DRBG_reseed_algorithm HMAC256.

Definition HMAC256_DRBG_reseed_function := DRBG_reseed_function HMAC256_DRBG_reseed_algorithm.

Inductive trace : Type :=
  | Instantiate : trace
  | Generate : trace -> nat -> trace.

Check (Generate (Generate (Instantiate) 10) 20).

Print HMAC256_DRBG_generate_function.
(* entropy input to instantiate (list Z -- minimum?) *)
(* actually, I might not need to reason about DRBG generic functions? they're the ones with the stream *)
(* calculation vs. theorem? *)
(* how to phrase "min amount of entropy needed is" ...? number of calls to get_entropy is __? (leaving aside min_entropy * ...) write a version of the code that passes around the number of times GE is called, doing nothing different; prove equivalent on fst; and prove that the snd (# times called) = function of # instantiate/generates? 

how annoying would it be to "pass it around" everywhere? could i just add it as a DRBG state parameter? it would be just like the reseed counter, which is ~ # times *generate* is called. 

how useful would this be for the application (bvector whatever)?
*)
(* TODO email andrew/lennart abt this *)

(* What are some helper theorems I could prove about instantiate and generate? *)

(* functions involved:

instantiate
generate
reseed
DRBG_ins
DRBG_gen
DRBG_reseed

and NOT update *)

(* what is the difference between HMAC_DRBG_X_algorithm and DRBG_generate_function? 
they're just error catching and entropy stream handling *)

(* note: when we execute, we have to pass everything around from instantiate to generate to generate etc... so we have to do what the DRBG functions do? handle errors etc? TODO think about passing around *)
(* Definition execute (calls : trace) : nat := *)
(*   match calls with *)
(*   | Instantiate =>  *)
(*   | Generate =>  *)

(* maybe this should just be a list of numbers
Instantiate is implicit anyway, always happens exactly once at the beginning
each number corresponds to a generate call 

[5, 10, 20] = >> I >> G 5 >> G 10 >> G 20

Theorem: 
forall (l : list call), 
NumCallsToEntropyFunction (execute l) = 1 + (length l % r)

implies total entropy = e * (1 + (length l % r))
  where 

(we only care about the state, not the output)

(with some error-handling)
(also, can this actually be executed? there are some abstract types like the stream. can I keep it as a parameter?)
execute l =
  let init_state := instantiate ... PRG
  match l with
  | nil => init_state
  | x :: xs =>
    let new_state := generate x 

NumCallsToEntropyFunction (execute l) = ...

generate:            (prediction_resistance_request: bool) -> (should_reseed : bool)
it automatically does the reseed by recursively calling its helper; i don't have to handle it

generate returns ENTROPY.result (list Z * DRBG_state_handle)
instantiate returns DRBG_working_state
(which is a subset of DRBG_state_handle)
so, just add a "numEntropyCalls" param to DRBG_working_state, and increment it each time getEntropy is called (inside reseed)

reseeding is for prediction resistance

Definition DRBG_state_handle: Type := 
(DRBG_working_state * Z * bool)%type. 
(* state, security_strength, prediction_resistance_flag *)

Definition DRBG_working_state: Type := 
(list Z * list Z * Z)%type. 
(* value * key * reseed_counter *)

>> denotes state-passing *)

Theorem entropy_function :
  forall (calls : list call),
    let n := length calls in
    0 = 
    (* entropy_required =  *)
Proof.

Admitted.

(* User can call instantiate once, followed by any number of generate (with some number of blocks). Generate may output "reseed needed" at the beginning of a call to generate. This is implicitly taken as "generate calls reseed" at the beginning of the call to generate, if needed, then continues after reseeding. So we aren't splitting that into 2 calls to generate. *)

(* Given that the (min) amount of entropy required per call is security_strength = e bits
  (112, 128, 192, or 256 bits)
and the reseed maximum is reseed_interval <= 2^48 requests (p39),
and the max number of blocks per generate call is b,

Given a list l of calls, 
want to prove that the total amount of entropy needed as a function of the above =
T(e, r, l) = e + (l / r) * e = e * (1 + (l / r))
(integer division, round down)

note: instantiate both takes an entropy input and calls
how many times is an RNG called in practice? >2^48?

assuming it's called 10 times a second, 

2^48 / 10 / 60 / 60 / 24 / 7 / 52 = 895,005 = you only have to reseed after 800,000 years? the reseed time interval isn't anything on the scale of 1s of years unless you call the RNG 
~100,000 times per second

> The NIST DRBG mechanism reseeds on either (a) first use; or (b) at the end of the seed life. The seed life of the DRBG mechanism is deliberately set high to reduce the risk of an attacker forcing a reseed by repeatedly requesting random data (which she could skew/seed herself?)
 *)


