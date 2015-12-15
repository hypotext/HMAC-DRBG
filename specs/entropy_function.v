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

(* Algebraic data type for the calls 

Type call = 
Instantiate | n -> Generate n :: call
*)

(* User can call instantiate once, followed by any number of generate (with some number of blocks). Generate may output "reseed needed" at the beginning of a call to generate. This is implicitly taken as "generate calls reseed" at the beginning of the call to generate, if needed, then continues after reseeding. So we aren't splitting that into 2 calls to generate. *)

(* Given that the (min) amount of entropy required per call is security_strength = e bits
  (112, 128, 192, or 256 bits)
and the reseed maximum is reseed_interval <= 2^48 requests (p39),
and the max number of blocks per generate call is b,

Given a list l of calls, 
the total amount of entropy needed as a function of the above =
T(e, r, l) = e + 

note: instantiate both takes an entropy input and calls
how many times is an RNG called in practice? >2^48?

assuming it's called 10 times a second, 

2^48 / 10 / 60 / 60 / 24 / 7 / 52 = 895,005 = you only have to reseed after 800,000 years? the reseed time interval isn't anything on the scale of 1s of years unless you call the RNG 
~100,000 times per second

> The NIST DRBG mechanism reseeds on either (a) first use; or (b) at the end of the seed life. The seed life of the DRBG mechanism is deliberately set high to reduce the risk of an attacker forcing a reseed by repeatedly requesting random data (which she could skew/seed herself?)
 *)

