Require Import Coqlib.
Require Import List. Import ListNotations.
Require Import HMAC_DRBG_update.
Require Import DRBG_working_state.

Fixpoint list_of_length (length: nat) (value: list Z): (list Z) :=
  match length with
    | O => []
    | S n => value ++ list_of_length n value
  end.

Definition initial_key: list Z := list_of_length 32 [0].

Definition initial_value: list Z := list_of_length 32 [1].

(*
Require Import HMAC256_functional_prog.
Require Import functional_prog.
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.

Definition provided_data := (hexstring_to_Zlist "ca851911349384bffe89de1cbdc46e6831e44d34a4fb935ee285dd14b71a7488"%string) ++ (hexstring_to_Zlist "659ba96c601dc69fc902940805ec0ca8")%string.

Definition HMAC256' key value := HMAC256 value key.

Definition key := HMAC256' initial_key (initial_value ++ [0] ++ provided_data).
Definition value := HMAC256' key initial_value.
Definition key2 := HMAC256' key (value ++ [1] ++ provided_data).
Definition value2 := HMAC256' key2 value.


Lemma printing: key2 = [].
vm_compute.
*)
Definition HMAC_DRBG_instantiate_algorithm 
           (HMAC: list Z -> list Z -> list Z) 
           (entropy_input nonce personalization_string: list Z) 
           (security_strength: Z):
  DRBG_working_state :=
  let seed_material := entropy_input ++ nonce ++ personalization_string in
  let key := initial_key in
  let value := initial_value in
  let (key, value) := HMAC_DRBG_update HMAC seed_material key value in
  let reseed_counter := 1 in
  (value, key, reseed_counter).