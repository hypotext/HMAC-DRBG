Require Import Integers.
Require Import Coqlib.
Require Import List. Import ListNotations.
Require Import DRBG_working_state.
Require Import DRBG_state_handle.
Require Import DRBG_generate_algorithm_result.
Require Import DRBG_reseed_function.
Require Import entropy.

(* 12/15/15

new fn: how many bits of entropy it consumed
prefix of that length that relates the two infinite streams
abstraction function
will need to have all of these (orange) parameters

stream = exists finite prefix of size n ++ stream'

see naphat's entropy.v

but how do i use the abstraction function??
what's the new theorem statement?

(also, how do i deal with the failures in the stream?)

? i don't remember what else

do i need some relation on the input stream? or just equality?

DRBG_generate_function_helper' ga rf stream state n prr ai reseed ct :=
  min_bits

f $ DRBG_gen inputs = DRBG_gen' inputs

note: stream in (entropy_stream), stream out (ENTROPY.result)

wouldn't it be an abstraction theorem instead?
would definitely need to reason about low-level parts of naphat's spec

would i need an abstraction function/theorem for reseed as well? since it actually calls GE

need to do the abstraction function on the stream of all successes

gen_func (filter entropy_stream) .. other inputs .. = entropy_stream' .. other outputs ..
is the filtering ok?

gen_func (filter entropy_stream) .. other inputs ..
is equivalent to
calling gen_func repeatedly until n successes? 

and then gen_func (filter entropy_stream) = 

should i say "after n generate calls, the stream returned is equal to the original stream with the first (e * 1 + (l/r)) bits removed"?

prove that after 1 generate call
(maybe i shouldn't be thinking in terms of # calls???)
if reseed_ctr then ... otherwise ... ?

maybe i should have a version that RETURNS the bits that it took off the head of the stream
/ returns their length / concats the stream

do the proof first for (filter entropy_stream)
(ALSO naphat has some relevant proofs/tests in entropy.v)

if the entropy fails, then the reseed fails, the gen func helper fails, and the gen func fails (up to the top level). so, i have to deal with calling generate until there is a success. that might not terminate? 

Inductive result X: Type: Type :=
| success: X -> stream -> @result X
| error : error_code -> stream -> @result X.

 *)
Fixpoint DRBG_generate_function_helper
         (generate_algorithm: DRBG_working_state -> Z -> list Z
                              -> DRBG_generate_algorithm_result)
         (reseed_function: ENTROPY.stream -> DRBG_state_handle
                           -> bool -> list Z -> ENTROPY.result DRBG_state_handle)
         (entropy_stream: ENTROPY.stream)
         (state_handle: DRBG_state_handle)
         (requested_number_of_bytes: Z)
         (prediction_resistance_request: bool)
         (additional_input: list Z)
         (should_reseed: bool)
         (count: nat): ENTROPY.result (list Z * DRBG_working_state) :=
  (* reseed happens here on second call to helper *)
  let result := if should_reseed then
                  (* passes the entropy stream to the reseed function *)
                  match reseed_function entropy_stream state_handle
                                        prediction_resistance_request
                                        additional_input with
                  | ENTROPY.success x entropy_stream =>
                    ENTROPY.success (x, []) entropy_stream
                  | ENTROPY.error e entropy_stream =>
                    ENTROPY.error e entropy_stream
                  end
                else ENTROPY.success (state_handle, additional_input) entropy_stream in
  match result with
    | ENTROPY.error e s => ENTROPY.error e s
    | ENTROPY.success (state_handle, additional_input) entropy_stream =>
      let '(working_state, security_strength, prediction_resistance_flag) := state_handle in
      (* generate_algorithm actually called here *)
      match generate_algorithm working_state requested_number_of_bytes additional_input with
        | generate_algorithm_reseed_required =>
          match count with
            | O => ENTROPY.error ENTROPY.generic_error entropy_stream (* impossible *)
            | S count' =>
              (* reseed happens here -- calls helper again *)
              DRBG_generate_function_helper
                generate_algorithm reseed_function entropy_stream state_handle
                requested_number_of_bytes prediction_resistance_request additional_input
                true count'
          end
        | generate_algorithm_success x y => ENTROPY.success (x, y) entropy_stream
      end
    end.

Definition DRBG_generate_function
           (generate_algorithm: Z -> DRBG_working_state -> Z -> list Z
                                -> DRBG_generate_algorithm_result)
           (reseed_function: ENTROPY.stream -> DRBG_state_handle -> bool
                             -> list Z -> ENTROPY.result DRBG_state_handle)
           (reseed_interval: Z)
           (max_number_of_bytes_per_request: Z)
           (max_additional_input_length: Z)
           (entropy_stream: ENTROPY.stream)
           (state_handle: DRBG_state_handle)
           (requested_number_of_bytes requested_security_strength: Z)
           (prediction_resistance_request: bool)
           (additional_input: list Z):
  ENTROPY.result (list Z * DRBG_state_handle) :=
  let '(working_state, security_strength, prediction_resistance_flag) := state_handle in
  if Z.gtb requested_number_of_bytes max_number_of_bytes_per_request 
  then ENTROPY.error ENTROPY.generic_error entropy_stream
  else
    if Z.gtb requested_security_strength security_strength 
    then ENTROPY.error ENTROPY.generic_error entropy_stream
    else
      if Z.gtb (Zlength additional_input) max_additional_input_length 
      then ENTROPY.error ENTROPY.generic_error entropy_stream
      else
        if prediction_resistance_request && (negb prediction_resistance_flag) 
        then ENTROPY.error ENTROPY.generic_error entropy_stream
        else
          (* helper used here *)
          match DRBG_generate_function_helper
                  (* generate_algorithm here *)
                  (generate_algorithm reseed_interval) reseed_function entropy_stream
                  state_handle requested_number_of_bytes prediction_resistance_request
                  additional_input prediction_resistance_request 1%nat with
            | ENTROPY.error e s => ENTROPY.error e s
            | ENTROPY.success (output, new_working_state) entropy_stream =>
              ENTROPY.success (output, (new_working_state, security_strength,
                                        prediction_resistance_flag))
                              entropy_stream
          end.

