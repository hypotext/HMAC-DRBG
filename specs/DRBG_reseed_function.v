Require Import Coqlib.
Require Import List. Import ListNotations.
Require Import DRBG_state_handle.
Require Import DRBG_working_state.
Require Import entropy.

Definition DRBG_reseed_function 
           (reseed_algorithm: DRBG_working_state -> list Z -> list Z -> DRBG_working_state) 
           (min_entropy_length max_entropy_length: Z) 
           (max_additional_input_length: Z) 
           (entropy_stream: ENTROPY.stream) 
           (state_handle: DRBG_state_handle) 
           (prediction_resistance_request: bool) 
           (additional_input: list Z): ENTROPY.result DRBG_state_handle :=

  let '(working_state, security_strength, prediction_resistance_flag) := state_handle in

  if prediction_resistance_request && (negb prediction_resistance_flag) 
  then ENTROPY.error ENTROPY.generic_error entropy_stream
  else
    if Z.gtb (Zlength additional_input) max_additional_input_length 
    then ENTROPY.error ENTROPY.generic_error entropy_stream
    else
      (* get_entropy finally called here *)
      match get_entropy security_strength min_entropy_length
                        max_entropy_length prediction_resistance_flag entropy_stream with
      | ENTROPY.error _ s =>
        ENTROPY.error ENTROPY.catastrophic_error s
      | ENTROPY.success entropy_input entropy_stream =>
        (* reseed_algorithm called here -- i'm guessing entropy_input has length > min *)
        let new_working_state := reseed_algorithm working_state
                                                  entropy_input additional_input in
        ENTROPY.success (new_working_state, security_strength,
                         prediction_resistance_flag) entropy_stream
      end.
