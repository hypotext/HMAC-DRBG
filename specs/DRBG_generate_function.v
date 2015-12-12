Require Import Integers.
Require Import Coqlib.
Require Import List. Import ListNotations.
Require Import DRBG_working_state.
Require Import DRBG_state_handle.
Require Import DRBG_generate_algorithm_result.
Require Import DRBG_reseed_function.
Require Import entropy.

Fixpoint DRBG_generate_function_helper (generate_algorithm: DRBG_working_state -> Z -> list Z -> DRBG_generate_algorithm_result) (reseed_function: ENTROPY.stream -> DRBG_state_handle -> bool -> list Z -> ENTROPY.result DRBG_state_handle) (entropy_stream: ENTROPY.stream) (state_handle: DRBG_state_handle) (requested_number_of_bytes: Z) (prediction_resistance_request: bool) (additional_input: list Z) (should_reseed: bool) (count: nat): ENTROPY.result (list Z * DRBG_working_state) :=
  let result := if should_reseed then
                        match reseed_function entropy_stream state_handle prediction_resistance_request additional_input with
                          | ENTROPY.success x entropy_stream => ENTROPY.success (x, []) entropy_stream
                          | ENTROPY.error e entropy_stream => ENTROPY.error e entropy_stream
                        end
                      else ENTROPY.success (state_handle, additional_input) entropy_stream in
  match result with
    | ENTROPY.error e s => ENTROPY.error e s
    | ENTROPY.success (state_handle, additional_input) entropy_stream =>
      let '(working_state, security_strength, prediction_resistance_flag) := state_handle in
      match generate_algorithm working_state requested_number_of_bytes additional_input with
        | generate_algorithm_reseed_required =>
          match count with
            | O => ENTROPY.error ENTROPY.generic_error entropy_stream (* impossible *)
            | S count' => DRBG_generate_function_helper generate_algorithm reseed_function entropy_stream state_handle requested_number_of_bytes prediction_resistance_request additional_input true count'
          end
        | generate_algorithm_success x y => ENTROPY.success (x, y) entropy_stream
      end
    end.

Definition DRBG_generate_function (generate_algorithm: Z -> DRBG_working_state -> Z -> list Z -> DRBG_generate_algorithm_result) (reseed_function: ENTROPY.stream -> DRBG_state_handle -> bool -> list Z -> ENTROPY.result DRBG_state_handle) (reseed_interval: Z) (max_number_of_bytes_per_request: Z) (max_additional_input_length: Z) (entropy_stream: ENTROPY.stream) (state_handle: DRBG_state_handle) (requested_number_of_bytes requested_security_strength: Z) (prediction_resistance_request: bool) (additional_input: list Z): ENTROPY.result (list Z * DRBG_state_handle) :=
  let '(working_state, security_strength, prediction_resistance_flag) := state_handle in
  if Z.gtb requested_number_of_bytes max_number_of_bytes_per_request then ENTROPY.error ENTROPY.generic_error entropy_stream
  else
    if Z.gtb requested_security_strength security_strength then ENTROPY.error ENTROPY.generic_error entropy_stream
    else
      if Z.gtb (Zlength additional_input) max_additional_input_length then ENTROPY.error ENTROPY.generic_error entropy_stream
      else
        if prediction_resistance_request && (negb prediction_resistance_flag) then ENTROPY.error ENTROPY.generic_error entropy_stream
        else
          match DRBG_generate_function_helper (generate_algorithm reseed_interval) reseed_function entropy_stream state_handle requested_number_of_bytes prediction_resistance_request additional_input prediction_resistance_request 1%nat with
            | ENTROPY.error e s => ENTROPY.error e s
                                                 | ENTROPY.success (output, new_working_state) entropy_stream =>
                                                   ENTROPY.success (output, (new_working_state, security_strength, prediction_resistance_flag)) entropy_stream
          end.

