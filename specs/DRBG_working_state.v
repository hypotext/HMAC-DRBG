Require Import Coqlib.
Require Import List. Import ListNotations.

Definition DRBG_working_state: Type := (list Z * list Z * Z)%type. (* value * key * reseed_counter *)