Require Import Recdef.
Require Import Coqlib.
Require Import List. Import ListNotations.

Require Import sha.general_lemmas.

Require Import DRBG_working_state.
Require Import HMAC_DRBG_update.
Require Import DRBG_generate_algorithm_result.

Function HMAC_DRBG_generate_helper (HMAC: list Z -> list Z -> list Z) (key v: list Z) (requested_number_of_bytes: nat) {measure (fun x => x) requested_number_of_bytes}: (list Z * list Z) :=
  match requested_number_of_bytes with
    | O => (v, [])
    | _ => 
    let len := 32%nat in (* TODO get this from property of HMAC *)
    let (v, rest) := HMAC_DRBG_generate_helper HMAC key v (requested_number_of_bytes - 32)%nat in
    let v := HMAC v key in
    let temp := v in
    (v, rest ++ temp)
  end. 
Proof.
  intros.
  omega.
Defined.

Function HMAC_DRBG_generate_helper_Z (HMAC: list Z -> list Z -> list Z) (key v: list Z) (requested_number_of_bytes: Z) {measure Z.to_nat requested_number_of_bytes}: (list Z * list Z) :=
  if Z.geb 0 requested_number_of_bytes then (v, [])
  else
    let len := 32%nat in (* TODO get this from property of HMAC *)
    let (v, rest) := HMAC_DRBG_generate_helper_Z HMAC key v (requested_number_of_bytes - (Z.of_nat len)) in
    let v := HMAC v key in
    let temp := v in
    (v, rest ++ temp). 
Proof.
  intros. rewrite Z2Nat.inj_sub by omega.
  rewrite Nat2Z.id.
  assert ((0 <? requested_number_of_bytes) = true).
  * 
    rewrite Z.ltb_antisym.
    rewrite <- Z.geb_leb.
    rewrite teq.
    auto.
  *
  apply Zlt_is_lt_bool in H.
  apply Z2Nat.inj_lt in H; omega.
Defined.

Lemma HMAC_DRBG_generate_helper_Z_any_prop_fst:
  forall (P: list Z -> Prop) HMAC key v z,
    0 <= z ->
    P v ->
    (forall x y, P (HMAC x y)) ->
    P (fst (HMAC_DRBG_generate_helper_Z HMAC key v z)).
Proof.
  intros P HMAC key v z Hz Hv H.
  remember (z/32) as n.
  rewrite <- (Z2Nat.id n) in Heqn by (subst; apply (Z_div_pos z 32); omega).
  generalize dependent z.
  induction (Z.to_nat n); intros.
  {
    rewrite (Z_div_mod_eq z 32); try omega.
    rewrite HMAC_DRBG_generate_helper_Z_equation.
    rewrite <- Heqn.
    change (Z.of_nat 0) with 0.
    change (32 * 0 + z mod 32) with (z mod 32).
    change (Z.of_nat 32) with 32.
    remember (0 >=? z mod 32) as zero_geb_z; destruct zero_geb_z.
    auto.
    rewrite HMAC_DRBG_generate_helper_Z_equation.
    remember (0 >=? z mod 32 - 32) as zero_geb_z_minus_32; destruct zero_geb_z_minus_32.
    apply H.
    {
      (* this case is bogus. z mod 32 - 32 cannot be bigger than 0 *)
      admit (* TODO *).
    }
  }
  {
    rewrite (Z_div_mod_eq z 32); try omega.
    rewrite <- Heqn.
    assert (Hn: 32 * Z.of_nat (S n0) + z mod 32 = 32 * Z.of_nat n0 + z mod 32 + 32). 
    {
      rewrite Nat2Z.inj_succ.
      rewrite <- Zmult_succ_r_reverse.
      omega.
    }
    rewrite Hn.
    rewrite HMAC_DRBG_generate_helper_Z_equation.
    destruct (0 >=? 32 * Z.of_nat n0 + z mod 32 + 32); auto.
    change (Z.of_nat 32) with 32.
    replace (32 * Z.of_nat n0 + z mod 32 + 32 - 32) with (32 * Z.of_nat n0 + z mod 32) by omega.
    remember (HMAC_DRBG_generate_helper_Z HMAC key v
               (32 * Z.of_nat n0 + z mod 32)) as result; destruct result.
    simpl.
    apply H.
  }
Qed.

Lemma HMAC_DRBG_generate_helper_Z_isbyteZ_fst:
  forall HMAC key v z,
    0 <= z ->
    Forall isbyteZ v ->
    (forall x y, Forall isbyteZ (HMAC x y)) ->
    Forall isbyteZ (fst (HMAC_DRBG_generate_helper_Z HMAC key v z)).
Proof.
  apply HMAC_DRBG_generate_helper_Z_any_prop_fst.
Qed.

Lemma HMAC_DRBG_generate_helper_Z_isbyteZ_snd:
  forall HMAC key v z,
    0 <= z ->
    (forall x y, Forall isbyteZ (HMAC x y)) ->
    Forall isbyteZ (snd (HMAC_DRBG_generate_helper_Z HMAC key v z)).
Proof.
  intros HMAC key v z Hz H.
  remember (z/32) as n.
  rewrite <- (Z2Nat.id n) in Heqn.
  generalize dependent z.
  induction (Z.to_nat n).
  {
    (* base case *)
    intros.
    rewrite (Z_div_mod_eq z 32); try omega.
    rewrite HMAC_DRBG_generate_helper_Z_equation.
    rewrite <- Heqn.
    change (Z.of_nat 0) with 0.
    change (32 * 0 + z mod 32) with (z mod 32).
    change (Z.of_nat 32) with 32.
    remember (0 >=? z mod 32) as zero_geb_z; destruct zero_geb_z.
    constructor.
    rewrite HMAC_DRBG_generate_helper_Z_equation.
    remember (0 >=? z mod 32 - 32) as zero_geb_z_minus_32; destruct zero_geb_z_minus_32.
    {
      simpl.
      apply H.
    }    
    {
      (* this case is bogus. z mod 32 - 32 cannot be bigger than 0 *)
      admit (* TODO *).
    }
  }
  {
    (* inductive case *)
    intros.
    rewrite (Z_div_mod_eq z 32); try omega.
    rewrite <- Heqn.
    replace (32 * Z.of_nat (S n0) + z mod 32) with (32 * Z.of_nat n0 + z mod 32 + 32).
    rewrite HMAC_DRBG_generate_helper_Z_equation.
    destruct (0 >=? 32 * Z.of_nat n0 + z mod 32 + 32); try constructor.
    change (Z.of_nat 32) with 32.
    replace (32 * Z.of_nat n0 + z mod 32 + 32 - 32) with (32 * Z.of_nat n0 + z mod 32) by omega.
    replace (HMAC_DRBG_generate_helper_Z HMAC key v
               (32 * Z.of_nat n0 + z mod 32)) with (fst (HMAC_DRBG_generate_helper_Z HMAC key v
               (32 * Z.of_nat n0 + z mod 32)), snd (HMAC_DRBG_generate_helper_Z HMAC key v
               (32 * Z.of_nat n0 + z mod 32))).
    change (snd
        (HMAC
           (fst
              (HMAC_DRBG_generate_helper_Z HMAC key v
                 (32 * Z.of_nat n0 + z mod 32))) key,
        snd
          (HMAC_DRBG_generate_helper_Z HMAC key v
             (32 * Z.of_nat n0 + z mod 32)) ++
        HMAC
          (fst
             (HMAC_DRBG_generate_helper_Z HMAC key v
                (32 * Z.of_nat n0 + z mod 32))) key)) with (snd
          (HMAC_DRBG_generate_helper_Z HMAC key v
             (32 * Z.of_nat n0 + z mod 32)) ++
        HMAC
          (fst
             (HMAC_DRBG_generate_helper_Z HMAC key v
                (32 * Z.of_nat n0 + z mod 32))) key).
    apply Forall_app.
    split.
    {
      apply IHn0.
      pose proof (Zle_0_nat n0).
      admit (* TODO *).
      admit (* TODO *).
    }
    {
      apply H.
    }
    destruct (HMAC_DRBG_generate_helper_Z HMAC key v (32 * Z.of_nat n0 + z mod 32)); reflexivity.
    rewrite Nat2Z.inj_succ.
    rewrite <- Zmult_succ_r_reverse.
    omega.
  }
  subst.
  apply (Z_div_pos z 32); omega.
Qed.

Lemma HMAC_DRBG_generate_helper_Z_incremental_fst:
  forall HMAC key v z v',
    0 <= z ->
    v' = fst (HMAC_DRBG_generate_helper_Z HMAC key v z) ->
    HMAC v' key = fst (HMAC_DRBG_generate_helper_Z HMAC key v (z + 32)).
Proof.
  intros HMAC key v z v' Hz H.
  remember (z/32) as n.
  rewrite <- (Z2Nat.id n) in Heqn by (subst; apply (Z_div_pos z 32); omega).
  assert (Hf: 0 >=? z + 32 = false).
  {
    assert (0 < z + 32) by omega.
    rewrite Z.geb_leb.
    apply Z.leb_gt; omega.
  }
  rewrite HMAC_DRBG_generate_helper_Z_equation.
  rewrite Hf.
  remember (HMAC_DRBG_generate_helper_Z HMAC key v (z + 32 - Z.of_nat 32)) as result; destruct result.
  simpl.
  replace l with (fst (HMAC_DRBG_generate_helper_Z HMAC key v (z + 32 - Z.of_nat 32))) by (rewrite <- Heqresult; auto).
  change (Z.of_nat 32) with 32.
  replace (z + 32 - 32) with z by omega.
  rewrite H.
  reflexivity.
Qed.

Lemma HMAC_DRBG_generate_helper_Z_incremental_snd:
  forall HMAC key v z v',
    0 <= z ->
    v' = fst (HMAC_DRBG_generate_helper_Z HMAC key v (z + 32)) ->
    snd (HMAC_DRBG_generate_helper_Z HMAC key v z) ++ v' = snd (HMAC_DRBG_generate_helper_Z HMAC key v (z + 32)).
Proof.
  intros HMAC key v z v' Hz H.
  remember (z/32) as n.
  rewrite <- (Z2Nat.id n) in Heqn by (subst; apply (Z_div_pos z 32); omega).
  assert (Hf: 0 >=? z + 32 = false).
  {
    assert (0 < z + 32) by omega.
    rewrite Z.geb_leb.
    apply Z.leb_gt; omega.
  }
  remember (HMAC_DRBG_generate_helper_Z HMAC key v z) as saved;
    rewrite HMAC_DRBG_generate_helper_Z_equation; subst saved.
  rewrite Hf.
  remember (HMAC_DRBG_generate_helper_Z HMAC key v (z + 32 - Z.of_nat 32)) as result; destruct result.
  simpl.
  replace l with (fst (HMAC_DRBG_generate_helper_Z HMAC key v (z + 32 - Z.of_nat 32))) by (rewrite <- Heqresult; auto).
  replace l0 with (snd (HMAC_DRBG_generate_helper_Z HMAC key v (z + 32 - Z.of_nat 32))) by (rewrite <- Heqresult; auto).
  change (Z.of_nat 32) with 32.
  replace (z + 32 - 32) with z by omega.
  rewrite H.
  erewrite <- HMAC_DRBG_generate_helper_Z_incremental_fst; eauto.
Qed.

Lemma HMAC_DRBG_generate_helper_Z_Zlength_fst:
  forall HMAC key v z,
    0 <= z ->
    Zlength v = 32 ->
    (forall x y, Zlength (HMAC x y) = 32) ->
    Zlength (fst (HMAC_DRBG_generate_helper_Z HMAC key v z)) = 32.
Proof.
  apply HMAC_DRBG_generate_helper_Z_any_prop_fst.
Qed.

Lemma HMAC_DRBG_generate_helper_Z_Zlength_snd:
  forall HMAC key v z,
    0 <= z ->
    (forall x y, Zlength (HMAC x y) = 32) ->
    (exists n, z = n * 32) ->
    Zlength (snd (HMAC_DRBG_generate_helper_Z HMAC key v z)) = z.
Proof.
  intros HMAC key v z Hz Hlength Hn.
  destruct Hn as [n Hn].
  rewrite <- (Z2Nat.id n) in Hn by omega.
  generalize dependent z. revert Hlength.
  induction (Z.to_nat n); intros.
  {
    change (Z.of_nat 0) with 0 in Hn.
    change (0 * 32) with 0 in Hn.
    subst.
    rewrite HMAC_DRBG_generate_helper_Z_equation.
    reflexivity.
  }
  {
    assert (Hn': z = Z.of_nat n0 * 32 + 32).
    {
      rewrite Nat2Z.inj_succ in Hn.
      rewrite <- Zmult_succ_l_reverse in Hn.
      auto.
    }
    clear Hn.
    rename Hn' into Hn.
    subst z.
    rewrite HMAC_DRBG_generate_helper_Z_equation.
    assert (Hf: 0 >=? Z.of_nat n0 * 32 + 32 = false).
    {
      rewrite Z.geb_leb.
      apply Z.leb_gt; omega.
    }
    rewrite Hf.
    remember (HMAC_DRBG_generate_helper_Z HMAC key v
               (Z.of_nat n0 * 32 + 32 - Z.of_nat 32)) as result; destruct result.
    simpl.
    rewrite Zlength_correct.
    rewrite app_length.
    rewrite Nat2Z.inj_add.
    do 2 rewrite <- Zlength_correct.
    rewrite Hlength.
    replace l0 with (snd (HMAC_DRBG_generate_helper_Z HMAC key v
                (Z.of_nat n0 * 32 + 32 - Z.of_nat 32))) by (rewrite <- Heqresult; auto).
    change (Z.of_nat 32) with 32.
    rewrite IHn0; auto; omega.
  }
Qed.

Lemma HMAC_DRBG_generate_helper_Z_incremental_equiv:
  forall HMAC key v z incr,
    0 <= z ->
    0 < incr <= 32 ->
    (exists n, z = n * 32) ->
    HMAC_DRBG_generate_helper_Z HMAC key v (z + incr) = HMAC_DRBG_generate_helper_Z HMAC key v (z + 32).
Proof.
  intros HMAC key v z incr Hz Hlength Hn.
  destruct Hn as [n Hn].
  rewrite <- (Z2Nat.id n) in Hn by omega.
  generalize dependent z.
  induction (Z.to_nat n); intros.
  {
    (* base case *)
    change ((Z.of_nat 0) * 32) with 0 in Hn.
    subst z; simpl.
    rewrite HMAC_DRBG_generate_helper_Z_equation.
    assert (Hf: 0 >=? incr = false).
    {
      rewrite Z.geb_leb.
      apply Z.leb_gt; omega.
    }
    rewrite Hf.
    rewrite HMAC_DRBG_generate_helper_Z_equation.
    assert (Hf2: 0 >=? incr - Z.of_nat 32 = true).
    {
      change (Z.of_nat 32) with 32.
      rewrite Z.geb_leb.
      apply Z.leb_le; omega.
    }
    rewrite Hf2.
    reflexivity.
  }
  {
    (* inductive case *)
    assert (Hn': z = Z.of_nat n0 * 32 + 32).
    {
      rewrite Nat2Z.inj_succ in Hn.
      rewrite <- Zmult_succ_l_reverse in Hn.
      auto.
    }
    clear Hn; rename Hn' into Hn.
    subst z.
    rewrite HMAC_DRBG_generate_helper_Z_equation.
    assert (Hf: 0 >=? Z.of_nat n0 * 32 + 32 + incr = false).
    {
      rewrite Z.geb_leb.
      apply Z.leb_gt; omega.
    }
    rewrite Hf.
    change (Z.of_nat 32) with 32.
    replace (Z.of_nat n0 * 32 + 32 + incr - 32) with (Z.of_nat n0 * 32 + incr) by omega.
    rewrite IHn0; try omega.
    remember (let (v0, rest) :=
        HMAC_DRBG_generate_helper_Z HMAC key v (Z.of_nat n0 * 32 + 32) in
    (HMAC v0 key, rest ++ HMAC v0 key)) as saved; rewrite HMAC_DRBG_generate_helper_Z_equation; subst saved.
    assert (Hf2: 0 >=? Z.of_nat n0 * 32 + 32 + 32 = false).
    {
      rewrite Z.geb_leb.
      apply Z.leb_gt; omega.
    }
    rewrite Hf2.
    change (Z.of_nat 32) with 32.
    replace (Z.of_nat n0 * 32 + 32 + 32 - 32) with (Z.of_nat n0 * 32 + 32) by omega.
    reflexivity.
  }
Qed.

Fixpoint HMAC_DRBG_generate_helper_rounds (HMAC: list Z -> list Z -> list Z) (key v: list Z) (rounds: nat): (list Z * list Z) :=
  match rounds with
    | O => (v, [])
    | S rounds' => 
    let (v, rest) := HMAC_DRBG_generate_helper_rounds HMAC key v rounds' in
    let v := HMAC v key in
    let temp := v in
    (v, rest ++ temp)
  end.

Fixpoint HMAC_v_n_times (HMAC: list Z -> list Z -> list Z) key v n :=
  match n with
    | O => v
    | S n' =>
      let v' := HMAC_v_n_times HMAC key v n' in
      HMAC v' key
  end
.

Lemma HMAC_DRBG_generate_helper_rounds_v:
  forall HMAC key v v' n b,
    (v', b) = HMAC_DRBG_generate_helper_rounds HMAC key v n ->
    v' = HMAC_v_n_times HMAC key v n.
Proof.
  intros until n.
  revert HMAC key v v'.
  induction n as [|n']; intros. simpl in H. inv H. reflexivity.
  simpl in H.
  simpl.
  remember (HMAC_DRBG_generate_helper_rounds HMAC key v n') as x; destruct x.
  apply IHn' in Heqx.
  subst.
  inv H.
  reflexivity.
Qed.

Definition HMAC_DRBG_generate_algorithm (HMAC: list Z -> list Z -> list Z) (reseed_interval: Z) (working_state: DRBG_working_state) (requested_number_of_bytes: Z) (additional_input: list Z): DRBG_generate_algorithm_result :=
  let '(v, key, reseed_counter) := working_state in
  if Z.gtb reseed_counter reseed_interval then generate_algorithm_reseed_required
  else
    let (key, v) := match additional_input with
                      | [] => (key, v)
                      | _::_ => HMAC_DRBG_update HMAC additional_input key v
                    end in
    let (v, temp) := HMAC_DRBG_generate_helper_Z HMAC key v requested_number_of_bytes in
    let returned_bits := firstn (Z.to_nat requested_number_of_bytes) temp in
    let (key, v) := HMAC_DRBG_update HMAC additional_input key v in
    let reseed_counter := reseed_counter + 1 in
    generate_algorithm_success returned_bits (v, key, reseed_counter).      
