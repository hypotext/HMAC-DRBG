Require Import floyd.proofauto.
Import ListNotations.
Local Open Scope logic.
Require Import floyd.sublist.

Require Import sha.HMAC256_functional_prog.
Require Import sha.general_lemmas.
Require Import sha.spec_sha.

Require Import HMAC_DRBG_reseed_algorithm.
Require Import HMAC256_DRBG_functional_prog.
Require Import HMAC_DRBG_generate_algorithm.
Require Import hmac_drbg.
Require Import spec_hmac_drbg.
Require Import DRBG_generate_function.
Require Import DRBG_reseed_function.
Require Import HMAC_DRBG_update.
Require Import entropy.

Lemma data_at_weak_valid_ptr: forall (sh : Share.t) (t : type) (v : reptype t) (p : val),
       sepalg.nonidentity sh ->
       sizeof cenv_cs t >= 0 -> data_at sh t v p |-- weak_valid_pointer p.
Proof.
Admitted.
Lemma sepcon_weak_valid_pointer2
: forall (P Q : mpred) (p : val),
    P |-- weak_valid_pointer p -> Q * P |-- weak_valid_pointer p.
Proof. Admitted.
Lemma sepcon_weak_valid_pointer1
: forall (P Q : mpred) (p : val),
    P |-- weak_valid_pointer p -> P * Q |-- weak_valid_pointer p.
Proof. Admitted.
Hint Resolve sepcon_weak_valid_pointer1 sepcon_weak_valid_pointer2 data_at_weak_valid_ptr: valid_pointer.

Lemma sublist_app_exact1:
  forall X (A B: list X), sublist 0 (Zlength A) (A ++ B) = A.
Proof.
  intros.
  pose proof (Zlength_nonneg A).
  rewrite sublist_app1; try omega.
  rewrite sublist_same; auto.
Qed.

Lemma sublist_app_exact2:
  forall X (A B: list X), sublist (Zlength A) (Zlength A + Zlength B) (A ++ B) = B.
Proof.
  intros.
  pose proof (Zlength_nonneg A).
  pose proof (Zlength_nonneg B).
  rewrite sublist_app2; try omega.
  rewrite sublist_same; auto; omega.
Qed.

Lemma data_at_complete_split:
  forall A B lengthA lengthB AB length p offset sh,
    field_compatible (tarray tuchar (Zlength A + Zlength B)) [] p ->
    lengthA = Zlength A ->
    lengthB = Zlength B ->
    length = lengthA + lengthB ->
    offset = lengthA ->
    AB = A ++ B ->
    (data_at sh (tarray tuchar length) (AB) p) = (data_at sh (tarray tuchar lengthA) A p) * (data_at sh (tarray tuchar lengthB) B (offset_val (Int.repr offset) p)).
Proof.
  intros until sh.
  intros Hfield.
  intros; subst.
  pose proof (Zlength_nonneg A).
  pose proof (Zlength_nonneg B).
  assert (Hisptr: isptr p) by (destruct Hfield; assumption).
  destruct p; try solve [inversion Hisptr]; clear Hisptr.
  unfold tarray.
  rewrite split2_data_at_Tarray_tuchar with (n1:=Zlength A); [|split; omega|rewrite Zlength_app; reflexivity].
  rewrite sublist_app_exact1, sublist_app_exact2.
  replace (Zlength A + Zlength B - Zlength A) with (Zlength B) by omega.
  replace (field_address0 (Tarray tuchar (Zlength A + Zlength B) noattr) [ArraySubsc (Zlength A)] (Vptr b i)) with (Vptr b (Int.add i (Int.repr (Zlength A)))).
  reflexivity.
  rewrite field_address0_offset.
  simpl. replace (0 + 1 * Zlength A) with (Zlength A) by omega. reflexivity.
  destruct Hfield as [Hfield1 [Hfield2 [Hfield3 [Hfield4 [Hfield5 [Hfield6 [Hfield7 Hfield8]]]]]]].
  unfold field_compatible0; repeat split; try assumption; auto; omega.
Qed.

Definition mpred_passed_to_function (F: mpred) (cond: bool): mpred :=
  if cond then emp else F.

Definition mpred_passed_to_frame (F: mpred) (cond: bool): mpred :=
  if cond then F else emp.

Lemma mpred_cond_correct:
  forall F cond, F = mpred_passed_to_function F cond * mpred_passed_to_frame F cond.
Proof.
  intros.
  destruct cond.
  simpl; symmetry; apply emp_sepcon.
  simpl; symmetry; apply sepcon_emp.
Qed.

Lemma metadata_preserved:
  forall key key0 V V0 reseed_counter reseed_counter0 entropy_len entropy_len0 prediction_resistance prediction_resistance0 reseed_interval reseed_interval0 contents done s,
  HMAC256DRBGabs key0 V0 reseed_counter0
                              entropy_len0 prediction_resistance0
                              reseed_interval0 =
                            hmac256drbgabs_hmac_drbg_update
                              (hmac256drbgabs_update_value
                                 (if if (prediction_resistance
                                         || (reseed_counter >?
                                             reseed_interval))%bool
                                     then false
                                     else
                                      match contents with
                                      | [] => false
                                      | _ :: _ => true
                                      end
                                  then
                                   hmac256drbgabs_hmac_drbg_update
                                     (HMAC256DRBGabs key V reseed_counter
                                        entropy_len prediction_resistance
                                        reseed_interval) contents
                                  else
                                   if (prediction_resistance
                                       || (reseed_counter >? reseed_interval))%bool
                                   then
                                    hmac256drbgabs_reseed
                                      (HMAC256DRBGabs key V reseed_counter
                                         entropy_len prediction_resistance
                                         reseed_interval) s contents
                                   else
                                    HMAC256DRBGabs key V reseed_counter
                                      entropy_len prediction_resistance
                                      reseed_interval)
                                 (fst
                                    (HMAC_DRBG_generate_helper_Z HMAC256
                                       (hmac256drbgabs_key
                                          (if if (prediction_resistance
                                                  || 
                                                  (reseed_counter >?
                                                  reseed_interval))%bool
                                              then false
                                              else
                                               match contents with
                                               | [] => false
                                               | _ :: _ => true
                                               end
                                           then
                                            hmac256drbgabs_hmac_drbg_update
                                              (HMAC256DRBGabs key V
                                                 reseed_counter entropy_len
                                                 prediction_resistance
                                                 reseed_interval) contents
                                           else
                                            if (prediction_resistance
                                                || 
                                                (reseed_counter >?
                                                 reseed_interval))%bool
                                            then
                                             hmac256drbgabs_reseed
                                               (HMAC256DRBGabs key V
                                                  reseed_counter entropy_len
                                                  prediction_resistance
                                                  reseed_interval) s contents
                                            else
                                             HMAC256DRBGabs key V
                                               reseed_counter entropy_len
                                               prediction_resistance
                                               reseed_interval))
                                       (hmac256drbgabs_value
                                          (if if (prediction_resistance
                                                  || 
                                                  (reseed_counter >?
                                                  reseed_interval))%bool
                                              then false
                                              else
                                               match contents with
                                               | [] => false
                                               | _ :: _ => true
                                               end
                                           then
                                            hmac256drbgabs_hmac_drbg_update
                                              (HMAC256DRBGabs key V
                                                 reseed_counter entropy_len
                                                 prediction_resistance
                                                 reseed_interval) contents
                                           else
                                            if (prediction_resistance
                                                || 
                                                (reseed_counter >?
                                                 reseed_interval))%bool
                                            then
                                             hmac256drbgabs_reseed
                                               (HMAC256DRBGabs key V
                                                  reseed_counter entropy_len
                                                  prediction_resistance
                                                  reseed_interval) s contents
                                            else
                                             HMAC256DRBGabs key V
                                               reseed_counter entropy_len
                                               prediction_resistance
                                               reseed_interval)) done)))
                              (if (prediction_resistance
                                   || (reseed_counter >? reseed_interval))%bool
                               then []
                               else contents) ->
  entropy_len = entropy_len0 /\ prediction_resistance = prediction_resistance0 /\ reseed_interval = reseed_interval0.
Proof.
  unfold hmac256drbgabs_reseed.
  unfold mbedtls_HMAC256_DRBG_reseed_function.
  unfold mbedtls_HMAC256_DRBG_generate_function.
  unfold HMAC256_DRBG_generate_function, HMAC256_DRBG_reseed_function.
  unfold DRBG_generate_function, DRBG_reseed_function.
  unfold DRBG_generate_function_helper.
  unfold HMAC256_DRBG_generate_algorithm.
  unfold HMAC_DRBG_generate_algorithm.
  unfold hmac256drbgabs_key.
  unfold hmac256drbgabs_value.
  unfold hmac256drbgabs_update_value.
  unfold hmac256drbgabs_hmac_drbg_update.
  unfold hmac256drbgabs_increment_reseed_counter.
  unfold hmac256drbgabs_to_state_handle.
  unfold HMAC256_DRBG_reseed_algorithm.
  unfold HMAC_DRBG_reseed_algorithm.
  unfold HMAC256_DRBG_update.
  unfold HMAC_DRBG_update.
  intros.
  destruct (match contents with
                | [] =>
                    (HMAC256 (V ++ [0] ++ contents) key,
                    HMAC256 V (HMAC256 (V ++ [0] ++ contents) key))
                | _ :: _ =>
                    (HMAC256
                       (HMAC256 V (HMAC256 (V ++ [0] ++ contents) key) ++
                        [1] ++ contents) (HMAC256 (V ++ [0] ++ contents) key),
                    HMAC256 (HMAC256 V (HMAC256 (V ++ [0] ++ contents) key))
                      (HMAC256
                         (HMAC256 V (HMAC256 (V ++ [0] ++ contents) key) ++
                          [1] ++ contents)
                         (HMAC256 (V ++ [0] ++ contents) key)))
                end).
  remember (prediction_resistance || (reseed_counter >? reseed_interval))%bool as should_reseed; clear Heqshould_reseed.
  remember (if should_reseed
                          then false
                          else
                           match contents with
                           | [] => false
                           | _ :: _ => true
                           end) as non_empty_additional; clear Heqnon_empty_additional.
  destruct non_empty_additional.
  {
    destruct should_reseed; destruct contents; inv H; auto.
  }
  destruct should_reseed.
  {
    rewrite andb_negb_r in H.
    destruct (Zlength contents >? 256); [inv H; auto| ].
    destruct (get_entropy 256 entropy_len entropy_len prediction_resistance s); [|inv H; auto].
    destruct (l1 ++ contents); inv H; auto.
  }
  {
    destruct contents; inv H; auto.
  }
Qed.

Lemma reseed_counter_values:
  forall key key0 V V0 reseed_counter reseed_counter0 entropy_len entropy_len0 prediction_resistance prediction_resistance0 reseed_interval reseed_interval0 contents done s,
  HMAC256DRBGabs key0 V0 reseed_counter0
                              entropy_len0 prediction_resistance0
                              reseed_interval0 =
                            hmac256drbgabs_hmac_drbg_update
                              (hmac256drbgabs_update_value
                                 (if if (prediction_resistance
                                         || (reseed_counter >?
                                             reseed_interval))%bool
                                     then false
                                     else
                                      match contents with
                                      | [] => false
                                      | _ :: _ => true
                                      end
                                  then
                                   hmac256drbgabs_hmac_drbg_update
                                     (HMAC256DRBGabs key V reseed_counter
                                        entropy_len prediction_resistance
                                        reseed_interval) contents
                                  else
                                   if (prediction_resistance
                                       || (reseed_counter >? reseed_interval))%bool
                                   then
                                    hmac256drbgabs_reseed
                                      (HMAC256DRBGabs key V reseed_counter
                                         entropy_len prediction_resistance
                                         reseed_interval) s contents
                                   else
                                    HMAC256DRBGabs key V reseed_counter
                                      entropy_len prediction_resistance
                                      reseed_interval)
                                 (fst
                                    (HMAC_DRBG_generate_helper_Z HMAC256
                                       (hmac256drbgabs_key
                                          (if if (prediction_resistance
                                                  || 
                                                  (reseed_counter >?
                                                  reseed_interval))%bool
                                              then false
                                              else
                                               match contents with
                                               | [] => false
                                               | _ :: _ => true
                                               end
                                           then
                                            hmac256drbgabs_hmac_drbg_update
                                              (HMAC256DRBGabs key V
                                                 reseed_counter entropy_len
                                                 prediction_resistance
                                                 reseed_interval) contents
                                           else
                                            if (prediction_resistance
                                                || 
                                                (reseed_counter >?
                                                 reseed_interval))%bool
                                            then
                                             hmac256drbgabs_reseed
                                               (HMAC256DRBGabs key V
                                                  reseed_counter entropy_len
                                                  prediction_resistance
                                                  reseed_interval) s contents
                                            else
                                             HMAC256DRBGabs key V
                                               reseed_counter entropy_len
                                               prediction_resistance
                                               reseed_interval))
                                       (hmac256drbgabs_value
                                          (if if (prediction_resistance
                                                  || 
                                                  (reseed_counter >?
                                                  reseed_interval))%bool
                                              then false
                                              else
                                               match contents with
                                               | [] => false
                                               | _ :: _ => true
                                               end
                                           then
                                            hmac256drbgabs_hmac_drbg_update
                                              (HMAC256DRBGabs key V
                                                 reseed_counter entropy_len
                                                 prediction_resistance
                                                 reseed_interval) contents
                                           else
                                            if (prediction_resistance
                                                || 
                                                (reseed_counter >?
                                                 reseed_interval))%bool
                                            then
                                             hmac256drbgabs_reseed
                                               (HMAC256DRBGabs key V
                                                  reseed_counter entropy_len
                                                  prediction_resistance
                                                  reseed_interval) s contents
                                            else
                                             HMAC256DRBGabs key V
                                               reseed_counter entropy_len
                                               prediction_resistance
                                               reseed_interval)) done)))
                              (if (prediction_resistance
                                   || (reseed_counter >? reseed_interval))%bool
                               then []
                               else contents) ->
  reseed_counter0 = 1 \/ reseed_counter0 = reseed_counter.
Proof.
  unfold hmac256drbgabs_reseed.
  unfold mbedtls_HMAC256_DRBG_reseed_function.
  unfold mbedtls_HMAC256_DRBG_generate_function.
  unfold HMAC256_DRBG_generate_function, HMAC256_DRBG_reseed_function.
  unfold DRBG_generate_function, DRBG_reseed_function.
  unfold DRBG_generate_function_helper.
  unfold HMAC256_DRBG_generate_algorithm.
  unfold HMAC_DRBG_generate_algorithm.
  unfold hmac256drbgabs_key.
  unfold hmac256drbgabs_value.
  unfold hmac256drbgabs_update_value.
  unfold hmac256drbgabs_hmac_drbg_update.
  unfold hmac256drbgabs_increment_reseed_counter.
  unfold hmac256drbgabs_to_state_handle.
  unfold HMAC256_DRBG_reseed_algorithm.
  unfold HMAC_DRBG_reseed_algorithm.
  unfold HMAC256_DRBG_update.
  unfold HMAC_DRBG_update.
  intros.
  destruct (match contents with
                | [] =>
                    (HMAC256 (V ++ [0] ++ contents) key,
                    HMAC256 V (HMAC256 (V ++ [0] ++ contents) key))
                | _ :: _ =>
                    (HMAC256
                       (HMAC256 V (HMAC256 (V ++ [0] ++ contents) key) ++
                        [1] ++ contents) (HMAC256 (V ++ [0] ++ contents) key),
                    HMAC256 (HMAC256 V (HMAC256 (V ++ [0] ++ contents) key))
                      (HMAC256
                         (HMAC256 V (HMAC256 (V ++ [0] ++ contents) key) ++
                          [1] ++ contents)
                         (HMAC256 (V ++ [0] ++ contents) key)))
                end).
  remember (prediction_resistance || (reseed_counter >? reseed_interval))%bool as should_reseed; clear Heqshould_reseed.
  remember (if should_reseed
                          then false
                          else
                           match contents with
                           | [] => false
                           | _ :: _ => true
                           end) as non_empty_additional; clear Heqnon_empty_additional.
  destruct non_empty_additional.
  {
    destruct should_reseed; destruct contents; inv H; auto.
  }
  destruct should_reseed.
  {
    rewrite andb_negb_r in H.
    destruct (Zlength contents >? 256); [inv H; auto| ].
    destruct (get_entropy 256 entropy_len entropy_len prediction_resistance s); [|inv H; auto].
    destruct (l1 ++ contents); inv H; auto.
  }
  {
    destruct contents; inv H; auto.
  }
Qed.

Lemma while_loop_post_incremental_snd:
  forall key0 V0 n out_len,
    0 <= (n * 32)%Z <= out_len ->
    (n * 32)%Z <> out_len ->
 snd (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0 (n * 32)%Z) ++
 fst
   (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
      ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z))) =
 snd
   (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
      ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z))).
Proof.
  intros.
  rewrite Zmin_spec.
  destruct (Z_lt_ge_dec 32 (out_len - (n * 32))) as [Hmin | Hmin].
  {
    rewrite zlt_true by assumption.
    apply HMAC_DRBG_generate_helper_Z_incremental_snd; auto; omega.
  }
  {
    rewrite zlt_false by assumption.
    assert (0 < out_len - (n * 32)%Z <= 32).
    {
      split.
      rewrite <- Z2Nat.id in *; try omega.
      remember (Z.to_nat (out_len - n * 32)) as n'; destruct n'.
      {
        (* contradiction. out_len - n <> 0 *)
        assert (0 = out_len - n * 32).
        {
          symmetry;
          apply Z2Nat_inj_0.
          omega.
          symmetry; assumption.
        }
        assert (out_len = (n * 32)%Z) by omega.
        omega.
      }
      rewrite Nat2Z.inj_succ.
      omega.
      omega.
    }
    assert (exists n', (n * 32)%Z = (n' * 32)%Z).
    {
      exists n; reflexivity.
    }
    rewrite HMAC_DRBG_generate_helper_Z_incremental_equiv; auto; try omega.
    apply HMAC_DRBG_generate_helper_Z_incremental_snd; auto; omega.
  }
Qed.

Lemma while_loop_post_incremental_fst:
  forall key0 V0 n out_len,
    0 <= (n * 32)%Z <= out_len ->
    (n * 32)%Z <> out_len ->
  fst (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
      ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z))) =
 HMAC256 (fst (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0 (n * 32)%Z)) key0.
Proof.
  intros.
  rewrite Zmin_spec.
  destruct (Z_lt_ge_dec 32 (out_len - (n * 32))) as [Hmin | Hmin].
  {
    rewrite zlt_true by assumption.
    symmetry; apply HMAC_DRBG_generate_helper_Z_incremental_fst; auto; omega.
  }
  {
    rewrite zlt_false by assumption.
    assert (0 < out_len - (n * 32)%Z <= 32).
    {
      split.
      rewrite <- Z2Nat.id in *; try omega.
      remember (Z.to_nat (out_len - n * 32)) as n'; destruct n'.
      {
        (* contradiction. out_len - n <> 0 *)
        assert (0 = out_len - n * 32).
        {
          symmetry;
          apply Z2Nat_inj_0.
          omega.
          symmetry; assumption.
        }
        assert (out_len = (n * 32)%Z) by omega.
        omega.
      }
      rewrite Nat2Z.inj_succ.
      omega.
      omega.
    }
    assert (exists n', (n * 32)%Z = (n' * 32)%Z).
    {
      exists n; reflexivity.
    }
    rewrite HMAC_DRBG_generate_helper_Z_incremental_equiv; auto; try omega.
    symmetry; apply HMAC_DRBG_generate_helper_Z_incremental_fst; auto; omega.
  }
Qed.

Lemma while_loop_post_sublist_app:
  forall key0 V0 n out_len,
    0 <= (n * 32)%Z <= out_len ->
    Zlength V0 = 32 ->
  sublist 0 (n * 32)
     (snd (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0 (n * 32))) ++
   sublist 0 (Z.min 32 (out_len - n * 32))
     (fst
        (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
           (n * 32 + Z.min 32 (out_len - n * 32)))) =
   sublist 0 (n * 32 + Z.min 32 (out_len - n * 32))
     (snd (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0 (n * 32)) ++
      fst
        (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
           (n * 32 + Z.min 32 (out_len - n * 32)))).
Proof.
  intros.
  remember (snd (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0 (n * 32))) as A.
  assert (HlengthA: Zlength A = (n * 32)%Z).
  {
    subst.
    apply HMAC_DRBG_generate_helper_Z_Zlength_snd.
    omega.
    apply hmac_common_lemmas.HMAC_Zlength.
    exists n; reflexivity.
  }
  clear HeqA.
  remember (fst
        (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
           (n * 32 + Z.min 32 (out_len - n * 32)))) as B.
  assert (HlengthB: Zlength B = 32).
  {
    subst.
    apply HMAC_DRBG_generate_helper_Z_Zlength_fst.
    rewrite Zmin_spec.
    destruct (Z_lt_ge_dec 32 (out_len - (n * 32))) as [Hmin | Hmin].
    rewrite zlt_true by assumption; omega.
    rewrite zlt_false by assumption; omega.
    assumption.
    apply hmac_common_lemmas.HMAC_Zlength.
  }
  clear HeqB.
  rewrite <- HlengthA in *.
  rewrite <- HlengthB in *.
  clear - H HlengthB.
  rewrite sublist_same; auto.
  SearchAbout sublist.  
  rewrite sublist_app; try now (
    rewrite Zmin_spec;
    destruct (Z_lt_ge_dec (Zlength B) (out_len - (Zlength A))) as [Hmin | Hmin]; [rewrite zlt_true by assumption| rewrite zlt_false by assumption]; omega).
  assert (Hmin0: Z.min 0 (Zlength A) = 0).
  {
    rewrite Zmin_spec.
    rewrite <- (Z2Nat.id (Zlength A)) in *; try apply Zlength_nonneg.
    destruct (Z.to_nat (Zlength A)).
    reflexivity.
    reflexivity.
  }
  rewrite Hmin0.
  assert (HminA: (Z.min (Zlength A + Z.min (Zlength B) (out_len - Zlength A)) (Zlength A)) = Zlength A).
  {
    rewrite Zmin_spec.
    rewrite zlt_false; auto.
    destruct (Z.min_dec (Zlength B) (out_len - Zlength A)) as [Hmin | Hmin]; rewrite Hmin; omega.
  }
  rewrite HminA.
  rewrite sublist_same with (hi:=Zlength A); try omega.
  assert (Hmax0: (Z.max (0 - Zlength A) 0) = 0).
  {
    rewrite Zmax_spec.
    rewrite zlt_false; auto; omega.
  }
  rewrite Hmax0.
  replace (Zlength A + Z.min (Zlength B) (out_len - Zlength A) - Zlength A) with (Z.min (Zlength B) (out_len - Zlength A)) by omega.
  assert (HmaxB: (Z.max (Z.min (Zlength B) (out_len - Zlength A)) 0) = (Z.min (Zlength B) (out_len - Zlength A))).
  {
    rewrite <- (Z2Nat.id (out_len - Zlength A)) in *; try omega.
    destruct (Z.to_nat (out_len - Zlength A)).
    {
      simpl.
      destruct (Z.min_dec (Zlength B) 0) as [Hmin | Hmin]; rewrite Hmin; try rewrite HlengthB; auto.
    }
    rewrite Zmax_spec.
    rewrite zlt_true; auto.
    rewrite Nat2Z.inj_succ.
    destruct (Z.min_dec (Zlength B) (Z.succ (Z.of_nat n))) as [Hmin | Hmin]; rewrite Hmin; omega.
  }
  rewrite HmaxB.
  reflexivity.
Qed.    
    
Lemma generate_correct:
  forall should_reseed non_empty_additional s initial_state_abs out_len contents,
    hmac256drbgabs_reseed_interval initial_state_abs = 10000 ->
    hmac256drbgabs_entropy_len initial_state_abs = 32 ->
    out_len >? 1024 = false ->
    Zlength contents >? 256 = false ->
    (should_reseed = true -> exists entropy_bytes s', get_entropy 256 (hmac256drbgabs_entropy_len initial_state_abs) (hmac256drbgabs_entropy_len initial_state_abs) (hmac256drbgabs_prediction_resistance initial_state_abs) s = ENTROPY.success entropy_bytes s') ->
    should_reseed = (hmac256drbgabs_prediction_resistance initial_state_abs
                       || (hmac256drbgabs_reseed_counter initial_state_abs >? hmac256drbgabs_reseed_interval initial_state_abs))%bool ->
    non_empty_additional = (if should_reseed
                            then false
                            else
                              match contents with
                                | [] => false
                                | _ :: _ => true
                              end) ->
  mbedtls_HMAC256_DRBG_generate_function s initial_state_abs out_len contents
  = ENTROPY.success (
        (sublist 0 out_len
                 (snd
                    (HMAC_DRBG_generate_helper_Z HMAC256
                       (hmac256drbgabs_key
                          (if non_empty_additional
                           then
                            hmac256drbgabs_hmac_drbg_update initial_state_abs
                              contents
                           else
                            if should_reseed
                            then
                             hmac256drbgabs_reseed initial_state_abs s
                               contents
                            else initial_state_abs))
                       (hmac256drbgabs_value
                          (if non_empty_additional
                           then
                            hmac256drbgabs_hmac_drbg_update initial_state_abs
                              contents
                           else
                            if should_reseed
                            then
                             hmac256drbgabs_reseed initial_state_abs s
                               contents
                            else initial_state_abs)) out_len))),
        (hmac256drbgabs_to_state_handle (hmac256drbgabs_increment_reseed_counter (hmac256drbgabs_hmac_drbg_update
           (hmac256drbgabs_update_value
              (if non_empty_additional
               then
                hmac256drbgabs_hmac_drbg_update initial_state_abs contents
               else
                if should_reseed
                then hmac256drbgabs_reseed initial_state_abs s contents
                else initial_state_abs)
              (fst
                 (HMAC_DRBG_generate_helper_Z HMAC256
                    (hmac256drbgabs_key
                       (if non_empty_additional
                        then
                         hmac256drbgabs_hmac_drbg_update initial_state_abs
                           contents
                        else
                         if should_reseed
                         then
                          hmac256drbgabs_reseed initial_state_abs s contents
                         else initial_state_abs))
                    (hmac256drbgabs_value
                       (if non_empty_additional
                        then
                         hmac256drbgabs_hmac_drbg_update initial_state_abs
                           contents
                        else
                         if should_reseed
                         then
                          hmac256drbgabs_reseed initial_state_abs s contents
                         else initial_state_abs)) out_len)))
           (if should_reseed then [] else contents))))
                      ) (if should_reseed
         then
          get_stream_result
            (mbedtls_HMAC256_DRBG_reseed_function s initial_state_abs
               contents)
         else s).
Proof.
  intros until contents.
  intros Hreseed_interval Hentropy_len Hout_lenb HZlength_contentsb Hget_entropy Hshould_reseed Hnon_empty_additional.
  destruct initial_state_abs.
  simpl in *.
  unfold hmac256drbgabs_reseed.
  unfold mbedtls_HMAC256_DRBG_reseed_function.
  unfold mbedtls_HMAC256_DRBG_generate_function.
  unfold HMAC256_DRBG_generate_function, HMAC256_DRBG_reseed_function.
  unfold DRBG_generate_function, DRBG_reseed_function.
  unfold DRBG_generate_function_helper.
  unfold HMAC256_DRBG_generate_algorithm.
  unfold HMAC_DRBG_generate_algorithm.
  unfold hmac256drbgabs_key.
  unfold hmac256drbgabs_value.
  unfold hmac256drbgabs_update_value.
  unfold hmac256drbgabs_hmac_drbg_update.
  unfold HMAC256_DRBG_update.
  unfold HMAC_DRBG_update.
  unfold hmac256drbgabs_increment_reseed_counter.
  unfold hmac256drbgabs_to_state_handle.
  rewrite Hout_lenb.
  change (0 >? 256) with false.
  rewrite HZlength_contentsb.
  rewrite andb_negb_r.

  unfold sublist.
  unfold skipn.
  replace (out_len - 0) with out_len by omega.
 
  destruct prediction_resistance.
  {
    (* pr = true *)
    subst.
    destruct Hget_entropy as [entropy_bytes [s' Hget_entropy]]; auto.
    rewrite Hget_entropy.
    destruct entropy_bytes.
    {
      (* contradiction, can't get 0 bytes back as entropy *)
      assert (contra: Zlength (@nil Z) = 32).
      {
        eapply ENTROPY.get_bytes_Zlength.
        omega.
        unfold get_entropy in Hget_entropy.
        subst.
        symmetry; apply Hget_entropy;auto.
        
      }
      change (Zlength (@nil Z)) with 0 in contra.
      inversion contra.
    }
    simpl.
    remember (HMAC_DRBG_generate_helper_Z HMAC256
              (HMAC256
                 (HMAC256 V
                    (HMAC256 (V ++ 0 :: z :: entropy_bytes ++ contents) key) ++
                  1 :: z :: entropy_bytes ++ contents)
                 (HMAC256 (V ++ 0 :: z :: entropy_bytes ++ contents) key))
              (HMAC256
                 (HMAC256 V
                    (HMAC256 (V ++ 0 :: z :: entropy_bytes ++ contents) key))
                 (HMAC256
                    (HMAC256 V
                       (HMAC256 (V ++ 0 :: z :: entropy_bytes ++ contents)
                          key) ++ 1 :: z :: entropy_bytes ++ contents)
                    (HMAC256 (V ++ 0 :: z :: entropy_bytes ++ contents) key)))
              out_len) as generate_helper_result; destruct generate_helper_result.
    reflexivity.
  }
  (* pr = false *)
  subst reseed_interval.
  unfold HMAC_DRBG_update.
  rewrite HZlength_contentsb.
  
  destruct (reseed_counter >? 10000).
  {
    (* must reseed *)
    subst.
    destruct Hget_entropy as [entropy_bytes [s' Hget_entropy]]; auto.
    rewrite Hget_entropy.
    destruct entropy_bytes.
    {
      (* contradiction, can't get 0 bytes back as entropy *)
      assert (contra: Zlength (@nil Z) = 32).
      {
        eapply ENTROPY.get_bytes_Zlength.
        omega.
        unfold get_entropy in Hget_entropy.
        subst.
        symmetry; apply Hget_entropy; auto.
        
      }
      change (Zlength (@nil Z)) with 0 in contra.
      inversion contra.
    }
    simpl.
    remember (HMAC_DRBG_generate_helper_Z HMAC256
              (HMAC256
                 (HMAC256 V
                    (HMAC256 (V ++ 0 :: z :: entropy_bytes ++ contents) key) ++
                  1 :: z :: entropy_bytes ++ contents)
                 (HMAC256 (V ++ 0 :: z :: entropy_bytes ++ contents) key))
              (HMAC256
                 (HMAC256 V
                    (HMAC256 (V ++ 0 :: z :: entropy_bytes ++ contents) key))
                 (HMAC256
                    (HMAC256 V
                       (HMAC256 (V ++ 0 :: z :: entropy_bytes ++ contents)
                          key) ++ 1 :: z :: entropy_bytes ++ contents)
                    (HMAC256 (V ++ 0 :: z :: entropy_bytes ++ contents) key)))
              out_len) as generate_helper_result; destruct generate_helper_result.
    reflexivity.
  }
  simpl in Hshould_reseed; subst should_reseed.
  destruct contents.
  {
    (* contents empty *)
    subst.
    simpl.
    remember (HMAC_DRBG_generate_helper_Z HMAC256 key V out_len) as generate_helper_result; destruct generate_helper_result.
    reflexivity.
  }
  (* contents not empty *)
  subst.
  destruct (HMAC_DRBG_generate_helper_Z HMAC256
                (HMAC256
                   (HMAC256 V (HMAC256 (V ++ [0] ++ z :: contents) key) ++
                    [1] ++ z :: contents)
                   (HMAC256 (V ++ [0] ++ z :: contents) key))
                (HMAC256
                   (HMAC256 V (HMAC256 (V ++ [0] ++ z :: contents) key))
                   (HMAC256
                      (HMAC256 V (HMAC256 (V ++ [0] ++ z :: contents) key) ++
                       [1] ++ z :: contents)
                      (HMAC256 (V ++ [0] ++ z :: contents) key))) out_len).
  reflexivity.
Qed.

Lemma body_hmac_drbg_reseed: semax_body HmacDrbgVarSpecs HmacDrbgFunSpecs 
       f_mbedtls_hmac_drbg_random_with_add hmac_drbg_generate_spec.
Proof.
  start_function.

  name p_rng' _p_rng.
  name ctx' _ctx.
  name add_len' _len.
  name additional' _additional.
  name out_len' _out_len.
  name output' _output.
  name out' _out.

  rename H5 into Hreseed_counter_in_range.
  
  destruct initial_state_abs.
  destruct initial_state as [md_ctx' [V' [reseed_counter' [entropy_len' [prediction_resistance' reseed_interval']]]]].
  unfold hmac256drbg_relate.
  normalize.
  unfold hmac256drbgstate_md_info_pointer.
  simpl.

  remember (HMAC256DRBGabs key V reseed_counter entropy_len prediction_resistance reseed_interval) as initial_state_abs.
  remember (md_ctx',
        (map Vint (map Int.repr V),
        (Vint (Int.repr reseed_counter),
        (Vint (Int.repr entropy_len),
        (Val.of_bool prediction_resistance, Vint (Int.repr reseed_interval)))))) as initial_state.

  (* mbedtls_hmac_drbg_context *ctx = p_rng; *)
  forward.
  assert_PROP (ctx = (force_val (sem_cast_neutral ctx))) as Hctx by entailer!.
  rewrite <- Hctx; clear Hctx.

  (* int left = out_len *)
  forward.

  (* out = output *)
  forward.

  (* prediction_resistance = ctx->prediction_resistance *)
  forward.
  {
    (* typechecking *)
    entailer!.
    destruct prediction_resistance; constructor.
  }

  (* reseed_counter = ctx->reseed_counter *)
  forward.

  (* reseed_interval = ctx->reseed_interval *)
  forward.

  (* info = ctx->md_ctx.md_info; *)
  forward.

  (* md_len = mbedtls_md_get_size(info); *)
  forward_call tt.
  Intros md_len.
  rewrite Heqinitial_state. rewrite <- Heqinitial_state.

  (* if( out_len > MBEDTLS_HMAC_DRBG_MAX_REQUEST ) *)
  forward_if (
      PROP  (out_len <= 1024)
      LOCAL  (temp _md_len md_len; temp _info (let (x, _) := md_ctx' in x);
      temp _reseed_interval (Vint (Int.repr reseed_interval));
      temp _reseed_counter (Vint (Int.repr reseed_counter));
      temp _prediction_resistance (Val.of_bool prediction_resistance);
      temp _out output; temp _left (Vint (Int.repr out_len)); 
      temp _ctx ctx; temp _p_rng ctx; temp _output output;
      temp _out_len (Vint (Int.repr out_len)); temp _additional additional;
      temp _add_len (Vint (Int.repr add_len)); gvar sha._K256 kv)
      SEP  (data_at_ Tsh (tarray tuchar out_len) output;
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional;
      data_at Tsh t_struct_hmac256drbg_context_st initial_state ctx;
      md_full key md_ctx';
      data_at Tsh t_struct_mbedtls_md_info info_contents (fst md_ctx');
      Stream s; K_vector kv)
    ).
  {
    (* return( MBEDTLS_ERR_HMAC_DRBG_REQUEST_TOO_BIG ); *)
    forward.

    (* prove post condition of the function *)
    unfold hmac_drbg_update_post, get_stream_result, hmac256drbg_relate.
    unfold hmac256drbgabs_generate, hmac256drbgabs_to_state.
    Exists (Vint (Int.repr (-3))).
    unfold mbedtls_HMAC256_DRBG_generate_function.
    unfold HMAC256_DRBG_generate_function.
    unfold DRBG_generate_function.
    rewrite andb_negb_r.
    assert (Hout_len: out_len >? 1024 = true).
    {
      rewrite Z.gtb_ltb.
      apply Z.ltb_lt.
      assumption.
    }
    rewrite Hout_len.
    entailer!.
  }
  {
    forward.
    entailer!.
  }

  normalize.
  assert (Hout_len: 0 <= out_len <= 1024) by omega.
  assert (Hout_lenb: out_len >? 1024 = false).
  {
    rewrite Z.gtb_ltb.
    apply Z.ltb_nlt.
    omega.
  }

  (* if( add_len > MBEDTLS_HMAC_DRBG_MAX_INPUT ) *)
  forward_if (
      PROP  (add_len <= 256) (* ADDED *)
      LOCAL  (temp _md_len md_len; temp _info (let (x, _) := md_ctx' in x);
      temp _reseed_interval (Vint (Int.repr reseed_interval));
      temp _reseed_counter (Vint (Int.repr reseed_counter));
      temp _prediction_resistance (Val.of_bool prediction_resistance);
      temp _out output; temp _left (Vint (Int.repr out_len)); 
      temp _ctx ctx; temp _p_rng ctx; temp _output output;
      temp _out_len (Vint (Int.repr out_len)); temp _additional additional;
      temp _add_len (Vint (Int.repr add_len)); gvar sha._K256 kv)
      SEP  (data_at_ Tsh (tarray tuchar out_len) output;
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional;
      data_at Tsh t_struct_hmac256drbg_context_st initial_state ctx;
      md_full key md_ctx';
      data_at Tsh t_struct_mbedtls_md_info info_contents (fst md_ctx');
      Stream s; K_vector kv)
  ).
  {
    (* return( MBEDTLS_ERR_HMAC_DRBG_INPUT_TOO_BIG ); *)
    forward.

    (* prove function post condition *)
    unfold hmac_drbg_update_post, get_stream_result, hmac256drbg_relate.
    unfold hmac256drbgabs_generate, hmac256drbgabs_to_state.
    Exists (Vint (Int.repr (-5))).
    unfold mbedtls_HMAC256_DRBG_generate_function.
    unfold HMAC256_DRBG_generate_function.
    unfold DRBG_generate_function.
    rewrite andb_negb_r.
    rewrite Hout_lenb.
    assert (Hlength_contents: Zlength contents >? 256 = true).
    {
      rewrite Z.gtb_ltb.
      apply Z.ltb_lt.
      assumption.
    }
    replace (Z.of_nat (length contents)) with (Zlength contents) by (rewrite Zlength_correct; auto).
    rewrite Hlength_contents.
    change (0 >? 256) with false.
    entailer!.
  }
  {
    forward.
    entailer!.
  }
  normalize.
  assert (Hadd_len: 0 <= add_len <= 256) by omega.
  assert (Hadd_lenb: add_len >? 256 = false).
  {
    rewrite Z.gtb_ltb.
    apply Z.ltb_nlt.
    omega.
  }
  remember (orb prediction_resistance (reseed_counter >? reseed_interval)) as should_reseed.
  forward_if (
      PROP  ()
      LOCAL  (temp _md_len md_len; temp _info (let (x, _) := md_ctx' in x);
      temp _reseed_interval (Vint (Int.repr reseed_interval));
      temp _reseed_counter (Vint (Int.repr reseed_counter));
      temp _prediction_resistance (Val.of_bool prediction_resistance);
      temp _out output; temp _left (Vint (Int.repr out_len)); 
      temp _ctx ctx; temp _p_rng ctx; temp _output output;
      temp _out_len (Vint (Int.repr out_len)); temp _additional additional;
      temp _add_len (Vint (Int.repr add_len)); gvar sha._K256 kv;
      temp 159%positive (Val.of_bool should_reseed) (* ADDED *)
      )
      SEP  (data_at_ Tsh (tarray tuchar out_len) output;
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional;
      data_at Tsh t_struct_hmac256drbg_context_st initial_state ctx;
      md_full key md_ctx';
      data_at Tsh t_struct_mbedtls_md_info info_contents (fst md_ctx');
      Stream s; K_vector kv)
    ).
  {
    forward.
    entailer!.
    
    rename H15 into Hpr.
    destruct prediction_resistance.
    (* true *) reflexivity.
    (* false *)
    inversion Hpr.
  }
  {
    rename H15 into Hpr.
    destruct prediction_resistance; try solve [inversion Hpr].
    simpl in Heqshould_reseed.
    forward.
    subst should_reseed.
    entailer!.
    rewrite <- H16.
    
    rewrite Z.gtb_ltb.
    unfold Int.lt.
      unfold hmac256drbgabs_reseed_counter in Hreseed_counter_in_range.
    destruct (zlt reseed_interval reseed_counter) as [Hlt | Hlt].
    {
      (* reseed_interval < reseed_counter *)
      assert (Hltb: reseed_interval <? reseed_counter = true) by (rewrite Z.ltb_lt; assumption).
      rewrite Hltb.
      rewrite zlt_true; [reflexivity | ].
      unfold hmac256drbgabs_reseed_interval in H4; rewrite H4.
      change (Int.signed (Int.repr 10000)) with 10000.
      rewrite Int.signed_repr; change Int.min_signed with (-2147483648); change Int.max_signed with (2147483647) in *; try omega.
    }
    {
      assert (Hltb: reseed_interval <? reseed_counter = false) by (rewrite Z.ltb_nlt; assumption).
      rewrite Hltb.
      rewrite zlt_false; [reflexivity | ].
      unfold hmac256drbgabs_reseed_interval in H4; rewrite H4.
      change (Int.signed (Int.repr 10000)) with 10000.
      rewrite Int.signed_repr; change Int.min_signed with (-2147483648); change Int.max_signed with (2147483647) in *; try omega.
    }
  }

  remember (if should_reseed then hmac256drbgabs_reseed initial_state_abs s contents else initial_state_abs) as after_reseed_state_abs.
  remember (if should_reseed then get_stream_result (mbedtls_HMAC256_DRBG_reseed_function s initial_state_abs contents) else s) as after_reseed_s.
  remember (if should_reseed then 0 else add_len) as after_reseed_add_len.
  forward_if (
      PROP (
          should_reseed = true -> exists entropy_bytes s', get_entropy 256 entropy_len entropy_len prediction_resistance s = ENTROPY.success entropy_bytes s'
      )
      LOCAL  (temp _md_len md_len; temp _info (let (x, _) := md_ctx' in x);
      temp _reseed_interval (Vint (Int.repr reseed_interval));
      temp _reseed_counter (Vint (Int.repr reseed_counter));
      temp _prediction_resistance (Val.of_bool prediction_resistance);
      temp _out output; temp _left (Vint (Int.repr out_len)); 
      temp _ctx ctx; temp _p_rng ctx; temp _output output;
      temp _out_len (Vint (Int.repr out_len)); temp _additional additional;
      temp _add_len (Vint (Int.repr (if should_reseed then 0 else add_len))); gvar sha._K256 kv; (* ADDED *)
      temp 159%positive (Val.of_bool should_reseed))
      SEP  (
        Stream after_reseed_s;
        hmac_drbg_update_post after_reseed_state_abs initial_state ctx info_contents
        ; (* ADDED *)
        data_at_ Tsh (tarray tuchar out_len) output;
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional;
      spec_sha.K_vector kv)).
  {
    rename H15 into Hshould_reseed.
    (* ret = mbedtls_hmac_drbg_reseed( ctx, additional, add_len ) *)
    forward_call (contents, additional, add_len, ctx, initial_state, initial_state_abs,
                  kv, info_contents, s).
    {
      unfold hmac256drbg_relate.
      rewrite Heqinitial_state, Heqinitial_state_abs.
      entailer!.
    }
    {
      (* prove the PROP clauses *)
      change (Int.max_unsigned) with 4294967295.
      repeat split; auto; omega.
    }
    Intros return_value.
      
    forward.

    forward_if (PROP  (return_value = Vzero) (* ADDED *)
      LOCAL  (temp _ret return_value; temp 158%positive return_value;
      temp _md_len md_len; temp _info (let (x, _) := md_ctx' in x);
      temp _reseed_interval (Vint (Int.repr reseed_interval));
      temp _reseed_counter (Vint (Int.repr reseed_counter));
      temp _prediction_resistance (Val.of_bool prediction_resistance);
      temp _out output; temp _left (Vint (Int.repr out_len)); 
      temp _ctx ctx; temp _p_rng ctx; temp _output output;
      temp _out_len (Vint (Int.repr out_len)); temp _additional additional;
      temp _add_len (Vint (Int.repr add_len)); gvar sha._K256 kv;
      temp 159%positive (Val.of_bool should_reseed))
      SEP 
      (hmac_drbg_update_post
         (hmac256drbgabs_reseed initial_state_abs s contents) initial_state
         ctx info_contents;
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional;
      Stream
        (get_stream_result
           (mbedtls_HMAC256_DRBG_reseed_function s initial_state_abs contents));
      K_vector kv; data_at_ Tsh (tarray tuchar out_len) output)).
    {
      (* return_value != 0 *)
      forward.

      rename H15 into Hreturn_value; simpl in Hreturn_value.
      assert (Hret_not_0: _id0 <> Int.zero).
      {
        clear - H18.
        intros contra. subst.
        inversion H18.
      }

      unfold hmac_drbg_update_post, get_stream_result, hmac256drbg_relate.
      unfold hmac256drbgabs_generate, hmac256drbgabs_reseed, hmac256drbgabs_to_state.
      Exists (Vint _id0).
      apply orb_true_iff in Hshould_reseed.
      replace (Z.of_nat (length contents)) with (Zlength contents) by (rewrite Zlength_correct; auto).
      unfold mbedtls_HMAC256_DRBG_generate_function.
      unfold HMAC256_DRBG_generate_function.
      unfold DRBG_generate_function.
      unfold DRBG_generate_function_helper.
      unfold mbedtls_HMAC256_DRBG_reseed_function.
      unfold HMAC256_DRBG_reseed_function.
      unfold DRBG_reseed_function.DRBG_reseed_function.
      replace (Z.of_nat (length contents)) with (Zlength contents) by (rewrite Zlength_correct; reflexivity).
      rewrite Hout_lenb in *. rewrite Hadd_lenb in *.
      rewrite andb_negb_r in *.
      change (0 >? 256) with false.
      
      remember (get_entropy 256 entropy_len entropy_len prediction_resistance s) as get_entropy_result; destruct get_entropy_result.
      {
        (* entropy succeeded - not possible *)
        clear - Hreturn_value Hret_not_0.
        unfold return_value_relate_result in Hreturn_value.
        idtac.
        inv Hreturn_value.
        assert (contra: False) by (apply Hret_not_0; auto); inversion contra.
      }
      (* entropy failed *)
      destruct Hshould_reseed as [Hpr | Hcount].
      {
        (* prediction_resistance = true *)
        subst.
        entailer!.
      }
      {
        (* reseed_counter > reseed_interval *)
        destruct prediction_resistance; [entailer!|].
        unfold HMAC256_DRBG_generate_algorithm.
        unfold HMAC_DRBG_generate_algorithm.
        rename H4 into Hreseed_interval.
        simpl in Hreseed_interval.
        subst reseed_interval.
        rewrite Hcount.
        rewrite Hadd_lenb.
        rewrite andb_negb_r.
        rewrite <- Heqget_entropy_result.
        entailer!.
      }
    }
    {
      (* return value = 0 *)
      forward.

      assert (Hret_eq_0: return_value = Vzero).
      {
        clear - H18.
        destruct return_value; inv H18.
        remember (Int.eq i (Int.repr 0)) as i_0; destruct i_0; inv H0.
        apply binop_lemmas2.int_eq_true in Heqi_0.
        rewrite Heqi_0; reflexivity.
      }
      subst return_value.
      unfold hmac_drbg_update_post.
      entailer!.
    }

    (* add_len = 0; *)
    forward.

    (* prove post condition of if statement *)
    rename H15 into Hreturn_value.
    subst return_value.
    subst after_reseed_state_abs after_reseed_add_len.
    rewrite Hshould_reseed.
    unfold result_success.
    unfold return_value_relate_result in Hreturn_value.
    entailer!.
    {
      clear - Hreturn_value.
      unfold mbedtls_HMAC256_DRBG_reseed_function in Hreturn_value.
      unfold HMAC256_DRBG_reseed_function in Hreturn_value; unfold DRBG_reseed_function in Hreturn_value.
      rewrite andb_negb_r in Hreturn_value.
      destruct (Zlength contents >? 256); try solve [inversion Hreturn_value]; try solve [assert (contra: False) by (apply Hreturn_value; reflexivity); inversion contra].
      destruct (get_entropy 256 entropy_len entropy_len prediction_resistance s); try solve [inversion Hreturn_value]; try solve [assert (contra: False) by (apply Hreturn_value; reflexivity); inversion contra].
      exists l; exists s0; reflexivity.
    }
    rewrite Hshould_reseed.
    apply derives_refl.
  }
  {
    forward.

    subst after_reseed_state_abs after_reseed_add_len.
    rewrite H15.
    unfold hmac_drbg_update_post, hmac256drbgabs_to_state.
    subst initial_state_abs initial_state.
    entailer!.
    rewrite H15; apply derives_refl.
  }

  normalize.
  rename H15 into Hshould_reseed_get_entropy.
  remember (if eq_dec additional nullval then false else if eq_dec after_reseed_add_len 0 then false else true) as non_empty_additional.

  assert_PROP (non_empty_additional = if should_reseed then false else
                                        match contents with
                                          | [] => false
                                          | _ => true end) as Hnon_empty_additional_contents.
  {
    clear Heqshould_reseed.
    entailer!.
    destruct (eq_dec additional' nullval) as [Hadd' | Hadd'].
    {
      (* additional = null *)
      subst additional'. destruct H20 as [Hisptr Hfield_compat]; inversion Hisptr.
    }
    {
      destruct should_reseed; try reflexivity.
      destruct contents.
      {
        (* contents is empty *)
        reflexivity.
      }
      {
        (* contents is not empty *)
        SearchAbout Zlength Z.succ.
        rewrite Zlength_cons.
        destruct (eq_dec (Z.succ (Zlength contents)) 0); try reflexivity.
        pose proof (Zlength_nonneg contents); omega.
      }
    }
  }
  assert (Hshould_reseed_non_empty_additional: should_reseed = true -> non_empty_additional = false).
  {
    intros Hshould_reseed_true; rewrite Hshould_reseed_true in *.
    subst after_reseed_add_len non_empty_additional.
    destruct (eq_dec additional nullval); reflexivity.
  }

  assert (Hnon_empty_additional_should_reseed: non_empty_additional = true -> should_reseed = false).
  {
    intros Hnon_empty_additional_true; rewrite Hnon_empty_additional_true in *.
    clear Hnon_empty_additional_contents.
    subst after_reseed_add_len non_empty_additional.
    destruct (eq_dec additional nullval); destruct should_reseed; try solve [inversion Heqnon_empty_additional].
    reflexivity.
  }

  (* additional != NULL && add_len != 0 *)
  forward_if (PROP  ()
      LOCAL  (temp _md_len md_len; temp _info (let (x, _) := md_ctx' in x);
      temp _reseed_interval (Vint (Int.repr reseed_interval));
      temp _reseed_counter (Vint (Int.repr reseed_counter));
      temp _prediction_resistance (Val.of_bool prediction_resistance);
      temp _out output; temp _left (Vint (Int.repr out_len)); 
      temp _ctx ctx; temp _p_rng ctx; temp _output output;
      temp _out_len (Vint (Int.repr out_len)); temp _additional additional;
      temp _add_len (Vint (Int.repr after_reseed_add_len)); 
      gvar sha._K256 kv;
      temp 160%positive (Val.of_bool non_empty_additional) (* ADDED *)
             )
      SEP 
      (Stream after_reseed_s;
      hmac_drbg_update_post after_reseed_state_abs initial_state ctx
        info_contents; data_at_ Tsh (tarray tuchar out_len) output;
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional; K_vector kv)).
  {
    (* prove that additional is comparable with the null pointer *)
    
    unfold denote_tc_comparable.
    assert (Hsize_of: sizeof cenv_cs (tarray tuchar (Zlength contents)) >= 0).
    {
      pose proof (Zlength_nonneg contents).
      simpl.
      rewrite Z.mul_1_l.
      rewrite Zmax0r by omega.
      omega.
    }
    assert_PROP (isptr additional) as Hisptr by entailer!. destruct additional; try solve [inversion Hisptr]; clear Hisptr.
    entailer!.
    auto 50 with valid_pointer.
  }
  {
    clear Hnon_empty_additional_contents.
    (* additional <> null *)
    forward.
    entailer!.

    rewrite <- H17.
    destruct (eq_dec additional' nullval); try contradiction.
    destruct (prediction_resistance || (reseed_counter >? reseed_interval))%bool.
    auto.
    destruct (eq_dec (Zlength contents) 0).
    rewrite e.
    reflexivity.
    unfold Int.eq, zeq.
    destruct (Z.eq_dec (Int.unsigned (Int.repr (Zlength contents)))
                    (Int.unsigned (Int.repr 0))).
    {
      rewrite Int.unsigned_repr in e; try omega.
      change (Int.unsigned (Int.repr 0)) with 0 in e.
      omega.
    }
    {
      reflexivity.
    }
  }
  {
    clear Hnon_empty_additional_contents.
    (* additional = null *)
    forward.

    entailer!.
    clear.
    destruct (eq_dec nullval nullval).
    reflexivity.
    assert (contra: False) by (apply n; reflexivity); inversion contra.
  }

  remember (if non_empty_additional then hmac256drbgabs_hmac_drbg_update initial_state_abs contents else after_reseed_state_abs) as after_update_state_abs.
  forward_if (PROP  ()
      LOCAL  (temp _md_len md_len; temp _info (let (x, _) := md_ctx' in x);
      temp _reseed_interval (Vint (Int.repr reseed_interval));
      temp _reseed_counter (Vint (Int.repr reseed_counter));
      temp _prediction_resistance (Val.of_bool prediction_resistance);
      temp _out output; temp _left (Vint (Int.repr out_len)); 
      temp _ctx ctx; temp _p_rng ctx; temp _output output;
      temp _out_len (Vint (Int.repr out_len)); temp _additional additional;
      temp _add_len (Vint (Int.repr after_reseed_add_len)); 
      gvar sha._K256 kv;
      temp 160%positive (Val.of_bool non_empty_additional))
      SEP  (
        Stream after_reseed_s;
        hmac_drbg_update_post after_update_state_abs initial_state ctx info_contents; (* ADDED *)
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional; data_at_ Tsh (tarray tuchar out_len) output; K_vector kv)).
  {
    (* mbedtls_hmac_drbg_update( ctx, additional, add_len ); *)
    assert (Hshould_reseed_false: should_reseed = false).
    {
      apply Hnon_empty_additional_should_reseed; assumption.
    }
    rewrite Hshould_reseed_false in *.
    unfold hmac_drbg_update_post.
    rewrite Heqafter_reseed_add_len, Heqafter_reseed_state_abs.
    forward_call (contents, additional, add_len, ctx,
                 initial_state,
                 initial_state_abs, kv, info_contents
                 ).
    {
      unfold hmac256drbgabs_to_state.
      rewrite Heqinitial_state_abs.
      rewrite Heqinitial_state.
      cancel.
    }
    subst after_update_state_abs after_reseed_state_abs after_reseed_add_len.
    subst initial_state_abs.
    rewrite H15.
    entailer!.
  }
  {
    forward.
    subst after_update_state_abs after_reseed_state_abs after_reseed_add_len.
    subst initial_state_abs.
    rewrite H15.
    entailer!.
  }

  remember (hmac256drbgabs_key after_update_state_abs) as after_update_key.
  remember (hmac256drbgabs_value after_update_state_abs) as after_update_value.
  assert (HZlength_after_update_value: Zlength after_update_value = Z.of_nat SHA256.DigestLength).
  {
    subst after_update_value after_update_state_abs.
    destruct non_empty_additional.
    {
      apply hmac256drbgabs_hmac_drbg_update_Zlength_V.
    }
    subst after_reseed_state_abs.
    destruct should_reseed;[
      apply hmac256drbgabs_reseed_Zlength_V|]; apply H1.
  }
  assert (HisbyteZ_after_update_value: Forall isbyteZ after_update_value).
  {
    subst after_update_value after_update_state_abs.
    destruct non_empty_additional.
    {
      apply hmac256drbgabs_hmac_drbg_update_isbyteZ_V.
    }
    subst after_reseed_state_abs.
    destruct should_reseed;[
      apply hmac256drbgabs_reseed_isbyteZ_V|]; auto.
  }

  (*
  assert_PROP (isptr output) as Hisptr_output by entailer!.
  destruct output; try solve [inversion Hisptr_output].
  rename i into output_i.
  rename b into output_b.
*)
  Definition is_multiple (multiple base: Z) : Prop := exists i, multiple = (i * base)%Z.
  forward_while (
    EX done: Z,
      PROP  (0 <= done <= out_len; (is_multiple done 32) \/ done = out_len)
      LOCAL  (temp _md_len md_len; temp _info (let (x, _) := md_ctx' in x);
      temp _reseed_interval (Vint (Int.repr reseed_interval));
      temp _reseed_counter (Vint (Int.repr reseed_counter));
      temp _prediction_resistance (Val.of_bool prediction_resistance);
      temp _out (offset_val (Int.repr done) output); temp _left (Vint (Int.repr (out_len - done))); 
      temp _ctx ctx; temp _p_rng ctx; temp _output output;
      temp _out_len (Vint (Int.repr out_len)); temp _additional additional;
      temp _add_len (Vint (Int.repr after_reseed_add_len));
      gvar sha._K256 kv
      )
      SEP  (Stream after_reseed_s;
      hmac_drbg_update_post (hmac256drbgabs_update_value after_update_state_abs (fst (HMAC_DRBG_generate_helper_Z HMAC256 after_update_key after_update_value done))) initial_state ctx
        info_contents;
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional; data_at Tsh (tarray tuchar out_len) ((map Vint (map Int.repr (sublist 0 done (snd (HMAC_DRBG_generate_helper_Z HMAC256 after_update_key after_update_value done))))) ++ list_repeat (Z.to_nat (out_len - done)) Vundef) output; 
      K_vector kv)
  ).
  {
    (* prove the current pre condition implies the loop condition *)
    Exists 0.
    change (sublist 0 0
                  (snd
                     (HMAC_DRBG_generate_helper_Z HMAC256 after_update_key
                        after_update_value 0))) with (@nil Z).
    replace (out_len - 0) with out_len by omega.
    change ((map Vint (map Int.repr []) ++
          list_repeat (Z.to_nat out_len) Vundef)) with (list_repeat (Z.to_nat out_len) Vundef).
    assert (Hafter_update: (hmac256drbgabs_update_value after_update_state_abs
            (fst
               (HMAC_DRBG_generate_helper_Z HMAC256 after_update_key
                  after_update_value 0))) = after_update_state_abs).
    {
      simpl.
      subst after_update_value; destruct after_update_state_abs; reflexivity.
    }
    rewrite Hafter_update.
    entailer!.
    left; exists 0.
    omega.
  }
  {
    (* prove the type checking of the loop condition *)
    entailer!.
  }
  {
    clear Heqafter_update_state_abs Heqafter_reseed_s.
    (* prove the loop body preserves the invariant *)
    idtac.
    destruct H16 as [Hmultiple | Hcontra]; [| subst done; omega].
    destruct Hmultiple as [n Hmultiple].
    unfold hmac_drbg_update_post.
    normalize.
    assert (Hfield_md_ctx: forall ctx', isptr ctx' -> field_compatible t_struct_hmac256drbg_context_st [StructField _md_ctx] ctx' -> ctx' = field_address t_struct_hmac256drbg_context_st [StructField _md_ctx] ctx').
    {
      intros ctx'' Hisptr Hfc.
      unfold field_address.
      destruct (field_compatible_dec t_struct_hmac256drbg_context_st); [|contradiction].
      simpl. change (Int.repr 0) with Int.zero. rewrite offset_val_force_ptr.
      destruct ctx''; inversion Hisptr. reflexivity.
    }
    assert (Hfield_V: forall ctx', isptr ctx' -> field_compatible t_struct_hmac256drbg_context_st [StructField _V] ctx' -> offset_val (Int.repr 12) ctx' = field_address t_struct_hmac256drbg_context_st [StructField _V] ctx').
    {
      intros ctx'' Hisptr Hfc.
      unfold field_address.
      destruct (field_compatible_dec t_struct_hmac256drbg_context_st); [reflexivity|contradiction].
    }
    assert_PROP (isptr ctx) as Hisptr_ctx by entailer!.
    unfold_data_at 1%nat.
    
    freeze [2;3;4;5] FR_unused_struct_fields.
    freeze [0;3;5;6] FR1.

    rewrite (field_at_data_at _ _ [StructField _md_ctx]);
    rewrite (field_at_data_at _ _ [StructField _V]);
    simpl.

    unfold hmac256drbg_relate.
    destruct after_update_state_abs.
    unfold hmac256drbgabs_update_value.
    rewrite Heqinitial_state.
    unfold hmac256drbgabs_to_state.
    rewrite Heqafter_update_key.
    unfold md_full.
    normalize.
    (* size_t use_len = left > md_len ? md_len : left; *)
    forward_if (
      PROP  ()
      LOCAL  (temp _md_len md_len; temp _info (let (x, _) := md_ctx' in x);
      temp _reseed_interval (Vint (Int.repr reseed_interval));
      temp _reseed_counter (Vint (Int.repr reseed_counter));
      temp _prediction_resistance (Val.of_bool prediction_resistance);
      temp _out (offset_val (Int.repr done) output); temp _left (Vint (Int.repr (out_len - done)));
      temp _ctx ctx; temp _p_rng ctx; temp _output output;
      temp _out_len (Vint (Int.repr out_len)); temp _additional additional;
      temp _add_len (Vint (Int.repr after_reseed_add_len));
      temp 161%positive (Vint (Int.repr (Z.min (Z.of_nat SHA256.DigestLength) (out_len - done))));
      gvar sha._K256 kv)
      SEP (FRZL FR1;
      data_at Tsh (Tstruct _mbedtls_md_context_t noattr) md_ctx'
        (field_address t_struct_hmac256drbg_context_st 
           [StructField _md_ctx] ctx);
      data_at Tsh (tarray tuchar 32)
        (map Vint
           (map Int.repr
              (fst
                 (HMAC_DRBG_generate_helper_Z HMAC256
                    (hmac256drbgabs_key
                       (HMAC256DRBGabs key0 V0 reseed_counter0 entropy_len0
                          prediction_resistance0 reseed_interval0))
                    after_update_value done))))
        (field_address t_struct_hmac256drbg_context_st [StructField _V] ctx);
      UNDER_SPEC.FULL key0 (snd (snd md_ctx'));
      data_at Tsh (tarray tuchar out_len)
        (map Vint
           (map Int.repr
              (sublist 0 done
                 (snd
                    (HMAC_DRBG_generate_helper_Z HMAC256
                       (hmac256drbgabs_key
                          (HMAC256DRBGabs key0 V0 reseed_counter0
                             entropy_len0 prediction_resistance0
                             reseed_interval0)) after_update_value done)))) ++
         list_repeat (Z.to_nat (out_len - done)) Vundef) output; 
      K_vector kv)
    ).
    {
      (* md_len < left *)
      assert (Hmin: 32 < out_len - done).
      {
        subst md_len.
        simpl in H16.
        unfold Int.ltu in H16.
        destruct (Z_lt_ge_dec 32 (out_len - done)) as [Hmin | Hmin].
        assumption.
        rewrite zlt_false in H16;[ inversion H16|].
        SearchAbout Int.unsigned Int.repr.
        change (Int.unsigned (Int.repr 32)) with 32.
        rewrite (Int.unsigned_repr (out_len - done)); try omega.
      }
      forward.
      subst md_len.
      entailer!.
      rewrite Z.min_l; [reflexivity | omega].
    }
    {
      (* md_len >= left *)
      assert (Hmin: 32 >= out_len - done).
      {
        subst md_len.
        simpl in H16.
        unfold Int.ltu in H16.
        destruct (Z_lt_ge_dec 32 (out_len - done)) as [Hmin | Hmin].
        rewrite zlt_true in H16;[ inversion H16|].
        change (Int.unsigned (Int.repr 32)) with 32.
        rewrite (Int.unsigned_repr (out_len - done)); try omega.
        assumption.
      }
      forward.
      subst md_len.
      entailer!.
      rewrite Z.min_r; [reflexivity | omega].
    }
    forward.

    (* mbedtls_md_hmac_reset( &ctx->md_ctx ); *)
    assert_PROP (field_compatible (Tarray tuchar 32 noattr) 
          []
          (field_address t_struct_hmac256drbg_context_st [StructField _V] ctx)) by entailer!.
    forward_call (field_address t_struct_hmac256drbg_context_st [StructField _md_ctx] ctx, md_ctx', key0, kv).
    {
      entailer!.
    }

    Intros vret; subst vret.

    (* mbedtls_md_hmac_update( &ctx->md_ctx, ctx->V, md_len ); *)
    assert (HZlength_V: Zlength (fst
              (HMAC_DRBG_generate_helper_Z HMAC256
                 (hmac256drbgabs_key
                    (HMAC256DRBGabs key0 V0 reseed_counter0 entropy_len0
                       prediction_resistance0 reseed_interval0))
                 after_update_value done)) = 32).
    {
      apply HMAC_DRBG_generate_helper_Z_Zlength_fst; auto; try omega.
      apply hmac_common_lemmas.HMAC_Zlength.
    }
    forward_call (key0, field_address t_struct_hmac256drbg_context_st [StructField _md_ctx] ctx, md_ctx', field_address t_struct_hmac256drbg_context_st [StructField _V] ctx, @nil Z, (fst (HMAC_DRBG_generate_helper_Z HMAC256
                    (hmac256drbgabs_key
                       (HMAC256DRBGabs key0 V0 reseed_counter0 entropy_len0
                          prediction_resistance0 reseed_interval0))
                    after_update_value done)), kv).
    {
      entailer!.
      rename H22 into HZlength.
      do 2 rewrite Zlength_map in HZlength.
      rewrite HZlength.
      reflexivity.
    }
    {
      rewrite HZlength_V.
      cancel.
    }
    {
      rewrite HZlength_V.
      change Int.max_unsigned with 4294967295.
      change (two_power_pos 61) with 2305843009213693952.
      repeat split; try omega.
      apply HMAC_DRBG_generate_helper_Z_isbyteZ_fst; auto; try omega.
      apply hmac_common_lemmas.isbyte_hmac.
    }

    Intros vret; subst vret.
    rewrite app_nil_l.

    replace_SEP 2 (memory_block Tsh 32 (field_address t_struct_hmac256drbg_context_st [StructField _V] ctx)).
    {
      entailer!.
      simpl in HZlength_V.
      unfold hmac256drbgabs_value.
      rewrite HZlength_V.
      apply data_at_memory_block.
    }

    (* mbedtls_md_hmac_finish( &ctx->md_ctx, ctx->V ); *)
    forward_call ((fst
               (HMAC_DRBG_generate_helper_Z HMAC256
                  (hmac256drbgabs_key
                     (HMAC256DRBGabs key0 V0 reseed_counter0 entropy_len0
                        prediction_resistance0 reseed_interval0))
                  after_update_value done)), key0, field_address t_struct_hmac256drbg_context_st [StructField _md_ctx] ctx, md_ctx', field_address t_struct_hmac256drbg_context_st [StructField _V] ctx, Tsh, kv).
    {
      entailer!.
    }
    Intros vret; subst vret.
    assert_PROP (field_compatible (tarray tuchar out_len) [] output) as Hfield_compat_output by entailer!.
    replace_SEP 5 (
                  data_at Tsh (tarray tuchar done) (map Vint
           (map Int.repr
              (sublist 0 done
                 (snd
                    (HMAC_DRBG_generate_helper_Z HMAC256
                       (hmac256drbgabs_key
                          (HMAC256DRBGabs key0 V0 reseed_counter0
                             entropy_len0 prediction_resistance0
                             reseed_interval0)) after_update_value done))))) output *
                  data_at Tsh (tarray tuchar (out_len - done)) (list_repeat (Z.to_nat (out_len - done)) Vundef) (offset_val (Int.repr done) output)
    ).
    {
      entailer!.
      apply derives_refl'.

      assert (HZlength1: Zlength (map Vint
        (map Int.repr
           (sublist 0 (n * 32)%Z
              (snd
                 (HMAC_DRBG_generate_helper_Z HMAC256 key0
                    (hmac256drbgabs_value
                       (HMAC256DRBGabs key0 V0 reseed_counter0 entropy_len0
                          prediction_resistance0 reseed_interval0)) (n * 32)%Z))))) = (n * 32)%Z).
      {
        do 2 rewrite Zlength_map.
        rewrite Zlength_sublist; [omega|omega|].
        rewrite HMAC_DRBG_generate_helper_Z_Zlength_snd; auto; try omega.
        apply hmac_common_lemmas.HMAC_Zlength.
        exists n; reflexivity.
      }
      
      apply data_at_complete_split; try rewrite HZlength1; try rewrite Zlength_list_repeat; auto; try omega.
      replace ((n * 32)%Z + (out_len - (n * 32)%Z)) with out_len by omega.
      assumption.
    }
    normalize.
    
    remember (offset_val (Int.repr done) output) as done_output.
    remember (Z.min 32 (out_len - done)) as use_len.
    assert_PROP (field_compatible (tarray tuchar (out_len - done)) [] done_output) as Hfield_compat_done_output.
    {
      clear Heqdone_output Hmultiple.
      entailer!.
      rewrite H23 (*Zlength = done *) in H25 (*field compatible *); apply H25.
    }
    replace_SEP 1 (
                  data_at Tsh (tarray tuchar use_len) (list_repeat (Z.to_nat use_len) Vundef) done_output *
                  data_at Tsh (tarray tuchar (out_len - done - use_len)) (list_repeat (Z.to_nat (out_len - done - use_len)) Vundef) (offset_val (Int.repr use_len) done_output)
    ).
    {
      clear Hmultiple Heqdone_output.
      entailer!.
      apply derives_refl'.
      rewrite Zmin_spec.
      destruct (Z_lt_ge_dec 32 (out_len - done)) as [Hmin | Hmin].
      {
        rewrite zlt_true by assumption.
        apply data_at_complete_split; repeat rewrite Zlength_list_repeat; auto; try omega.
        replace (32 + (out_len - done - 32)) with (out_len - done) by omega; assumption.
        rewrite list_repeat_app.
        rewrite <- Z2Nat.inj_add; try omega.
        replace (32 + (out_len - done - 32)) with (out_len - done) by omega; reflexivity.
      }
      {
        rewrite zlt_false by assumption.
        apply data_at_complete_split; repeat rewrite Zlength_list_repeat; auto; try omega.
        replace (out_len - done + (out_len - done - (out_len - done))) with (out_len - done) by omega; assumption.
        replace (out_len - done - (out_len - done)) with 0 by omega; simpl; rewrite app_nil_r; reflexivity.
      }
    }
    normalize.

    replace_SEP 0 (memory_block Tsh use_len done_output).
    {
      clear Hmultiple.
      entailer!.
      eapply derives_trans; [apply data_at_memory_block|].
      replace (sizeof cenv_cs (tarray tuchar (Z.min 32 (out_len - done)))) with (Z.min 32 (out_len - done)).
      apply derives_refl.
      simpl.
      destruct (Z.min_dec 32 (out_len - done));
      rewrite Zmax0r; omega.
    }
    replace_SEP 6 (data_at Tsh (tarray tuchar use_len) (sublist 0 use_len (map Vint (map Int.repr (HMAC256
                 (fst
                    (HMAC_DRBG_generate_helper_Z HMAC256
                       (hmac256drbgabs_key
                          (HMAC256DRBGabs key0 V0 reseed_counter0
                             entropy_len0 prediction_resistance0
                             reseed_interval0)) after_update_value done))
                 key0)))) (field_address t_struct_hmac256drbg_context_st [StructField _V] ctx) *
                   data_at Tsh (tarray tuchar (32 - use_len)) (sublist use_len 32 (map Vint (map Int.repr (HMAC256
                 (fst
                    (HMAC_DRBG_generate_helper_Z HMAC256
                       (hmac256drbgabs_key
                          (HMAC256DRBGabs key0 V0 reseed_counter0
                             entropy_len0 prediction_resistance0
                             reseed_interval0)) after_update_value done))
                 key0)))) (offset_val (Int.repr use_len) (field_address t_struct_hmac256drbg_context_st [StructField _V] ctx))
    ).
    {
      clear Hmultiple.
      entailer!.
      apply derives_refl'.
      rewrite hmac_common_lemmas.HMAC_Zlength.
      remember (fst
                 (HMAC_DRBG_generate_helper_Z HMAC256 key0
                    (hmac256drbgabs_value
                       (HMAC256DRBGabs key0 V0 reseed_counter0 entropy_len0
                          prediction_resistance0 reseed_interval0)) done)) as V0'; clear HeqV0'.
      rewrite Zmin_spec.
      destruct (Z_lt_ge_dec 32 (out_len - done)) as [Hmin | Hmin].
      {
        rewrite zlt_true by assumption.
        apply data_at_complete_split; repeat rewrite Zlength_sublist; repeat rewrite Zlength_map; repeat rewrite hmac_common_lemmas.HMAC_Zlength; auto; try omega.
        SearchAbout sublist.
        rewrite sublist_nil.
        rewrite app_nil_r.
        symmetry; apply sublist_same.
        reflexivity.
        repeat rewrite Zlength_map; rewrite hmac_common_lemmas.HMAC_Zlength; reflexivity.
      }
      {
        rewrite zlt_false by assumption.
        apply data_at_complete_split; repeat rewrite Zlength_sublist; repeat rewrite Zlength_map; repeat rewrite hmac_common_lemmas.HMAC_Zlength; auto; try omega.
        replace (out_len - done - 0 + (32 - (out_len - done))) with 32 by omega; auto.
        rewrite sublist_rejoin; repeat rewrite Zlength_map; try rewrite hmac_common_lemmas.HMAC_Zlength; try omega.
        rewrite sublist_same; try reflexivity; repeat rewrite Zlength_map; try rewrite hmac_common_lemmas.HMAC_Zlength; try omega.
      }
    }
    (* memcpy( out, ctx->V, use_len ); *)
    forward_call ((Tsh, Tsh), done_output, (field_address t_struct_hmac256drbg_context_st [StructField _V] ctx), use_len, sublist 0 use_len (map Int.repr
              (HMAC256
                 (fst
                    (HMAC_DRBG_generate_helper_Z HMAC256
                       (hmac256drbgabs_key
                          (HMAC256DRBGabs key0 V0 reseed_counter0
                             entropy_len0 prediction_resistance0
                             reseed_interval0)) after_update_value done))
                 key0))).
    {
      replace (map Vint
            (sublist 0 use_len
               (map Int.repr
                  (HMAC256
                     (fst
                        (HMAC_DRBG_generate_helper_Z HMAC256
                           (hmac256drbgabs_key
                              (HMAC256DRBGabs key0 V0 reseed_counter0
                                 entropy_len0 prediction_resistance0
                                 reseed_interval0)) after_update_value done))
                     key0)))) with (
            sublist 0 use_len
            (map Vint 
               (map Int.repr
                  (HMAC256
                     (fst
                        (HMAC_DRBG_generate_helper_Z HMAC256
                           (hmac256drbgabs_key
                              (HMAC256DRBGabs key0 V0 reseed_counter0
                                 entropy_len0 prediction_resistance0
                                 reseed_interval0)) after_update_value done))
                     key0)))).
      change (@data_at CompSpecs (fst (Tsh, Tsh)) (tarray tuchar use_len)
         (sublist 0 use_len
            (map Vint
               (map Int.repr
                  (HMAC256
                     (fst
                        (HMAC_DRBG_generate_helper_Z HMAC256
                           (hmac256drbgabs_key
                              (HMAC256DRBGabs key0 V0 reseed_counter0
                                 entropy_len0 prediction_resistance0
                                 reseed_interval0)) after_update_value done))
                     key0))))
         (field_address t_struct_hmac256drbg_context_st [StructField _V] ctx)) with
      (@data_at hmac_drbg_compspecs.CompSpecs (fst (Tsh, Tsh)) (tarray tuchar use_len)
         (sublist 0 use_len
            (map Vint
               (map Int.repr
                  (HMAC256
                     (fst
                        (HMAC_DRBG_generate_helper_Z HMAC256
                           (hmac256drbgabs_key
                              (HMAC256DRBGabs key0 V0 reseed_counter0
                                 entropy_len0 prediction_resistance0
                                 reseed_interval0)) after_update_value done))
                     key0))))
         (field_address t_struct_hmac256drbg_context_st [StructField _V] ctx)).
      entailer!.
      apply sublist_map.
    }
    {
      change (Int.max_unsigned) with 4294967295.
      repeat split; auto;
      subst use_len; destruct (Z.min_dec 32 (out_len - done)); omega.
    }

    Intros vret; subst vret.

    simpl.
    gather_SEP 0 7.
    replace_SEP 0 (data_at Tsh (tarray tuchar 32) (map Vint
               (map Int.repr
                  (HMAC256
                     (fst
                        (HMAC_DRBG_generate_helper_Z HMAC256 key0
                           after_update_value done)) key0))) (field_address t_struct_hmac256drbg_context_st [StructField _V] ctx)).
    {
      clear Hmultiple.
      entailer!.
      apply derives_refl'.
      rewrite <- sublist_map.
      remember (fst
                    (HMAC_DRBG_generate_helper_Z HMAC256 key0
                       (hmac256drbgabs_value
                          (HMAC256DRBGabs key0 V0 reseed_counter0
                             entropy_len0 prediction_resistance0
                             reseed_interval0)) (done))) as V0'; clear HeqV0'.
      symmetry.
      rewrite Zmin_spec.
      destruct (Z_lt_ge_dec 32 (out_len - done)) as [Hmin | Hmin].
      {
        rewrite zlt_true by assumption.
        rewrite sublist_nil.
        rewrite sublist_same; repeat rewrite Zlength_map; try rewrite hmac_common_lemmas.HMAC_Zlength; try omega.
        remember (map Vint (map Int.repr (HMAC256 V0' key0))) as data.
        apply data_at_complete_split; subst data; repeat rewrite Zlength_map; repeat rewrite hmac_common_lemmas.HMAC_Zlength; auto; try omega.
        rewrite app_nil_r; reflexivity.
      }
      {
        rewrite zlt_false by assumption.
        remember (sublist 0 (out_len - done) (map Vint (map Int.repr (HMAC256 V0' key0)))) as data_left.
        remember (sublist (out_len - done) 32
        (map Vint (map Int.repr (HMAC256 V0' key0)))) as data_right.
        apply data_at_complete_split; subst data_left data_right; repeat rewrite Zlength_sublist; repeat rewrite Zlength_map; repeat rewrite hmac_common_lemmas.HMAC_Zlength; auto; try omega.
        replace (out_len - done - 0 + (32 - (out_len - done))) with 32 by omega; auto.
        rewrite sublist_rejoin; repeat rewrite Zlength_map; try rewrite hmac_common_lemmas.HMAC_Zlength; try omega.
        rewrite sublist_same; try reflexivity; repeat rewrite Zlength_map; try rewrite hmac_common_lemmas.HMAC_Zlength; try omega.
      }
    }

    gather_SEP 1 2.
    replace_SEP 0 (data_at Tsh (tarray tuchar (out_len - done)) ((map Vint
           (sublist 0 use_len
              (map Int.repr
                 (HMAC256
                    (fst
                       (HMAC_DRBG_generate_helper_Z HMAC256 key0
                                                    after_update_value done)) key0)))) ++ (list_repeat (Z.to_nat (out_len - done - use_len)) Vundef))
                                       done_output).
    {
      clear Heqdone_output Hmultiple.
      entailer!.
      apply derives_refl'.
      rewrite Zmin_spec.
      symmetry.
      destruct (Z_lt_ge_dec 32 (out_len - done)) as [Hmin | Hmin].
      {
        rewrite zlt_true by assumption.
        apply data_at_complete_split; change ((fix map (l : list int) : list val :=
               match l with
               | [] => []
               | a :: t => Vint a :: map t
               end)
              (sublist 0 32
                 (map Int.repr
                    (HMAC256
                       (fst
                          (HMAC_DRBG_generate_helper_Z HMAC256 key0
                             (hmac256drbgabs_value
                                (HMAC256DRBGabs key0 V0 reseed_counter0
                                   entropy_len0 prediction_resistance0
                                   reseed_interval0)) done)) key0)))) with (map Vint
              (sublist 0 32
                 (map Int.repr
                    (HMAC256
                       (fst
                          (HMAC_DRBG_generate_helper_Z HMAC256 key0
                             (hmac256drbgabs_value
                                (HMAC256DRBGabs key0 V0 reseed_counter0
                                   entropy_len0 prediction_resistance0
                                   reseed_interval0)) done)) key0)))); repeat rewrite Zlength_list_repeat; auto; try omega;
        rewrite Zlength_map; rewrite Zlength_sublist; try rewrite Zlength_map; try rewrite hmac_common_lemmas.HMAC_Zlength; auto; try omega.
        replace (32 - 0 + (out_len - done - 32)) with (out_len - done) by omega.
        assumption.
      }
      {
        rewrite zlt_false by assumption.
        apply data_at_complete_split; change ((fix map (l : list int) : list val :=
               match l with
               | [] => []
               | a :: t => Vint a :: map t
               end)
              (sublist 0 (out_len - done)
                 (map Int.repr
                    (HMAC256
                       (fst
                          (HMAC_DRBG_generate_helper_Z HMAC256 key0
                             (hmac256drbgabs_value
                                (HMAC256DRBGabs key0 V0 reseed_counter0
                                   entropy_len0 prediction_resistance0
                                   reseed_interval0)) done)) key0)))) with (map Vint
              (sublist 0 (out_len - done)
                 (map Int.repr
                    (HMAC256
                       (fst
                          (HMAC_DRBG_generate_helper_Z HMAC256 key0
                             (hmac256drbgabs_value
                                (HMAC256DRBGabs key0 V0 reseed_counter0
                                   entropy_len0 prediction_resistance0
                                   reseed_interval0)) done)) key0)))); repeat rewrite Zlength_list_repeat; auto; try omega;
        rewrite Zlength_map; rewrite Zlength_sublist; try rewrite Zlength_map; try rewrite hmac_common_lemmas.HMAC_Zlength; auto; try omega.
        replace (out_len - done - 0 + (out_len - done - (out_len - done))) with (out_len - done) by omega.
        assumption.
      }
    }

    gather_SEP 2 0.
    replace_SEP 0 (
                  data_at Tsh (tarray tuchar out_len) ((map Vint
           (map Int.repr
              (sublist 0 done
                 (snd
                    (HMAC_DRBG_generate_helper_Z HMAC256 key0
                       after_update_value done))))) ++ (map Vint
            (sublist 0 use_len
               (map Int.repr
                  (HMAC256
                     (fst
                        (HMAC_DRBG_generate_helper_Z HMAC256 key0
                           after_update_value done)) key0))) ++
          list_repeat (Z.to_nat (out_len - done - use_len)) Vundef)) output
    ).
    {
      entailer!.
      apply derives_refl'.
      symmetry.
      assert (HZlength1: Zlength (
              (snd
                 (HMAC_DRBG_generate_helper_Z HMAC256 key0
                    (hmac256drbgabs_value
                       (HMAC256DRBGabs key0 V0 reseed_counter0 entropy_len0
                          prediction_resistance0 reseed_interval0)) (n * 32)%Z))) = (n * 32)%Z).
      {
        rewrite HMAC_DRBG_generate_helper_Z_Zlength_snd; auto; try omega.
        apply hmac_common_lemmas.HMAC_Zlength.
        exists n; reflexivity.
      }
      rewrite Zmin_spec.
      destruct (Z_lt_ge_dec 32 (out_len - (n * 32)%Z)) as [Hmin | Hmin]; [rewrite zlt_true by assumption | rewrite zlt_false by assumption];
      apply data_at_complete_split; repeat rewrite Zlength_app; repeat rewrite Zlength_map; try rewrite HZlength1; repeat rewrite Zlength_list_repeat; repeat rewrite Zlength_sublist; repeat rewrite Zlength_map; try rewrite hmac_common_lemmas.HMAC_Zlength; auto; try omega.
      replace ((n * 32)%Z - 0 + (32 - 0 + (out_len - (n * 32)%Z - 32))) with out_len by omega;
      assumption.
      replace ((n * 32)%Z - 0 + (out_len - (n * 32)%Z - 0 + (out_len - (n * 32)%Z - (out_len - (n * 32)%Z)))) with out_len by omega;
      assumption.
    }

    (* out += use_len; *)
    forward.

    (* left -= use_len; *)
    forward.

    
    Exists (done + use_len).
    unfold hmac_drbg_update_post; normalize.

    unfold_data_at 4%nat.
    rewrite (field_at_data_at _ _ [StructField _md_ctx]);
    rewrite (field_at_data_at _ _ [StructField _V]).
    
    unfold md_full.
    
    thaw FR1.
    thaw FR_unused_struct_fields.
    subst.

    entailer!.
    {
      rewrite Zmin_spec.
      destruct (Z_lt_ge_dec 32 (out_len - (n * 32)%Z)) as [Hmin | Hmin]; [rewrite zlt_true by assumption | rewrite zlt_false by assumption]; repeat split; try omega.
      left; exists (n + 1); omega.
      replace (out_len - ((n * 32)%Z + 32)) with (out_len - (n * 32)%Z - 32) by omega;
      reflexivity.
      right; omega.
      replace (out_len - ((n * 32)%Z + (out_len - (n * 32)%Z))) with (out_len - (n * 32)%Z - (out_len - (n * 32)%Z)) by omega;
      reflexivity.
    }

    unfold md_full.
    replace (HMAC256 (fst (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0 (n * 32)%Z))
              key0) with (fst
                  (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
                     ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z)))).
    cancel.
    apply derives_refl'.
    
    rewrite app_assoc.
    replace (map Vint
        (map Int.repr
           (sublist 0 (n * 32)%Z
              (snd (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0 (n * 32)%Z)))) ++
      map Vint
        (sublist 0 (Z.min 32 (out_len - (n * 32)%Z))
           (map Int.repr
              (fst
                 (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
                    ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z))))))) with (map Vint
        (map Int.repr
           (sublist 0 ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z))
              (snd
                 (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
                    ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z))))))).
    replace (out_len - (n * 32)%Z - Z.min 32 (out_len - (n * 32)%Z)) with (out_len - ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z))) by omega.
    reflexivity.
    rewrite <- map_app.
    rewrite sublist_map.
    rewrite <- map_app.
    replace (sublist 0 ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z))
           (snd
              (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
                 ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z))))) with (sublist 0 (n * 32)%Z
           (snd (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0 (n * 32)%Z)) ++
         sublist 0 (Z.min 32 (out_len - (n * 32)%Z))
           (fst
              (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
                 ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z))))).
    reflexivity.
    replace (snd
              (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
                 ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z)))) with (snd (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0 (n * 32)%Z) ++ fst
              (HMAC_DRBG_generate_helper_Z HMAC256 key0 V0
                 ((n * 32)%Z + Z.min 32 (out_len - (n * 32)%Z)))).
    {
      apply while_loop_post_sublist_app; auto.
    }
    {
      apply while_loop_post_incremental_snd; auto.
      intros contra; rewrite contra in HRE; omega.
    }
    {
      apply while_loop_post_incremental_fst; auto.
      idtac.
      intros contra; rewrite contra in HRE; omega.
    }
  }

  assert (Hdone: done = out_len).
  {
    clear - HRE H15 Hout_len.
    assert (Hdiff: out_len - done = 0).
    {
      unfold Int.eq in HRE.
      SearchAbout zeq.
      unfold zeq in HRE.
      destruct (Z.eq_dec (Int.unsigned (Int.repr (out_len - done)))
                (Int.unsigned (Int.repr 0))).
      rewrite (Int.unsigned_repr (out_len - done)) in e.
      rewrite e; reflexivity.
      change (Int.max_unsigned) with 4294967295; omega.
      inversion HRE.
    }
    omega.
  }
  rewrite Hdone.
  replace (out_len - out_len) with 0 by omega.
  change (list_repeat (Z.to_nat 0) Vundef) with (@nil val).
  rewrite app_nil_r.
  unfold hmac_drbg_update_post.
  normalize.

  assert_PROP (isptr additional) as Hisptr_add by entailer!.
  
  replace_SEP 4 (mpred_passed_to_function (data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional) should_reseed * mpred_passed_to_frame (data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional) should_reseed).
  {
    clear Heqshould_reseed.
    entailer!.
    apply derives_refl'.
    apply mpred_cond_correct.
  }

  (* mbedtls_hmac_drbg_update( ctx, additional, add_len ); *)
  forward_call (if should_reseed then @nil Z else contents, additional, after_reseed_add_len, ctx, (hmac256drbgabs_to_state
         (hmac256drbgabs_update_value after_update_state_abs
            (fst
               (HMAC_DRBG_generate_helper_Z HMAC256 after_update_key
                  after_update_value out_len))) initial_state), (hmac256drbgabs_update_value after_update_state_abs
         (fst
            (HMAC_DRBG_generate_helper_Z HMAC256 after_update_key
               after_update_value out_len))), kv, info_contents).
  {
    assert (Stream after_reseed_s *
   mpred_passed_to_frame
     (data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional) should_reseed *
   data_at Tsh (tarray tuchar out_len)
     (map Vint
        (map Int.repr
           (sublist 0 out_len
              (snd
                 (HMAC_DRBG_generate_helper_Z HMAC256 after_update_key
                    after_update_value out_len))))) output |-- fold_right sepcon emp Frame)
    by cancel.
    subst after_reseed_add_len.
    unfold mpred_passed_to_function; destruct should_reseed; cancel.
    entailer!.
    admit (* TODO *).
  }
  {
    subst after_reseed_add_len; split.
    destruct should_reseed; omega.
    replace (hmac256drbgabs_value
        (hmac256drbgabs_update_value after_update_state_abs
           (fst
              (HMAC_DRBG_generate_helper_Z HMAC256 after_update_key
                 after_update_value out_len)))) with (fst
              (HMAC_DRBG_generate_helper_Z HMAC256 after_update_key
                 after_update_value out_len)) by (
      destruct after_update_state_abs; unfold hmac256drbgabs_value; unfold hmac256drbgabs_update_value; reflexivity).
    repeat split; try now (destruct should_reseed; auto).
    {
      rewrite HMAC_DRBG_generate_helper_Z_Zlength_fst; auto; try omega.
      apply hmac_common_lemmas.HMAC_Zlength.
    }
    {
      apply HMAC_DRBG_generate_helper_Z_isbyteZ_fst; auto; try omega.
      apply hmac_common_lemmas.isbyte_hmac.
    }
  }

  gather_SEP 1 4.
  replace_SEP 0 (data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents)) additional).
  {
    clear Heqshould_reseed.
    entailer!.
    rewrite mpred_cond_correct with (cond:=should_reseed).
    cancel.
    unfold mpred_passed_to_function.
    destruct should_reseed; [| apply derives_refl].
    eapply derives_trans.
    apply data_at_memory_block.
    simpl.
    destruct additional'; try solve [inversion Hisptr_add].
    apply derives_refl';
    apply memory_block_zero_Vptr.
  }

  (* ctx->reseed_counter++; *)
  remember (hmac256drbgabs_hmac_drbg_update
           (hmac256drbgabs_update_value after_update_state_abs
              (fst
                 (HMAC_DRBG_generate_helper_Z HMAC256 after_update_key
                    after_update_value out_len)))
           (if should_reseed then [] else contents)) as semi_final_state_abs.
  replace_SEP 1 (hmac_drbg_update_post semi_final_state_abs
        initial_state ctx
        info_contents).
  {
    destruct semi_final_state_abs.
    destruct (hmac256drbgabs_update_value after_update_state_abs
            (fst
               (HMAC_DRBG_generate_helper_Z HMAC256 after_update_key
                  after_update_value out_len))).
    unfold hmac_drbg_update_post, hmac256drbgabs_to_state.
    entailer!.
    entailer!.
  }
  
  unfold hmac_drbg_update_post. normalize.
  forward.
  idtac.
  {
    (* type checking *)
    subst initial_state initial_state_abs.
    entailer!.
    unfold hmac256drbgabs_to_state, hmac256drbgabs_update_value, hmac256drbgabs_hmac_drbg_update, hmac256drbgabs_reseed, hmac256drbgabs_value, hmac256drbgabs_key, HMAC256_DRBG_update, HMAC_DRBG_update. 
    destruct (mbedtls_HMAC256_DRBG_reseed_function s
                               (HMAC256DRBGabs key V reseed_counter
                                  entropy_len prediction_resistance
                                  reseed_interval) contents) as [d s' | e s'];
      try destruct d as [[[[V' key'] reseed_counter'] security_strength'] prediction_resistance'];
      destruct (prediction_resistance || (reseed_counter >? reseed_interval))%bool; destruct contents; auto; unfold is_int; constructor.
  }

  forward.

  (* return 0; *)
  forward.
  
  Exists Vzero.
  
  unfold hmac256drbgabs_generate.

  clear Heqnon_empty_additional.

  rewrite generate_correct with (should_reseed:=(prediction_resistance
                                         || (reseed_counter >?
                                             reseed_interval))%bool) (non_empty_additional:=if (prediction_resistance
                               || (reseed_counter >? reseed_interval))%bool
                           then false
                           else
                            match contents with
                            | [] => false
                            | _ :: _ => true
                            end); auto.
  remember (hmac256drbgabs_hmac_drbg_update
               (hmac256drbgabs_update_value
                  (if if (prediction_resistance
                          || (reseed_counter >? reseed_interval))%bool
                      then false
                      else
                       match contents with
                       | [] => false
                       | _ :: _ => true
                       end
                   then
                    hmac256drbgabs_hmac_drbg_update
                      (HMAC256DRBGabs key V reseed_counter entropy_len
                         prediction_resistance reseed_interval) contents
                   else
                    if (prediction_resistance
                        || (reseed_counter >? reseed_interval))%bool
                    then
                     hmac256drbgabs_reseed
                       (HMAC256DRBGabs key V reseed_counter entropy_len
                          prediction_resistance reseed_interval) s contents
                    else
                     HMAC256DRBGabs key V reseed_counter entropy_len
                       prediction_resistance reseed_interval)
                  (fst
                     (HMAC_DRBG_generate_helper_Z HMAC256
                        (hmac256drbgabs_key
                           (if if (prediction_resistance
                                   || (reseed_counter >? reseed_interval))%bool
                               then false
                               else
                                match contents with
                                | [] => false
                                | _ :: _ => true
                                end
                            then
                             hmac256drbgabs_hmac_drbg_update
                               (HMAC256DRBGabs key V reseed_counter
                                  entropy_len prediction_resistance
                                  reseed_interval) contents
                            else
                             if (prediction_resistance
                                 || (reseed_counter >? reseed_interval))%bool
                             then
                              hmac256drbgabs_reseed
                                (HMAC256DRBGabs key V reseed_counter
                                   entropy_len prediction_resistance
                                   reseed_interval) s contents
                             else
                              HMAC256DRBGabs key V reseed_counter entropy_len
                                prediction_resistance reseed_interval))
                        (hmac256drbgabs_value
                           (if if (prediction_resistance
                                   || (reseed_counter >? reseed_interval))%bool
                               then false
                               else
                                match contents with
                                | [] => false
                                | _ :: _ => true
                                end
                            then
                             hmac256drbgabs_hmac_drbg_update
                               (HMAC256DRBGabs key V reseed_counter
                                  entropy_len prediction_resistance
                                  reseed_interval) contents
                            else
                             if (prediction_resistance
                                 || (reseed_counter >? reseed_interval))%bool
                             then
                              hmac256drbgabs_reseed
                                (HMAC256DRBGabs key V reseed_counter
                                   entropy_len prediction_resistance
                                   reseed_interval) s contents
                             else
                              HMAC256DRBGabs key V reseed_counter entropy_len
                                prediction_resistance reseed_interval)) done))) (if (prediction_resistance
                              || (reseed_counter >? reseed_interval))%bool
                          then []
                          else contents)) as semi_final_state_abs.
  remember (sublist 0 done
                    (snd
                       (HMAC_DRBG_generate_helper_Z HMAC256
                          (hmac256drbgabs_key
                             (if if (prediction_resistance
                                     || (reseed_counter >? reseed_interval))%bool
                                 then false
                                 else
                                  match contents with
                                  | [] => false
                                  | _ :: _ => true
                                  end
                              then
                               hmac256drbgabs_hmac_drbg_update
                                 (HMAC256DRBGabs key V reseed_counter
                                    entropy_len prediction_resistance
                                    reseed_interval) contents
                              else
                               if (prediction_resistance
                                   || (reseed_counter >? reseed_interval))%bool
                               then
                                hmac256drbgabs_reseed
                                  (HMAC256DRBGabs key V reseed_counter
                                     entropy_len prediction_resistance
                                     reseed_interval) s contents
                               else
                                HMAC256DRBGabs key V reseed_counter
                                  entropy_len prediction_resistance
                                  reseed_interval))
                          (hmac256drbgabs_value
                             (if if (prediction_resistance
                                     || (reseed_counter >? reseed_interval))%bool
                                 then false
                                 else
                                  match contents with
                                  | [] => false
                                  | _ :: _ => true
                                  end
                              then
                               hmac256drbgabs_hmac_drbg_update
                                 (HMAC256DRBGabs key V reseed_counter
                                    entropy_len prediction_resistance
                                    reseed_interval) contents
                              else
                               if (prediction_resistance
                                   || (reseed_counter >? reseed_interval))%bool
                               then
                                hmac256drbgabs_reseed
                                  (HMAC256DRBGabs key V reseed_counter
                                     entropy_len prediction_resistance
                                     reseed_interval) s contents
                               else
                                HMAC256DRBGabs key V reseed_counter
                                  entropy_len prediction_resistance
                                  reseed_interval)) done))) as generate_output_bytes.
  destruct semi_final_state_abs.
  pose proof (Heqsemi_final_state_abs) as Hsemi_final_metadata.
  apply metadata_preserved in Hsemi_final_metadata.
  pose proof (Heqsemi_final_state_abs) as Hsemi_final_reseed_counter.
  apply reseed_counter_values in Hsemi_final_reseed_counter.
  clear Heqgenerate_output_bytes Heqsemi_final_state_abs.
  unfold return_value_relate_result.
  unfold get_stream_result.
  unfold hmac_drbg_update_post, hmac256drbgabs_to_state_handle, hmac256drbgabs_increment_reseed_counter.
  unfold hmac256drbgabs_to_state, hmac256drbg_relate.
  unfold hmac256drbgstate_md_info_pointer.
  entailer!.
  simpl in *.
  destruct Hsemi_final_metadata as [Hentropy_len0 [Hpr0 Hreseed_interval0]].
  subst entropy_len0.
  subst reseed_interval0.
  entailer!.
Qed.
