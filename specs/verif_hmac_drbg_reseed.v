Require Import floyd.proofauto.
Import ListNotations.
Local Open Scope logic.

Require Import hmac_drbg.
Require Import spec_hmac_drbg.

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

Lemma isbyteZ_app: forall A B, Forall general_lemmas.isbyteZ A -> Forall general_lemmas.isbyteZ B -> Forall general_lemmas.isbyteZ (A ++ B).
Proof.
  intros A B HA HB.
  induction A as [|hdA tlA].
  simpl; assumption.
  simpl. inversion HA. constructor.
  assumption.
  apply IHtlA.
  assumption.
Qed.

Check data_at_valid_ptr.
Lemma data_at_weak_valid_ptr: forall (sh : Share.t) (t : type) (v : reptype t) (p : val),
       sepalg.nonidentity sh ->
       sizeof cenv_cs t >= 0 -> data_at sh t v p |-- weak_valid_pointer p.
Proof.
Admitted.
Hint Resolve data_at_weak_valid_ptr: valid_pointer.

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

Lemma body_hmac_drbg_reseed: semax_body HmacDrbgVarSpecs HmacDrbgFunSpecs 
       f_mbedtls_hmac_drbg_reseed hmac_drbg_reseed_spec.
Proof.
  start_function.
  
  name ctx' _ctx.
  name add_len' _len.
  name additional' _additional.

  rename lvar0 into seed.
  destruct initial_state_abs.
  destruct initial_state as [md_ctx' [V' [reseed_counter' [entropy_len' [prediction_resistance' reseed_interval']]]]].
  unfold hmac256drbg_relate.
  normalize.

  (* entropy_len = ctx->entropy_len *)
  forward.

  remember (if zlt 256 add_len then true else false) as add_len_too_high.

  (* if (len > MBEDTLS_HMAC_DRBG_MAX_INPUT ||
        entropy_len + len > MBEDTLS_HMAC_DRBG_MAX_SEED_INPUT) *)
  forward_if (PROP  ()
      LOCAL  (temp _entropy_len (Vint (Int.repr entropy_len));
      lvar _seed (tarray tuchar 384) seed; temp _ctx ctx;
      temp _additional additional; temp _len (Vint (Int.repr add_len));
      temp 146%positive (Val.of_bool add_len_too_high);
      gvar sha._K256 kv)
      SEP  (data_at_ Tsh (tarray tuchar 384) seed;
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional;
      data_at Tsh t_struct_hmac256drbg_context_st
        (md_ctx',
        (map Vint (map Int.repr V),
        (Vint (Int.repr reseed_counter),
        (Vint (Int.repr entropy_len),
        (Val.of_bool prediction_resistance, Vint (Int.repr reseed_interval)))))) ctx;
      md_full key md_ctx';
      data_at Tsh t_struct_mbedtls_md_info info_contents
        (hmac256drbgstate_md_info_pointer
           (md_ctx',
           (map Vint (map Int.repr V),
           (Vint (Int.repr reseed_counter),
           (Vint (Int.repr entropy_len),
           (Val.of_bool prediction_resistance, Vint (Int.repr reseed_interval)))))));
      Stream s; spec_sha.K_vector kv)
  ).
  {
    rewrite zlt_true in Heqadd_len_too_high by assumption.
    forward.
    entailer!.    
  }
  {
    rewrite zlt_false in Heqadd_len_too_high by assumption.
    forward.
    entailer!.
    rewrite <- H12.
    simpl in H2. subst entropy_len.
    unfold Int.ltu.
    destruct (zlt (Int.unsigned (Int.repr 384))
                 (Int.unsigned (Int.repr (32 + Zlength contents)))).
    rewrite Int.unsigned_repr_eq in l.
    rewrite Zmod_small in l by auto.
    rewrite Int.unsigned_repr_eq in l.
    rewrite Zmod_small in l.
    omega.
    assert (0 <= Zlength contents <= 256) by omega.
    rewrite hmac_pure_lemmas.IntModulus32. simpl.
    change (Z.pow_pos 2 32) with 4294967296.
    omega.
    reflexivity.
  }

  forward_if (PROP  (add_len_too_high = false)
      LOCAL  (temp _entropy_len (Vint (Int.repr entropy_len));
      lvar _seed (tarray tuchar 384) seed; temp _ctx ctx;
      temp _additional additional; temp _len (Vint (Int.repr add_len));
      gvar sha._K256 kv)
      SEP  (data_at_ Tsh (tarray tuchar 384) seed;
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional;
      data_at Tsh t_struct_hmac256drbg_context_st
        (md_ctx',
        (map Vint (map Int.repr V),
        (Vint (Int.repr reseed_counter),
        (Vint (Int.repr entropy_len),
        (Val.of_bool prediction_resistance, Vint (Int.repr reseed_interval)))))) ctx;
      md_full key md_ctx';
      data_at Tsh t_struct_mbedtls_md_info info_contents
        (hmac256drbgstate_md_info_pointer
           (md_ctx',
           (map Vint (map Int.repr V),
           (Vint (Int.repr reseed_counter),
           (Vint (Int.repr entropy_len),
           (Val.of_bool prediction_resistance, Vint (Int.repr reseed_interval)))))));
      Stream s; spec_sha.K_vector kv)
  ).
  {
    forward.
    unfold hmac_drbg_update_post, get_stream_result, hmac256drbg_relate.
    Exists seed (Vint (Int.neg (Int.repr 5))).
    rewrite andb_negb_r.
    destruct (zlt 256 (Zlength contents)); inv Heqadd_len_too_high.
    rewrite Z.gtb_ltb.
    assert (Hlt: 256 <? Zlength contents = true) by (apply Z.ltb_lt; assumption).
    rewrite Hlt.
    unfold hmac256drbgabs_to_state.
    entailer!.
  }
  {
    forward.
    entailer!.
  }
  assert_PROP (0 <= Zlength contents <= 256) as HZlength.
  {
    entailer!. destruct (zlt 256 (Zlength contents)); inversion H11. omega.
  }

  (* memset( seed, 0, MBEDTLS_HMAC_DRBG_MAX_SEED_INPUT ); *)
  forward_call (Tsh, seed, 384, Int.zero).
  {
    rewrite data_at__memory_block.
    change (sizeof cenv_cs (tarray tuchar 384)) with 384.
    entailer!.
  }
  Intros vret; subst vret.

  assert_PROP (field_compatible (tarray tuchar 384) [] seed) as Hfield by entailer!.
  replace_SEP 0 ((data_at Tsh (tarray tuchar entropy_len)
         (list_repeat (Z.to_nat entropy_len) (Vint Int.zero)) seed) * (data_at Tsh (tarray tuchar (384 - entropy_len))
         (list_repeat (Z.to_nat (384 - entropy_len)) (Vint Int.zero)) (offset_val (Int.repr entropy_len) seed))).
  {
    simpl in H2.
    subst entropy_len.
    entailer!.
    apply derives_refl'; apply data_at_complete_split; auto.
  }
  normalize.

  replace_SEP 0 (memory_block Tsh entropy_len seed).
  {
    entailer!.
    simpl in H2; subst entropy_len.
    apply data_at_memory_block.
  }

  (* get_entropy(seed, entropy_len ) *)
  forward_call (Tsh, s, seed, entropy_len).
  {
    simpl in H2; subst entropy_len.
    auto.
  }
  Intros vret.

  (* if( get_entropy(seed, entropy_len ) != 0 ) *)
  forward_if (
      PROP  (vret=Vzero)
      LOCAL  (temp 147%positive vret;
      temp _entropy_len (Vint (Int.repr entropy_len));
      lvar _seed (tarray tuchar 384) seed; temp _ctx ctx;
      temp _additional additional; temp _len (Vint (Int.repr add_len));
      gvar sha._K256 kv)
      SEP 
      (Stream
         (get_stream_result
            (entropy.get_entropy 0 entropy_len entropy_len false s));
      match entropy.ENTROPY.get_bytes (Z.to_nat entropy_len) s with
      | entropy.ENTROPY.success bytes _ =>
          data_at Tsh (tarray tuchar entropy_len)
            (map Vint (map Int.repr bytes)) seed
      | entropy.ENTROPY.error _ _ => memory_block Tsh entropy_len seed
      end;
      data_at Tsh (tarray tuchar (384 - entropy_len))
        (list_repeat (Z.to_nat (384 - entropy_len)) (Vint Int.zero))
        (offset_val (Int.repr entropy_len) seed);
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional;
      data_at Tsh t_struct_hmac256drbg_context_st
        (md_ctx',
        (map Vint (map Int.repr V),
        (Vint (Int.repr reseed_counter),
        (Vint (Int.repr entropy_len),
        (Val.of_bool prediction_resistance, Vint (Int.repr reseed_interval)))))) ctx;
      md_full key md_ctx';
      data_at Tsh t_struct_mbedtls_md_info info_contents
        (hmac256drbgstate_md_info_pointer
           (md_ctx',
           (map Vint (map Int.repr V),
           (Vint (Int.repr reseed_counter),
           (Vint (Int.repr entropy_len),
           (Val.of_bool prediction_resistance, Vint (Int.repr reseed_interval)))))));
      spec_sha.K_vector kv)
  ).
  {
    (* != 0 case *)
    forward.
    unfold hmac_drbg_update_post.
    Exists seed (Vint (Int.neg (Int.repr (9)))).
    unfold entropy.get_entropy in *.
    destruct (entropy.ENTROPY.get_bytes (Z.to_nat entropy_len) s).
    {
      (* contradiction. cannot be a success *)
      hnf in H11.
      inv H11.
      inversion H12.
    }
    rewrite andb_negb_r.
    destruct (zlt 256 (Zlength contents)); inv Heqadd_len_too_high.
    rewrite Z.gtb_ltb.
    assert (Hlt: 256 <? Zlength contents = false) by (apply Z.ltb_nlt; assumption).
    rewrite Hlt.
    unfold hmac256drbg_relate, get_stream_result.
    rewrite data_at__memory_block.
    unfold hmac256drbgabs_to_state.
    entailer!.    
    eapply derives_trans. apply sepcon_derives; [apply derives_refl | apply data_at_memory_block].
    apply derives_refl'.
    destruct seed; inv Pseed.
    simpl.
    rewrite <- repr_unsigned with (i:=i).
    simpl in H2; subst entropy_len.
    change (1 * Z.max 0 (384 - 32))%Z with 352.
    rewrite add_repr.
    rewrite <- memory_block_split; auto.
    clear - H14. rename H14 into Hlvar.
    unfold lvar in Hlvar; unfold size_compatible in Hlvar.
    destruct (Map.get (ve_of rho) _seed); try solve [inversion Hlvar].
    destruct p. destruct (eqb_type (tarray tuchar 384) t); try solve [inversion Hlvar].
    destruct Hlvar as [Hblock Hsize].
    simpl in Hsize.
    assert (Int.unsigned i >= 0) by (pose proof (Int.unsigned_range i); omega).
    omega.
  }
  {
    forward.
    entailer!.
    replace _id with Int.zero; [reflexivity|].
    clear - H12. rename H12 into Hid.
    pose proof (negb_sym (Int.eq _id (Int.repr 0)) false).
    symmetry in Hid; apply H in Hid.
    simpl in Hid.
    symmetry; apply binop_lemmas2.int_eq_true. 
    auto.
  }

  (* now that we know entropy call succeeded, use that fact to simplify the SEP clause *)
  remember (entropy.ENTROPY.get_bytes (Z.to_nat entropy_len) s) as entropy_result.
  unfold entropy.get_entropy in H11;
  rewrite <- Heqentropy_result in H11;
  destruct entropy_result; [|
  normalize;
  simpl in H11; destruct e; [inversion H11 |
  assert (contra: False) by (apply H11; reflexivity); inversion contra]
  ].

  rename l into entropy_bytes.

  assert (Hentropy_bytes_length: Zlength (map Vint (map Int.repr entropy_bytes)) = 32).
  {
    repeat rewrite Zlength_map.
    eapply entropy.ENTROPY.get_bytes_Zlength.
    omega.
    simpl in H2; subst entropy_len.
    eassumption.
  }
  
  gather_SEP 1 2.
  replace_SEP 0 (data_at Tsh (tarray tuchar 384)
         ((map Vint
            (map Int.repr entropy_bytes)) ++ (list_repeat (Z.to_nat (384 - entropy_len)) (Vint Int.zero))) seed).
  {
    simpl in H2.
    subst entropy_len.
    entailer!.
    apply derives_refl'; symmetry; apply data_at_complete_split; auto.
    replace (Zlength (map Vint (map Int.repr entropy_bytes))) with 32 by assumption.
    auto.
  }

  (* seedlen = entropy_len; *)
  forward.

  remember (if eq_dec additional nullval then false else if eq_dec add_len 0 then false else true) as non_empty_additional.

  (* if additional != null *)
  forward_if (
      PROP  ()
      LOCAL  (temp _seedlen (Vint (Int.repr entropy_len));
      temp _entropy_len (Vint (Int.repr entropy_len));
      lvar _seed (tarray tuchar 384) seed; temp _ctx ctx;
      temp _additional additional; temp _len (Vint (Int.repr add_len));
      temp 148%positive (Val.of_bool non_empty_additional);
      gvar sha._K256 kv)
      SEP  (
        data_at Tsh (tarray tuchar 384)
         (map Vint (map Int.repr entropy_bytes) ++
          list_repeat (Z.to_nat (384 - entropy_len)) (Vint Int.zero)) seed;
        Stream
        (get_stream_result
           (entropy.get_entropy 0 entropy_len entropy_len false s));
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional;
      data_at Tsh t_struct_hmac256drbg_context_st
        (md_ctx',
        (map Vint (map Int.repr V),
        (Vint (Int.repr reseed_counter),
        (Vint (Int.repr entropy_len),
        (Val.of_bool prediction_resistance, Vint (Int.repr reseed_interval)))))) ctx;
      md_full key md_ctx';
      data_at Tsh t_struct_mbedtls_md_info info_contents
        (hmac256drbgstate_md_info_pointer
           (md_ctx',
           (map Vint (map Int.repr V),
           (Vint (Int.repr reseed_counter),
           (Vint (Int.repr entropy_len),
           (Val.of_bool prediction_resistance, Vint (Int.repr reseed_interval)))))));
      spec_sha.K_vector kv)
  ).
  {
    (* TODO this should be easy with weakly valid pointer *)
    unfold denote_tc_comparable.
    assert_PROP (isptr additional) as Hisptr by entailer!. destruct additional; try solve [inversion Hisptr]; clear Hisptr.
    entailer!.
    admit.
  }
  {
    forward.
    entailer!.
    rewrite <- H14.
    destruct (eq_dec additional' nullval) as [additional_pos | additional_neg].
    subst additional'; assert (contra: False) by (apply H12; reflexivity); inversion contra.
    destruct (eq_dec (Zlength contents) 0) as [Zlength_pos | Zlength_neg].
    rewrite Zlength_pos. reflexivity.
    rewrite Int.eq_false. reflexivity.
    intros contra.
    apply repr_inj_unsigned in contra; omega.
  }
  {
    forward.
    entailer!.
    destruct (eq_dec nullval nullval).
    reflexivity.
    assert (contra: False) by auto; inversion contra.
  }

  forward_if (
      PROP  ()
      LOCAL  (temp _seedlen (Vint (Int.repr (entropy_len + Zlength contents)));
      temp _entropy_len (Vint (Int.repr entropy_len));
      lvar _seed (tarray tuchar 384) seed; temp _ctx ctx;
      temp _additional additional; temp _len (Vint (Int.repr add_len));
      gvar sha._K256 kv)
      SEP 
      (data_at Tsh (tarray tuchar 384)
         (map Vint
            (map Int.repr entropy_bytes) ++ (map Vint (map Int.repr contents)) ++
          list_repeat (Z.to_nat (384 - entropy_len - Zlength contents)) (Vint Int.zero)) seed;
      Stream
     (get_stream_result
        (entropy.get_entropy 0 entropy_len entropy_len false s));
      data_at Tsh (tarray tuchar add_len) (map Vint (map Int.repr contents))
        additional;
      data_at Tsh t_struct_hmac256drbg_context_st
        (md_ctx',
        (map Vint (map Int.repr V),
        (Vint (Int.repr reseed_counter),
        (Vint (Int.repr entropy_len),
        (Val.of_bool prediction_resistance, Vint (Int.repr reseed_interval)))))) ctx;
      md_full key md_ctx';
      data_at Tsh t_struct_mbedtls_md_info info_contents
        (hmac256drbgstate_md_info_pointer
           (md_ctx',
           (map Vint (map Int.repr V),
           (Vint (Int.repr reseed_counter),
           (Vint (Int.repr entropy_len),
           (Val.of_bool prediction_resistance, Vint (Int.repr reseed_interval)))))));
      spec_sha.K_vector kv)
  ).
  {
    replace_SEP 0 ((data_at Tsh (tarray tuchar entropy_len)
         (map Vint
            (map Int.repr entropy_bytes)) seed) * (data_at Tsh (tarray tuchar (384 - entropy_len))
         (list_repeat (Z.to_nat (384 - entropy_len)) (Vint Int.zero)) (offset_val (Int.repr entropy_len) seed))).
    {
      entailer!.
      simpl in H2.
      subst entropy_len.
      replace (384 - 32) with 352 by omega.
      apply derives_refl'; apply data_at_complete_split; auto.
      rewrite Hentropy_bytes_length.
      auto.
    }
    normalize.
    assert_PROP (isptr seed) as Hisptr by entailer!. destruct seed; try solve [inversion Hisptr]; change (offset_val (Int.repr entropy_len) (Vptr b i)) with (Vptr b (Int.add i (Int.repr entropy_len))).
    assert_PROP (field_compatible (Tarray tuchar (384 - entropy_len) noattr) 
          [] (Vptr b (Int.add i (Int.repr entropy_len)))) by entailer!.
    replace_SEP 1 ((data_at Tsh (tarray tuchar (Zlength contents))
         (list_repeat (Z.to_nat (Zlength contents)) (Vint Int.zero)) (Vptr b (Int.add i (Int.repr entropy_len)))) * (data_at Tsh (tarray tuchar (384 - entropy_len - Zlength contents))
         (list_repeat (Z.to_nat (384 - entropy_len - Zlength contents)) (Vint Int.zero)) (offset_val (Int.repr (Zlength contents)) (Vptr b (Int.add i (Int.repr entropy_len)))))).
    {
      simpl in H2; subst entropy_len.
      replace (384 - 32) with 352 by omega.
      remember (Vptr b (Int.add i (Int.repr 32))) as seed'.
      clear Heqseed'.
      entailer!.
      replace (length contents) with (Z.to_nat (Zlength contents)) by
        (rewrite Zlength_correct; apply Nat2Z.id).
      apply derives_refl'; apply data_at_complete_split; repeat rewrite Zlength_list_repeat; try omega; auto.
      {
        replace (Zlength contents + (352 - Zlength contents)) with (384 - 32) by omega.
        assumption.
      }
      {
        rewrite list_repeat_app.
        rewrite <- Z2Nat.inj_add; try omega.
        replace (Zlength contents + (352 - Zlength contents)) with 352 by omega.
        reflexivity.
      }
    }
    normalize.
    replace_SEP 0 (memory_block Tsh (Zlength contents) (Vptr b (Int.add i (Int.repr entropy_len)))).
    entailer!. replace (Zlength contents) with (sizeof cenv_cs (tarray tuchar (Zlength contents))) at 2. apply data_at_memory_block. simpl. rewrite Zmax0r by omega. omega.
    forward_call ((Tsh, Tsh), (Vptr b (Int.add i (Int.repr entropy_len))), additional, Zlength contents, map Int.repr contents).
    {
      (* type checking *)
      unfold lvar in H15.
      unfold eval_var.
      destruct (Map.get (ve_of rho) _seed); [|inversion H15].
      destruct p.
      destruct (eqb_type (tarray tuchar 384)); [|inversion H15].
      simpl. constructor.
    }
    {
      (* match up function parameter *)
      simpl in H2; rewrite H2.
      entailer!.
    }
    {
      (* match up SEP clauses *)
      change (fst (Tsh, Tsh)) with Tsh;
      change (snd (Tsh, Tsh)) with Tsh.
      change (@data_at spec_sha.CompSpecs Tsh (tarray tuchar (@Zlength Z contents))
         (@map int val Vint (@map Z int Int.repr contents)) additional) with (@data_at hmac_drbg_compspecs.CompSpecs Tsh (tarray tuchar (@Zlength Z contents))
         (@map int val Vint (@map Z int Int.repr contents)) additional).
      rewrite H1.
      cancel.
    }
    {
      (* prove the PROP clauses *)
      repeat split; auto; omega.
    }
    Intros memcpy_vret. subst memcpy_vret.
    forward.
    change (fst (Tsh, Tsh)) with Tsh;
    change (snd (Tsh, Tsh)) with Tsh.
    change (@data_at spec_sha.CompSpecs Tsh (tarray tuchar (@Zlength Z contents))
         (@map int val Vint (@map Z int Int.repr contents)) additional) with (@data_at hmac_drbg_compspecs.CompSpecs Tsh (tarray tuchar (@Zlength Z contents))
         (@map int val Vint (@map Z int Int.repr contents)) additional).
    gather_SEP 1 2.
    replace_SEP 0 (data_at Tsh (tarray tuchar (384 - entropy_len)) ((map Vint (map Int.repr contents)) ++ (list_repeat (Z.to_nat (384 - entropy_len - Zlength contents)) (Vint Int.zero))) (Vptr b (Int.add i (Int.repr entropy_len)))).
    {
      simpl in H2; subst entropy_len.
      replace (384 - 32) with 352 by omega.
      remember (Vptr b (Int.add i (Int.repr 32))) as seed'.
      clear Heqseed'.
      entailer!.
      apply derives_refl'; symmetry; apply data_at_complete_split; repeat rewrite Zlength_list_repeat; try omega; auto.
      change ((fix map (l : list int) : list val :=
               match l with
               | [] => []
               | a :: t => Vint a :: map t
               end) (map Int.repr contents)) with (map Vint (map Int.repr contents)).
      repeat rewrite Zlength_map.
      replace (Zlength contents + (352 - Zlength contents)) with 352 by omega.
      assumption.
    }
    change (Vptr b (Int.add i (Int.repr entropy_len))) with (offset_val (Int.repr entropy_len) (Vptr b i)). remember (Vptr b i) as seed; clear Heqseed.
    gather_SEP 2 0.
    replace_SEP 0 (data_at Tsh (tarray tuchar 384) ((map Vint
         (map Int.repr entropy_bytes)) ++ (map Vint (map Int.repr contents) ++
       list_repeat (Z.to_nat (384 - entropy_len - Zlength contents))
         (Vint Int.zero))) seed).
    {
      simpl in H2;
      subst entropy_len.
      replace (384 - 32) with 352 by omega.
      entailer!.
      apply derives_refl'; symmetry; apply data_at_complete_split; repeat rewrite Zlength_list_repeat; try omega; auto.
      rewrite Hentropy_bytes_length.
      rewrite Zlength_app; rewrite Zlength_list_repeat; repeat rewrite Zlength_map; try omega.
      replace (32 +
         (Zlength contents + (352 - Zlength contents))) with 384 by omega.
      assumption.
    }
    entailer!.
  }
  {
    forward.
    assert_PROP (contents = []).
    {
      destruct (eq_dec additional nullval). entailer!. destruct H20 as [contra H20']; inversion contra.
      destruct (eq_dec add_len 0). entailer!. destruct contents; [reflexivity|]. rewrite Zlength_correct in e; simpl in e. inversion e.
      rewrite H12 in Heqnon_empty_additional. inversion Heqnon_empty_additional.
    }
    subst contents.
    change (Zlength []) with 0.
    replace (384 - entropy_len - 0) with (384 - entropy_len) by omega.
    entailer!.
  }

  replace_SEP 0 (
    (data_at Tsh (tarray tuchar (entropy_len + Zlength contents)) (map Vint
            (map Int.repr entropy_bytes) ++
            map Vint (map Int.repr contents)) seed) *
    (data_at Tsh (tarray tuchar (384 - (entropy_len + Zlength contents))) (list_repeat (Z.to_nat (384 - entropy_len - Zlength contents))
            (Vint Int.zero)) (offset_val (Int.repr (entropy_len + Zlength contents)) seed))
      ).
  {
    simpl in H2; subst entropy_len.
    replace (384 - (32 + Zlength contents)) with (352 - Zlength contents) by omega.
    replace (384 - 32) with 352 by omega.
    rewrite app_assoc.
    entailer!.
    apply derives_refl'; apply data_at_complete_split; repeat rewrite Zlength_list_repeat; try omega; auto; rewrite Zlength_app; rewrite Hentropy_bytes_length; repeat rewrite Zlength_map; auto.
    replace (32 + Zlength contents + (352 - Zlength contents)) with 384 by omega.
    assumption.
  }
  normalize.

  do 2 rewrite map_map.
  rewrite <- map_app.
  rewrite <- map_map.

  forward_call ((entropy_bytes ++ contents), seed, (entropy_len + Zlength contents), ctx, (md_ctx',
        (map Vint (map Int.repr V),
        (Vint (Int.repr reseed_counter),
        (Vint (Int.repr entropy_len),
        (Val.of_bool prediction_resistance, Vint (Int.repr reseed_interval)))))), (HMAC256DRBGabs key V reseed_counter entropy_len prediction_resistance reseed_interval), kv, info_contents).
  {
    (* prove the SEP clauses match up *)
    unfold hmac256drbg_relate.
    entailer!.
  }
  {
    (* prove the PROP clauses *)
    simpl in H2; subst entropy_len.
    rewrite int_max_unsigned_eq.
    repeat split; try omega.
    {
      rewrite Zlength_app.
      repeat rewrite Zlength_map in Hentropy_bytes_length.
      rewrite Hentropy_bytes_length.
      change (Z.of_nat (Z.to_nat 32)) with 32.
      reflexivity.
    }
    {
      simpl; assumption.
    }
    {
      apply isbyteZ_app.
      eapply entropy.ENTROPY.get_bytes_isbyteZ; eauto. assumption.
    }
  }
  unfold hmac_drbg_update_post; normalize.

  gather_SEP 3 5.
  replace_SEP 0 (data_at Tsh (tarray tuchar 384) ((map Vint
         (map Int.repr entropy_bytes)) ++ (map Vint (map Int.repr contents) ++
       list_repeat (Z.to_nat (384 - entropy_len - Zlength contents))
         (Vint Int.zero))) seed).
  {
    simpl in H2;
    subst entropy_len.
    replace (384 - 32) with 352 by omega.
    replace (384 - (32 + Zlength contents)) with (352 - Zlength contents) by omega.
    rewrite app_assoc.
    rewrite map_map.
    rewrite map_app.
    rewrite <- map_map.
    replace (map (fun x : Z => Vint (Int.repr x)) contents) with (map Vint (map Int.repr contents)) by (rewrite map_map; auto).
    entailer!.
    apply derives_refl'; symmetry; apply data_at_complete_split; repeat rewrite Zlength_list_repeat; try omega; auto; rewrite Zlength_app; rewrite Hentropy_bytes_length; repeat rewrite Zlength_map; auto.
    replace (32 + Zlength contents + (352 - Zlength contents)) with 384 by omega.
    assumption.
  }
  
  (* ctx->reseed_counter = 1; *)
  forward.

  (* return 0 *)
  forward.

  unfold hmac_drbg_update_post.
  unfold hmac256drbgabs_to_state.
  Exists seed (Vint (Int.repr 0)).
  rewrite andb_negb_r.
  assert (HcontentsLength: Zlength contents >? 256 = false).
  {
    rewrite Z.gtb_ltb.
    apply Z.ltb_nlt.
    omega.
  }
  rewrite HcontentsLength.
  unfold HMAC_DRBG_update.HMAC_DRBG_update.
  idtac.
  replace (map (fun x : Z => Vint (Int.repr x)) contents) with (map Vint (map Int.repr contents)) by (rewrite map_map; auto).
  unfold hmac256drbg_relate.
  unfold get_stream_result.
  unfold entropy.get_entropy.
  rewrite <- Heqentropy_result.
  assert (Hnonempty_seed: exists hdSeed tlSeed, (entropy_bytes ++ contents) = hdSeed::tlSeed).
  {
    remember (entropy_bytes ++ contents) as seedContents.
    destruct seedContents as [|hdSeed tlSeed].
    {
      (* this case can't be true. case: seedContents = [] *)
      simpl in H2; subst entropy_len.
      assert (contra: Zlength (entropy_bytes ++ contents) = 0) by (rewrite <- HeqseedContents; reflexivity).
      rewrite Zlength_app in contra.
      repeat rewrite Zlength_map in Hentropy_bytes_length; rewrite Hentropy_bytes_length in contra.
      omega.
    }
    exists hdSeed. exists tlSeed.
    reflexivity.
  }
  unfold HMAC256_DRBG_functional_prog.HMAC256_DRBG_update.
  unfold HMAC_DRBG_update.HMAC_DRBG_update.
  destruct Hnonempty_seed as [hdSeed [tlSeed Hnonempty_seed]];
  rewrite Hnonempty_seed.
  entailer!.
  split; [apply hmac_common_lemmas.HMAC_Zlength| apply hmac_common_lemmas.isbyte_hmac].
Qed.
