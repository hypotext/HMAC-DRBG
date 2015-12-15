Require Import Coqlib.
Require Import List. Import ListNotations.
Require Import Coq.Logic.FunctionalExtensionality.

Require Import sha.ByteBitRelations.
Require Import sha.general_lemmas.

Module Type ABSTRACT_ENTROPY.

Parameter stream: Type.

Inductive error_code: Type :=
| catastrophic_error
| generic_error.

Inductive result X: Type: Type :=
| success: X -> stream -> @result X
| error : error_code -> stream -> @result X.

Arguments success {X} _ _. 
Arguments error {X} _ _. 

Parameter get_bytes: nat -> stream -> result (list Z).
Parameter get_bits: nat -> stream -> result (list bool).

Parameter get_bits_length:
  forall k s b s',
    success b s' = get_bits k s ->
    length b = k.
Parameter get_bytes_length:
  forall k s b s',
    success b s' = get_bytes k s ->
    length b = k.
Parameter get_bytes_Zlength:
  forall k s b s',
    k >= 0 ->
    success b s' = get_bytes (Z.to_nat k) s ->
    Zlength b = k.
Parameter get_bytes_isbyteZ:
  forall k s b s',
    success b s' = get_bytes k s ->
    Forall isbyteZ b.
End ABSTRACT_ENTROPY.

Module OPTIONAL_ENTROPY <: ABSTRACT_ENTROPY.

Definition stream: Type := nat -> option bool.

Inductive error_code: Type :=
| catastrophic_error
| generic_error
.

Inductive result X: Type: Type :=
| success: X -> stream -> @result X
| error : error_code -> stream -> @result X
.

Arguments success {X} _ _. 
Arguments error {X} _ _. 

Fixpoint get_bits (k: nat) (s: stream): result (list bool) :=
  match k with
    | O => success [] s
    | S k' =>
      match get_bits k' s with (* recursive call -- get the (k-1) bits *)
      | error e s' => error  e s'
      | success b s' =>
        match s' O with (* get the first bit *)
        | None =>
          (* what does the caller do on error? call again? *)
          error catastrophic_error (fun i => match nat_compare i k' with
                                             | Lt => s i
                                             | Eq | Gt => s (1 + i)%nat
                                             end)
        | Some e => success (b ++ [e]) (fun i => s (k + i)%nat)
        (* not 1+i? (otherwise it'll be moving it 1 + 2 + 3 forward instead of 3 forward. not ::? *)
        end
      end
  end.

Example get_bits_test1:
  forall bits s output s',
    bits = [Some false; Some true; Some false; Some false] ->
    s = (fun i => nth i bits None) ->
    success output s' = get_bits (length bits) s ->
    s' = (fun i => s (length bits + i)%nat) /\ output = [false; true; false; false].
Proof.
  intros.
  subst.
  inv H1.
  split.
  extensionality i. destruct i; reflexivity.
  reflexivity.
Qed.

Example get_bits_test2:
  forall bits s output s' s'',
    bits = [Some false; None; Some true; Some false; Some false] ->
    s = (fun i => nth i bits None) ->
    error catastrophic_error s' = get_bits 4%nat s ->
    success output s'' = get_bits 4%nat s' ->
    s'' = (fun i => s (length bits + i)%nat) /\ output = [false; true; false; false].
Proof.
  intros.
  subst.
  inv H1.
  inv H2.
  split.
  extensionality i. reflexivity.
  reflexivity.
Qed.

Example get_bits_test3:
  forall bits s output s' s'',
    bits = [None; Some false; Some true; Some false; Some false] ->
    s = (fun i => nth i bits None) ->
    error catastrophic_error s' = get_bits 4%nat s ->
    success output s'' = get_bits 4%nat s' ->
    s'' = (fun i => s (length bits + i)%nat) /\ output = [false; true; false; false].
Proof.
  intros.
  subst.
  inv H1.
  inv H2.
  split.
  extensionality i. reflexivity.
  reflexivity.
Qed.

Example get_bits_test4:
  forall bits s output s' s'',
    bits = [Some false; Some true; Some false; None; Some false] ->
    s = (fun i => nth i bits None) ->
    error catastrophic_error s' = get_bits 4%nat s ->
    success output s'' = get_bits 4%nat s' ->
    s'' = (fun i => s (length bits + i)%nat) /\ output = [false; true; false; false].
Proof.
  intros.
  subst.
  inv H1.
  inv H2.
  split.
  extensionality i. reflexivity.
  reflexivity.
Qed.
(*
Fixpoint get_bits_concrete (k: nat) (s: stream) (max: nat): @result (list bool) :=
  match k with
    | O => success [] s
    | S k' =>
      match s (max - k)%nat with
        | None => error catastrophic_error (fun i => s (1 + i)%nat)
        | Some bit =>
          match get_bits_concrete k' s max with
            | error e s' => error e (fun i => match nat_compare i (max - k)%nat with
                                                  | Gt => s' i
                                                  | Eq | Lt => s i
                                     end)
            | success b s' => success (bit::b) (fun i => s (k + i)%nat)
          end
      end
  end
.
Lemma get_bits_concrete_correct:
  forall k s, get_bits k s = get_bits_concrete k s k.
Proof.
  intros k.
  induction k as [|k']; [reflexivity|].
  intros s.
  simpl.
  rewrite IHk'. clear IHk'.
  replace (k' - k')%nat with O by omega.
  remember (s O) as sO.
  destruct sO.
  {
    (* s 0 <> None *)
    (*
    remember (get_bits_concrete k' s k') as result.
    destruct result as [string s' | e s'].
    {
      remember (get_bits_concrete k' s (S k')) as result2.
      destruct result2 as [string2 s'2 | e2 s'2].
      {
        (* case where both result return success *)
        generalize dependent s.
        induction k' as [|k'']; intros.
        simpl in *. inv Heqresult. inv Heqresult2. rewrite <- HeqsO. auto.
        simpl in *. replace (k'' - k'')%nat with O in * by omega. rewrite <- HeqsO in *.
        rewrite IHk''.
      }
      unfold get_bits_concrete.
    }
*)
    induction k' as [|k''].
    {
      simpl. rewrite <- HeqsO.
      reflexivity.
    }
    simpl.
    replace (k'' - k'')%nat with O by omega.
    rewrite <- HeqsO in *.
    replace (match k'' with
             | 0%nat => S k''
             | S l => (k'' - l)%nat
             end) with 1%nat by (destruct k''; omega).
    
  }
  {
    destruct k' as [|k''].
    {
      simpl. rewrite <- HeqsO.
      replace (fun i : nat =>
      match nat_compare i 0 with
      | Eq => s (S i)
      | Lt => s i
      | Gt => s (S i)
      end) with (fun i => s (S i)); [reflexivity|].
      extensionality i.
      destruct i as [|i']; reflexivity.
    }
    simpl.
    replace (k'' - k'')%nat with O by omega; rewrite <- HeqsO.
    reflexivity.
  }
  simpl.
  *)

Fixpoint get_first_None (limit:nat) (s: stream): nat :=
  match limit with
    | O => O (* bogus *)
    | S limit' =>
      match s O with
        | None => O
        | Some _ => S (get_first_None limit' (fun i => s (S i)))
      end
  end
.


Lemma get_bits_stream_success:
  forall k s b s',
    success b s' = get_bits k s ->
    s' = fun i => s (k + i)%nat.
Proof.
  intros k.
  destruct k as [|k'].
  {
    simpl. intros. inv H. extensionality i; reflexivity.
  }
  simpl.
  intros.
  destruct (get_bits k' s); try solve [inversion H].
  destruct (s0 0%nat); try solve [inversion H].
  inv H.
  reflexivity.
Qed.

Lemma get_bits_stream_success_Some:
  forall k s b s',
    success b s' = get_bits k s ->
    (forall i, (i < k)%nat -> s i <> None).
Proof.
  intros k.
  induction k as [|k']; intros. omega.
  simpl in H.
  remember (get_bits k' s) as get_bits_k'_s; destruct get_bits_k'_s; try solve [inversion H].
  remember (s0 0%nat) as s0_0; destruct s0_0; try solve [inversion H].
  inv H.
  pose proof Heqget_bits_k'_s as IHsimpl.
  pose proof (Heqget_bits_k'_s) as Hstream.
  apply get_bits_stream_success in Hstream; subst.
  destruct (le_lt_eq_dec i k'). omega.
  {
    (* i < k', use inductive case *)
    eapply IHk'; eassumption.
  }
  {
    (* i = k', use known info *)
    subst.
    replace (k' + 0)%nat with k' in Heqs0_0 by omega.
    intros contra; rewrite <- Heqs0_0 in contra; inversion contra.
  }  
Qed.

Lemma get_first_None_limit:
  forall k s i,
    i = get_first_None k s ->
    (i <= k)%nat.
Proof.
  induction k as [|k']; intros. simpl in H. omega.
  simpl in H.
  remember (s O) as s_O; destruct s_O.
  destruct i as [|i']; try solve [omega].
  inversion H.
  pose proof (IHk' (fun i => s (S i)) i').
  subst; omega.
  subst; omega.
Qed.

(* TODO convert this to use get_first_None_limit *)
Lemma get_bits_stream_error:
  forall k s e s',
    error e s' = get_bits k s ->
    (exists i, (i < k)%nat /\ (forall i', s' i' = match nat_compare i' i with
                                   | Lt => s i'
                                   | Eq | Gt => s (S i')
                                 end) /\ s i = None /\ (forall i', (i' < i)%nat -> s i' <> None)).
Proof.
  intros k.
  induction k as [|k']; intros. inversion H.
  simpl in H.
  remember (get_bits k' s) as get_bits_k'_s; destruct get_bits_k'_s.
  {
    (* get_bits k' s = success *)
    pose proof (Heqget_bits_k'_s) as Hstream.
    apply get_bits_stream_success in Hstream; subst.
    replace (k' + 0)%nat with k' in H by omega.
    remember (s k') as s_k'; destruct s_k'; inv H.
    exists k'.
    repeat split; auto.
    intros.
    eapply get_bits_stream_success_Some; eassumption.
  }
  {
    (* get_bits k' s = error *)
    pose proof (IHk' s e0 s0 Heqget_bits_k'_s) as IHsimpl.
    destruct IHsimpl.
    destruct (le_lt_dec k' x).
    {
      (* x >= k' *)
      destruct H0. omega.
    }
    {
      exists x.
      inv H.
      destruct H0.
      split; auto.
    }
  }
Qed.

Lemma get_bits_length:
  forall k s b s',
    success b s' = get_bits k s ->
    length b = k.
Proof.
  intros k.
  induction k as [|k'].
  {
    intros.
    inv H.
    reflexivity.
  }
  intros until s'.
  simpl.
  intros H.
  remember (get_bits k' s) as result.
  destruct result; [|inversion H].
  remember (s0 0%nat) as s0_0.
  destruct s0_0; try solve [inversion H].
  inv H.
  rewrite app_length.
  simpl. replace (length l + 1)%nat with (S (length l)) by omega.
  rewrite IHk' with (s':=s0) (s:=s); auto.
Qed.

Definition get_bytes (k: nat) (s: stream): result (list Z) :=
  match get_bits (8 * k)%nat s with
    | success bits s' => success (bitsToBytes bits) s'
    | error e s' => error e s'
  end
.

Lemma get_bytes_length:
  forall k s b s',
    success b s' = get_bytes k s ->
    length b = k.
Proof.
  unfold get_bytes; intros.
  remember (get_bits (8 * k)%nat s) as get_bits_s. 
  destruct get_bits_s; inv H.
  assert (length l = (8 * k)%nat) by (apply get_bits_length with (s:=s) (s':=s0); auto).
  apply bitsToBytes_len_gen.
  omega.
Qed.

Lemma get_bytes_Zlength:
  forall k s b s',
    k >= 0 ->
    success b s' = get_bytes (Z.to_nat k) s ->
    Zlength b = k.
Proof.
  intros.
  rewrite Zlength_correct.
  erewrite get_bytes_length.
  rewrite Z2Nat.id; [reflexivity | omega].
  eauto.
Qed.

Lemma get_bytes_isbyteZ:
  forall k s b s',
    success b s' = get_bytes k s ->
    Forall isbyteZ b.
Proof.
  intros.
  unfold get_bytes in H.
  destruct (get_bits (8 * k) s); inv H.
  eapply bitsToBytes_isbyteZ; reflexivity.
Qed.

End OPTIONAL_ENTROPY.

Module ENTROPY := OPTIONAL_ENTROPY.

Definition get_entropy (security_strength min_length max_length: Z) (prediction_resistance: bool) s := ENTROPY.get_bytes (Z.to_nat min_length) s.
