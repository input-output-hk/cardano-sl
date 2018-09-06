Require Import Coq.Lists.List.
Require Import Coq.Logic.Classical_Prop.
Require Import Decidable.
Require Import Coq.Bool.Bool.
Require Import Coq.Init.Logic.
Require Import Coq.Logic.ClassicalFacts.
Require Import Coq.Logic.FunctionalExtensionality.

Generalizable All Variables.


Open Scope list_scope.

(* Proposititonal extensionality, ie. equivalent propositions are equal (found in the ClassicalFacts std lib) *)
Axiom prf_prop_ex : prop_extensionality .

(* PRIMITIVE TYPES *)

(* transaction id *)
Variable TxId : Type.

Variable id1 : TxId.
Variable id2 : TxId.
Variable id3 : TxId.
Variable id4 : TxId.
Variable id5 : TxId.


(* transaction id's 1 to 3 are distinct *)
Variable txid_distinct : 
~(id1 = id2) /\ ~(id1 = id3) /\ ~(id2 = id3).

(* index*)
Variable Ix : Type.

Variable ix1 : Ix.
Variable ix2 : Ix.
Variable ix3 : Ix.
Variable ix4 : Ix.
Variable ix5 : Ix.

(* address *)
Variable Addr : Type.

Variable a1 : Addr.
Variable a2 : Addr.
Variable a3 : Addr.
Variable a4 : Addr.
Variable a5 : Addr.


(* currency value *)
Definition Coin := nat.


(* DERIVED TYPES *)


(* transaction input *)
Definition TxIn : Type := TxId * Ix.

(* transaction output *)
Definition TxOut : Type := Addr * Coin.

(* collection of finite subsets of A represented by membership predicates *)
Definition Pow (A:Type) := A -> Prop.


(* finite map as a supsed of A*B *)  
Definition FinMap (A B : Type) := @Pow (A*B).

(* a finite map A to B is well-defined whenever for each input there is a single output *)
Definition well_defined (A B: Type) (f : FinMap A B) := 
  (forall a b c, ((f (a, b)) /\ (f (a, c))) -> b=c).


(* transaction *)
Definition Tx : Type := (Pow TxIn) * (FinMap Ix TxOut).

(* unspent transaction outputs represented as a finite map *)
Definition UTxO := FinMap TxIn TxOut.

(* NOTE: an arbitrary UTxO's are assumed to be well-defined for now! *)


(* sanity check examples where TxIn and TxOut are nat's *)
Definition add_coin_type : Type := nat*nat.

(* finite map representing utxo1 is well-defined *)
Example utxo1_well_def : forall (a : TxIn) (b c : TxOut),
((a, b) = (id1, ix1, (a1, 10)) \/
 (a, b) = (id2, ix2, (a2, 20)) \/ (a, b) = (id3, ix3, (a3, 30))) /\
((a, c) = (id1, ix1, (a1, 10)) \/
 (a, c) = (id2, ix2, (a2, 20)) \/ (a, c) = (id3, ix3, (a3, 30))) ->
b = c.
Proof.
  Admitted.

(* utxo1 example *)
Example utxo1: UTxO :=
 (fun io => 
     io = ((id1, ix1), (a1, 10))
  \/ io = ((id2, ix2), (a2, 20))
  \/ io = ((id3, ix3), (a3, 30))).



(* block *)
Definition Block := Pow Tx.

(* pending transactions *)
Definition Pending := Pow Tx.


(* FUNCTIONS *)


(* hash map assumed to be the identity in the following module! *)
Module Type TxTxId_identity.

Axiom TxTxId_identity_assm : Tx = TxId.

(* computes transaction id *)
Definition txid (tx : Tx): TxId.
  rewrite <- TxTxId_identity_assm. exact tx.
Defined.

(* fetches transaction with given txid - defined here because Tx = TxId *)
Definition get_tx (txid : TxId): Tx.
  rewrite TxTxId_identity_assm. exact txid.
Defined.

(* allows to treat a tx as a txid *)
Coercion txid : Tx >-> TxId.

(* allows to treat a txid as a tx *)
Coercion get_tx : TxId >-> Tx.

End TxTxId_identity.

 

Module WithIdHash (TxTxId_id : TxTxId_identity).

(* NOTE: hash map is the identity is a proof obligation in this module *)
Import TxTxId_id.

(* FILTERED SETS *)

(* our addresses *)
Definition OurAddr := Pow Addr.

(* our unspent transaction outputs *)
Definition OurOuts (ours : OurAddr) := 
  fun (txout:TxOut) => (ours (fst txout)).

(* OPERATIONS ON UTxO *)

(* OR for predicates *)
Definition orPP' (A : Type) (u v : A -> Prop) :=
  (fun a => (u a) \/ (v a)).

(* NOT for predicates *)
Definition notP (A : Type) (P : A -> Prop) := (fun a => ~(P a)).

(* AND for predicates *)
Definition andPP' (A : Type) (P P' : A -> Prop) := (fun a => (P a) /\ (P' a)).

(* minus for predicates *)
Definition minusPP' (A : Type) (P P' : A -> Prop) := (fun a => (P a) /\ ~(P' a)).

(* domain of a utxo *)
Definition dom `{A : Type} `{B : Type} (f : FinMap A B) : Pow A := fun a => exists b : B, f (a, b).

(* any subset of a UTxO is still a finite map *)
Definition UTxO_r (p p' : Prop) (utxo : UTxO) :
  (forall a b c, (((utxo (a, b))) /\ ((utxo (a, c))) -> b=c))  ->
  (forall a b c, (((utxo (a, b)) /\ p) /\ ((utxo (a, c)) /\ p')-> b=c)).
Proof.
  intros. apply (H a b c). tauto.
Qed.

(* domain restriction *)
(* needs to be finite map *)
Definition d_r (ins : Pow TxIn ) (utxo : UTxO) : UTxO :=
  (fun io => (utxo io) /\ ins (fst io)).  

(* domain exclusion *)
Definition d_e (ins : Pow TxIn ) (utxo : UTxO) : UTxO :=
  (fun io => utxo io /\ ~(ins (fst io))).


(* range restriction *)
Definition r_r (utxo : UTxO) (outs : Pow TxOut) : UTxO :=
  (fun io => utxo io /\ outs (snd io)).

(* sanity checks assuming all primitive types are nat's *)
(* domain of utxo1 *)
Example dom_utxo1 : Pow TxIn := fun i =>
     i = (id1, ix1)
  \/ i = (id2, ix2)
  \/ i = (id3, ix3).

(* domain defined in dom_utxo1 is the same as computed by dom function *)
Fact dom_utxo1_pf :
  dom utxo1 = dom_utxo1.
Proof.
  unfold dom ; unfold dom_utxo1 ; unfold utxo1.
  apply functional_extensionality ; intro i.
  apply prf_prop_ex ; split ; intro H.
  inversion H as [b Hb] ; inversion Hb as [Hb' | [Hb' | Hb']] ; inversion Hb' ; auto.
  inversion H as [Hi | [Hi | Hi]] ; rewrite Hi; simpl;  eauto.
Qed.



(* ins (id1, ix1) which maps to |-> (a1, 10) and (id3, ix3) which maps to |-> (a3, 30) *)
Example d_r_utxo1_ins12 : UTxO :=
  (fun io => (io = ((id1, ix1), (a1, 10)) \/ (io = ((id3, ix3), (a3, 30))))).



(* domain restriction of utxo1 to ins 0 and 3 *)
Example drutxo1_ins : Pow TxIn :=
 (fun i => ((i=(id1, ix1)) \/ (i=(id3, ix3)))).


(* domain restriction defined by drutxo1_ins for ins defined by d_r_utxo1_ins12 
   the same as computed by d_r function *)
Example d_r_utxo1_ins12_pf :
   d_r_utxo1_ins12 = (d_r drutxo1_ins utxo1).
Proof.
  compute. apply functional_extensionality. intro. apply prf_prop_ex. destruct x; simpl.
  split; try split; try tauto.
  inversion H; try (inversion H0); try tauto. intro.
  inversion H as [[H1 | [H2 | H2']] [H3 | H4]]; try tauto; 
  destruct txid_distinct; inversion H2; destruct p; 
  inversion H5 as [(H7,H8)]; try (inversion H3 as [(H9, H10)]);
  try (inversion H4 as [(H9, H10)]);
  rewrite H9 in H7; try tauto; symmetry in H7; try tauto.
Qed.

(* subset of outputs in a UTxO, ie. forall io : (tx,i) |-> (addr, c) \in u , io \in v *)
Definition UTxOinc (u v : UTxO) : Prop :=
  forall io, u io -> v io.

(* subset of the transactions in the UTxO *)
Definition S_UTxOinc (u v : UTxO) : Prop :=
  (UTxOinc u v) /\
  (forall io io', u io -> v io' -> (fst io = fst io' -> u io')).

  

(* PROPERTIES OF UTxO OPERATIONS *)


(* Lemma says x in utxo implies fst x is in the domain of x *)
Lemma fstx_in_dom (u : UTxO) : forall x, (u x -> (exists b : TxOut, u (fst x, b))).
  intro. exists (snd x). destruct x. simpl. auto.
Qed.


(* tactic for proving properties 1 to 9*)
Ltac prove_prop := 
  unfold Pow; unfold orPP'; unfold d_r; unfold UTxOinc;
  unfold andPP'; unfold d_e; unfold r_r;
  unfold minusPP'; unfold dom; simpl; intros; try tauto;
  try (apply functional_extensionality); try intros; apply prf_prop_ex; try tauto. 


(* property 1 *)
Definition UTxO_prop1 : forall (ins : Pow TxIn) (u : UTxO),
  UTxOinc (d_r ins u) u.
Proof.
  prove_prop.
Qed.

(* property 2 *)
Definition UTxO_prop2 : forall (u : UTxO) (outs : Pow TxOut),
  UTxOinc (r_r u outs) u.
Proof.
  prove_prop.
Qed.

(* property 3 *)
Definition UTxO_prop3 : forall (ins : Pow TxIn) (u : UTxO),
  UTxOinc (d_e ins u) u.
Proof.
  prove_prop.
Qed.

(* property 4 *)
Definition UTxO_prop4 : forall (ins : Pow TxIn) (u v : UTxO),  
  (d_r ins (orPP' _ u v)) = orPP' _ (d_r ins u) (d_r ins v).
Proof.
  prove_prop.
Qed.

(* property 5 *)
Definition UTxO_prop5 : forall (ins : Pow TxIn) (u v : UTxO),  
  d_e ins (orPP' _ u v) = (orPP' _ (d_e ins u) (d_e ins v)).
Proof.
  prove_prop.
Qed.

(* property 6 *)
Definition UTxO_prop6 : forall (ins : Pow TxIn) (u v : UTxO), 
  (d_r (andPP' _ (dom u) ins) u) = (d_r ins u).
Proof.
  prove_prop. generalize (fstx_in_dom u x); intro; try tauto.
Qed.

(* property 7 *)
Definition UTxO_prop7 : forall (ins : Pow TxIn) (u v : UTxO), 
  (d_e (andPP' _ (dom u) ins) u) = (d_e ins u).
Proof.
    prove_prop. generalize (fstx_in_dom u x); intro; try tauto.
Qed.

(* property 8 *)
Definition UTxO_prop8 : forall (ins : Pow TxIn) (u v : UTxO), 
  (d_e (orPP' _ (dom u) ins) (orPP' _ u v)) = (d_e (orPP' _ (dom u) ins) v).
Proof.
    prove_prop. generalize (fstx_in_dom u x); intro; try tauto.
Qed.

(* property 9 *)
Definition UTxO_prop9 : forall (ins : Pow TxIn) (u : UTxO), 
  (d_e ins u) = (d_r (minusPP' _ (dom u) ins) u).
Proof.
    prove_prop. generalize (fstx_in_dom u x); intro; try tauto.
Qed.

(* property 10 added *)
Definition UTxO_prop10 : forall (ins : Pow TxIn) (u : UTxO), 
  (orPP' (TxIn * TxOut) (d_e ins u) (d_r ins u)) = u.
Proof. 
  prove_prop.
Qed.

(* property 11 added *)
Definition UTxO_prop11 : forall (ins : Pow TxIn) (u : UTxO), 
  andPP' TxIn (dom (d_e ins u)) (dom (d_r ins u)) = (fun _ : TxIn => False).
Proof.
  prove_prop. split; intro H. 
  destruct H as [[(H1, H1')] [(H2, H2')]]. tauto.
  tauto. 
Qed.

(* AUXILIARY OPERATIONS *)


Definition txins (txs : Pow Tx) : (Pow TxIn) := 
  fun (t_in : TxIn) => (exists (b : (FinMap Ix TxOut)), 
    exists (t_in_set : Pow TxIn), txs (t_in_set, b)).


Definition txouts (txs : Pow Tx) : UTxO :=
  fun (io : TxIn * TxOut) => txs (get_tx (fst (fst io))) /\
    ((snd (get_tx (fst (fst io)))) (snd (fst io), snd io)).

Open Scope nat_scope.

(* subtraction lemma *)
Lemma subt_comp (a b c:nat) : (a + b = c -> a = c - b).
Proof.
  Admitted.
(*
  induction b.
(* case 0 *) replace (a+0) with a. 
  replace (c-0) with c. tauto. compute.
*)

Variable balance : UTxO -> Coin.

(* use property 2.6.1 as the invariant property defining balance *) 
Variable balance_calc :  (balance (fun u => False) = 0) /\
  (forall u v, (andPP' TxIn (dom u) (dom v) = (fun _ : TxIn => False) ->
  balance (orPP' (TxIn * TxOut) u v) = balance u + balance v)).



(* define balance calculation for a UTxO presented as a list *)
Fixpoint coin_sum (utxo_list : list (TxIn*TxOut)) : Coin :=
  match utxo_list with 
  | nil => 0
  | u :: l => (snd (snd u)) + (coin_sum l)
  end.

(* cannot prove this propery directly by induction from the coin_sum and balance_calc definitions *)
(* the balance of a utxo represented by the list a::utxo_list is the balance of the utxo 
   excluding a plus the coin value of a *)
Variable balance_with_a : forall (utxo_list : list (TxIn*TxOut)) a (utxo:UTxO), 
  utxo a -> balance utxo = snd (snd a) + coin_sum utxo_list.


(* any balance calculation for a utxo satisfying the balance_calc invariant property must coincide
  with the coin_sum balace calculation of this utxo presented as a list *)
Lemma coin_sum_balance :
  forall (utxo : UTxO) (l : list (TxIn*TxOut)), (forall t, utxo t <-> In t l) -> 
    coin_sum l = (balance utxo).
Proof.
  intros. induction l; simpl; simpl in H; destruct balance_calc as [b0 b_c]. 
(* empty wallet *)
  replace utxo with (fun (u:TxIn * TxOut) => False).
  auto. apply functional_extensionality.
  intro x. symmetry. apply prf_prop_ex. exact (H x).
(* a :: l wallet *)
  symmetry. apply balance_with_a. apply H. tauto.
Qed.

(* dependence *)
Definition depends (t2 t1 : Tx) :=  
  exists ix, (txins (fun t => t=t2)) (txid t1, ix).

(* set of independent transactions *)
Definition independednt (txs : Pow Tx) :=
  andPP' _ (txins txs) (dom (txouts txs)) = (fun t => False).



(* property 2.6.2' *)
Definition balance_prop2 (u : UTxO) (ins : Pow TxIn) :
  balance (d_e ins u) = (balance u) -(balance (d_r ins u)).
Proof.
  destruct balance_calc as [b0 b_c]. generalize (b_c (d_e ins u) (d_r ins u)).
  simpl. intro. apply subt_comp. rewrite <- H. 
  rewrite UTxO_prop10. auto.
  apply UTxO_prop11.
Qed.
