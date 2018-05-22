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

(* index*)
Variable Ix : Type.

(* address *)
Variable Addr : Type.

(* currency value *)
Variable Coin : Type. 

(* key pair stand-in *)
(* what should this type be? *)
Variable Bk : Type.


(* DERIVED TYPES *)


(* transaction input *)
Definition TxIn : Type := TxId * Ix.

(* transaction output *)
Definition TxOut : Type := Addr * Coin.

(* collection of finite subsets of A represented by membership predicates *)
Definition Pow (A : Type) := A -> Prop.

(* finite map as a supsed of A*B *)  
Definition FinMap (A B : Type) := Pow (A*B).
(* note *)
(* should have the property that ((f (a, b)) /\ (f (a, c))) -> b=c *)

(* transaction *)
Definition Tx : Type := (Pow TxIn) * (FinMap Ix TxOut).

(* unspent transaction outputs represented as a finite map *)
Definition UTxO := FinMap TxIn TxOut.



(* sanity check examples where TxIn and TxOut are nat's *)
Definition add_coin_type : Type := nat*nat.

Definition utxo1 (inpf : TxIn = nat) (outpf : TxOut = add_coin_type): UTxO.
unfold UTxO. unfold FinMap. unfold Pow. rewrite inpf. rewrite outpf.
unfold add_coin_type. intro io. 
exact ((io = (0,(0,10))) \/ (io = (1,(0,15))) \/ (io = (2,(0,20))) 
  \/ (io = (3,(1,11))) \/ (io = (4,(2,12)))).
Defined.

Definition outs1 (outpf : TxOut = add_coin_type) : Pow TxOut.
unfold Pow. unfold add_coin_type in outpf. rewrite outpf. intro o. 
exact ((o=(0,10)) \/ (o=(1,11))).
Defined.

Definition ins1 (inpf : TxIn = nat) : Pow TxIn.
unfold Pow. rewrite inpf. intro i. exact ((i=1) \/ (i=4)).
Defined.

(* block *)
Definition Block := Pow Tx.

(* pending transactions *)
Definition Pending := Pow Tx.


(* FUNCTIONS *)

(* computes transaction id *)
(* hash map assumed to be the identity! *)
Variable txid : Tx -> TxId.

Variable get_tx_with_id : TxId -> Tx.

Axiom unique_id : forall tx, get_tx_with_id (txid tx) = tx.

Axiom unique_id_get : forall id, txid (get_tx_with_id id) = id.
 
(* addresses that belong to the wallet *)
Variable ours : Addr -> Bk.

(* FILTERED SETS *)

(* our addresses *)
Definition OurAddr (ours : Addr -> Prop) := { a : Addr | ours a}.

(* our unspent transaction outputs *)
Definition OurOuts (ours : Addr -> Prop) : Type := (OurAddr ours) * Coin.

(* OPERATIONS ON UTxO *)

(* OR for predicates *)
Definition orPP' (A : Type) (P P' : A -> Prop) := (fun a => (P a) \/ (P' a)).

(* NOT for predicates *)
Definition notP (A : Type) (P : A -> Prop) := (fun a => ~(P a)).

(* AND for predicates *)
Definition andPP' (A : Type) (P P' : A -> Prop) := (fun a => (P a) /\ (P' a)).

(* minus for predicates *)
Definition minusPP' (A : Type) (P P' : A -> Prop) := (fun a => (P a) /\ ~(P' a)).

(* domain of a utxo *)
Definition dom `{A : Type} `{B : Type} (f : FinMap A B) : Pow A.
  unfold FinMap in f. unfold Pow; unfold Pow in f. intro a.
  exact (exists b, f (a,b)).
Defined.

(* domain restriction *)
Definition d_r (ins : Pow TxIn ) (utxo : UTxO) :=
  fun io => utxo io /\ ins (fst io).

(* domain exclusion *)
Definition d_e (ins : Pow TxIn ) (utxo : UTxO) :=
  fun io => utxo io /\ ~(ins (fst io)).

(* range restriction *)
Definition r_r (utxo : UTxO) (outs : Pow TxOut) :=
  fun io => utxo io /\ outs (snd io). 

(* sanity checks assuming all primitive types are nat's *)
(* domain of utxo1 *)
Definition dom_utxo1 (A : Type) (Anat: A = nat) : Pow A.
rewrite Anat.
unfold Pow. intro i. exact ((i=0) \/ (i=1) \/ (i=2) \/ (i=3) \/ (i=4)).
Defined.

(* domain defined in dom_utxo1 is the same as computed by dom function *)
Definition dom_utxo1_pf (inpf : TxIn = nat) (outpf : TxOut = add_coin_type) :
  dom (utxo1 inpf outpf) = (dom_utxo1 TxIn inpf).
unfold dom. unfold dom_utxo1. unfold utxo1. unfold add_coin_type in outpf. 
rewrite inpf. rewrite outpf. 
apply functional_extensionality. intros. compute.
apply prf_prop_ex. split; intro. destruct H. inversion H;
try (inversion H0); auto;
try (inversion H1); auto;
try (inversion H2); auto;
try (inversion H3); auto. inversion H. exists (0, 10). rewrite H0. auto.
inversion H0. exists (0, 15). rewrite H1. auto.
inversion H1. exists (0, 20). rewrite H2. auto.
inversion H2. exists (1, 11). rewrite H3. auto.
rewrite H3. exists (2, 12). auto.
Qed.

(* ins 1 which maps to |-> (0,15) and 2 which maps to |-> (0,20) *)
Definition d_r_utxo1_ins12 (A : Type) (inpf : TxIn = nat) (outpf : TxOut = add_coin_type) : 
  TxIn * TxOut -> Prop.
unfold UTxO. rewrite inpf. rewrite outpf. unfold add_coin_type.
intro io. exact ((io = (1,(0,15))) \/ (io = (2,(0,20)))).
Defined.

(* domain restriction of utxo1 to ins 0 and 2 *)
Definition drutxo1_ins (inpf : TxIn = nat) : TxIn -> Prop.
rewrite inpf. exact (fun (i:nat) => ((i=1) \/ (i=2))).
Defined.

(* domain restriction defined by drutxo1_ins for ins defined by d_r_utxo1_ins12 
   the same as computed by d_r function *)
Definition d_r_utxo1_ins12_pf (inpf : TxIn = nat) (outpf : TxOut = add_coin_type) :
   (d_r_utxo1_ins12 TxIn inpf outpf) = (d_r (drutxo1_ins inpf) (utxo1 inpf outpf)).
unfold UTxO. unfold d_r_utxo1_ins12. unfold d_r. unfold drutxo1_ins.
apply functional_extensionality. unfold utxo1. rewrite inpf. rewrite outpf.
unfold add_coin_type. intro. apply prf_prop_ex. split; compute; intro. 
inversion H. rewrite H0. tauto. destruct x. rewrite H0. inversion H0. tauto.
destruct x. destruct H. inversion H0. rewrite H1. rewrite H1 in H. 
apply or_introl. assert (~((1, p) = (0, (0, 10)))). intro. inversion H2.
assert (~((1, p) = (2, (0, 20)))). intro. inversion H3.
assert (~((1, p) = (3, (1, 11)))). intro. inversion H4.
assert (~((1, p) = (4, (2, 12)))). intro. inversion H5. tauto.
rewrite H1. rewrite H1 in H.
apply or_intror. assert (~((2, p) = (0, (0, 10)))). intro. inversion H2.
assert (~((2, p) = (1, (0, 15)))). intro. inversion H3.
assert (~((2, p) = (3, (1, 11)))). intro. inversion H4.
assert (~((2, p) = (4, (2, 12)))). intro. inversion H5. tauto.
Defined.

(* subset of outputs in a UTxO, ie. forall io : (tx,i) |-> (addr, c) \in u , io \in v *)
Definition UTxOinc (u v : UTxO) : Prop :=
  forall (io : _), u io -> v io.

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
  d_r ins (orPP' _ u v) = orPP' _ (d_r ins u) (d_r ins v).
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

(* AUXILIARY OPERATIONS *)


Definition txins (txs : Pow Tx) : (Pow TxIn) := 
  fun (t_in : TxIn) => (exists (b : (FinMap Ix TxOut)), 
    exists (t_in_set : Pow TxIn), txs (t_in_set, b)).
