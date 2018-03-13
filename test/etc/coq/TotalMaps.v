(*
Software Foundations: Maps
*)

Require Import Coq.Arith.Arith.
Require Import Coq.Bool.Bool.
Require Import Coq.Strings.String.
Require Import Coq.Logic.FunctionalExtensionality.

Inductive id : Type :=
  | Id : string -> id.

Definition beq_id x y :=
  match x,y with
    | Id n1, Id n2 => if string_dec n1 n2 then true else false
  end.


Theorem beq_id_refl : forall id, true = beq_id id id.
Proof.
  intros [n]. simpl. destruct (string_dec n n).
  - reflexivity.
  - destruct n0. reflexivity.
Qed.

Theorem beq_id_true_iff : forall x y : id,
    beq_id x y = true <-> x = y.
Proof.
  intros [n1] [n2].
  unfold beq_id.
  destruct (string_dec n1 n2).
  - subst. split. reflexivity. reflexivity.
  - split.
    + intros contra. inversion contra.
    + intros H. inversion H. subst. destruct n. reflexivity.
Qed.

Theorem beq_id_false_iff : forall x y : id,
    beq_id x y = false <-> x <> y.
Proof.
  intros x y. rewrite <- beq_id_true_iff.
  rewrite not_true_iff_false. reflexivity.
Qed.

Theorem false_beq_id : forall x y : id,
    x <> y -> beq_id x y = false.
Proof.
  intros x y. rewrite beq_id_false_iff.
  intros H. apply H.
Qed.

Definition total_map (A:Type) := id -> A.

Definition t_empty {A:Type} (v : A) : total_map A :=
  (fun _ => v).

Definition t_update {A:Type} (m : total_map A)
           (x : id) (v : A) :=
  fun x' => if beq_id x x' then v else m x'.

(* Some examples of map usage *)
Definition examplemap :=
  t_update (t_update (t_empty false) (Id "foo") false)
           (Id "bar") true.

Example update_example1 : examplemap (Id "baz") = false.
Proof. reflexivity. Qed.

Example update_example2 : examplemap (Id "bar") = true.
Proof. reflexivity. Qed.

Example update_example3 : examplemap (Id "foo") = false.
Proof. reflexivity. Qed.

(* Properties of total maps. *)

(* t_empty returns the default element for all inputs. *)
Lemma t_apply_empty: forall A x v, @t_empty A v x = v.
Proof.
  intros A x v. unfold t_empty. reflexivity.
Qed.

(* Updating map m at key x with value v and then looking up x results in v. *)
Lemma t_update_eq : forall A (m: total_map A) x v,
  (t_update m x v) x = v.
Proof.
  intros A m x f. unfold t_update.
  rewrite <- beq_id_refl.
  reflexivity.
Qed.

(* Updating map m at key x then looking up a different key, x', gives the same
result as looking up x' in m. *)
Theorem t_update_neq : forall (X:Type) v x1 x2
                              (m : total_map X),
  x1 <> x2 -> (t_update m x1 v) x2 = m x2.
Proof.
  intros X v x1 x2 m.
  unfold t_update.
  rewrite <- beq_id_false_iff.
  intro H. rewrite H.
  reflexivity.
Qed.

(* Updating map m at key x with value v1, then updating x with another value v2,
the resulting map behaves the same as the map from just applying the second
update. *)

Lemma t_update_shadow : forall A (m: total_map A) v1 v2 x,
    t_update (t_update m x v1) x v2
  = t_update m x v2.
Proof.
  intros A m v1 v2 x. apply functional_extensionality. intros x'.
  unfold t_update.
  destruct (beq_id x x'); reflexivity.
Qed.

Lemma beq_idP : forall x y, reflect (x = y) (beq_id x y).
Proof.
  intros x y. apply iff_reflect.
  rewrite <- beq_id_true_iff.
  reflexivity.
Qed.

(* Updating map m to assign key x same value as it already has, results in the
same map, m. *)
Theorem t_update_same : forall X x (m : total_map X),
  t_update m x (m x) = m.
Proof.
  intros X x m. apply functional_extensionality. intros x'.
  unfold t_update.
  destruct (beq_idP x x') as [ H | H ].
  - rewrite H. reflexivity.
  - reflexivity.
Qed.

(* When updating map m at two distinct keys, the order of updates doesn't
matter. *)
Theorem t_update_permute : forall (X:Type) v1 v2 x1 x2
                                  (m : total_map X),
    x2 <> x1 ->
    (t_update (t_update m x2 v2) x1 v1)
  = (t_update (t_update m x1 v1) x2 v2).
Proof.
  intros X v1 v2 x1 x2 m H.
  apply functional_extensionality. intros x'.
  unfold t_update.
  destruct (beq_idP x1 x') as [ H1 | H1 ].
  - subst. rewrite <- beq_id_false_iff in H. rewrite H. reflexivity.
  - reflexivity.
Qed.


(* Definition of Partial Maps. *)

Definition partial_map (A:Type) := total_map (option A).

Definition empty {A:Type} : partial_map A :=
  t_empty None.

Definition update {A:Type} (m : partial_map A)
                  (x : id) (v : A) :=
  t_update m x (Some v).

(* Properties of partial maps. *)
Lemma apply_empty : forall A x, @empty A x = None.
Proof.
  intros. unfold empty. apply t_apply_empty.
Qed.

Lemma update_eq : forall A (m: partial_map A) x v,
  (update m x v) x = Some v.
Proof.
  intros. unfold update. apply t_update_eq.
Qed.

Theorem update_neq : forall (X:Type) v x1 x2
                            (m : partial_map X),
    x2 <> x1 ->
    (update m x2 v) x1 = m x1.
Proof.
  intros. unfold update. apply t_update_neq. assumption.
Qed.


Lemma update_shadow : forall A (m: partial_map A) v1 v2 x,
    update (update m x v1) x v2 = update m x v2.
Proof.
  intros. unfold update. apply t_update_shadow.
Qed.

Theorem update_same : forall X v x (m : partial_map X),
    m x = Some v ->
    update m x v = m.
Proof.
  intros. unfold update. rewrite <- H. apply t_update_same.
Qed.

Theorem update_permute : forall (X:Type) v1 v2 x1 x2
                                (m : partial_map X),
    x2 <> x1 ->
    (update (update m x2 v2) x1 v1)
    = (update (update m x1 v1) x2 v2).
Proof.
  intros. unfold update. apply t_update_permute. assumption.
Qed.