(** Mutual recursion *)

Fixpoint even n :=
  match n with O => true | S n => odd n end

with odd (n:nat) :=
  match n with O => false | S n => even n end.

Definition testeven := even 10.

(** Record and projections *)

Record reco := { a : nat; h : a <> 0; b : bool }.

Definition use_reco t := if t.(b) then t.(a) else 0.

Definition mk_reco a (H:a<>0) := Build_reco a H true.

Definition match_reco t :=
 match t with Build_reco x _ _ => x end.

Definition match_reco2 t :=
 match t with Build_reco x _ y => if y then x else 0 end.

Lemma ten_nz : 10<>0. easy. Qed.

Definition ex_reco := mk_reco 10 ten_nz.

Definition test_reco :=
  use_reco ex_reco + match_reco ex_reco + match_reco2 ex_reco.

(** Coinduction *)

CoInductive stream (A:Type) : Type :=
  Cons : A -> stream A -> stream A.

Definition hd {A} (x:stream A) :=
  match x with
    | Cons _ a _ => a
  end.

Definition tl {A} (x:stream A) :=
  match x with
    | Cons _ _ s => s
  end.

CoFixpoint map {A B} (f:A->B) (x:stream A) :=
  Cons _ (f (hd x)) (map f (tl x)).

Fixpoint nth {A} n (x:stream A) :=
  match n with
    | O => hd x
    | S n => nth n (tl x)
  end.

CoFixpoint seq n := Cons _ n (seq (S n)).
Definition nats := seq O.

Definition test_stream := nth 10 (map S nats).
