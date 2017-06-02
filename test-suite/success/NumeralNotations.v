Module PolyInd.
  Local Set Universe Polymorphism.
  Inductive test := tt.
  Definition to_test (x : Decimal.uint) : test := tt.
  Definition of_test (x : test) : option Decimal.uint := None.
  Fail Numeral Notation test to_test of_test : nat_scope (abstract after 5000).
End PolyInd.
Module PolyConv.
  Polymorphic Definition of_uint@{i} := Nat.of_uint.
  Polymorphic Definition to_uint@{i} := Nat.to_uint.
  Numeral Notation nat Nat.of_uint Nat.to_uint : nat_scope (abstract after 5000).
  Fail Numeral Notation nat Nat.of_uint to_uint : nat_scope (abstract after 5000).
  Fail Numeral Notation nat of_uint Nat.to_uint : nat_scope (abstract after 5000).
End PolyConv.
