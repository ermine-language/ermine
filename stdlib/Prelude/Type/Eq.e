module Type.Eq where

-- builtin:
-- infix type 4 ==

-- data (==) {k} (a: k) (b: k) where
--   Refl : forall {k} (a: k). Eq a a

trans : forall {k} (a: k) (b : k) (c : k). b == c -> a == b -> a == c
trans = subst
-- trans Refl Refl = Refl -- fails because we don't do GADT type refinement

refl : forall {k} (a: k). a == a
refl = Refl

private
  data Flip p a b = Flip (p b a)
  flop : Flip p a b -> p b a
  flop (Flip p) = p

symm : a == b -> b == a
symm p = flop (subst p (Flip refl))
