{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Arbitrary where

import Bound
import Control.Applicative
import Data.Map
import Data.Monoid
import Data.Void
import Ermine.Syntax.Core        as Core
import Ermine.Syntax.Constructor as Constructor
import Ermine.Syntax.Data        as Data
import Ermine.Syntax.Global      as Global
import Ermine.Syntax.Hint        as Hint
import Ermine.Syntax.Kind        as Kind
import Ermine.Syntax.Literal     as Lit
import Ermine.Syntax.Module      as Module
import Ermine.Syntax.ModuleName  as ModuleName
import Ermine.Syntax.Pattern     as Pat
import Ermine.Syntax.Type        as Type
import Ermine.Syntax.Term        as Term
import Prelude.Extras
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances

-- Orphans
instance Arbitrary HardKind where
  arbitrary = oneof $ return <$> [ Star, Constraint, Rho, Phi ]

instance Arbitrary a => Arbitrary (Hinted a) where
  arbitrary = oneof [ Unhinted <$> arbitrary, Hinted <$> arbitrary <*> arbitrary ]

instance Arbitrary a => Arbitrary (Kind a) where
  arbitrary = genKind $ Just arbitrary

instance Arbitrary Assoc where
  arbitrary = Test.QuickCheck.elements [L, R, N]

genPrecedence = choose (0, 9) :: Gen Int

instance Arbitrary Fixity where
  arbitrary = oneof [
    Infix   <$> arbitrary <*> genPrecedence,
    Prefix  <$> genPrecedence,
    Postfix <$> genPrecedence,
    return  Idfix ]

instance Arbitrary ModuleName where
  arbitrary = mkModuleName <$> arbitrary <*> arbitrary

instance Arbitrary Global where
  arbitrary = glob <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Var a b) where
  arbitrary = oneof [ B <$> arbitrary, F <$> arbitrary ]

instance (Arbitrary b,Arbitrary v,Arbitrary1 f,Functor f) => Arbitrary (Scope b f v) where
  arbitrary = Scope . fmap (fmap lower1) <$> arbitrary1

instance Arbitrary a => Arbitrary (Schema a) where
  arbitrary = Schema <$> arbitrary <*> arbitrary

instance Arbitrary Type.HardType where
  arbitrary = oneof [
    Type.Tuple  <$> arbitrary,
    return Type.Arrow,
    -- TODO: learn more about Data.Void
    -- | Con !Global !(Schema Void)
    --Type.Con    <$> arbitrary <*> arbitrary,
    Type.ConcreteRho <$> arbitrary ]

instance (Arbitrary k, Arbitrary a) => Arbitrary (Type k a) where
  arbitrary = genType (Just arbitrary) (Just arbitrary)

-- | Combinator for decreasing the size of a generator. Should be used when
-- generating tree structures, as relying on probability to terminate them
-- can lead to excessive memory consumption.
smaller :: Gen a -> Gen a
smaller g = sized $ \n -> resize (n`div`3) g

maybeGen :: Applicative f => Maybe (Gen a) -> [Gen (f a)]
maybeGen Nothing  = []
maybeGen (Just g) = [ pure <$> g ]

-- | Generates kinds with optional delegation to a variable generator. If the
-- argument is Nothing, the generated Kinds will (necessarily) be closed.
genKind :: Maybe (Gen k) -> Gen (Kind k)
genKind mgk = sized $ \n ->
  oneof
    $  maybeGen mgk
    ++ (HardKind <$> arbitrary)
    : if n < 1 then [smaller $ (:->) <$> genKind mgk <*> genKind mgk] else []

-- | Given a definite generator for bound varibles, and an optional one for
-- free variables, definitely generates Vars.
genVar :: Gen b -> Maybe (Gen a) -> Gen (Var b a)
genVar gb Nothing   = B <$> gb
genVar gb (Just ga) = oneof [ B <$> gb , F <$> ga ]

-- | As genVar, but allows for the possibility that bound variables cannot
-- be generated, either. Potentially useful for generating well-scoped
-- terms.
genVar' :: Maybe (Gen b) -> Maybe (Gen a) -> Maybe (Gen (Var b a))
genVar' (Just gb) mga       = Just $ genVar gb mga
genVar' Nothing   (Just ga) = Just (F <$> ga)
genVar' Nothing   Nothing   = Nothing

-- | Given possible generators for kind and type variables, generates
-- a type. This allows for the possibility of closed type generation.
genType :: Maybe (Gen k) -> Maybe (Gen t) -> Gen (Type k t)
genType mgk mgt = sized go
 where
 go n | n <= 0 = HardType <$> arbitrary
      | n > 0  = smaller . oneof $ maybeGen mgt ++
                   [ HardType <$> arbitrary
                   , Type.App <$> (genType mgk mgt) <*> (genType mgk mgt)
                   , Forall <$> arbitrary
                            <*> (listOf (Unhinted <$> genScope arbitrary genKind mgk))
                            <*> (genScope arbitrary (genTK mgk) mgt)
                            <*> (genScope arbitrary (genTK mgk) mgt)
                   , Exists <$> arbitrary
                            <*> (listOf (Unhinted <$> genScope arbitrary genKind mgk))
                            <*> (genScope arbitrary (genTK mgk) mgt)
                   , And <$> (listOf (genType mgk mgt))
                   ]

-- | A simple combinator to generate TKs, as Types.
genTK :: Maybe (Gen k) -> Maybe (Gen t) -> Gen (TK k t)
genTK mgk = genType (Just $ genVar arbitrary mgk)

-- | Generates scopes with a definite supply of bound variables. The
-- higher-order generator must be able to handle a lack of free variables.
genScope :: Gen b -> (forall z. Maybe (Gen z) -> Gen (f z)) -> Maybe (Gen a)
         -> Gen (Scope b f a)
genScope gb gf mga = Scope <$> gf (Just . genVar gb . Just $ gf mga)

-- | As genScope, but with the possibility of no bound variables.
genScope' :: Maybe (Gen b) -> (forall z. Maybe (Gen z) -> Gen (f z)) -> Maybe (Gen a)
          -> Gen (Scope b f a)
genScope' mgb gf mga = Scope <$> gf (genVar' mgb . Just $ gf mga)

instance Arbitrary Literal where
  arbitrary = oneof [
    Int     <$> arbitrary,
    Long    <$> arbitrary,
    Byte    <$> arbitrary,
    Short   <$> arbitrary,
    String  <$> arbitrary,
    Char    <$> arbitrary,
    Float   <$> arbitrary,
    Double  <$> arbitrary ]

instance Arbitrary t => Arbitrary (Pattern t) where
  arbitrary = sized tree' where
    tree' 0 =  oneof [ SigP <$> arbitrary, return WildcardP, LitP <$> arbitrary ]
    tree' n = smaller $ oneof [
      SigP    <$> arbitrary,
      return WildcardP,
      AsP     <$> arbitrary,
      StrictP <$> arbitrary,
      LazyP   <$> arbitrary,
      LitP    <$> arbitrary,
      ConP    <$> arbitrary <*> arbitrary,
      TupP    <$> arbitrary ]

instance (Arbitrary t,Arbitrary1 f,Arbitrary a,Functor f) => Arbitrary (Alt t f a) where
  arbitrary = Alt <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (BindingType t) where
  arbitrary = oneof [ Explicit <$> arbitrary, return Implicit ]

instance Arbitrary BodyBound where
  arbitrary = oneof [ BodyDecl <$> arbitrary, BodyPat <$> arbitrary, BodyWhere <$> arbitrary ]

instance Arbitrary WhereBound where
  arbitrary = oneof [ WhereDecl <$> arbitrary, WherePat <$> arbitrary ]

instance Arbitrary tm => Arbitrary (Guarded tm) where
  arbitrary = oneof [ Unguarded <$> arbitrary, Guarded <$> arbitrary ]

instance (Arbitrary t, Arbitrary a) => Arbitrary (Body t a) where
  arbitrary = sized body' where
    body' 0 = Body <$> return [] <*> arbitrary <*> return []
    body' n = smaller $ Body <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary t, Arbitrary a) => Arbitrary (Binding t a) where
  arbitrary = sized binding' where
    binding' 0 = Binding <$> return mempty <*> arbitrary <*> return []
    binding' n = smaller $ Binding <$> return mempty <*> arbitrary <*> arbitrary

instance Arbitrary HardTerm where
  arbitrary = oneof [
    Term.Lit     <$> arbitrary,
    DataCon      <$> arbitrary <*> genType Nothing Nothing,
    Term.Tuple   <$> arbitrary,
    return Hole ]

-- TODO: generate PatternPaths that actually make sense.
instance (Arbitrary t, Arbitrary a) => Arbitrary (Term t a) where
  arbitrary = sized term' where
    term' 0 = oneof [ Term.Var <$> arbitrary, HardTerm <$> arbitrary ]
    term' n = smaller $ oneof [
      Term.Var  <$> arbitrary,
      Term.App  <$> arbitrary <*> arbitrary,
      HardTerm  <$> arbitrary,
      Term.Sig  <$> arbitrary <*> arbitrary,
      Term.Lam  <$> arbitrary <*> arbitrary,
      Term.Case <$> arbitrary <*> arbitrary,
      Term.Let  <$> arbitrary <*> arbitrary,
      Term.Loc  <$> return mempty <*> arbitrary,
      Term.Remember <$> arbitrary <*> arbitrary ]

instance Arbitrary PatternPath where
  arbitrary = oneof [ pure LeafPP
                    , FieldPP <$> arbitrary <*> arbitrary
                    , ArgPP   <$> arbitrary <*> arbitrary
                    ]

instance Arbitrary JavaLike where
  arbitrary = oneof [
    Method           <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
    Core.Constructor <$> arbitrary <*> arbitrary,
    Value            <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Arbitrary Foreign where
  arbitrary = oneof [ JavaLike <$> arbitrary , JavaLike <$> arbitrary ]

instance Arbitrary HardCore where
  arbitrary = oneof [
    Super    <$> arbitrary,
    Slot     <$> arbitrary,
    Core.Lit <$> arbitrary,
    PrimOp   <$> arbitrary,
    Foreign  <$> arbitrary,
    Error    <$> arbitrary ]

instance Arbitrary a => Arbitrary (Core a) where
  arbitrary = sized core' where
    core' 0 = oneof [ Core.Var <$> arbitrary, HardCore <$> arbitrary ]
    core' n = smaller $ oneof [
      Core.Var     <$> arbitrary,
      HardCore     <$> arbitrary,
      Core.App     <$> arbitrary <*> arbitrary,
      Core.Lam     <$> arbitrary <*> arbitrary,
      Core.Let     <$> arbitrary <*> arbitrary,
      Core.Case    <$> arbitrary <*> arbitrary <*> arbitrary,
      Core.Dict    <$> arbitrary <*> arbitrary,
      Core.LamDict <$> arbitrary,
      Core.AppDict <$> arbitrary <*> arbitrary ]

instance (Arbitrary k, Arbitrary t) => Arbitrary (Constructor.Constructor k t) where
  arbitrary = Constructor.Constructor <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary k, Arbitrary t) => Arbitrary (DataType k t) where
  arbitrary = DataType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

genConstructor :: Maybe (Gen k) -> Maybe (Gen t) -> Gen (Constructor.Constructor k t)
genConstructor mgk mgt =
  smaller $ Constructor.Constructor <$> arbitrary <*>  arbitrary <*>
    (listOf (Unhinted <$> genScope arbitrary genKind mgk)) <*>
    (listOf (genScope arbitrary (genTK mgk) mgt))

genDataType :: Maybe (Gen k) -> Maybe (Gen t) -> Gen (DataType k t)
genDataType mgk mgt =
  smaller $ DataType <$> arbitrary <*> arbitrary <*>
    (listOf (Unhinted <$> genScope arbitrary genKind mgk)) <*>
    (listOf (genConstructor (Just $ genVar arbitrary mgk) (Just $ genVar arbitrary mgt)))

instance Arbitrary Module where
  arbitrary = Module <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> smaller arbTypes <*> smaller dataTypes where
    arbTypes =  fmap fromList $ listOf ((,) <$> arbitrary <*> (genType Nothing Nothing))
    dataTypes = listOf (genDataType Nothing Nothing)

-- Higher-order arbitrary
class Arbitrary1 f where
  arbitrary1 :: Arbitrary a => Gen (f a)
  default arbitrary1 :: Arbitrary (f a) => Gen (f a)
  arbitrary1 = arbitrary

instance Arbitrary a => Arbitrary1 ((,) a)
instance Arbitrary1 Maybe
instance Arbitrary1 []
instance Arbitrary a => Arbitrary1 (Either a)
instance Arbitrary1 Hinted
instance Arbitrary1 Kind
instance Arbitrary1 Core
instance Arbitrary k => Arbitrary1 (Type k)
instance Arbitrary t => Arbitrary1 (Term t)

instance (Arbitrary1 f, Arbitrary u) => Arbitrary (Lift1 f u) where
  arbitrary = Lift1 <$> arbitrary1
