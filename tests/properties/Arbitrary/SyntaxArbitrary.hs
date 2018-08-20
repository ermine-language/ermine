{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Arbitrary.SyntaxArbitrary where

import Arbitrary.Arbitrary
import Bound
import Control.Applicative
import Data.List.NonEmpty hiding (fromList)
import Data.Map
import Data.Monoid hiding (Alt)
import Data.Void
import Ermine.Syntax.Class       as Class
import Ermine.Syntax.Constructor as Constructor
import Ermine.Syntax.Convention  as Convention
import Ermine.Syntax.Core        as Core
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
instance Arbitrary1 Kind
instance Arbitrary cc => Arbitrary1 (Core cc)
instance Arbitrary k => Arbitrary1 (Type k)
instance Arbitrary t => Arbitrary1 (Term t)

instance Arbitrary HardKind where
  arbitrary = oneof $ return <$> [ Star, Constraint, Rho, Phi, Unboxed ]

instance Arbitrary a => Arbitrary (Kind a) where
  arbitrary = genKind $ Just arbitrary

instance Arbitrary Assoc where
  arbitrary = Test.QuickCheck.elements [Global.L, Global.R, Global.N]

genPrecedence = choose (0, 9) :: Gen Int

instance Arbitrary Convention where
  arbitrary = oneof $ return <$> [ Convention.C, Convention.U, Convention.D, Convention.N ]

instance (Arbitrary t, Functor c, Arbitrary1 c) => Arbitrary1 (Match t c) where
  liftArbitrary g = Match <$> arbitrary <*> arbitrary <*> liftArbitrary g

instance (Arbitrary t, Functor c, Arbitrary1 c, Arbitrary a) => Arbitrary (Match t c a) where
  arbitrary = arbitrary1

instance Arbitrary Fixity where
  arbitrary = oneof
    [ Infix   <$> arbitrary <*> genPrecedence
    , Prefix  <$> genPrecedence
    , Postfix <$> genPrecedence
    , return  Idfix
    ]

instance Arbitrary ModuleName where
  arbitrary = mkModuleName <$> arbitrary <*> arbitrary

instance Arbitrary Global where
  arbitrary = glob <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Schema a) where
  arbitrary = Schema <$> arbitrary <*> arbitrary

instance Arbitrary FixityDeclLevel where
  arbitrary = oneof [ return TypeLevel, return TermLevel ]

instance Arbitrary FixityDecl where
  arbitrary = FixityDecl <$> arbitrary <*> arbitrary <*> listOf arbitrary

instance Arbitrary Privacy where 
  arbitrary = oneof [ return Public, return Private ]

instance Arbitrary g => Arbitrary (Module.Explicit g) where
  arbitrary = Module.Explicit <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary g => Arbitrary (ImportsInScope g) where
  arbitrary = oneof [ Using <$> listOf arbitrary, Hiding <$> listOf arbitrary ]

instance Arbitrary g => Arbitrary (Import g) where
  arbitrary = Import <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ResolvedTerms where
  arbitrary = ResolvedTerms <$> arbitrary

instance Arbitrary ResolvedTypes where
  arbitrary = ResolvedTypes <$> arbitrary

instance Arbitrary ImportResolution where
  arbitrary = ImportResolution <$> listOf arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary k, Arbitrary a) => Arbitrary (Annot k a) where
  arbitrary = Annot <$> listOf arbitrary <*> listOf arbitrary <*> arbitrary

instance (Arbitrary k, Arbitrary t) => Arbitrary (Class k t) where
  arbitrary = Class  <$>
    listOf arbitrary <*>
    listOf arbitrary <*>
    listOf arbitrary <*>
    arbitrary        <*>
    error "todo" -- defaults :: Map Global (Bodies (Annot Void t) Void)

instance Arbitrary Module where
  arbitrary = Module <$> 
    arbitrary        <*>  --name
    arbitrary        <*>  --imports
    arbitrary        <*>  --fixity
    listOf arbitrary <*>  --data
    listOf (error "todo") <*> -- moduleBindings :: [(Privacy, Text, Binding (Annot Void Text) Text)]
    arbitrary where

instance Arbitrary Type.HardType where
  arbitrary = oneof
    [ Type.Tuple <$> arbitrary
    , return Type.Arrow
    -- | Con !Global !(Schema Void)
    -- , Type.Con <$> arbitrary <*> arbitrary
    , Type.ConcreteRho <$> arbitrary
    ]

instance (Arbitrary k, Arbitrary a) => Arbitrary (Type k a) where
  arbitrary = genType (Just arbitrary) (Just arbitrary)

-- | Generates kinds with optional delegation to a variable generator. If the
-- argument is Nothing, the generated Kinds will (necessarily) be closed.
genKind :: Maybe (Gen k) -> Gen (Kind k)
genKind mgk = sized $ \n ->
  oneof
    $  maybeGen mgk
    ++ (HardKind <$> arbitrary)
    : if n < 1 then [smaller $ (:->) <$> genKind mgk <*> genKind mgk] else []

-- | Given possible generators for kind and type variables, generates
-- a type. This allows for the possibility of closed type generation.
genType :: Maybe (Gen k) -> Maybe (Gen t) -> Gen (Type k t)
genType mgk mgt = sized go
 where
 go n | n <= 0 = HardType <$> arbitrary
      | n > 0  = smaller . oneof $ maybeGen mgt ++
                   [ HardType <$> arbitrary
                   , Type.App <$> genType mgk mgt <*> genType mgk mgt
                   , Forall <$> arbitrary
                            <*> listOf ((,) <$> arbitrary <*> genScope arbitrary genKind mgk)
                            <*> genScope arbitrary (genTK mgk) mgt
                            <*> genScope arbitrary (genTK mgk) mgt
                   , Exists <$> arbitrary
                            <*> listOf ((,) <$> arbitrary <*> genScope arbitrary genKind mgk)
                            <*> genScope arbitrary (genTK mgk) mgt
                   , And <$> listOf (genType mgk mgt)
                   ]

-- | A simple combinator to generate TKs, as Types.
genTK :: Maybe (Gen k) -> Maybe (Gen t) -> Gen (TK k t)
genTK mgk = genType (Just $ genVar arbitrary mgk)

instance Arbitrary Literal where
  arbitrary = oneof
    [ Int     <$> arbitrary
    , Long    <$> arbitrary
    , Byte    <$> arbitrary
    , Short   <$> arbitrary
    , String  <$> arbitrary
    , Char    <$> arbitrary
    , Float   <$> arbitrary
    , Double  <$> arbitrary
    ]

instance Arbitrary t => Arbitrary (Pattern t) where
  arbitrary = sized tree' where
    tree' 0 =  oneof
      [ SigP <$> arbitrary
      , return WildcardP
      , LitP <$> arbitrary
      ]
    tree' n = smaller $ oneof
      [ SigP    <$> arbitrary
      , return WildcardP
      , AsP     <$> arbitrary
      , StrictP <$> arbitrary
      , LazyP   <$> arbitrary
      , LitP    <$> arbitrary
      , ConP    <$> arbitrary <*> arbitrary
      , TupP    <$> arbitrary
      ]

instance (Arbitrary t,Arbitrary1 f,Arbitrary a,Functor f) => Arbitrary (Alt t f a) where
  arbitrary = Alt <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (BindingType t) where
  arbitrary = oneof [ Term.Explicit <$> arbitrary, return Implicit ]

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

instance (Arbitrary t, Arbitrary a) => Arbitrary (Bodies t a) where
  arbitrary = sized bodies where
    bodies 0 = return $ Bodies mempty []
    bodies _ = smaller $ Bodies mempty <$> arbitrary

instance (Arbitrary t, Arbitrary a) => Arbitrary (Binding t a) where
  arbitrary = Binding <$> arbitrary <*> arbitrary

instance Arbitrary HardTerm where
  arbitrary = oneof
    [ Term.Lit     <$> arbitrary
    , DataCon      <$> arbitrary <*> genType Nothing Nothing
    , Term.Tuple   <$> arbitrary
    , return Hole
    ]

-- TODO: generate PatternPaths that actually make sense.
instance (Arbitrary t, Arbitrary a) => Arbitrary (Term t a) where
  arbitrary = sized term' where
    term' 0 = oneof [ Term.Var <$> arbitrary, HardTerm <$> arbitrary ]
    term' n = smaller $ oneof
      [ Term.Var  <$> arbitrary
      , Term.App  <$> arbitrary <*> arbitrary
      , HardTerm  <$> arbitrary
      , Term.Sig  <$> arbitrary <*> arbitrary
      , Term.Lam  <$> arbitrary <*> arbitrary
      , Term.Case <$> arbitrary <*> arbitrary
      , Term.Let  <$> arbitrary <*> arbitrary
      , Term.Loc  <$> return mempty <*> arbitrary
      , Term.Remember <$> arbitrary <*> arbitrary
      ]

instance Arbitrary PatternPath where
  arbitrary = oneof
    [ pure LeafPP
    , FieldPP <$> arbitrary <*> arbitrary
    , ArgPP   <$> arbitrary <*> arbitrary
                    ]

instance Arbitrary JavaLike where
  arbitrary = oneof
    [ Method           <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Core.Constructor <$> arbitrary <*> arbitrary
    , Value            <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary Foreign where
  arbitrary = oneof [ JavaLike <$> arbitrary , JavaLike <$> arbitrary ]

instance Arbitrary HardCore where
  arbitrary = oneof
    [ Super    <$> arbitrary
    , Slot     <$> arbitrary
    , Core.Lit <$> arbitrary
    , Foreign  <$> arbitrary
    , Error    <$> arbitrary
    ]

instance (Arbitrary cc, Arbitrary a) => Arbitrary (Core cc a) where
  arbitrary = sized core' where
    core' 0 = oneof [ Core.Var <$> arbitrary, HardCore <$> arbitrary ]
    core' n = smaller $ oneof
      [ Core.Var      <$> arbitrary
      , HardCore      <$> arbitrary
      , Core.App      <$> arbitrary <*> arbitrary <*> arbitrary
      , Core.Lam      <$> arbitrary <*> arbitrary
      , Core.Let      <$> arbitrary <*> arbitrary
      , Core.Case     <$> arbitrary <*> arbitrary <*> arbitrary
      , Core.Dict     <$> arbitrary <*> arbitrary
      ]

instance (Arbitrary k, Arbitrary t) => Arbitrary (Constructor.Constructor k t) where
  arbitrary = Constructor.Constructor <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary k, Arbitrary t) => Arbitrary (DataType k t) where
  arbitrary = DataType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

genConstructor :: Maybe (Gen k) -> Maybe (Gen t) -> Gen (Constructor.Constructor k t)
genConstructor mgk mgt =
  smaller $ Constructor.Constructor <$> arbitrary <*>  arbitrary <*>
    (listOf ((,) <$> arbitrary <*> genScope arbitrary genKind mgk)) <*>
    (listOf (genScope arbitrary (genTK mgk) mgt))

genDataType :: Maybe (Gen k) -> Maybe (Gen t) -> Gen (DataType k t)
genDataType mgk mgt =
  smaller $ DataType <$> arbitrary <*> arbitrary <*>
    (listOf ((,) <$> arbitrary <*> genScope arbitrary genKind mgk)) <*>
    (listOf (genConstructor (Just $ genVar arbitrary mgk) (Just $ genVar arbitrary mgt)))

