module Validation where

import Map
import Either
import List as List
import Prelude using id; cons; fieldName; toString
import Parse
import Function
import Maybe
import Either
import String as String

type FormValidator r = Map String String -> Either (List Err) {..r}
type Err = (String, String)
type Validator a b = a -> Either String b

validate : FormValidator r -> Map String String -> Either (List Err) {..r}
validate = id

nilV : FormValidator (| |)
nilV _ = Right {}

consV : (t <- (r,s)) => Field r a -> Validator (Maybe String) a -> FormValidator s -> FormValidator t
consV fld fn vs env = combineErrors (cons fld) (fieldName fld) (fn (lookup (fieldName fld) env)) (vs env)

combineErrors : (a -> b -> c) -> String -> Either String a -> Either (List Err) b -> Either (List Err) c
combineErrors f n (Right a) (Right b) = Right (f a b)
combineErrors _ n (Left  a) (Left  b) = Left ((n,a)::b)
combineErrors _ n (Left  a)  _        = Left [(n,a)]_List
combineErrors _ n _         (Left  b) = Left b

empty_Bracket = nilV
cons_Bracket (f, v) = consV f v

-- validator library

lookupForm : Validator (Maybe String) String
lookupForm = maybe (Left "Value not found") Right

validateInt    : Validator String Int
validateInt    = maybe (Left "Bad Int") Right . parseInt 10
validateDouble : Validator String Double
validateDouble = maybe (Left "Bad Double") Right . parseDouble
validateBool   : Validator String Bool
validateBool   = maybe (Left "Bad Bool") Right . parseBool

composeValidators : Validator a b -> Validator b c -> Validator a c
composeValidators vab vbc a = either Left vbc $ vab a

infixl 5 >=>
(>=>) x y = composeValidators x y

--type Validator a b = a -> Either String b
--lookupInt : Maybe String -> Either String Int
lookupInt : Validator (Maybe String) Int
lookupInt    = lookupForm >=> validateInt
lookupDouble = lookupForm >=> validateDouble
lookupBool   = lookupForm >=> validateBool
lookupString = lookupForm
lookupStringDef : String -> Validator (Maybe String) String
lookupStringDef def = maybe (Right def) Right

within : List b -> (b -> b -> Bool) -> Validator a b -> Validator a b
within bs pred vab a =
  let errmsg = "couldn't find " ++_String (toString a) ++_String " in " ++_String (toString bs)
      existsb b = find_List (pred b) bs
      checkWithin = maybe (Left errmsg) Right . existsb
  in either Left checkWithin $ vab a