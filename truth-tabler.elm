module TruthTabler exposing (..)

import Types exposing (..)

type alias Propvar = String

type ValuationTree
  = Leaf Bool Propvar
  | Not Bool ValuationTree
  | Binary Bool ValuationTree BinaryOperator ValuationTree

growValuationTree : ParseTree -> List Bool -> List Propvar -> ValuationTree
growValuationTree parseTree truthValues propvars =
  
  
collectPropvars : ParseTree -> List Propvar
collectPropvars parseTree =
  case parseTree of
    Binary left _ right ->
      let
        leftPropvars = collectPropvars left
        rightPropvars = collectPropvars right
      in
        List.append leftPropvars rightPropvars
    
    Not subTree ->
      collectPropvars subTree
    
    Leaf string ->
      [ string ]

getUniquePropvars : List Propvar -> List Propvar
getUniquePropvars propvars =
  reduceToUniques propvars

reduceToUniques : List a -> List a
reduceToUniques lst =
  case lst of
    [] -> []
    x :: rest ->
      x :: (reduceToUniques (filterFromList x rest))

filterFromList : a -> List a -> List a
filterFromList el lst =
  List.filter (\ x -> (not (x == el))) lst