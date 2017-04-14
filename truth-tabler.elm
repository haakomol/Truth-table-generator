module TruthTabler exposing (..)

import Array exposing (Array, get)

import Types exposing (..)

type alias Propvar = String

type SemanticTree
  = SemLeaf Bool Propvar
  | SemNot Bool SemanticTree
  | SemBinary Bool SemanticTree BinaryOperator SemanticTree

semTreeValue : SemanticTree -> Bool
semTreeValue semanticTree =
  case semanticTree of
    SemLeaf value _ ->
      value
    SemNot value _ ->
      value
    SemBinary value _ _ _ ->
      value

growValuationTree : ParseTree -> (Array Propvar) -> (Array Bool) -> SemanticTree
growValuationTree parseTree propvars truthValues =
  case parseTree of
    Leaf propvar ->
      SemLeaf (findValForPropvar propvar propvars truthValues) propvar
    Not subTree ->
      let
        semSubTree = growValuationTree subTree propvars truthValues
        subTreeValue = semTreeValue semSubTree
      in
        SemNot (not subTreeValue) semSubTree
    Binary left op right ->
      let
        semLeftTree = growValuationTree left propvars truthValues
        semRightTree = growValuationTree right propvars truthValues
        leftValue = semTreeValue semLeftTree
        rightValue = semTreeValue semRightTree
        combinedValue = binaryValue op leftValue rightValue
      in
        SemBinary combinedValue semLeftTree op semRightTree

findValForPropvar : Propvar -> Array Propvar -> Array Bool -> Bool
findValForPropvar propvar propvars truthValues =
  let
    propVarIndex = indexInArray propvar propvars
    truthValueMaybe = Array.get propVarIndex truthValues
  in
    case truthValueMaybe of
      Nothing -> False
      Just truthValue -> truthValue

indexInArray : a -> Array a -> Int
indexInArray elToLookFor array =
  let
    innerHelper index =
      let
        elAtIndexMaybe = Array.get index array
      in
        case elAtIndexMaybe of
          Nothing -> -1
          Just elAtIndex ->
            if   elAtIndex == elToLookFor
            then index
            else innerHelper (index + 1)
  in
    innerHelper 0

binaryValue : BinaryOperator -> Bool -> Bool -> Bool
binaryValue op val1 val2 =
  case op of
    And ->
      val1 && val2
    Or ->
      val1 || val2
    Implication ->
      (not val1) || val2


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
  removeDuplicates propvars

removeDuplicates : List a -> List a
removeDuplicates lst =
  case lst of
    [] -> []
    x :: rest ->
      x :: (removeDuplicates (filterFromList x rest))

filterFromList : a -> List a -> List a
filterFromList el lst =
  List.filter (\ x -> (not (x == el))) lst