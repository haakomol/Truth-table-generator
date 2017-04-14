module TruthTabler exposing (getSimpleTruthTableRows)

import Array exposing (Array, get)

import Types exposing (..)

type alias Propvar = String

type SemanticTree
  = SemLeaf Bool Propvar
  | SemNot Bool SemanticTree
  | SemBinary Bool SemanticTree BinaryOperator SemanticTree

type alias PropvarWithValue = (Propvar, Bool)

type alias Valuation = List PropvarWithValue

type alias SemTreeWithValuation = (SemanticTree, Valuation)

simpleTruthTableRow : SemTreeWithValuation -> List Bool
simpleTruthTableRow semTreeWithValuation =
  let
    (semTree, valuation) = semTreeWithValuation
  in
    List.append
      (extractTruthValuesFromValuation valuation)
      (extractTruthValuesFromSemTree semTree)

extractTruthValuesFromValuation : Valuation -> List Bool
extractTruthValuesFromValuation valuation =
  List.map (\ (propvar, truthValue) -> truthValue) valuation

extractTruthValuesFromSemTree : SemanticTree -> List Bool
extractTruthValuesFromSemTree semanticTree =
  case semanticTree of
    SemLeaf truthValue _ ->
      [ truthValue ]
    SemNot truthValue subTree ->
      truthValue :: extractTruthValuesFromSemTree subTree
    SemBinary truthValue left op right ->
      List.append
        (extractTruthValuesFromSemTree left)
        (truthValue ::
          (extractTruthValuesFromSemTree right))

growSemTreesForAllValuations : ParseTree -> List Valuation -> List SemTreeWithValuation
growSemTreesForAllValuations parseTree valuations =
  List.map (growSemanticTreeWithValues parseTree) valuations

getSimpleTruthTableRows : ParseTree -> List (List Bool)
getSimpleTruthTableRows parseTree =
  let
    uniquePropvars = getUniquePropvars parseTree
    allValuations = getAllValuations uniquePropvars
    semTreeTable = growSemTreesForAllValuations parseTree allValuations
  in
    List.map simpleTruthTableRow semTreeTable


semTreeValue : SemanticTree -> Bool
semTreeValue semanticTree =
  case semanticTree of
    SemLeaf value _ ->
      value
    SemNot value _ ->
      value
    SemBinary value _ _ _ ->
      value

growSemanticTree : ParseTree -> Valuation -> SemanticTree
growSemanticTree parseTree propvarsWithValues =
  case parseTree of
    Leaf propvar ->
      SemLeaf (findValForPropvar propvar propvarsWithValues) propvar
    Not subTree ->
      let
        semSubTree = growSemanticTree subTree propvarsWithValues
        subTreeValue = semTreeValue semSubTree
      in
        SemNot (not subTreeValue) semSubTree
    Binary left op right ->
      let
        semLeftTree = growSemanticTree left propvarsWithValues
        semRightTree = growSemanticTree right propvarsWithValues
        leftValue = semTreeValue semLeftTree
        rightValue = semTreeValue semRightTree
        combinedValue = binaryValue op leftValue rightValue
      in
        SemBinary combinedValue semLeftTree op semRightTree

growSemanticTreeWithValues : ParseTree -> Valuation -> (SemanticTree, Valuation)
growSemanticTreeWithValues parseTree propvarsWithValues =
  ((growSemanticTree parseTree propvarsWithValues), propvarsWithValues)

getAllValuations : List Propvar -> List Valuation
getAllValuations propvars =
  let
    n = List.length propvars
    allValuationValues = getAllValuationValues n []
  in
    List.map
      (\ valuation ->
        List.map2
          (\ var val ->
            (var, val))
          propvars valuation)
      allValuationValues

getAllValuationValues : Int -> List Bool -> List (List Bool)
getAllValuationValues n truthValues =
  if
    (List.length truthValues) < n
  then
    List.append
      (getAllValuationValues n (True :: truthValues))
      (getAllValuationValues n (False :: truthValues))
  else
    [ truthValues ]

findValForPropvar : Propvar -> List (Propvar, Bool) -> Bool
findValForPropvar propvarMatching propvarsWithValues =
  case propvarsWithValues of
    [] -> False
    (propvar, truthValue) :: tail ->
      if   propvar == propvarMatching
      then truthValue
      else findValForPropvar propvarMatching tail
  
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

getUniquePropvars : ParseTree -> List Propvar
getUniquePropvars parseTree =
  let
    propvars = collectPropvars parseTree
  in
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