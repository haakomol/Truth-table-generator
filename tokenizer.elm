module Tokenizer exposing (tokenize)

import Regex exposing (regex)

import Types exposing (..)

tokenize : String -> Result String (List Token)
tokenize inputString =
  let
    innerHelper tokensSoFar inputString =
      if   String.isEmpty inputString
      then Ok (List.reverse tokensSoFar)
      else
        let
          tokenizeOneResult = tokenizeOne inputString
        in
          case tokenizeOneResult of
            Err errorString ->
              Err errorString
            Ok (token, restOfInputString) ->
              innerHelper (token :: tokensSoFar) restOfInputString 
  in
    innerHelper [] inputString

tokenizeOne : String -> (Result String (Token, String))
tokenizeOne inputString =
  let
    firstCharMaybe = String.uncons inputString
  in
    case firstCharMaybe of
      Nothing ->
        Err "toknizeOne: empty input"
      Just (firstChar, restOfInput) ->
        case firstChar of
          '(' -> Ok (Parenthesis LeftParToken, restOfInput)
          ')' -> Ok (Parenthesis RightParToken, restOfInput)
          '-' -> Ok (NotToken, restOfInput)
          '&' -> Ok (BinaryOperator AndToken, restOfInput)
          'a' -> Ok (BinaryOperator AndToken, restOfInput)
          '|' -> Ok (BinaryOperator OrToken, restOfInput)
          'v' -> Ok (BinaryOperator OrToken, restOfInput)
          '>' -> Ok (BinaryOperator ImplicationToken, restOfInput)
          ' ' -> tokenizeOne (String.dropLeft 1 inputString)
          _ ->
            if firstChar |> toString |> Regex.contains (regex "[A-Z]")
            then
              tokenizePropvar inputString
            else
              Err ("invalid input: " ++ (toString firstChar))

tokenizePropvar : String -> (Result String (Token, String))
tokenizePropvar inputString =
  let
    innerHelper readSoFar curInput =
      let
        curCharMaybe = String.uncons curInput
      in
        case curCharMaybe of
          Nothing ->
            Ok (PropvarToken readSoFar, "")
          Just (firstChar, restOfInput) ->
            let
              firstCharStr = toString firstChar
            in
              if (Regex.contains (regex "[A-Za-z]") firstCharStr) &&
                 (not (Regex.contains (regex "[aev]") firstCharStr))
              then
                innerHelper ( readSoFar ++ (String.fromChar firstChar)) restOfInput
              else
                Ok (PropvarToken readSoFar, (String.cons firstChar restOfInput))
  in
    if String.isEmpty inputString
    then
      Err "tokenizePropvar: emptyInput"
    else
      innerHelper "" inputString