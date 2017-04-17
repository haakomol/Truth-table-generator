module Tokenizer exposing (tokenize)

import Regex exposing (regex)
import Char

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
            Ok (Nothing, _) ->
              Ok (List.reverse tokensSoFar)
            Ok (Just token, restOfInputString) ->
              innerHelper (token :: tokensSoFar) restOfInputString 
  in
    innerHelper [] inputString

tokenizeOne : String -> (Result String (Maybe Token, String))
tokenizeOne inputString =
  let
    firstCharMaybe = String.uncons inputString
  in
    case firstCharMaybe of
      Nothing ->
        Ok (Nothing, "")
      Just (firstChar, restOfInput) ->
        case firstChar of
          '(' -> Ok (Just (Parenthesis LeftParToken), restOfInput)
          ')' -> Ok (Just (Parenthesis RightParToken), restOfInput)
          '-' -> Ok (Just (NotToken), restOfInput)
          '&' -> Ok (Just (BinaryOperator AndToken), restOfInput)
          'a' -> Ok (Just (BinaryOperator AndToken), restOfInput)
          '|' -> Ok (Just (BinaryOperator OrToken), restOfInput)
          'v' -> Ok (Just (BinaryOperator OrToken), restOfInput)
          '>' -> Ok (Just (BinaryOperator ImplicationToken), restOfInput)
          ' ' -> tokenizeOne (String.dropLeft 1 inputString)
          _ ->
            if
              firstChar |> toString |> Regex.contains (regex "[A-Z]")
            then
              let
                propvarToken = tokenizePropvar inputString
              in
                case propvarToken of
                  Ok (token, restOfInput) ->
                    Ok ((Just token), restOfInput)
                  Err errorMessage ->
                    Err errorMessage
            else if 
                Char.isLower firstChar
            then
              Err ("Skjønner ikke dette tegnet: " ++ (toString firstChar) ++ ". Utsagnsvariabler må begynne med stor forbokstav!")
            else
              Err ("Skjønner ikke dette tegnet: " ++ (toString firstChar))

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
                 (not (Regex.contains (regex "[av]") firstCharStr))
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