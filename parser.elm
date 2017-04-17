module Parser exposing (parseTokens)

import Types exposing (..)
import Tokenizer exposing (..)

interpret : String -> Result String ParseTree
interpret inputString =
  let
    tokenizeResult = tokenize inputString
  in
    case tokenizeResult of
      Err message ->
        Err message
      Ok tokens ->
        let
          parseResult = parseTokens tokens
        in
          case parseResult of
            Err message ->
              Err message
            Ok parseTree ->
              Ok parseTree

parseTokens : List Token -> Result String ParseTree
parseTokens tokens =
  let
    parseTreeResult = parseSubExpression tokens
  in
    case parseTreeResult of
      Err message ->
        Err message
      Ok (parseTree, restOfTokens) ->
        case restOfTokens of
          [] ->
            Ok parseTree
          _ ->
            Err (parserHint "Skjønner ikke slutten av formelen. Mangler du en venstreparentes i begynnelsen?")

parseSubExpression : List Token -> Result String (ParseTree, List Token)
parseSubExpression tokens =
  let
    firstToken = List.head tokens
  in
    case firstToken of
      Nothing ->
        Err (parserHint "Savner en delformel som må begynne med en venstreparentes, ikke-konnektivet eller en utsagnsvariabel.")
      Just token ->
        case token of
          Parenthesis LeftParToken ->
            parseParenthesisExpression (List.drop 1 tokens)
          NotToken ->
            parseNotExpression (List.drop 1 tokens)
          BinaryOperator operator ->
            Err (parserHint ("Her er det et " ++ (binaryOperatorTokenToString operator) ++
                            "-konnektiv der jeg forventet en delformel."))
          Parenthesis RightParToken ->
            Err (parserHint "Her er det en høyreparentes der jeg forventet en delformel.")
          PropvarToken propvar ->
            Ok (Leaf propvar, List.drop 1 tokens)


parseParenthesisExpression : List Token -> Result String (ParseTree, List Token)
parseParenthesisExpression tokens =
  let
    firstToken = List.head tokens
  in
    case firstToken of
      Nothing ->
        Err (parserHint "Ser ingenting etter en venstreparentes.")
      Just token ->
        let
          firstOperandResult = parseSubExpression tokens
        in
          case firstOperandResult of
            Err errorMessage ->
              Err errorMessage
            Ok (firstOperandTree, restOfTokens) ->
              let
                binaryOperatorResult = parseBinaryOperator restOfTokens
              in
                case binaryOperatorResult of
                  Err errorMessage ->
                    Err errorMessage
                  Ok (binaryOperatorToken, restOfTokens) ->
                    let
                      secondOperandResult = parseSubExpression restOfTokens
                    in
                      case secondOperandResult of
                        Err errorMessage ->
                          Err errorMessage
                        Ok (secondOperandTree, restOfTokens) ->
                          let
                            parseRightParenthesisResult =
                              parseToken (Parenthesis RightParToken) restOfTokens
                          in
                            case parseRightParenthesisResult of
                              Err message ->
                                Err (parserHint "Savner en høyreparentes.")
                              Ok _ ->
                                case binaryOperatorToken of
                                  AndToken ->
                                    Ok (Binary firstOperandTree And secondOperandTree, List.drop 1 restOfTokens)
                                  OrToken ->
                                    Ok (Binary firstOperandTree Or secondOperandTree, List.drop 1 restOfTokens)
                                  ImplicationToken ->
                                    Ok (Binary firstOperandTree Implication secondOperandTree, List.drop 1 restOfTokens)

parseNotExpression : List Token -> Result String (ParseTree, List Token)
parseNotExpression tokens =
  let
    innerExpressionResult = parseSubExpression tokens
  in
    case innerExpressionResult of
      Err message ->
        Err message
      Ok (innerExpressionTree, restTokens) ->
        Ok (Not innerExpressionTree, restTokens)

parseBinaryOperator : List Token -> Result String (BinaryOperatorToken, List Token)
parseBinaryOperator tokens =
  let
    firstToken = List.head tokens
  in
    case firstToken of
      Nothing ->
        Err (parserHint "Savner et og/eller/implikasjon-konnektiv på slutten.")
      Just token ->
        case token of
          BinaryOperator binaryOperatorToken ->
            Ok (binaryOperatorToken, List.drop 1 tokens)
          _ ->
            Err (parserHint "Savner et og/eller/implikasjon-konnektiv.")

-- Checks that first token in tokens is token, and that there are more tokens
parseToken : Token -> List Token -> Result String String
parseToken tokenToParse tokens =
  let
    firstTokenMaybe = List.head tokens
  in
    case firstTokenMaybe of
      Nothing ->
        Err "Parser: Expected more tokens, got nothing"
      Just firstToken ->
        if firstToken == tokenToParse
        then
          Ok "ok"
        else
          Err ("Parser: Expected " ++ (toString tokenToParse) ++ ", got " ++ (toString firstToken))

parserHint : String -> String
parserHint hint =
  "Venter på en gyldig formel... (" ++ hint ++ ")"

binaryOperatorTokenToString : BinaryOperatorToken -> String
binaryOperatorTokenToString binaryOperatorToken =
  case binaryOperatorToken of
    AndToken ->
      "og"
    OrToken -> 
      "eller"
    ImplicationToken ->
      "implikasjon"
