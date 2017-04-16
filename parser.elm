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
        Ok parseTree

parseSubExpression : List Token -> Result String (ParseTree, List Token)
parseSubExpression tokens =
  let
    firstToken = List.head tokens
  in
    case firstToken of
      Nothing ->
        Err "Parser: Expected left parentheis, operand or not operator, got nothing!"
      Just token ->
        case token of
          Parenthesis LeftParToken ->
            parseParenthesisExpression (List.drop 1 tokens)
          NotToken ->
            parseNotExpression (List.drop 1 tokens)
          BinaryOperator operator ->
            Err ("Parser: Got binary operator: " ++ (toString operator) ++
                ", expected left parentheis, operand or not operator")
          Parenthesis RightParToken ->
            Err "Parser: Got right parenthesis, expected left parentheis, operand or not operator"
          PropvarToken propvar ->
            Ok (Leaf propvar, List.drop 1 tokens)


parseParenthesisExpression : List Token -> Result String (ParseTree, List Token)
parseParenthesisExpression tokens =
  let
    firstToken = List.head tokens
  in
    case firstToken of
      Nothing ->
        Err "Parser: Got nothing after left parentheis"
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
                                Err message
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
        Err "Parser: Expected binary operator, got nothing."
      Just token ->
        case token of
          BinaryOperator binaryOperatorToken ->
            Ok (binaryOperatorToken, List.drop 1 tokens)
          _ ->
            Err ("Parser: Expected binary operator, but got this token: " ++ toString token ++ ".")

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

parseTokenAndCheckNotEnd : Token -> List Token -> Result String (List Token)
parseTokenAndCheckNotEnd tokenToParse tokens =
  let
    parseTokenResult = parseToken tokenToParse tokens
  in
    case parseTokenResult of
      Err errorMessage ->
        Err errorMessage
      Ok _ ->
        let
          restOfTokensMaybe = List.tail tokens
        in
          case restOfTokensMaybe of
            Nothing ->
              Err "Parser: Expected more tokens"
            Just restOfTokens ->
              Ok (restOfTokens)