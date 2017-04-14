import Html exposing (Html, Attribute, div, input, text, button, h2)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (placeholder)
import Regex exposing (regex)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { input : String
  , tokens : List Token
  , parseTree : ParseTree
  , errorMessage : String
  }

init : (Model, Cmd Msg)
init =
  (Model "" [] Empty "", Cmd.none)

type ParseTree
  = Leaf String
  | And ParseTree ParseTree
  | Or ParseTree ParseTree
  | Implication ParseTree ParseTree
  | Not ParseTree
  | Empty

type Token
  = PropvarToken String
  | NotToken
  | BinaryOperator BinaryOperatorToken
  | Parenthesis ParenthesisToken

type BinaryOperatorToken
  = AndToken
  | OrToken
  | ImplicationToken

type ParenthesisToken
  = LeftParToken
  | RightParToken

-- UPDATE

type Msg
  = ChangeInput String
  | ButtonTest

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeInput newInput ->
    --   ( { model | input = newInput}, Cmd.none)
    -- ButtonTest ->
      let
        parseTreeResult = interpret newInput
      in
        case parseTreeResult of
          Err errorMessage ->
            ( {model | errorMessage = errorMessage }, Cmd.none)
          Ok parseTree ->
            ( { model | parseTree = parseTree, errorMessage = "" }, Cmd.none)

    ButtonTest ->
      (model, Cmd.none)


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
          '|' -> Ok (BinaryOperator OrToken, restOfInput)
          '>' -> Ok (BinaryOperator ImplicationToken, restOfInput)
          ' ' -> tokenizeOne (String.dropLeft 1 inputString)
          _ ->
            if firstChar |> toString |> Regex.contains (regex "[A-Za-z]")
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
            if firstChar |> toString |> Regex.contains (regex "[A-Za-z]")
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
        Err "Parser: Got nothing"
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
                                    Ok (And firstOperandTree secondOperandTree, List.drop 1 restOfTokens)
                                  OrToken ->
                                    Ok (Or firstOperandTree secondOperandTree, List.drop 1 restOfTokens)
                                  ImplicationToken ->
                                    Ok (Implication firstOperandTree secondOperandTree, List.drop 1 restOfTokens)

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
          Err "Parser: Wrong kind of token"

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


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Formula", onInput ChangeInput ] []
    , button [ onClick ButtonTest ] [ text "tokenize" ]
    , viewErrorMessage model
    , div [] (viewTokens model)
    , viewParseTree model
    ]

viewErrorMessage : Model -> Html msg
viewErrorMessage model =
  if   not (String.isEmpty model.errorMessage)
  then h2  [] [ text ("Error: " ++ model.errorMessage) ]
  else div [] []

viewTokens : Model -> List (Html Msg)
viewTokens model =
  List.map (\ token -> div [] [ text (toString token) ]) model.tokens

viewParseTree : Model -> Html msg
viewParseTree model =
  div [] [ text (toString model.parseTree)]