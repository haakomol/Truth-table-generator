import Html exposing (Html, Attribute, div, input, text, button)
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
  , errorMessage : String
  }

init : (Model, Cmd Msg)
init =
  (Model "" [] "", Cmd.none)

type ParseTree
  = Leaf String
  | And ParseTree ParseTree
  | Or ParseTree ParseTree
  | Implication ParseTree ParseTree
  | Not ParseTree

type Token
  = PropvarToken String
  | LeftParToken
  | RightParToken
  | AndToken
  | OrToken
  | ImplicationToken
  | NotToken

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
        tokenizeResult = tokenize newInput
      in
        case tokenizeResult of
          Ok tokens ->
            ( { model | tokens = tokens, errorMessage="" }, Cmd.none)
          Err errorMessage ->
            ( { model | tokens = [], errorMessage = errorMessage }, Cmd.none)

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
          '(' -> Ok (LeftParToken, restOfInput)
          ')' -> Ok (RightParToken, restOfInput)
          '&' -> Ok (AndToken, restOfInput)
          '|' -> Ok (OrToken, restOfInput)
          '-' -> Ok (NotToken, restOfInput)
          '>' -> Ok (ImplicationToken, restOfInput)
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

parseInput : String -> Model
parseInput newInput =
  Model "" [] ""


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Formula", onInput ChangeInput ] []
    , button [ onClick ButtonTest ] [ text "tokeinze" ]
    , div [] [ text ("Error message " ++ model.errorMessage) ]
    , div [] (viewTokens model)
    ]

viewTokens : Model -> List (Html Msg)
viewTokens model =
  List.map (\ token -> div [] [ text (toString token) ]) model.tokens