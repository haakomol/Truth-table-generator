import Html exposing (Html, Attribute, div, input, text)
import Html.Events exposing (onInput)
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
  { inputFormula : String
  }

init : (Model, Cmd Msg)
init =
  ({ inputFormula = "" }, Cmd.none)

type ParseTree
  = Leaf PropvarToken
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
  = Change String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newFormula ->
      (parseFormula newFormula, Cmd.none)


tokenize : String -> Result String (List Token)
tokenize inputString =
  let
    innerHelper tokensSoFar inputString =
      if   String.isEmpty inputString
      then List.reverse tokensSoFar
      else
        let
          tokenizeOneResult = tokenizeOne inputString
        in
          case tokenizeOneResult of
            Err errorString ->
              Err errorString

            Ok (token, restOfInputString) ->
              innerHelper (List.cons token tokensSoFar) restOfInputString 

tokenizeOne String -> (Result String (Token, String))
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
          ' ' -> tokenizeOne String.dropLeft 1 inputString
          _ -> tokenizePropVar inputString

tokenizePropvar readSoFar curInput =
  let
    curCharMaybe = String.uncons curInput
  in
    case curCharMaybe of
      Nothing ->
        Ok (PropvarToken readSoFar, curInput)
      Just (firstChar, restOfInput) ->
        if firstChar |> toString |> Regex.contains (regex "A-Za-z")
          tokenizePropvar (readSoFar ++ toString firstChar) restOfInput
        else
          Ok (PropvarToken readSoFar, curInput)

parseFormula : String -> Model
parseFormula newFormula =

    
  -- Model ""


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Formula", onInput Change ] []
    , div [] [ text "Wait for it" ]
    ]