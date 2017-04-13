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
  
tokenizeOne inputString =
  let
    firstCharMaybe = String.uncons inputString
    
    tokenizePropvar readSoFar input =
      let
        curCharMaybe = String.uncons input
      in
        case curCharMaybe of
          Nothing -> Err "tokenizeProvar: empty input"
          Just (firstChar, restOfInput) ->
            if Regex.contains (regex "A-Za-z") toString firstChar
              tokenizePropvar (readSoFar ++ toString firstChar) restOfInput
            else
              PropvarToken readSoFar
  in
    case firstCharMaybe of
      Nothing -> Err "toknizeOne: empty input"
      Just (firstChar, _) ->
        case firstChar of
          "&" -> Token And
          "|" -> Token Or
          "-" -> Token Not
          ">" -> Token Implication
          " " -> tokenizeOne String.dropLeft 1 inputString
          _ -> tokenizePropVar inputString


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