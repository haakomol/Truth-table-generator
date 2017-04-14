import Html exposing (Html, Attribute, div, input, text, button, h1, h2, h3, h4, h5, h6)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (placeholder)

import Types exposing (..)
import Tokenizer exposing (..)
import Parser exposing (..)


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
  , mode : Mode
  , tokens : List Token
  , parseTree : ParseTree
  , truthTable : TruthTable
  , errorMessage : String
  }

init : (Model, Cmd Msg)
init =
  (Model "" TruthTabler [] Empty (TruthTable 0) "", Cmd.none)

-- UPDATE

type Msg
  = ChangeInput String
  | ChangeMode Mode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeMode newMode ->
      ( { model | mode = newMode }, Cmd.none)
    ChangeInput newInput ->
      let
        tokenizeResult = tokenize newInput
      in
        case tokenizeResult of
          Err errorMessage ->
            ( { model | errorMessage = errorMessage }, Cmd.none)
          Ok tokens ->
            let
              parseTreeResult = parseTokens tokens
            in
              case parseTreeResult of
                Err errorMessage ->
                  ( { model | errorMessage = errorMessage, tokens = tokens }, Cmd.none)
                Ok parseTree ->
                  ( { model | tokens = tokens, parseTree = parseTree, errorMessage = "" }, Cmd.none)
        
      --   TruthTabler ->
      --     let
      --       parseTreeResult = interpret newInput
      --     in
      --       case parseTreeResult of
      --         Err errorMessage ->
      --           ( { model | errorMessage = errorMessage }, Cmd.none)
      --         Ok parseTree ->
      --           let
      --             nPropvars = countPropvars parseTree
      --           in
      --             ( { model | parseTree = parseTree, truthTable = TruthTable nPropvars }, Cmd.none)

countPropvars : ParseTree -> Int
countPropvars parseTree =
  case parseTree of
    Leaf _ -> 1
    And left right ->
      (countPropvars left) + (countPropvars right)
    Or left right ->
      (countPropvars left) + (countPropvars right)
    Implication left right ->
      (countPropvars left) + (countPropvars right)
    Not subtree ->
      countPropvars subtree
    Empty -> 0


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Formula", onInput ChangeInput ] []
    , viewErrorMessage model
    , viewTokens model
    , viewParseTree model
    , viewTruthTable model
    ]

viewErrorMessage : Model -> Html msg
viewErrorMessage model =
  div []
    [ h4 [] [ text (getErrorOrOkMessage model) ]
    ]

getErrorOrOkMessage : Model -> String
getErrorOrOkMessage model =
  if   String.isEmpty model.errorMessage
  then "---"
  else "Error: " ++ model.errorMessage

viewTokens : Model -> Html msg
viewTokens model =
  let
    tokenDivs = List.map (\ token -> div [] [ text (toString token) ]) model.tokens
  in
    div []
      ((h4 [] [ text "Tokenizer - Token list:" ] ) :: tokenDivs)

viewParseTree : Model -> Html msg
viewParseTree model =
  div []
    [ h4 [] [ text "Parser - Parse tree:" ]
    , text (toString model.parseTree)
    ]

viewTruthTable : Model -> Html msg
viewTruthTable model =
  div []
    [ h4 [] [ text "Truth table" ]
    , text (toString model.truthTable.nPropvars)
    ]

viewModeSelector : Html Msg
viewModeSelector =
  div []
    [ text "Mode selector: "
    , button [ onClick (ChangeMode Tokenizer) ] [ text "Tokenizer"]
    , button [ onClick (ChangeMode Parser) ] [ text "Parser"]
    , button [ onClick (ChangeMode TruthTabler) ] [ text "TruthTabler"]
    ]