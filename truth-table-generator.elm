import Html exposing (Html)

import Types exposing (..)
import Tokenizer exposing (..)
import Parser exposing (..)
import TruthTabler exposing (..)
import View exposing (..)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ToggleParseInfo ->
      ({ model | showParseInfo = not model.showParseInfo }, Cmd.none)
    ChangeInput newInput ->
      let
        tokenizeResult = tokenize newInput
      in
        case tokenizeResult of
          Err errorMessage ->
            ( { model | errorMessage = errorMessage,
                        tokens = [],
                        parseTree = Nothing }, Cmd.none)
          Ok tokens ->
            let
              parseTreeResult = parseTokens tokens
            in
              case parseTreeResult of
                Err errorMessage ->
                  ( { model | errorMessage = errorMessage,
                              tokens = tokens,
                              parseTree = Nothing }, Cmd.none)
                Ok parseTree ->
                  let
                    truthTable = getTruthTable parseTree
                  in
                    ( { model | tokens = tokens,
                                parseTree = Just parseTree,
                                truthTable = truthTable,
                                errorMessage = "" }, Cmd.none)
        
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none