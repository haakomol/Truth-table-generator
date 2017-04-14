module View exposing (view)

import Html exposing (Html, Attribute, div, span, input, text, button, h1, h2, h3, h4, h5, h6, fieldset, label)
import Html.Attributes exposing (placeholder, type_, checked)
import Html.Events exposing (onInput, onClick)

import Types exposing (..)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Formula", onInput ChangeInput ] []
    , viewParseInfoPanel model
    , viewTruthTable model
    ]

viewParseInfoPanel : Model -> Html Msg
viewParseInfoPanel model =
  let
    viewParseInfoSwitch =
      label []
        [ input [ type_ "checkbox", onClick ToggleParseInfo, checked True ] []
        , text "Show parse info"
        ]
    viewParseInfoContent model =
      if model.showParseInfo
      then
        div []
          [ viewErrorMessage model
          , viewTokens model
          , viewParseTree model
          ]
      else
        div [] []
  in
    div []
      [ -- viewParseInfoSwitch,
        viewParseInfoContent model
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
    tokenSpans = List.map (\ token -> span [] [ text ("[" ++ (toString token) ++ "]  ") ]) model.tokens
  in
    div []
      ((h4 [] [ text "Tokenizer - Token list:" ] ) :: tokenSpans)

viewParseTree : Model -> Html msg
viewParseTree model =
  div []
    [ h4 [] [ text "Parser - Parse tree:" ]
    , text (renderParseTreeToString model)
    ]

renderParseTreeToString : Model -> String
renderParseTreeToString model =
  case model.parseTree of
    Nothing ->
      "Parse tree is empty"
    Just parseTree ->
      toString parseTree

viewTruthTable : Model -> Html msg
viewTruthTable model =
  div []
    (List.append
      [ h4 [] [ text "Truth table" ] ]
      (List.map (\ truthTableRow -> (div [] [ text (toString truthTableRow) ] ) ) model.truthTable))