module View exposing (view)

import Html exposing
  (Html, Attribute, div, span, input, text, button, h1, h2, h3, h4, h5, h6, label, table, th, tr, td)
import Html.Attributes exposing (placeholder, type_, checked)
import Html.Events exposing (onInput, onClick)

import Types exposing (..)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Formula", onInput ChangeInput ] []
    , viewParseInfoPanel model
    , viewTruthTable model.truthTable
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

viewTruthTable : TruthTable -> Html msg
viewTruthTable truthTable =
  let
    ttHeader = ttHeaderToTableRow truthTable.ttHeader
    ttRows = List.map ttRowToTableRow truthTable.ttRows
  in
    div []
        [ h4 [] [ text "Truth table" ]
        , table []
          (ttHeader :: ttRows)
      ]

ttHeaderToTableRow : TTHeader -> Html msg
ttHeaderToTableRow ttHeader =
  let
    headerStrings = List.map ttHeaderTokenToString ttHeader
  in
    listToTableRow headerStrings True

ttRowToTableRow : List (Maybe Bool) -> Html msg
ttRowToTableRow ttRow =
  let
    rowStrings = List.map (rowMaybeBoolToString OneAndZero) ttRow
  in
    listToTableRow rowStrings False

ttHeaderTokenToString : TTHeaderToken -> String
ttHeaderTokenToString ttHeaderToken =
  case ttHeaderToken of
    TTHPropvarToken propvar -> propvar
    TTHLeftParToken -> "("
    TTHRightParToken -> ")"
    TTHAndToken -> "∧"
    TTHOrToken -> "∨"
    TTHImplicationToken -> "→"
    TTHNotToken -> "¬"

rowMaybeBoolToString : TruthValueFormat -> (Maybe Bool) -> String
rowMaybeBoolToString truthValueFormat maybeBool =
  case maybeBool of
    Nothing -> ""
    Just bool ->
      case truthValueFormat of
        OneAndZero ->
          case bool of
            True -> "1"
            False -> "0"
        TAndF ->
          case bool of
            True -> "T"
            False -> "F"
        TrueAndFalse ->
          case bool of
            True -> "True"
            False -> "False"

listToTableRow : List String -> Bool -> Html msg
listToTableRow lst isHeaderRow =
  let
    tableCellType =
      case isHeaderRow of
        True -> th
        False -> td
    cells = List.map (\ item -> tableCellType [] [ text item ]) lst
  in
    tr [] cells