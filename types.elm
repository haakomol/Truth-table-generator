module Types exposing (..)

type alias Model =
  { input : String
  , errorMessage : String
  , showParseInfo : Bool
  , tokens : List Token
  , parseTree : Maybe ParseTree
  , truthTable : TruthTable
  }

init : (Model, Cmd Msg)
init =
  (Model "" "" True [] Nothing (TruthTable 0), Cmd.none)

type Msg
  = ChangeInput String
  | ToggleParseInfo

type alias TruthTable =
  { nPropvars : Int
  }

type ParseTree
  = Leaf String
  | Binary ParseTree BinaryOperator ParseTree
  | Not ParseTree

type BinaryOperator
  = And
  | Or
  | Implication

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