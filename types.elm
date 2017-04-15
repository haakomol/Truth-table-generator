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
  ((Model "" "" True [] Nothing emptyTruthTable), Cmd.none)

type Msg
  = ChangeInput String
  | ToggleParseInfo

type alias TruthTable =
  { ttHeader : TTHeader
  , ttRows : List TTRow
  }

emptyTruthTable : TruthTable
emptyTruthTable =
  { ttHeader = []
  , ttRows = []
  }

type TruthValueFormat
  = OneAndZero
  | TAndF
  | TrueAndFalse

type alias TTRow = List (Maybe Bool)

type alias TTHeader = List TTHeaderToken

type TTHeaderToken
  = TTHPropvarToken Propvar
  | TTHLeftParToken
  | TTHRightParToken
  | TTHAndToken
  | TTHOrToken
  | TTHImplicationToken
  | TTHNotToken

type alias Propvar = String

type SemanticTree
  = SemLeaf Bool Propvar
  | SemNot Bool SemanticTree
  | SemBinary Bool SemanticTree BinaryOperator SemanticTree

type alias PropvarWithValue = (Propvar, Bool)

type alias Valuation = List PropvarWithValue

type alias SemTreeWithValuation = (SemanticTree, Valuation)

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