module Types exposing (..)

type Mode
  = Tokenizer
  | Parser
  | TruthTabler

type alias TruthTable =
  { nPropvars : Int
  }

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