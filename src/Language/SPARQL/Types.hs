{-# LANGUAGE DeriveAnyClass
           , TypeFamilies
           #-}

module Language.SPARQL.Types where

import Data.RDF.Types

import qualified Data.Set  as S

import Data.String

import qualified Data.Text as T

import GHC.Exts

newtype Var = Var { unVar :: T.Text }
            deriving ( Eq
                     , Ord
                     , Read
                     , Show
                     )

instance IsString Var where
    fromString = Var . T.pack

var :: Var -> Term a
var = TermVar

data Term a = TermLit a
            | TermVar Var
            deriving ( Eq
                     , Ord
                     , Read
                     , Show
                     )

instance IsString a => IsString (Term a) where
    fromString = TermLit . fromString

type SubjectTerm = Term Subject

type PredicateTerm = Term Predicate

type ObjectTerm = Term Object

data Pattern = TriplePattern !SubjectTerm !PredicateTerm !ObjectTerm
             deriving ( Eq
                      , Ord
                      , Read
                      , Show
                      )

triple :: SubjectTerm -> PredicateTerm -> ObjectTerm -> Pattern
triple = TriplePattern

data Group = BlockGroup    (S.Set Pattern)
           | OptionalGroup Group Group
           | UnionGroup    Group Group
           | GraphGroup    IRI   Group
           deriving ( Eq
                    , Ord
                    , Read
                    , Show
                    )

instance IsList Group where
    type Item Group = Pattern
    fromList              = BlockGroup . fromList
    toList (BlockGroup s) = toList s
    toList _              = error "Only BlockGroup may be converted to a list."

infixl 6 `optional`

optional :: Group -> Group -> Group
optional = OptionalGroup

infixr 5  `union`

union :: Group -> Group -> Group
union = UnionGroup

data ResultSet = AllVars
               | Vars (S.Set Var)
               deriving ( Eq
                        , Ord
                        , Read
                        , Show
                        )

instance IsList ResultSet where
    type Item ResultSet = Var
    fromList        = Vars . fromList
    toList (Vars s) = toList s
    toList _        = error "Only Vars may be converted to a list."

data Query = SelectQuery ResultSet Group
           | ConstructQuery Group Group
           | AskQuery Group
           | DescribeQuery ResultSet Group
           deriving ( Eq
                    , Ord
                    , Read
                    , Show
                    )

-- Implement checks that result set is valid.

select :: ResultSet -> Group -> Query
select = SelectQuery

construct :: Group -> Group -> Query
construct = ConstructQuery

ask :: Group -> Query
ask = AskQuery

describe :: ResultSet -> Group -> Query
describe = DescribeQuery
