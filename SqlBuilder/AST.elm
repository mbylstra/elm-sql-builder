module SqlBuilder.AST exposing (..)

-- SELECT opt_all_clause opt_target_list
-- into_clause from_clause where_clause
-- group_clause having_clause window_clause

type alias SimpleSelect =
  { targetList : TargetList
  , fromClause : Maybe FromClause
  , whereClause : Maybe WhereClause
  , groupClause : Maybe GroupClause

  -- This is a simplification of the Postgres AST. The sortClause should not
  -- be part of SimpleSelect, it should be part of select_no_parens, which
  -- simpleSelect is a member of.
  , sortClause : Maybe SortClause
  }

type alias WhereClause = AExpr

type alias FromClause = List TableRef

type TableRef =
  TableRef RelationExpr (Maybe AliasClause)
  | TableRefSelect SimpleSelect AliasClause -- there is actually a select_no_parens in between, but we're assuming SimpleSelect will get us far enough at the moment
  | TableRefJoinedTable JoinedTable

type RelationExpr =
  QualifiedName ColId

type JoinedTable =
  JoinedTable TableRef JoinType TableRef JoinQual

type JoinType =
  InnerJoin
  | LeftJoin
  | LeftOuterJoin
  | RightJoin
  | RightOuterJoin
  | FullJoin
  | FullOuterJoin

type JoinQual =
  JoinQualOn AExpr

type alias TargetList = List TargetEl

type TargetEl =
  TargetElAExpr AExpr
  | TargetElAexprWithAlias AExpr ColLabel
  | AllColumns

type alias ColLabel = String

type AExpr =
  CExpr CExpr
  | And AExpr AExpr
  | Or AExpr AExpr
  | GreaterThan AExpr AExpr
  | GreaterThanEquals AExpr AExpr
  | LessThan AExpr AExpr
  | LessThanEquals AExpr AExpr
  | Equals AExpr AExpr

type CExpr =
  ColId ColId
  | AExprConst AExprConst
  | FuncExpr FuncExpr

type AExprConst =
  Iconst Int
  | Sconst String

type AliasClause =
 AliasClauseAsColId ColId
 | AliasClauseJustColId ColId

type alias GroupClause =
  List AExpr

type alias SortClause =
  List SortBy

type alias SortBy =
  (AExpr, AscDesc) -- TODO: opt_nulls_order

type AscDesc =
  Ascending | Descending


 -------------------------------------------------------------------------------
-- Examples
 -------------------------------------------------------------------------------

type alias ColId = String

ratingGt10 : AExpr
ratingGt10 =
  GreaterThan (CExpr <| ColId "rating") (CExpr <| AExprConst <| Iconst 3)

orExample : AExpr
orExample =
  Or
  ratingGt10
  ratingGt10


categoryEqualsComedy : AExpr
categoryEqualsComedy =
  Equals (CExpr <| ColId "category" ) (CExpr <| AExprConst <| Sconst "comedy" )


columns : TargetList
columns =
    [ TargetElAExpr <| CExpr <| ColId "id"
    , TargetElAExpr <| CExpr <| ColId "name"
    , TargetElAExpr <| CExpr <| ColId "rating"
    ]

-- fromMovie : List TableRef
-- fromMovie =
--   [ TableRef <| QualifiedNameList [ "movie" ] ]

-- simpleSelectExample : SimpleSelect
-- simpleSelectExample =
--   { targetList = columns
--   , fromClause = Just fromMovie
--   , whereClause = Just (Or ratingGt10 categoryEqualsComedy)
--   , groupClause = Nothing
--   , sortClause = Nothing
--   }


-- funcExpr

type alias FuncExpr =
  FuncApplication

type FuncApplication =
  Function (String, List AExpr)
  | FunctionStar String


--------------------------------------------------------------------------------
-- Links
--------------------------------------------------------------------------------
-- gram.y (postgresql grammer)

-- SelectStmt
-- https://github.com/postgres/postgres/blob/master/src/backend/parser/gram.y#L10041
-- https://github.com/postgres/postgres/blob/master/src/backend/parser/gram.y#L10061
-- https://github.com/postgres/postgres/blob/master/src/backend/parser/gram.y#L10148
-- https://github.com/postgres/postgres/blob/master/src/backend/parser/gram.y#L10148

-- a_expr
-- https://github.com/postgres/postgres/blob/master/src/backend/parser/gram.y#L11599

-- #12082 c_expr

-- OR
-- https://github.com/postgres/postgres/blob/master/src/backend/parser/gram.y#L11661

-- AND
-- https://github.com/postgres/postgres/blob/master/src/backend/parser/gram.y#L11663

-- LIKE
-- https://github.com/postgres/postgres/blob/master/src/backend/parser/gram.y#L11670

-- qualOp
-- https://github.com/postgres/postgres/blob/master/src/backend/parser/gram.y#L12924
