module SqlBuilder exposing (..)

import String


type Placeholder =
  Int Int
  | String String
  -- TODO: add more types such as dates

type QueryPart =
  Query String | Placeholder Placeholder

type alias Query = List QueryPart


-- select

select : List String -> Query
select columns =
  let
    columnsString = String.join ", " columns
  in
    [ Query <| "SELECT " ++ columnsString ]

allColumns : List String
allColumns =
  ["*"]


-- from

from : String -> Query -> Query
from table pre =
  pre ++ [ Query <| "\nFROM " ++ table ]


-- where

where' : Query -> Query -> Query
where' whereQuery pre =
  pre ++ [ Query "\nWHERE " ] ++ whereQuery

-- join


-- join =
--   join' "JOIN"
--
-- innerJoin =
--   join' "INNER JOIN"
--
-- leftJoin =
--   join' "LEFT JOIN"
--
-- leftOuterJoin =
--   join' "LEFT OUTER JOIN"
--
-- rightJoin =
--   join' "RIGHT JOIN"
--
-- rightOuterJoin =
--   join' "RIGHT OUTER JOIN"
--
-- fullJoin =
--   join' "FULL JOIN"
--
-- fullOuterJoin =
--   join' "FULL OUTER JOIN"
--
-- join' : Query -> Query -> Query
-- join' joinType pre =
--   pre ++  [ Query <| "\n" ++ joinType ]

-- boolean

and : Query -> Query -> Query
and pre post =
  pre ++ [ Query " AND " ] ++  post

or : Query -> Query -> Query
or pre post =
  pre ++ [Query " OR "] ++  post


-- string matching

contains : String -> String -> Query
contains column s =
  [ Query <| column ++ " LIKE " ++ "\"%" ++ s ++ "%\"" ]


-- comparators

gt : String -> number -> Query
gt column n =
  [ Query <| column ++ " > " ++ toString n ]

gte : String -> number -> Query
gte column n =
  [ Query <| column ++ " >= " ++ toString n ]

lt : String -> number -> Query
lt column n =
  [ Query <| column ++ " < " ++ toString n ]

lte : String -> number -> Query
lte column n =
  [ Query <| column ++ " <= " ++ toString n ]


-- ordering

type OrderBy =
  Asc | Desc

orderBy : String -> OrderBy -> Query -> Query
orderBy column orderBy pre =
  let
    order = toString orderBy |> String.toUpper
  in
    pre ++ [ Query <| "\nORDER BY " ++ column ++ " " ++ order ]

-- TODO
-- orderByMulti : List (String, OrderBy) -> String -> String
-- orderByMulti


-- Display SQL

prettyPrint : Query -> String
prettyPrint query =
  let
    displayQueryPart queryPart =
      case queryPart of
        Query s ->
          s
        Placeholder p ->
          case p of
            Int i ->
              toString i
            String s ->
              s
  in
    List.map displayQueryPart query
    |> String.join " "
