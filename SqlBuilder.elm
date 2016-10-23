module SqlBuilder exposing (..)

import String


type Parameter =
  Number String
  | String String
  -- TODO: add more types such as dates

type QueryPart =
  Sql String | Parameter Parameter


type alias Query = List QueryPart


-- select

select : List String -> Query
select columns =
  let
    columnsString = String.join ", " columns
  in
    [ Sql <| "SELECT " ++ columnsString ]

allColumns : List String
allColumns =
  ["*"]


-- from

from : String -> Query -> Query
from table pre =
  pre ++ [ Sql <| "\nFROM " ++ table ]


-- where

where' : Query -> Query -> Query
where' whereSql pre =
  pre ++ [ Sql "\nWHERE " ] ++ whereSql

-- join


join =
  join' "JOIN"

innerJoin =
  join' "INNER JOIN"

leftJoin =
  join' "LEFT JOIN"

leftOuterJoin =
  join' "LEFT OUTER JOIN"

rightJoin =
  join' "RIGHT JOIN"

rightOuterJoin =
  join' "RIGHT OUTER JOIN"

fullJoin =
  join' "FULL JOIN"

fullOuterJoin =
  join' "FULL OUTER JOIN"

join' : String -> String -> Query -> Query -> Query
join' joinType table onCondition pre =
  pre ++ [ Sql <| "\n" ++ joinType ++ " " ++ table ++ " ON " ] ++ onCondition

-- boolean

and : Query -> Query -> Query
and pre post =
  pre ++ [ Sql " AND " ] ++  post

or : Query -> Query -> Query
or pre post =
  pre ++ [Sql " OR "] ++  post


-- string matching

contains : String -> String -> Query
contains column s =
  [ Sql <| column ++ " LIKE " ++ "\"%" ] ++ [ Parameter <| String s ] ++ [ Sql "%\"" ]


-- comparators

eq : String -> comparable -> Query
eq column n =
  [ Sql <| column ++ " = " ] ++ [ Parameter <| Number (toString n) ]

gt : String -> number -> Query
gt column n =
  [ Sql <| column ++ " > " ] ++ [ Parameter <| Number (toString n) ]

gte : String -> number -> Query
gte column n =
  [ Sql <| column ++ " >= " ] ++ [ Parameter <| Number (toString n) ]

lt : String -> number -> Query
lt column n =
  [ Sql <| column ++ " < " ] ++ [ Parameter <| Number (toString n) ]

lte : String -> number -> Query
lte column n =
  [ Sql <| column ++ " <= " ] ++ [ Parameter <| Number (toString n) ]


-- ordering

type OrderBy =
  Asc | Desc

orderBy : String -> OrderBy -> Query -> Query
orderBy column orderBy pre =
  let
    order = toString orderBy |> String.toUpper
  in
    pre ++ [ Sql <| "\nORDER BY " ++ column ++ " " ++ order ]

-- TODO
-- orderByMulti : List (String, OrderBy) -> String -> String
-- orderByMulti


-- Display SQL

prettyPrint : Query -> String
prettyPrint query =
  let
    displaySqlPart queryPart =
      case queryPart of
        Sql s ->
          s
        Parameter p ->
          case p of
            Number i ->
              toString i
            String s ->
              s
  in
    List.map displaySqlPart query
    |> String.join " "
