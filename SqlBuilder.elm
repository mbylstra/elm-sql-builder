module SqlBuilder exposing (..)

import String


-- select

select : List String -> String
select columns =
  let
    columnsString = String.join ", " columns
  in
    "SELECT " ++ columnsString

allColumns : List String
allColumns =
  ["*"]


-- from

from : String -> String -> String
from table pre =
  pre ++ "\nFROM " ++ table


-- where

where' : String -> String -> String
where' s pre =
  pre ++ "\nWHERE " ++ s


-- boolean

and : String -> String -> String
and pre post =
  pre ++ " AND " ++  post

or : String -> String -> String
or pre post =
  pre ++ " AND " ++  post


-- string matching

contains : String -> String -> String
contains column s =
  column ++ " LIKE " ++ "\"%" ++ s ++ "%\""


-- comparators

gt : String -> number -> String
gt column n =
  column ++ " > " ++ toString n

gte : String -> number -> String
gte column n =
  column ++ " >= " ++ toString n

lt : String -> number -> String
lt column n =
  column ++ " < " ++ toString n

lte : String -> number -> String
lte column n =
  column ++ " <= " ++ toString n


-- ordering

type OrderBy =
  Asc | Desc

orderBy : String -> OrderBy -> String -> String
orderBy column orderBy pre =
  let
    order = toString orderBy |> String.toUpper
  in
    pre ++ "\nORDER BY " ++ column ++ " " ++ order

-- TODO
-- orderByMulti : List (String, OrderBy) -> String -> String
-- orderByMulti
