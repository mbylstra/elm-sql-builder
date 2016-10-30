module SqlBuilder exposing (..)

import String
import AST exposing (..)


-- select

select : TargetList -> SimpleSelect
select targetList =
  { targetList = targetList
  , fromClause = Nothing
  , whereClause = Nothing
  , groupClause = Nothing
  , sortClause = Nothing
  }


column : String -> TargetEl
column name =
  TargetElAExpr <| CExpr <| ColId name


selectColumns : List String -> SimpleSelect
selectColumns columnNames =
  let
    targetList =
      List.map (\name -> column name) columnNames
  in
    select targetList


as_ : String -> TargetEl -> TargetEl
as_ label targetEl =
  case targetEl of
    TargetElAExpr aExpr ->
        TargetElAexprWithAlias aExpr label
    TargetElAexprWithAlias aExpr _ ->
        TargetElAexprWithAlias aExpr label
    AllColumns ->
      Debug.crash "Sorry, but you can't use the as_ function with an AllColumns target element"


type DatePrecision =
  Microseconds
  | Milliseconds
  | Second
  | Minute
  | Hour
  | Day
  | Week
  | Month
  | Quarter
  | Year
  | Decade
  | Century
  | Millennium


function : String -> List AExpr -> AExpr
function functionName args =
  CExpr <| FuncExpr <| Function (functionName, args)


withPrecision : DatePrecision -> TargetEl -> TargetEl
withPrecision precision targetEl =
  -- we are unwrapping (which can lead to Debug.crash) to make the DSL a little
  -- nicer. To be decided if this is a good idea.
  let
    wrapAExpr aExpr =
      let
        precisionString = toString precision |> String.toLower
        precisionAExpr = CExpr <| AExprConst <| Sconst precisionString
      in
        function "date_trunc" [precisionAExpr, aExpr]

  in
    case targetEl of
      TargetElAExpr aExpr ->
        TargetElAExpr (wrapAExpr aExpr)
      TargetElAexprWithAlias aExpr colLabel ->
        TargetElAexprWithAlias (wrapAExpr aExpr) colLabel
      AllColumns ->
        Debug.crash "Sorry, but you can't use the withPrecision function with an AllColumns expression"


formatDate : String -> TargetEl -> TargetEl
formatDate format targetEl =
  -- we are unwrapping (which can lead to Debug.crash) to make the DSL a little
  -- nicer. To be decided if this is a good idea.
  let
    wrapAExpr aExpr =
      let
        formatAExpr = CExpr <| AExprConst <| Sconst format
      in
        function "to_char" [aExpr, formatAExpr]

  in
    case targetEl of
      TargetElAExpr aExpr ->
        TargetElAExpr (wrapAExpr aExpr)
      TargetElAexprWithAlias aExpr colLabel ->
        TargetElAexprWithAlias (wrapAExpr aExpr) colLabel
      AllColumns ->
        Debug.crash "Sorry, but you can't use the formatDate function with an AllColumns expression"


from : FromClause -> SimpleSelect -> SimpleSelect
from fromClause simpleSelect =
  { simpleSelect
  | fromClause = Just fromClause
  }


fromTable : String -> SimpleSelect -> SimpleSelect
fromTable tableName simpleSelect =
  simpleSelect
  |> from [ TableRef <| QualifiedNameList [ tableName ] ]


fromSelect : SimpleSelect -> String -> SimpleSelect -> SimpleSelect
fromSelect subSelect alias_ currentSelect =
  currentSelect
  |> from
    [ TableRefSelect
        subSelect
        (AliasClauseAsColId alias_)
    ]


where_ : WhereClause -> SimpleSelect -> SimpleSelect
where_ whereClause simpleSelect =
  { simpleSelect
  | whereClause = Just whereClause
  }


countStar : TargetEl
countStar =
  TargetElAExpr <| CExpr <| FuncExpr <| FunctionStar "COUNT"


groupBy : List AExpr -> SimpleSelect -> SimpleSelect
groupBy items currentSelect =
  { currentSelect
  | groupClause = Just items
  }


groupByColumn : String -> SimpleSelect -> SimpleSelect
groupByColumn columnName currentSelect =
  groupBy [CExpr <| ColId columnName] currentSelect


sortBy : List SortBy -> SimpleSelect -> SimpleSelect
sortBy items currentSelect =
  { currentSelect
  | sortClause = Just items
  }


sortByColumn : String -> AscDesc -> SimpleSelect -> SimpleSelect
sortByColumn columnName ascDesc currentSelect =
  sortBy [(CExpr <| ColId columnName, ascDesc)] currentSelect
