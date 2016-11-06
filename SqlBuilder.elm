module SqlBuilder exposing (..)

import String
import SqlBuilder.AST exposing (..)


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

-- ouch, this is getting really ugly! Need type classes!
onColumn : String -> AExpr
onColumn name =
  CExpr <| ColId name


selectColumns : List String -> SimpleSelect
selectColumns columnNames =
  let
    targetList =
      List.map (\name -> column name) columnNames
  in
    select targetList


count : String -> TargetEl
count columnName =
  TargetElAExpr <| function "COUNT" [CExpr <| ColId columnName]


sum : String -> TargetEl
sum columnName =
  TargetElAExpr <| function "SUM" [CExpr <| ColId columnName]

-- function : String -> List AExpr -> AExpr
-- function functionName args =
--   CExpr <| FuncExpr <| Function (functionName, args)

countStar : TargetEl
countStar =
  TargetElAExpr <| CExpr <| FuncExpr <| FunctionStar "COUNT"


asColumn : String -> TargetEl -> TargetEl
asColumn label targetEl =
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

fromOne : TableRef -> SimpleSelect -> SimpleSelect
fromOne tableRef simpleSelect =
  { simpleSelect
  | fromClause = Just [tableRef]
  }


fromTable : String -> SimpleSelect -> SimpleSelect
fromTable tableName simpleSelect =
  simpleSelect
  |> from [ TableRef (QualifiedName tableName) Nothing ]

table : String -> TableRef
table tableName =
  TableRef (QualifiedName tableName) Nothing


fromSelect : SimpleSelect -> String -> SimpleSelect -> SimpleSelect
fromSelect subSelect alias_ currentSelect =
  currentSelect
  |> from
     [ TableRefSelect
          subSelect
          (AliasClauseAsColId alias_)
     ]

selectionAsTable : SimpleSelect -> String -> TableRef
selectionAsTable simpleSelect name =
  TableRefSelect simpleSelect (AliasClauseAsColId name)

asTable : String -> TableRef -> TableRef
asTable columnName tableRef =
  case tableRef of
    TableRef relationExpr maybeAliasClause ->
      TableRef relationExpr (Just <| AliasClauseAsColId columnName)
    TableRefSelect _ _ -> -- there is actually a select_no_parens in between, but we're assuming SimpleSelect will get us far enough at the moment
      Debug.crash "Sorry, asTable is not yet supported for type TableRefSelect"
    TableRefJoinedTable _ ->
      Debug.crash "Sorry, asTable is not yet supported for type TableRefJoinedTable"

-- joinedTables leftAExpr joinType rightAexpr

-- A little trick so the DSL reads a bit better
type alias On = ()
on : On
on = ()

join : JoinType -> TableRef -> On -> AExpr -> TableRef -> TableRef
join joinType rightTableRef  _ aExpr leftTableRef=
  TableRefJoinedTable
    <| JoinedTable leftTableRef joinType rightTableRef (JoinQualOn aExpr)

innerJoin : TableRef -> On -> AExpr -> TableRef -> TableRef
innerJoin =
  join InnerJoin


innerJoinTable : String -> On -> AExpr -> TableRef -> TableRef
innerJoinTable tableName =
  join InnerJoin (TableRef (QualifiedName tableName) Nothing)


innerJoinTableAs : String -> String -> On -> AExpr -> TableRef -> TableRef
innerJoinTableAs tableName tableAlias =
  join
    InnerJoin
    (TableRef (QualifiedName tableName) (Just <| AliasClauseAsColId tableAlias))

-- innerJoinSelection : SimpleSelect -> String -> On -> AExpr -> TableRef -> TableRef
-- innerJoinSelection selection tableAlias =
--   let
--     selectionTableRef
--   in
--     join
--       InnerJoin
--       (TableRef TableRefJoinedTable ) (Just <| AliasClauseAsColId tableAlias))
--   JoinedTable TableRef JoinType TableRef JoinQual

-- join : JoinType -> TableRef -> On -> AExpr -> TableRef -> TableRef
-- join joinType rightTableRef  _ aExpr leftTableRef =
innerJoinSelectionAs : SimpleSelect -> String -> On ->  AExpr -> TableRef -> TableRef
innerJoinSelectionAs rightSelection rightSelectionAlias _ joinExpr leftTableRef =
  let
    rightTableRef = TableRefSelect rightSelection (AliasClauseAsColId rightSelectionAlias)
  in
    TableRefJoinedTable
      <| JoinedTable leftTableRef InnerJoin rightTableRef (JoinQualOn joinExpr)

equalColumns : (String, String) -> AExpr
equalColumns (leftColumn, rightColumn) =
  let
    leftAExpr = CExpr <| ColId leftColumn
    rightAExpr = CExpr <| ColId rightColumn
  in
    Equals leftAExpr rightAExpr

gte : AExpr -> AExpr -> AExpr
gte rightAExpr leftAExpr =
    GreaterThanEquals leftAExpr rightAExpr



where_ : WhereClause -> SimpleSelect -> SimpleSelect
where_ whereClause simpleSelect =
  { simpleSelect
  | whereClause = Just whereClause
  }




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
