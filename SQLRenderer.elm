module SQLRenderer exposing (..)

import String
import AST exposing (..)

escapeString : String -> String
escapeString s = s
  -- BIG TODO! needs LOTS of care


renderAllColumns : String
renderAllColumns = "*"


renderCExpr : CExpr -> String
renderCExpr cExpr =
  case cExpr of
    ColId colId ->
      colId
    AExprConst const ->
      case const of
        Iconst i ->
          toString i
        Sconst s ->
          -- TODO: this is the really important part where we need to escape stuff!
          "'" ++ escapeString s ++ "'"
    FuncExpr funcExpr ->
      case funcExpr of
        Function (name, args) ->
          let
            argsStrings =
              List.map renderAExpr args
          in
            name ++ "(" ++ (String.join ", " argsStrings) ++ ")"
        FunctionStar name ->
            name ++ "(*)"


renderAExpr : AExpr -> String
renderAExpr aExpr =
  case aExpr of
    CExpr cExpr ->
      renderCExpr cExpr
    And leftAExpr rightAExpr ->
      renderAExpr leftAExpr ++ " AND " ++ renderAExpr rightAExpr
    Or leftAExpr rightAExpr ->
      renderAExpr leftAExpr ++ " OR " ++ renderAExpr rightAExpr
    GreaterThan leftAExpr rightAExpr ->
      renderAExpr leftAExpr ++ " > " ++ renderAExpr rightAExpr
    Equals leftAExpr rightAExpr ->
      renderAExpr leftAExpr ++ " = " ++ renderAExpr rightAExpr


renderTargetEl : TargetEl -> String
renderTargetEl targetEl =
  case targetEl of
    TargetElAExpr aExpr ->
      renderAExpr aExpr
    TargetElAexprWithAlias aExpr label ->
      renderAExpr aExpr ++ " AS " ++ label
    AllColumns ->
      renderAllColumns


renderTargetList : TargetList -> String
renderTargetList targetList =
  List.map renderTargetEl targetList
  |> String.join ", "


renderRelationExpr : RelationExpr -> String
renderRelationExpr relationExpr =
  case relationExpr of
    QualifiedNameList names ->
      String.join "," names


renderTableRef : TableRef -> String
renderTableRef tableRef =
  case tableRef of
    TableRef relationExpr ->
      renderRelationExpr relationExpr
    TableRefSelect simpleSelect aliasClause ->
      "(" ++ renderSimpleSelect simpleSelect ++ "\n) " ++ renderAliasClause aliasClause

renderAliasClause : AliasClause -> String
renderAliasClause aliasClause =
  case aliasClause of
    AliasClauseAsColId colId ->
      " AS " ++ colId
    AliasClauseJustColId colId ->
      colId
      -- renderCol


renderFromClause : Maybe FromClause -> String
renderFromClause maybeFromClause =
  case maybeFromClause of
    Just fromClause ->
      "\nFROM " ++ (List.map renderTableRef fromClause |> String.join "")
    Nothing ->
      ""


renderWhereClause : Maybe WhereClause -> String
renderWhereClause maybeWhereClause =
  case maybeWhereClause of
    Nothing ->
      ""
    Just whereClause ->
      "\nWHERE " ++ renderAExpr whereClause

renderGroupClause : Maybe GroupClause -> String
renderGroupClause maybeGroupClause =
  case maybeGroupClause of
    Nothing ->
      ""
    Just listAExpr ->
      "\nGROUP BY " ++
      ( List.map renderAExpr listAExpr
        |> String.join ", "
      )

renderSortBy : (AExpr, AscDesc) -> String
renderSortBy (aExpr, ascDesc) =
  renderAExpr aExpr ++ " " ++ renderAscDesc ascDesc

renderAscDesc : AscDesc -> String
renderAscDesc ascDesc =
  case ascDesc of
    Ascending -> "ASC"
    Descending -> "DESC"

renderSortClause : Maybe SortClause -> String
renderSortClause maybeSortClause =
  case maybeSortClause of
    Nothing ->
      ""
    Just listSortBy ->
      "\nORDER BY " ++
      ( List.map renderSortBy listSortBy
        |> String.join ", "
      )


renderSimpleSelect : SimpleSelect -> String
renderSimpleSelect simpleSelect =
  "\nSELECT "
  ++ renderTargetList simpleSelect.targetList
  ++ renderFromClause simpleSelect.fromClause
  ++ renderWhereClause simpleSelect.whereClause
  ++ renderGroupClause simpleSelect.groupClause
  ++ renderSortClause simpleSelect.sortClause
