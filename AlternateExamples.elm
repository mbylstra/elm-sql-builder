select
  { columns = ...
  , from = ...
  , where = ...
  , orderBy = Nothing
  , having = Nothing
  }


select
  { defaults |
    columns = ...
  , from = ...
  , where = ...
  }


-- can just go straight to the AST, but the capitals are a bit inconsistent
Select
  { defaults
  | columns = ...
  | from = ...
  | where = ...
  }


selectAllColumns
  |> ("date_created" |> withPrecision "month"|> as "month")


where
  "rating" |> greaterThan 7
  |> and
     (  ("title" |> contains "little")
        |> or ("title" |> contains "big")
     )


selectAllColumns
  |> date"
