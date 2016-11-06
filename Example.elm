module Example exposing (..)
import SqlBuilder.AST exposing (..)

import SqlBuilder exposing (..)

example: SimpleSelect
example = formattedNumReposCreatedPerMonth


formattedNumReposCreatedPerMonth : SimpleSelect
formattedNumReposCreatedPerMonth =
  select
    [ column "created_at_month" |> formatDate "YYYY" |> asColumn "year"
    , column "created_at_month" |> formatDate "Mon" |> asColumn "month"
    , column "total_repos_created"
    ]
  |> fromSelect numReposCreatedPerMonth "unformatted_repos_created_per_month"


numReposCreatedPerMonth : SimpleSelect
numReposCreatedPerMonth =
  select
    [ countStar |> asColumn "total_repos_created"
    , column "created_at" |> withPrecision Month |> asColumn "created_at_month"
    ]
  |> fromTable "github_repository"
  |> groupByColumn "created_at_month"
  |> sortByColumn "created_at_month" Ascending

runningTotalReposByMonth : SimpleSelect
runningTotalReposByMonth =
  select
    [ sum "repos_per_month_2.total_repos_created" |> asColumn "running_total"
    , column "repos_per_month_1.created_at_month"
    ]
  |> from
     [ selectionAsTable numReposCreatedPerMonth "repos_per_month_1"
       |> innerJoinSelectionAs numReposCreatedPerMonth "repos_per_month_2"
          on ( onColumn "repos_per_month_1.created_at_month"
               |> gte (onColumn "repos_per_month_2.created_at_month")
              )
     ]
  |> groupByColumn "repos_per_month_1.created_at_month"


-- generates:
-- SELECT
--    to_char(created_at_month, 'YYYY') AS month,
--    to_char(created_at_month, 'Mon') AS year,
--    total_repos_created
-- FROM
--    (
--        SELECT
--            COUNT(*) AS total_repos_created,
--            date_trunc('month', created_at) AS created_at_month
--         FROM github_repository
--         GROUP BY created_at_month
--         ORDER BY created_at_month ASC
--    ) AS unformatted_repos_created_per_month


-- tableRef = TableRef <| QualifiedNameList [ "author" ]

-- tableRef = table "author"


-- joinedTableRef =
--   tableRef
--   |> innerJoinTable "author" on (equalColumns ("book.author_id", "author.id"))

basicJoin : SimpleSelect
basicJoin =
  selectColumns ["book.name", "author.name"]
  |> from
    [ table "book"
      |> innerJoinTable "author"
         on (equalColumns ("book.author_id", "author.id"))
    ]


-- basicJoin =
--   selectColumns ["blah"]
--   |> ( fromTable "book"
--        |> (innerJoinTable "author" on (equalColumns ("book.author_id", "author.id"))
--      )


-- join : JoinType -> TableRef -> On -> AExpr -> TableRef -> TableRef
-- join joinType rightTableRef  _ aExpr leftTableRef=
--   TableRefJoinedTable
--     <| JoinedTable leftTableRef joinType rightTableRef (JoinQualOn aExpr)


 -- example of cumulative total

-- SELECT
--   s1.created_at,
--   COUNT(s2.email) AS cumul_count
-- FROM subscriptions s1
--   INNER JOIN subscriptions s2 ON s1.created_at >= s2.created_at
-- GROUP BY s1.created_at


-- rendered:

-- SELECT
--    s1.created_at,
--    COUNT(s2.email) AS cumul_count
-- FROM subscriptions
--  INNER JOINsubscriptions
--    ON s1.created_at >= s2.created_at
-- TODO: render AS for FROM, support AS for joins, fix spaces

-- SELECT
--   s1.created_at,
--   COUNT(s2.email) AS cumul_count
-- FROM subscriptions AS s1
--   INNER JOIN subscriptions AS s2
--     ON s1.created_at >= s2.created_at

cumulativeExample : SimpleSelect
cumulativeExample =
  select
    [ column "s1.created_at"
    , count "s2.email" |> asColumn "cumul_count"
    ]
    |> from
      [ table "subscriptions" |> asTable "s1"
        |> ( innerJoinTableAs "subscriptions" "s2"
               on (onColumn "s1.created_at" |> gte (onColumn "s2.created_at"))
              --  on ("s1.created_at" equals "s2.created_at")
           )
      ]
--   |> ( fromTable "subscriptions"
--        |> ( innerJoinTable "subscriptions"
--               on ("s1.created_at" gte "s2.created_at")
--           )
--      )

-- TODO:
--    COUNT
--    innerJoinTableAs
--    fromTableAs (or |> asColumn ?)
--        maybe asColumn and asTable ??
