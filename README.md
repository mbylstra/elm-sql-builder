# Elm SQL Builder
A little experiment to see what an embedded DSL for SQL might look like in Elm.

The following code:

```elm
formattedNumReposCreatedPerMonth =
  select
    [ column "created_at_month" |> formatDate "YYYY" |> as_ "year"
    , column "created_at_month" |> formatDate "Mon" |> as_ "month"
    , column "total_repos_created"
    ]
  |> fromSelect numReposCreatedPerMonth "unformatted_repos_created_per_month"

numReposCreatedPerMonth =
  select
    [ countStar |> as_ "total_repos_created"
    , column "created_at" |> withPrecision Month |> as_ "created_at_month"
    ]
  |> fromTable "github_repository"
  |> groupByColumn "created_at_month"
  |> sortByColumn "created_at_month" Ascending
```

produces this SQL:

```sql
SELECT
    to_char(created_at_month, 'YYYY') AS month,
    to_char(created_at_month, 'Mon') AS year,
    total_repos_created
FROM
    (
        SELECT
            COUNT(*) AS total_repos_created,
            date_trunc('month', created_at) AS created_at_month
        FROM github_repository
        GROUP BY created_at_month
        ORDER BY created_at_month ASC
    ) AS unformatted_repos_created_per_month
```

which generates these results (example):

```
year  | month  | total_repos_created
-------------------------
2016	| Jun	   | 332
2016	| Jul	   | 315
2016	| Aug	   | 349
2016	| Sep	   | 390
```

This is not even remotely ready to be used. There's a lot of huge features missing:

- [ ] comparison operators
- [ ] joins
- [ ] so much more
