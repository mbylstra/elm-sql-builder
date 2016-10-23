# Elm SQL Builder
A little experiment to see what an embedded DSL for SQL might look like in Elm.

The following code:

```elm
sql =
  select ["name", "rating"]
    |> from "movie"
    |> where'
      ( "name" `contains` "big"
        `or` "name" `contains` "little"
        `and` "rating" `gt` 7
      )
    |> orderBy "rating" Desc

```

produces this SQL:

```sql
SELECT name, rating
FROM movie
WHERE name LIKE "%big%" OR name LIKE "%little%" AND rating > 7
ORDER BY rating DESC
```

This is not even remotely ready to be used. There's a lot of huge features missing:

- [ ] joins
- [ ] sub queries
- [ ] so much more
