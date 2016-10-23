module Example exposing (sql)

import SqlBuilder exposing (..)


-- Some boilerplate for additional type safety of table and column names.
-- Not essential, but makes the query a little easier to read and makes the query
-- a little less prone to typos.

id = "id"
name = "name"
rating = "rating"

movie =
  { id = id
  , name = name
  , rating = rating
  }

tables =
  { movie = "movie"
  }


-- the query

sql =
  select [id, name, rating]
    |> from tables.movie
    -- |> join
    |> where'
      ( movie.name `contains` "big"
        `or` (movie.name `contains` "little")
        `and` (movie.rating `gt` 7)
      )
    |> orderBy movie.rating Desc
