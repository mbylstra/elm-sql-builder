module Example exposing (sql)

import SqlBuilder exposing (..)


-- Some boilerplate for additional type safety of table and column names.
-- Not essential, but makes the query a little easier to read and makes the query
-- a little less prone to typos.

id = "id"
name = "name"
rating = "rating"
directorId = "directorId"

movie =
  { id = id
  , name = name
  , rating = rating
  , directorId = directorId
  }

tables =
  { movie = "movie"
  , director = "director"
  }

director =
  { id = id
  , name = name
  }


-- the query

-- joinThingo = join tables.director

sql =
  select [id, name, rating]
    |> from tables.movie
    |> join tables.director (director.id `eq` movie.directorId)
    |> where'
      ( movie.name `contains` "big"
        `or` (movie.name `contains` "little")
        `and` (movie.rating `gt` 7)
      )
    |> orderBy movie.rating Desc
