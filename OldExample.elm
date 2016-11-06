module Example exposing (sql)

import SqlBuilder exposing (..)
-- import SqlBuilder exposing ((>>))
-- import SqlBuilder


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
    |> join tables.director (director.id === movie.directorId)
    |> where_
      ( contains movie.name "big"
        &&& (contains movie.name "little")
        ||| (movie.rating >>>= 7)
      )
    |> orderBy movie.rating Desc
