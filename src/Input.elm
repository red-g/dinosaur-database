module Input exposing (..)

import Data exposing (Areas, Region(..), Regions, stringToRegion)
import Filter exposing (Bounded(..), Queries)


type alias Input =
    String


type alias InputRange =
    { start : String, end : String }


emptyRange : InputRange
emptyRange =
    { start = "", end = "" }



--consider matching based on which continents fit the letters provided (abbreviated or full)


toRegions : Input -> Maybe Regions
toRegions str =
    str
        |> String.split ","
        |> List.filterMap (String.trim >> stringToRegion >> Result.toMaybe)
        |> toFull


toMaybe : Input -> Maybe Input
toMaybe str =
    case str of
        "" ->
            Nothing

        _ ->
            Just str


toAreas : Input -> Maybe Areas
toAreas str =
    str
        |> String.split ","
        |> List.filterMap (String.trim >> toMaybe)
        |> toFull


toFull : List a -> Maybe (List a)
toFull list =
    case list of
        [] ->
            Nothing

        _ ->
            Just list


toBounded : InputRange -> Maybe Bounded
toBounded inputRange =
    let
        maybeStart =
            String.toFloat inputRange.start

        maybeEnd =
            String.toFloat inputRange.end
    in
    case ( maybeStart, maybeEnd ) of
        ( Just start, Just end ) ->
            Just (Two { start = start, end = end })

        ( Just start, Nothing ) ->
            Just (Single start)

        ( Nothing, Just end ) ->
            Just (Single end)

        ( Nothing, Nothing ) ->
            Nothing


type alias Inputs =
    { species : Input
    , genus : Input
    , clade : Input
    , weight : InputRange
    , length : InputRange
    , times : InputRange
    , regions : Input
    , areas : Input
    }


toQueries : Inputs -> Queries
toQueries { species, genus, clade, weight, length, regions, times, areas } =
    { species = toMaybe species
    , genus = toMaybe genus
    , clade = toMaybe clade
    , weight = toBounded weight
    , times = toBounded times
    , length = toBounded length
    , regions = toRegions regions
    , areas = toAreas areas
    }
