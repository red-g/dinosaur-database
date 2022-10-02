module Filter exposing (..)

import Data exposing (Areas, Clade, Clades, Genus, Range, Regions, Species)
import Dict exposing (Dict)


type alias Filter a =
    a -> Bool


type alias Filter2 a b =
    a -> b -> Bool


type alias Cat a =
    a -> a -> a


and : Cat (Filter a)
and f g =
    \x -> f x && g x


and2 : Cat (Filter2 a b)
and2 f g =
    \x y -> f x y && g x y


or : Cat (Filter a)
or f g =
    \x -> f x || g x


trycat : Cat a -> Cat (Maybe a)
trycat c f g =
    case ( f, g ) of
        ( Just x, Just y ) ->
            Just (c x y)

        ( Just x, Nothing ) ->
            Just x

        ( Nothing, Just y ) ->
            Just y

        ( Nothing, Nothing ) ->
            Nothing


tryand : Cat (Maybe (Filter a))
tryand =
    trycat and


tryand2 : Cat (Maybe (Filter2 a b))
tryand2 =
    trycat and2


tryor : Cat (Maybe (Filter a))
tryor =
    trycat or


within : String -> Filter String
within a b =
    let
        shorten =
            String.toLower << (String.left <| min (String.length a) (String.length b))
    in
    shorten a == shorten b


measurementFilter : Bounded -> Filter Float
measurementFilter bounded =
    case bounded of
        Single query ->
            (==) query

        Two query ->
            \data -> data >= query.start && data <= query.end


lengthFilter : Bounded -> Filter Species
lengthFilter query =
    .length >> measurementFilter query


weightFilter : Bounded -> Filter Species
weightFilter query =
    .weight >> measurementFilter query


inTimeRange : Float -> Range -> Bool
inTimeRange time { start, end } =
    time <= start && time >= end


encompassesRange : Range -> Range -> Bool
encompassesRange query data =
    query.start >= data.start && query.end <= data.end


timesFilter : Bounded -> Filter Species
timesFilter times =
    (case times of
        Single query ->
            inTimeRange query

        Two query ->
            \data -> (inTimeRange query.start data || inTimeRange query.end data) || encompassesRange query data
    )
        << .times


areasFilter : Areas -> Filter Species
areasFilter areas =
    .areas >> List.any (\i -> areas |> List.any (within i))


regionsFilter : Regions -> Filter Species
regionsFilter regions =
    .regions >> List.any (\i -> regions |> List.any ((==) i))


tryFold : (v -> acc -> acc) -> Maybe v -> acc -> acc
tryFold f m acc =
    case m of
        Just v ->
            f v acc

        Nothing ->
            acc


nameFilter : String -> Dict String a -> Dict String a
nameFilter =
    within >> skipSecond >> Dict.filter


skipFirst : f -> (a -> f)
skipFirst f =
    \_ -> f


skipSecond : (a -> b) -> (a -> c -> b)
skipSecond f =
    \a _ -> f a


removeEmpties : Dict comparable (Dict b c) -> Dict comparable (Dict b c)
removeEmpties =
    Dict.filter (Dict.isEmpty >> not |> skipFirst)


filterNestedDict : (Dict String a -> Dict String a) -> Dict String (Dict String a) -> Dict String (Dict String a)
filterNestedDict subFilter =
    (subFilter |> skipFirst |> Dict.map)
        >> removeEmpties


attributeFilter : Queries -> Maybe (Filter Species)
attributeFilter { length, weight, times, areas, regions } =
    Maybe.map areasFilter areas
        |> tryor (Maybe.map regionsFilter regions)
        |> tryand (Maybe.map lengthFilter length)
        |> tryand (Maybe.map timesFilter times)
        |> tryand (Maybe.map weightFilter weight)


speciesFilter : Queries -> Maybe (Filter2 String Species)
speciesFilter queries =
    tryand2
        (Maybe.map (within >> skipSecond) queries.species)
        (Maybe.map skipFirst <| attributeFilter queries)


genusFilter : Queries -> Genus -> Genus
genusFilter queries =
    tryFold Dict.filter <| speciesFilter queries


cladeFilter : Queries -> Clade -> Clade
cladeFilter queries =
    tryFold nameFilter queries.genus
        >> (filterNestedDict <| genusFilter queries)


cladesFilter : Queries -> Clades -> Clades
cladesFilter queries =
    tryFold nameFilter queries.clade >> (filterNestedDict <| cladeFilter queries)


type Bounded
    = Single Float
    | Two Range


type alias Queries =
    { species : Maybe String
    , genus : Maybe String
    , clade : Maybe String
    , weight : Maybe Bounded
    , length : Maybe Bounded
    , times : Maybe Bounded
    , regions : Maybe Regions
    , areas : Maybe Areas
    }
