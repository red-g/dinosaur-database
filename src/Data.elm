module Data exposing (..)

import Dict exposing (Dict)


type alias Range =
    { start : Float, end : Float }


type Region
    = Africa
    | Asia
    | Europe
    | NorthAmerica
    | SouthAmerica
    | Oceania
    | Antarctica


stringToRegion : String -> Result String Region
stringToRegion str =
    case String.toLower str of
        "africa" ->
            Ok Africa

        "af" ->
            Ok Africa

        "asia" ->
            Ok Asia

        "as" ->
            Ok Asia

        "europe" ->
            Ok Europe

        "eu" ->
            Ok Europe

        "north america" ->
            Ok NorthAmerica

        "na" ->
            Ok NorthAmerica

        "south america" ->
            Ok SouthAmerica

        "sa" ->
            Ok SouthAmerica

        "oceania" ->
            Ok Oceania

        "oc" ->
            Ok Oceania

        "australia" ->
            Ok Oceania

        "au" ->
            Ok Oceania

        "an" ->
            Ok Antarctica

        "antarctica" ->
            Ok Antarctica

        _ ->
            Err "Invalid region"


regionToString : Region -> String
regionToString region =
    case region of
        Africa ->
            "Africa"

        Asia ->
            "Asia"

        Europe ->
            "Europe"

        NorthAmerica ->
            "North America"

        SouthAmerica ->
            "South America"

        Oceania ->
            "Oceania"

        Antarctica ->
            "Antarctica"


regionsToString : List Region -> String
regionsToString regions =
    String.join ", " <| List.map regionToString regions


type alias Regions =
    List Region


type alias Areas =
    List String


type alias Species =
    { weight : Float
    , length : Float
    , times : Range
    , areas : Areas
    , regions : Regions
    }


type alias Genus =
    Dict String Species


type alias Clade =
    Dict String Genus


type alias Clades =
    Dict String Clade
