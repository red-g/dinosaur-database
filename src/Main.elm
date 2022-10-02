module Main exposing (Model, Msg, cladeSearch, init, main, update, view)

import Browser
import Data exposing (Clade, Clades, Genus, Range, Regions, Species)
import Decode exposing (data)
import Dict exposing (Dict)
import Element exposing (Element, column, layout, row, text)
import Element.Input exposing (button, labelAbove, placeholder, search)
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Filter exposing (cladesFilter)
import Html exposing (Html)
import Input exposing (..)
import Json.Decode exposing (errorToString)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { query : Inputs, result : Result Json.Decode.Error Clades }


init : Model
init =
    { query =
        { species = ""
        , clade = ""
        , genus = ""
        , weight = emptyRange
        , length = emptyRange
        , times = emptyRange
        , areas = ""
        , regions = ""
        }
    , result = data
    }


type UpdateInput
    = WithSpecies Input
    | WithGenus Input
    | WithClade Input
    | WithWeight InputRange
    | WithLength InputRange
    | WithTime InputRange
    | WithAreas Input
    | WithRegions Input


type Msg
    = ChangeQuery UpdateInput
    | Search


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeQuery updateInput ->
            { model | query = updateInputs updateInput model.query }

        Search ->
            { model | result = Result.map (cladesFilter <| toQueries model.query) data }


updateInputs : UpdateInput -> Inputs -> Inputs
updateInputs msg inputs =
    case msg of
        WithSpecies species ->
            { inputs | species = species }

        WithGenus genus ->
            { inputs | genus = genus }

        WithClade clade ->
            { inputs | clade = clade }

        WithWeight weight ->
            { inputs | weight = weight }

        WithLength length ->
            { inputs | length = length }

        WithTime times ->
            { inputs | times = times }

        WithAreas areas ->
            { inputs | areas = areas }

        WithRegions regions ->
            { inputs | regions = regions }


view : Model -> Html Msg
view { query, result } =
    layout [] <|
        case result of
            Ok clades ->
                column []
                    [ lazy querySearches query
                    , searchButton
                    , lazy cladesDisplay clades
                    ]

            Err err ->
                text <| "Failed to parse data: " ++ errorToString err


cladesDisplay : Clades -> Element Msg
cladesDisplay =
    dictDisplay cladeDisplay


cladeDisplay : Clade -> Element Msg
cladeDisplay =
    dictDisplay genusDisplay


genusDisplay : Genus -> Element Msg
genusDisplay =
    dictDisplay speciesDisplay


dictDisplay : (a -> Element Msg) -> Dict String a -> Element Msg
dictDisplay f =
    Dict.map (\id value -> namedEl id <| f value) >> Dict.toList >> Keyed.column []


namedEl : String -> Element Msg -> Element Msg
namedEl name el =
    column [] [ text name, Element.el [ Element.padding 20 ] el ]


speciesDisplay : Species -> Element Msg
speciesDisplay { weight, times, length, regions } =
    column []
        [ numberDisplay { label = "Weight", unit = "kg" } weight
        , numberDisplay { label = "Length", unit = "m" } length
        , rangeDisplay { label = "Times", unit = "mya" } times
        , regionsDisplay regions
        ]


regionsDisplay : Regions -> Element Msg
regionsDisplay regions =
    row [ Element.spacing 5 ] [ text "Regions:", text <| Data.regionsToString regions ]


numberDisplay : { label : String, unit : String } -> Float -> Element Msg
numberDisplay { label, unit } value =
    row [ Element.spacing 5 ] [ text label, text <| String.fromFloat value, text unit ]


rangeDisplay : { label : String, unit : String } -> Range -> Element Msg
rangeDisplay { label, unit } { start, end } =
    row [ Element.spacing 5 ] [ text label, text <| String.fromFloat start, text "to", text <| String.fromFloat end, text unit ]


querySearches : Inputs -> Element Msg
querySearches inputs =
    column []
        [ row []
            [ cladeSearch inputs.clade
            , genusSearch inputs.genus
            , speciesSearch inputs.species
            ]
        , row []
            [ weightSearch inputs.weight
            , lengthSearch inputs.length
            , timeSearch inputs.times
            ]
        , row [ Element.width Element.fill ]
            [ regionsSearch inputs.regions
            , areasSearch inputs.areas
            ]
        ]
        |> Element.map ChangeQuery


searchButton : Element Msg
searchButton =
    button [] { onPress = Just Search, label = text "Search" }


cladeSearch : Input -> Element UpdateInput
cladeSearch =
    textSearch { onChange = WithClade, label = "Clade" }


genusSearch : Input -> Element UpdateInput
genusSearch =
    textSearch { onChange = WithGenus, label = "Genus" }


speciesSearch : Input -> Element UpdateInput
speciesSearch =
    textSearch { onChange = WithSpecies, label = "Species" }


areasSearch : Input -> Element UpdateInput
areasSearch =
    textSearch { onChange = WithAreas, label = "Areas" }


regionsSearch : Input -> Element UpdateInput
regionsSearch =
    textSearch { onChange = WithRegions, label = "Regions" }


textSearch : { onChange : String -> UpdateInput, label : String } -> Input -> Element UpdateInput
textSearch { onChange, label } =
    textInput { onChange = onChange, placeholder = "Type your " ++ String.toLower label ++ "!", label = label }


textInput : { onChange : String -> UpdateInput, placeholder : String, label : String } -> Input -> Element UpdateInput
textInput attributes value =
    search []
        { onChange = attributes.onChange
        , text = value
        , placeholder = Just <| placeholder [] <| text attributes.placeholder
        , label = labelAbove [] <| text attributes.label
        }


weightSearch : InputRange -> Element UpdateInput
weightSearch =
    rangeSearch { onChange = WithWeight, label = "Weight (kg)" }


lengthSearch : InputRange -> Element UpdateInput
lengthSearch =
    rangeSearch { onChange = WithLength, label = "Length (m)" }


timeSearch : InputRange -> Element UpdateInput
timeSearch =
    rangeSearch { onChange = WithTime, label = "Time (mya)" }


rangeSearch : { onChange : InputRange -> UpdateInput, label : String } -> InputRange -> Element UpdateInput
rangeSearch { onChange, label } { start, end } =
    column [ Element.width Element.fill ]
        [ text label
        , textInput
            { onChange = \s -> onChange <| InputRange s end
            , placeholder = "Starting value"
            , label = "Start"
            }
            start
        , textInput
            { onChange = InputRange start >> onChange
            , placeholder = "Ending value"
            , label = "End"
            }
            end
        ]
