module Main exposing (Model, Msg, cladeSearch, init, main, update, view)

import Browser
import Data exposing (Clade, Clades, Genus, Range, Regions, Species)
import Decode exposing (data)
import Dict exposing (Dict)
import Element exposing (Element, column, layout, row, text)
import Element.Font as Font
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
                    , lazy cladesView clades
                    ]

            Err err ->
                text <| "Failed to parse data: " ++ errorToString err


red : Element.Color
red =
    Element.rgb 1 0 0


green : Element.Color
green =
    Element.rgb 0 1 0


blue : Element.Color
blue =
    Element.rgb 0 0 1


cladesView : Clades -> Element Msg
cladesView =
    dictView <|
        \name clade ->
            column []
                [ Element.el [ Font.color red ] <| text name
                , Element.el [ Element.padding 20 ] <| cladeView clade
                ]


cladeView : Clade -> Element Msg
cladeView =
    dictView <|
        \name genus ->
            column []
                [ Element.el [ Font.color green ] <| text name
                , Element.el [ Element.padding 20 ] <| genusView genus
                ]


genusView : Genus -> Element Msg
genusView =
    dictView <|
        \name species ->
            column []
                [ Element.el [ Font.color blue ] <| text name
                , Element.el [ Element.padding 20 ] <| speciesView species
                ]


dictView : (String -> a -> Element Msg) -> Dict String a -> Element Msg
dictView f =
    Dict.map f >> Dict.toList >> Keyed.column []


speciesView : Species -> Element Msg
speciesView { weight, times, length, regions } =
    column []
        [ numberView { label = "Weight", unit = "kg" } weight
        , numberView { label = "Length", unit = "m" } length
        , rangeView { label = "Times", unit = "mya" } times
        , regionsView regions
        ]


labelView : String -> Element msg
labelView str =
    text <| str ++ ":"


regionsView : Regions -> Element Msg
regionsView regions =
    row [ Element.spacing 5 ] [ labelView "Regions", text <| Data.regionsToString regions ]


numberView : { label : String, unit : String } -> Float -> Element Msg
numberView { label, unit } value =
    row [ Element.spacing 5 ] [ labelView label, text <| String.fromFloat value, text unit ]


rangeView : { label : String, unit : String } -> Range -> Element Msg
rangeView { label, unit } { start, end } =
    row [ Element.spacing 5 ] [ labelView label, text <| String.fromFloat start, text "to", text <| String.fromFloat end, text unit ]


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
