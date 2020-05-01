port module Main exposing (..)

import Browser
import Browser.Dom exposing (focus)
import EntityUUID
import Foods exposing (..)
import Html exposing (Html, a, br, div, h1, h2, hr, li, text, ul)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import License exposing (..)
import Random
import Task
import UIComponents exposing (..)



---- MODEL ----


type alias Amount =
    Int


type alias CHAmount =
    Int


type alias Index =
    Int


type Msg
    = ChangeSearchQuery String
    | ClearSuggestions
    | SelectFood FoodId
    | AddFood String
    | DeleteFood FoodId
    | UnSelectFood Index
    | UpdateAmount Index Amount
    | UpdateCarbs Index Amount
    | FocusResult (Result Browser.Dom.Error ())
    | ShowDrawer Bool
    | SelectWidget Widget


type alias SelectedFood =
    { idx : Index, food : Food, amount : Amount, chAmount : CHAmount }


type alias Model =
    { foods : List Food
    , suggestedFoods : List Food
    , searchQuery : String
    , selectedFoods : List SelectedFood
    , showDrawer : Bool
    , activeWidget : Widget
    , licenses : Licenses
    , uuidSeed : Random.Seed
    }


type Widget
    = Main
    | About
    | LicenseGeneral
    | RobotoFontLicense


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { foods =
            case Decode.decodeValue Foods.decoder flags.foods.foods of
                Ok foods ->
                    foods

                Err _ ->
                    []
      , suggestedFoods = []
      , searchQuery = ""
      , selectedFoods = []
      , showDrawer = False
      , activeWidget = Main
      , licenses = { robotoFontLicense = flags.robotoFontLicense }
      , uuidSeed = Random.initialSeed flags.uuidInitialSeed
      }
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSearchQuery newSearchQuery ->
            ( { model
                | searchQuery = newSearchQuery
                , suggestedFoods = filterAndSort newSearchQuery model.foods
              }
            , Cmd.none
            )

        ClearSuggestions ->
            ( { model | suggestedFoods = [], searchQuery = "" }
            , Task.attempt FocusResult (focus "searchInput")
            )

        SelectFood foodId ->
            ( { model
                | suggestedFoods = []
                , searchQuery = ""
                , selectedFoods =
                    model.selectedFoods
                        ++ List.map
                            (\food ->
                                { idx = List.length model.selectedFoods
                                , food = food
                                , amount = food.gDefault
                                , chAmount = calcCHAmount food.gDefault food
                                }
                            )
                            (List.filter (\food -> food.id == foodId) model.foods)
              }
            , Cmd.none
            )

        AddFood foodName ->
            let
                ( uuid, newSeed ) =
                    EntityUUID.generate model.uuidSeed

                newFood =
                    { id = uuid
                    , name = foodName
                    , gPercentage = 1
                    , gDefault = 0
                    , source = Foods.USER
                    }
            in
            ( { model
                | suggestedFoods = []
                , searchQuery = ""
                , foods = newFood :: model.foods
                , uuidSeed = newSeed
                , selectedFoods =
                    model.selectedFoods
                        ++ [ { idx = List.length model.selectedFoods
                             , food = newFood
                             , amount = newFood.gDefault
                             , chAmount = calcCHAmount newFood.gDefault newFood
                             }
                           ]
              }
            , sendFood newFood
            )

        DeleteFood foodId ->
            ( { model
                | foods = List.filter (\food -> food.id /= foodId) model.foods
                , selectedFoods = List.filter (\selectedFood -> selectedFood.food.id /= foodId) model.selectedFoods
                , suggestedFoods = List.filter (\suggestion -> suggestion.id /= foodId) model.suggestedFoods
              }
            , Cmd.none
            )

        UnSelectFood foodIdx ->
            ( { model | selectedFoods = List.filter (\selectedFood -> selectedFood.idx /= foodIdx) model.selectedFoods }, Cmd.none )

        UpdateAmount foodIdx newAmount ->
            let
                foodMaybe =
                    model.selectedFoods
                        |> List.filter (\sf -> sf.idx == foodIdx && sf.food.source == Foods.USER)
                        |> List.map (\sf -> sf.food)
                        |> List.map (\f -> { f | gDefault = newAmount })
                        |> List.head
            in
            ( { model
                | foods =
                    case foodMaybe of
                        Nothing ->
                            model.foods

                        Just updatedFood ->
                            model.foods
                                |> List.map
                                    (\food ->
                                        if food.id == updatedFood.id then
                                            updatedFood

                                        else
                                            food
                                    )
                , selectedFoods =
                    List.map
                        (\selectedFood ->
                            { idx = selectedFood.idx
                            , food = selectedFood.food
                            , amount =
                                if foodIdx == selectedFood.idx then
                                    newAmount

                                else
                                    selectedFood.amount
                            , chAmount =
                                if foodIdx == selectedFood.idx then
                                    calcCHAmount newAmount selectedFood.food

                                else
                                    selectedFood.chAmount
                            }
                        )
                        model.selectedFoods
              }
            , case foodMaybe of
                Nothing ->
                    Cmd.none

                Just food ->
                    sendFood food
            )

        UpdateCarbs foodIdx newAmount ->
            let
                newGPercentage =
                    toFloat newAmount / 100

                foodMaybe =
                    model.selectedFoods
                        |> List.filter (\sf -> sf.idx == foodIdx && sf.food.source == Foods.USER)
                        |> List.map (\sf -> sf.food)
                        |> List.map (\f -> { f | gPercentage = newGPercentage })
                        |> List.head
            in
            ( { model
                | foods =
                    case foodMaybe of
                        Nothing ->
                            model.foods

                        Just updatedFood ->
                            model.foods
                                |> List.map
                                    (\food ->
                                        if food.id == updatedFood.id then
                                            updatedFood

                                        else
                                            food
                                    )
                , selectedFoods =
                    model.selectedFoods
                        |> List.map
                            (\sf ->
                                if sf.idx == foodIdx then
                                    { sf
                                        | food = { id = sf.food.id, name = sf.food.name, gPercentage = newGPercentage, gDefault = 0, source = Foods.USER }
                                    }

                                else
                                    sf
                            )
              }
            , case foodMaybe of
                Nothing ->
                    Cmd.none

                Just food ->
                    sendFood food
            )

        FocusResult result ->
            case result of
                Err (Browser.Dom.NotFound id) ->
                    ( model
                    , Cmd.none
                    )

                Ok () ->
                    ( model
                    , Cmd.none
                    )

        ShowDrawer state ->
            ( { model | showDrawer = state }, Cmd.none )

        SelectWidget widget ->
            ( { model
                | activeWidget = widget
                , showDrawer = False
              }
            , Cmd.none
            )


calcCHAmount : Amount -> Food -> CHAmount
calcCHAmount amount food =
    round (toFloat amount * food.gPercentage)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        suggestions =
            model.suggestedFoods
    in
    container "framework"
        [ container "drawer"
            [ div
                [ class "left"
                , style "left"
                    (if model.showDrawer then
                        "0"

                     else
                        "-70%"
                    )
                ]
                [ div [ class "header" ] []
                , ul []
                    [ li [ onClick (SelectWidget About) ] [ text "Om" ]
                    , li [ onClick (SelectWidget LicenseGeneral) ] [ text "Licenser" ]
                    ]
                ]
            , div
                [ class "right"
                , style "opacity"
                    (if model.showDrawer then
                        "0.5"

                     else
                        "0"
                    )
                , style "visibility"
                    (if model.showDrawer then
                        "visible"

                     else
                        "hidden"
                    )
                , onClick (ShowDrawer False)
                ]
                []
            ]
        , widget "main"
            (model.activeWidget == Main)
            [ toolbar
                [ menu "menu" "Kolhydraträknaren" (ShowDrawer True)
                ]
            , toolbar
                [ searchCard model.searchQuery (\x -> ChangeSearchQuery x) ClearSuggestions (\x -> SelectFood x) (\x -> AddFood x) (\x -> DeleteFood x) suggestions
                ]
            , cardContainer
                (List.map
                    (\selectedFood ->
                        let
                            g =
                                round (selectedFood.food.gPercentage * 100)

                            carbs =
                                if selectedFood.food.source == Foods.USER then
                                    numberinput (\x -> UpdateCarbs selectedFood.idx x) g

                                else
                                    text (String.fromInt g)
                        in
                        card
                            [ clearButton (\x -> UnSelectFood x) selectedFood.idx
                            , numberinput (\x -> UpdateAmount selectedFood.idx x) selectedFood.amount
                            , text "g"
                            , br [] []
                            , text (selectedFood.food.name ++ " (")
                            , carbs
                            , text "g/100g)"
                            , br [] []
                            , text ("= " ++ String.fromInt selectedFood.chAmount ++ " g kh")
                            ]
                    )
                    model.selectedFoods
                )
            , bottombar
                [ text
                    ("Totalt "
                        ++ (List.map (\selectedFood -> selectedFood.chAmount) model.selectedFoods
                                |> List.sum
                                |> String.fromInt
                           )
                        ++ " g kh"
                    )
                ]
            ]
        , widget "about"
            (model.activeWidget == About)
            [ toolbar
                [ menu "arrow_back" "Om" (SelectWidget Main)
                ]
            , card
                [ br [] []
                , br [] []
                , logo
                , br [] []
                , br [] []
                , h1 [] [ text "Kolhydraträknaren" ]
                , h2 [] [ text "Version 1.1.0" ]
                , br [] []
                , br [] []
                , text "Alla livsmedelsdata är hämtade från Livsmedelsverkets livsmedelsdatabas 2020-01-16. \"Livsmedelsdatabasen ska spegla det svenska livsmedelsutbudet.\""
                , br [] []
                , br [] []
                , text "Kolhydraträknaren är skapad av Mattias Eklöf. Om du uppskattar appen, vänligen överväg att donera en slant till Barndiabetesfonden."
                , br [] []
                , br [] []
                , br [] []
                ]
            ]
        , widget "license"
            (model.activeWidget == LicenseGeneral)
            [ toolbar
                [ menu "arrow_back" "Licenser" (SelectWidget Main)
                ]
            , card
                [ h1 [] [ text "Roboto Font" ]
                , text "© Google Inc. - "
                , a [ onClick (SelectWidget RobotoFontLicense) ] [ text "Apache License 2.0" ]
                , hr [] []
                ]
            ]
        , widget "robotoFontLicense"
            (model.activeWidget == RobotoFontLicense)
            [ toolbar
                [ menu "arrow_back" "Roboto Font License" (SelectWidget LicenseGeneral)
                ]
            , card
                [ text model.licenses.robotoFontLicense
                ]
            ]
        ]



---- PROGRAM ----


type alias Flags =
    { foods : { foods : Decode.Value }
    , robotoFontLicense : RobotoFontLicense
    , uuidInitialSeed : Int
    }


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


port sendFoodPort : Encode.Value -> Cmd msg


sendFood : Food -> Cmd Msg
sendFood food =
    sendFoodPort <| Foods.encoder food
