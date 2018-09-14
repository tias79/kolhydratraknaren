module Main exposing (..)

import Html exposing (Html, text, div, h1, h2, img, input, br, ul, li, hr, a, pre)
import Html.Attributes exposing (class, style, href)
import Html.Events exposing (onClick)
import Foods exposing (..)
import Json.Decode exposing (Value)
import UIComponents exposing(..)
import Dom exposing (focus)
import Task exposing (attempt)
import License exposing (..)


---- MODEL ----

type alias Amount = Int
type alias CHAmount = Int
type alias Index = Int

type Msg
    = ChangeSearchQuery String
    | ClearSuggestions
    | SelectFood FoodId
    | UnSelectFood Index
    | UpdateAmount Index Amount
    | FocusResult (Result Dom.Error ())
    | ShowDrawer Bool
    | SelectWidget Widget

type alias Model =
    {
        foods : List Food,
        suggestedFoods : List Food,
        searchQuery : String,
        selectedFoods : List (Index, Food, Amount, CHAmount),
        showDrawer : Bool,
        activeWidget : Widget,
        licenses : Licenses 
    }

type Widget
    = Main 
    | About
    | LicenseGeneral
    | RobotoFontLicense

init : Flags -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue Foods.decoder (Tuple.first(flags)).foods of
        Ok foods ->
            {
                foods = foods,
                suggestedFoods = [],
                searchQuery = "",
                selectedFoods = [],
                showDrawer = False,
                activeWidget = Main,
                licenses = {robotoFontLicense = Tuple.second(flags)} 
            } ! []
        Err err ->
            Debug.crash err


---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ChangeSearchQuery newSearchQuery ->
      ({
          model | searchQuery = newSearchQuery,
         suggestedFoods = filterAndSort newSearchQuery model.foods
        }, Cmd.none )
    ClearSuggestions ->
        { model | suggestedFoods = [], searchQuery = "" } ! [Task.attempt FocusResult (focus "searchInput")]
    SelectFood foodId ->
        ({ 
            model | suggestedFoods = [], searchQuery = "", 
            selectedFoods = model.selectedFoods ++ List.map (\food -> ((List.length model.selectedFoods),
            food, food.gDefault, (calcCHAmount food.gDefault food))) (List.filter (\food -> food.id == foodId) model.foods)
        }, Cmd.none)
    UnSelectFood foodIdx ->
        ({ model | selectedFoods = List.filter (\(idx, _ , _, _) -> idx /= foodIdx) model.selectedFoods}, Cmd.none)
    UpdateAmount foodIdx newAmount ->
        ({ model | selectedFoods = List.map (\(idx, food, amount, chAmount) -> (idx, food, if foodIdx == idx then newAmount else amount, if foodIdx == idx then (calcCHAmount newAmount food) else chAmount)) model.selectedFoods}, Cmd.none)
    FocusResult result ->
        case result of
            Err (Dom.NotFound id) ->
                Debug.crash ("Can not find Dom element with id: " ++ id)
            Ok () ->
                model ! []
    ShowDrawer state ->
         ({ model | showDrawer = state}, Cmd.none)
    SelectWidget widget ->
        ({
            model | activeWidget = widget,
            showDrawer = False
        }, Cmd.none )
        

calcCHAmount : Amount -> Food -> CHAmount
calcCHAmount amount food = round (toFloat amount*food.gPercentage)

---- VIEW ----


view : Model -> Html Msg
view model =
    let
        suggestions = List.map (\food -> (food.id, food.name)) model.suggestedFoods
    in
        container "framework" [
            container "drawer" [
                div [class "left", style [("left", if model.showDrawer then "0" else "-70%")]] [
                    div [class "header"] [],
                    ul [] [
                        li [onClick (SelectWidget About)] [text "Om"],
                        li [onClick (SelectWidget LicenseGeneral)] [text "Licenser"]
                    ]
                ]
                ,
                div [class "right", style [("opacity", if model.showDrawer then "0.5" else "0"), ("visibility", if model.showDrawer then "visible" else "hidden")], onClick (ShowDrawer False)] []
            ],
            widget "main" (model.activeWidget == Main) [
                toolbar [
                    menu "menu" "Kolhydraträknaren" (ShowDrawer True)
                ],
                toolbar [
                    searchCard model.searchQuery (\x -> ChangeSearchQuery x) ClearSuggestions (\x -> SelectFood x) suggestions
                ],
                cardContainer
                    (List.map (
                        \(idx, food, amount, chAmount) ->
                            card [
                                clearButton (\x -> UnSelectFood x) idx,
                                numberinput (\x -> UpdateAmount idx x) amount,
                                text "g",
                                br [] [],
                                text (food.name ++ " (" ++ toString (round (food.gPercentage * 100)) ++ "g/100g)"),
                                br [] [],
                                text ("= " ++ toString chAmount ++ " g kh")
                            ]) model.selectedFoods),
                bottombar [
                    text ("Totalt " ++
                        (List.map (\(_, _, _, chAmount) -> chAmount) model.selectedFoods
                            |> List.sum
                            |> toString)
                        ++ " g kh")
                ]
            ],
            widget "about" (model.activeWidget == About) [
                toolbar [
                    menu "arrow_back" "Om" (SelectWidget Main)
                ],
                card [
                    br [] [],
                    br [] [],
                    logo,
                    br [] [],
                    br [] [],
                    h1 [] [text "Kolhydraträknaren"],
                    h2 [] [text "Version 1.0.0"],
                    br [] [],
                    br [] [],
                    (text "Alla livsmedelsdata är hämtade från Livsmedelsverkets livsmedelsdatabas 2018-08-08. \"Livsmedelsdatabasen ska spegla det svenska livsmedelsutbudet.\""),
                    br [] [],
                    br [] [],
                    (text "Kolhydraträknaren är skapad av Mattias Eklöf. Om du uppskattar appen, vänligen överväg att donera en slant till Barndiabetesfonden."),
                    br [] [],
                    br [] [],
                    br [] []
                ]
            ],
            widget "license" (model.activeWidget == LicenseGeneral) [
                toolbar [
                    menu "arrow_back" "Licenser" (SelectWidget Main)
                ],
                card [
                    h1 [] [ text "Roboto Font" ],
                    text "© Google Inc. - ", a [onClick (SelectWidget RobotoFontLicense)] [text "Apache License 2.0"],
                    hr [] []
                ]
            ],
            widget "robotoFontLicense" (model.activeWidget == RobotoFontLicense) [
                toolbar [
                    menu "arrow_back" "Roboto Font License" (SelectWidget LicenseGeneral)
                ],
                card [
                    text model.licenses.robotoFontLicense
                ]
            ]
        ]

---- PROGRAM ----

type alias Flags =
    ({ foods : Json.Decode.Value }, RobotoFontLicense)

main : Program Flags Model Msg
main =
        Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }