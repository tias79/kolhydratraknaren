module Main exposing (..)

import Html exposing (Html, text, div, h1, h2, img, input, br)
import Foods exposing (..)
import Json.Decode exposing (Value)
import UIComponents exposing(..)
import Dom exposing (focus) 
import Task


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
    | UpdateInfo Bool

type alias Model =
    {
        foods : List Food,
        suggestedFoods : List Food,
        searchQuery : String,
        selectedFoods : List (Index, Food, Amount, CHAmount),
        infoShowing : Bool
    }

init : Flags -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue Foods.decoder flags.foods of
        Ok foods ->
            { foods = foods, suggestedFoods = [], searchQuery = "", selectedFoods = [], infoShowing = False } ! []
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
    UpdateInfo state ->
         ({ model | infoShowing = state}, Cmd.none)
        

calcCHAmount : Amount -> Food -> CHAmount
calcCHAmount amount food = round (toFloat amount*food.gPercentage)

---- VIEW ----


view : Model -> Html Msg
view model =
    let
        suggestions = List.map (\food -> (food.id, food.name)) model.suggestedFoods
    in
        container "framework" [
            info model.infoShowing (UpdateInfo False)
                [ 
                    h1 [] [text "Kolhydraträknaren"],
                    h2 [] [text "Version 1.0.0"],
                    br [] [],
                    (text "Alla data är hämtade från Livsmedelsverkets Livsmedelsdatabasen (2018-08-08).")
                ],
            container "main" [
                toolbar [
                    menu "Kolhydraträknaren" (UpdateInfo True),
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
                                text food.name,
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
            ]
        ]

---- PROGRAM ----


type alias Flags =
    { foods : Json.Decode.Value }

main : Program Flags Model Msg
main =
        Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }