module Main exposing (..)

import Html exposing (Html, text, div, h1, img, input)
import Foods exposing (..)
import Json.Decode exposing (Value)
import UIComponents exposing(..)


---- MODEL ----


type Msg
    = ChangeSearchQuery String | ClearSuggestions | SelectFood Int | UnSelectFood Int

type alias Model =
    { foods : List Food, suggestedFoods : List Food, searchQuery : String, selectedFoods : List (Food, Int) }

init : Flags -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue Foods.decoder flags.foods of
        Ok foods ->
            { foods = foods, suggestedFoods = [], searchQuery = "", selectedFoods = [] } ! []

        Err err ->
            Debug.crash err


---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ChangeSearchQuery newSearchQuery ->
      ({
          model | searchQuery = newSearchQuery,
         suggestedFoods = List.filter (\{name} -> String.length newSearchQuery >= 2 && String.contains newSearchQuery name) model.foods
        }, Cmd.none )
    ClearSuggestions ->
        ({ model | suggestedFoods = [] }, Cmd.none)
    SelectFood foodId ->
        ({ model | suggestedFoods = [], selectedFoods = model.selectedFoods ++ List.map (\food -> (food, 0)) (List.filter (\food -> food.id == foodId) model.foods)}, Cmd.none)
    UnSelectFood foodId->
        ({ model | selectedFoods = List.filter (\(food, _) -> food.id /= foodId) model.selectedFoods}, Cmd.none)
        

---- VIEW ----


view : Model -> Html Msg
view model =
    let
        suggestions = List.map (\food -> (food.id, food.name)) model.suggestedFoods
    in
        div [] 
            ([searchCard (\x -> ChangeSearchQuery x) ClearSuggestions (\x -> SelectFood x) suggestions] ++
        (List.map (\(food, amount) -> card [ clearButton (\x -> UnSelectFood x) food.id, text food.name ]) model.selectedFoods))


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