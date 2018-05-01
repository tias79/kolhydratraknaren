module Main exposing (..)

import Html exposing (Html, text, div, h1, img, input)
import Foods exposing (..)
import Json.Decode exposing (Value)
import UIComponents exposing(..)


type Msg
    = ChangeSearchQuery String | ClearSuggestions

---- MODEL ----


type alias Model =
    { foods : List Food, filteredFoods : List Food, searchQuery : String }

init : Flags -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue Foods.decoder flags.foods of
        Ok foods ->
            { foods = foods, filteredFoods = [], searchQuery = "" } ! []

        Err err ->
            Debug.crash err


---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ChangeSearchQuery newSearchQuery ->
      ({
          model | searchQuery = newSearchQuery,
          filteredFoods = List.filter (\{name} -> String.length newSearchQuery >= 2 && String.contains newSearchQuery name) model.foods
        }, Cmd.none )
    ClearSuggestions ->
        ({ model | filteredFoods = [] }, Cmd.none)


---- VIEW ----


view : Model -> Html Msg
view model =
    let
        suggestions = List.map (\food -> (food.id, food.name)) model.filteredFoods
    in
        searchCard (\x -> ChangeSearchQuery x) ClearSuggestions suggestions


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