module Main exposing (..)

import Html exposing (Html, text, div, h1, img, input, br)
import Foods exposing (..)
import Json.Decode exposing (Value)
import UIComponents exposing(..)


---- MODEL ----

type alias Amount = Maybe Int
type alias Index = Int

type Msg
    = ChangeSearchQuery String
    | ClearSuggestions
    | SelectFood FoodId
    | UnSelectFood Index
    | UpdateAmount Index Amount

type alias Model =
    {
        foods : List Food,
        suggestedFoods : List Food,
        searchQuery : String,
        selectedFoods : List (Index, Food, Amount)
    }

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
        ({ 
            model | suggestedFoods = [],
            selectedFoods = model.selectedFoods ++ List.map (\food -> ((List.length model.selectedFoods),
            food, Nothing)) (List.filter (\food -> food.id == foodId) model.foods)
        }, Cmd.none)
    UnSelectFood foodIdx ->
        ({ model | selectedFoods = List.filter (\(idx, _ ,_) -> idx /= foodIdx) model.selectedFoods}, Cmd.none)
    UpdateAmount foodIdx newAmount ->
        ({ model | selectedFoods = List.map (\(idx, food, amount) -> (idx, food, if foodIdx == idx then newAmount else amount)) model.selectedFoods}, Cmd.none)

---- VIEW ----


view : Model -> Html Msg
view model =
    let
        suggestions = List.map (\food -> (food.id, food.name)) model.suggestedFoods
    in
        div [] 
            ([searchCard (\x -> ChangeSearchQuery x) ClearSuggestions (\x -> SelectFood x) suggestions] ++
        (List.map (
            \(idx, food, amount) ->
                let
                    currentAmount = Maybe.withDefault food.gDefault amount                    
                    currentAmountCH = round (toFloat currentAmount*food.gPercentage)
                in
                    card [ clearButton (\x -> UnSelectFood x) idx, numberinput (\x -> UpdateAmount idx (Just x)) currentAmount, text "g", br [] [], text food.name, br [] [], text (toString currentAmountCH ++ " g kh") ]) model.selectedFoods))


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