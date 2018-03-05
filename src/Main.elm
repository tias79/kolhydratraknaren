module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Foods exposing (..)
import Json.Decode exposing (Value)
import List exposing(..)

---- MODEL ----


type alias Model =
    { foods : List Food }

initModel : Model
initModel =
  { foods = [] }

type alias Flags =
    { foods : Json.Decode.Value }


init : Flags -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue Foods.decoder flags.foods of
        Ok foods ->
            { foods = foods } ! []

        Err err ->
            Debug.crash err
--init flags =
--    ( { initModel | foods = Json.Decode.decodeValue Foods.decoder flags.foods }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text (toString (List.length model.foods)) ]
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
        Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
