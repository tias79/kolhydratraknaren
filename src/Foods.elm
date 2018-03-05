module Foods exposing (Food, decoder)

import Json.Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (decode, required)

type alias Food = { id: Int, name: String }

foodDecoder : Decoder Food
foodDecoder =
  decode Food
    |> required "id" int
    |> required "name" string

foodsDecoder : Json.Decode.Decoder (List Food)
foodsDecoder =
    Json.Decode.list foodDecoder

decoder : Json.Decode.Decoder (List Food)
decoder = foodsDecoder