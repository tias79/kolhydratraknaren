module Foods exposing (Food, FoodId, decoder)

import Json.Decode exposing (Decoder, int, string, float)
import Json.Decode.Pipeline exposing (decode, required)

type alias FoodId = Int
type alias Food = { id: FoodId, name: String, gPercentage: Float, gDefault: Int }

foodDecoder : Decoder Food
foodDecoder =
  decode Food
    |> required "id" int
    |> required "name" string
    |> required "gPercentage" float
    |> required "gDefault" int

foodsDecoder : Json.Decode.Decoder (List Food)
foodsDecoder =
    Json.Decode.list foodDecoder

decoder : Json.Decode.Decoder (List Food)
decoder = foodsDecoder