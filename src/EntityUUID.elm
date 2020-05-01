module EntityUUID exposing(T, toJson, toString, decoder, generate)

import UUID
import Json.Encode as Encode
import Json.Decode as Decode
import Random


type alias T = UUID.UUID


toJson : T -> Encode.Value
toJson uuid = Encode.string <| UUID.toString uuid


decoder : Decode.Decoder T
decoder =
    Decode.string
        |> Decode.andThen uuidDecoder


uuidDecoder : String -> Decode.Decoder T
uuidDecoder str = case UUID.fromString str of
                    Ok uuid -> Decode.succeed uuid
                    Err _ -> Decode.fail "Not a UUID"


toString : T -> String
toString uuid = UUID.toString uuid


generate : Random.Seed -> (T, Random.Seed)
generate currentSeed = Random.step (UUID.generator) currentSeed