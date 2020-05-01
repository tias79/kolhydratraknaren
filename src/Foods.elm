module Foods exposing (Food, FoodId, Source(..), decoder, encoder, filterAndSort)

import EntityUUID
import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode exposing (Value)


type alias FoodId =
    EntityUUID.T


type alias Food =
    { id : FoodId
    , name : String
    , gPercentage : Float
    , gDefault : Int
    , source : Source
    }


type Source
    = LIVSMEDELSDB
    | USER


sourceDecoder : Decoder Source
sourceDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "LIVSMEDELSDB" ->
                        Json.Decode.succeed LIVSMEDELSDB

                    "USER" ->
                        Json.Decode.succeed USER

                    unknownSource ->
                        Json.Decode.fail <| "Unknown source: " ++ unknownSource
            )


sourceEncoder : Source -> Json.Encode.Value
sourceEncoder source =
    let
        sourceAsStr =
            case source of
                LIVSMEDELSDB ->
                    "LIVSMEDELSDB"

                USER ->
                    "USER"
    in
    Json.Encode.string sourceAsStr


foodDecoder : Decoder Food
foodDecoder =
    succeed Food
        |> required "id" EntityUUID.decoder
        |> required "name" Json.Decode.string
        |> required "gPercentage" Json.Decode.float
        |> required "gDefault" Json.Decode.int
        |> required "source" sourceDecoder


foodsDecoder : Json.Decode.Decoder (List Food)
foodsDecoder =
    Json.Decode.list foodDecoder


decoder : Json.Decode.Decoder (List Food)
decoder =
    foodsDecoder


encoder : Food -> Json.Encode.Value
encoder food =
    Json.Encode.object
        [ ( "id", Json.Encode.string (EntityUUID.toString food.id) )
        , ( "name", Json.Encode.string food.name )
        , ( "gDefault", Json.Encode.int food.gDefault )
        , ( "gPercentage", Json.Encode.float food.gPercentage )
        , ( "source", sourceEncoder food.source )
        ]


filterAndSort : String -> List Food -> List Food
filterAndSort query inputFoods =
    filter query inputFoods
        |> sort query


filter : String -> List Food -> List Food
filter query foods =
    let
        subQueries =
            findSubQueries query
    in
    List.filter
        (\{ name } ->
            String.length query
                >= 2
                && List.all (\x -> x) (List.map (\subQuery -> String.contains (String.toUpper subQuery) (String.toUpper name)) subQueries)
        )
        foods


findSubQueries : String -> List String
findSubQueries query =
    String.words query |> List.filter (\q -> String.length q >= 2)


sort : String -> List Food -> List Food
sort query =
    List.sortWith
        (\a b ->
            let
                queryHitRatioA =
                    queryHitRatio query a

                queryHitRatioB =
                    queryHitRatio query b

                queryHitRankA =
                    queryHitRank query a

                queryHitRankB =
                    queryHitRank query b

                queryHitLengthA =
                    queryHitLength query a

                queryHitLengthB =
                    queryHitLength query b
            in
            if queryHitRatioA == queryHitRatioB then
                if queryHitRankA == queryHitRankB then
                    compare queryHitLengthA queryHitLengthB

                else
                    compare queryHitRankA queryHitRankB

            else
                compare queryHitRatioA queryHitRatioB
        )


queryHitRatio : String -> Food -> Float
queryHitRatio query food =
    let
        foodNameLength =
            String.length food.name

        subQueries =
            findSubQueries query
    in
    if
        (String.length query > 0)
            && (foodNameLength > 0)
    then
        1.0
            - (subQueries
                |> List.map
                    (\subquery ->
                        if String.contains (String.toUpper subquery) (String.toUpper food.name) then
                            1

                        else
                            0
                    )
                |> List.sum
              )
            / (subQueries
                |> List.length
                |> toFloat
              )

    else
        1.0


queryHitRank : String -> Food -> Int
queryHitRank query food =
    let
        foodNameLength =
            String.length food.name

        subQueries =
            findSubQueries query
    in
    if String.length query > 0 && foodNameLength > 0 then
        subQueries
            |> List.map
                (\subquery ->
                    case
                        List.head (String.indexes (String.toUpper subquery) (String.toUpper food.name))
                    of
                        Nothing ->
                            0

                        Just x ->
                            x
                )
            |> List.sum

    else
        0


queryHitLength : String -> Food -> Int
queryHitLength query food =
    String.length food.name
