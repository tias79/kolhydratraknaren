module Foods exposing (Food, FoodId, decoder, filterAndSort)

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



filterAndSort : String -> List Food -> List Food
filterAndSort query inputFoods = filter query inputFoods
                            |> sort query

filter : String -> List Food -> List Food
filter query foods =
    let
        subQueries = findSubQueries query
    in       
        List.filter
            (\{name} ->
                String.length query >= 2
                &&
                (List.all (\x -> x) (List.map (\subQuery -> String.contains (String.toUpper subQuery) (String.toUpper name)) subQueries))
            )
            foods

findSubQueries : String -> List String
findSubQueries query = (String.words query |> List.filter (\query -> String.length query >= 2))

sort : String -> List Food -> List Food
sort query  = List.sortWith (\a b ->
        let
            queryHitRatioA = queryHitRatio query a
            queryHitRatioB = queryHitRatio query b
            queryHitRankA = queryHitRank query a
            queryHitRankB = queryHitRank query b
            queryHitLengthA = queryHitLength query a
            queryHitLengthB = queryHitLength query b
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
        foodNameLength = String.length food.name
        subQueries = findSubQueries query
    in      
        if 
            ((String.length query) > 0)
            &&
            (foodNameLength > 0)
        then
            1.0 - (subQueries
                |> List.map (\subquery -> if (String.contains (String.toUpper subquery) (String.toUpper food.name)) then 1 else 0)
                |> List.sum)
            /
            (subQueries
                |> List.length
                |> toFloat)
        else 1.0

queryHitRank : String -> Food -> Int
queryHitRank query food =
    let
        foodNameLength = String.length food.name
        subQueries = findSubQueries query
    in 
        if 
            String.length query > 0 && foodNameLength > 0
        then
            subQueries
                |> List.map (\subquery ->
                    case
                        (List.head (String.indexes (String.toUpper subquery) (String.toUpper food.name))) of
                            Nothing -> 0
                            Just x -> x
                )
                |> List.sum
        else 0

queryHitLength : String -> Food -> Int
queryHitLength query food =
    String.length food.name
