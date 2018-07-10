module UIComponents exposing (toolbar, card, cardContainer, searchCard, clearButton, numberinput)

import Html exposing (Html, text, div, h1, img, input, i, hr, ul, li, span, a)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)

search : String -> (String -> msg) -> msg -> (Int -> msg) -> Bool -> Html msg
search searchQuery changeSearchQueryMsg clearSuggestionsMsg selectMsg enableBack =
    let
        leftIcon = i [ class "icon material-icons", onClick clearSuggestionsMsg ] [ if enableBack then text "arrow_back" else text "search"]
        rightIcon = i [ class "icon material-icons", onClick clearSuggestionsMsg ] [ if enableBack then text "clear" else text ""]
    in
        div [ class "search" ]
            [
            leftIcon,
            input [ id "searchInput", type_ "text", placeholder "Sök livsmedel", onInput changeSearchQueryMsg, value searchQuery ] [],
            rightIcon
            ]

searchCard : String -> (String -> msg) -> msg -> (Int -> msg) -> List (Int, String) -> Html msg
searchCard searchQuery changeSearchQueryMsg clearSuggestionsMsg selectMsg suggestions =
    let
        hasSuggestions = not (List.isEmpty suggestions)
        separatorComponent = if hasSuggestions then separator else empty
        listComponent = if hasSuggestions then list suggestions selectMsg else empty
    in
        card [
            search searchQuery changeSearchQueryMsg clearSuggestionsMsg selectMsg hasSuggestions,
            separatorComponent,
            listComponent
        ]

toolbar  : List (Html msg) -> Html msg
toolbar = div [ class "toolbar" ]

cardContainer : List (Html msg) -> Html msg
cardContainer =
    div [ class "cardcontainer" ]

card : List (Html msg) -> Html msg
card =
    div [ class "card" ]

clearButton : (Int -> msg) -> Int -> Html msg
clearButton clearMsg id =
    div [] [
        i [ class "smallIcon material-icons", onClick (clearMsg id)] [text "clear"]
    ]

separator : Html msg
separator = hr [] []

list : List (Int, String) -> (Int -> msg) -> Html msg
list items selectMsg =
    div [ class "list" ]
        (List.concatMap (\(id, name) -> [
--            i [ class "icon material-icons"] [
--              text "info"
--           ]
            div [][],
            div [onClick (selectMsg id)] [
                text name
            ]
        ]
        ) items)

empty : Html msg
empty = text ""

numberinput : (Int -> msg) -> Int -> Html msg
numberinput msg nr = input [type_ "number", style [("width", "3rem")], onInput (\str ->
    msg (case (toInt str) of
        Err err -> 0
        Ok val -> val)), value (toString nr)] []