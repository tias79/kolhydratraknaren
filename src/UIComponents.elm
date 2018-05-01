module UIComponents exposing (searchCard)

import Html exposing (Html, text, div, h1, img, input, i, hr, ul, li, span)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

search : (String -> msg) -> msg -> Bool -> Html msg
search changeSearchQueryMsg clearSuggestionsMsg enableBack =
    let
        icon = i [ class "icon material-icons", onClick clearSuggestionsMsg ] [ if enableBack then text "arrow_back" else text "search"]
    in
        div [ class "search" ]
            [
            icon,
            input [ type_ "text", placeholder "Sök livsmedel", onInput changeSearchQueryMsg ] []
            ]

searchCard : (String -> msg) -> msg -> List (Int, String) -> Html msg
searchCard changeSearchQueryMsg clearSuggestionsMsg suggestions =
    let
        hasSuggestions = not (List.isEmpty suggestions)
        separatorComponent = if hasSuggestions then separator else empty
        listComponent = if hasSuggestions then list suggestions  else empty
    in    
        card [
            search changeSearchQueryMsg clearSuggestionsMsg hasSuggestions,
            separatorComponent,
            listComponent
        ]

card : List (Html msg) -> Html msg
card =
    div [ class "card" ]

separator : Html msg
separator = hr [] []

list : List (Int, String) -> Html msg
list items =
    div [ class "list" ]
        (List.concatMap (\(id, name) -> [
            i [ class "icon material-icons"] [text "info"],
           span [] [text name]
        ]) items)

empty : Html msg
empty = text ""
