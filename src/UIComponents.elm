module UIComponents exposing (bottombar, button, card, cardContainer, clearButton, container, drawer, icon, logo, menu, numberinput, searchCard, separator, toolbar, widget)

import Foods exposing (Food, FoodId)
import Html exposing (Html, div, hr, i, img, input, li, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)


search : String -> (String -> msg) -> msg -> Bool -> Html msg
search searchQuery changeSearchQueryMsg clearSuggestionsMsg enableBack =
    let
        leftIcon =
            icon [ onClick clearSuggestionsMsg ]
                (if enableBack then
                    "arrow_back"

                 else
                    "search"
                )

        rightIcon =
            icon [ onClick clearSuggestionsMsg ]
                (if enableBack then
                    "clear"

                 else
                    ""
                )
    in
    div [ class "search" ]
        [ leftIcon
        , input [ id "searchInput", type_ "text", placeholder "Sök livsmedel", onInput changeSearchQueryMsg, value searchQuery ] []
        , rightIcon
        ]


searchCard : String -> (String -> msg) -> msg -> (FoodId -> msg) -> (String -> msg) -> (FoodId -> msg) -> List Food -> Html msg
searchCard searchQuery changeSearchQueryMsg clearSuggestionsMsg selectMsg addMsg deleteMsg suggestions =
    let
        hasSearchQuery =
            not (String.isEmpty searchQuery)

        listComponent =
            if hasSearchQuery then
                card [ list searchQuery suggestions selectMsg addMsg deleteMsg ]

            else
                empty
    in
    div [ class "searchCard" ]
        [ card
            [ search searchQuery changeSearchQueryMsg clearSuggestionsMsg hasSearchQuery
            ]
        , listComponent
        ]


toolbar : List (Html msg) -> Html msg
toolbar =
    div [ class "toolbar" ]


bottombar : List (Html msg) -> Html msg
bottombar =
    div [ class "toolbar-bottom" ]


cardContainer : List (Html msg) -> Html msg
cardContainer =
    div [ class "cardcontainer" ]


card : List (Html msg) -> Html msg
card =
    div [ class "card" ]


clearButton : (Int -> msg) -> Int -> Html msg
clearButton clearMsg id =
    div []
        [ smallIcon [ onClick (clearMsg id) ] "clear"
        ]


button : String -> msg -> Html msg
button name msg =
    icon [ onClick msg ] name


separator : Html msg
separator =
    hr [] []


list : String -> List Food -> (FoodId -> msg) -> (String -> msg) -> (FoodId -> msg) -> Html msg
list searchQuery items selectMsg addMsg deleteMsg =
    div [ class "list" ]
        ([ icon [ onClick (addMsg searchQuery) ] "add", div [ class "add" ] [ text searchQuery ], div [] [] ]
            ++ List.concatMap
                (\food ->
                    [ div []
                        [ if food.source == Foods.USER then
                            icon [ onClick (deleteMsg food.id) ] "delete"

                          else
                            empty
                        ]
                    , div [ onClick (selectMsg food.id) ]
                        [ text food.name ]
                    , div [] []
                    ]
                )
                items
        )


empty : Html msg
empty =
    text ""


numberinput : (Int -> msg) -> Int -> Html msg
numberinput msg nr =
    input
        [ type_ "number"
        , style "width" "2.5rem"
        , onInput
            (\str ->
                msg
                    (case toInt str of
                        Just val ->
                            val

                        Nothing ->
                            0
                    )
            )
        , value (String.fromInt nr)
        ]
        []


icon : List (Html.Attribute msg) -> String -> Html msg
icon attrs name =
    i ([ class "icon material-icons" ] ++ attrs) [ text name ]


smallIcon : List (Html.Attribute msg) -> String -> Html msg
smallIcon attrs name =
    i ([ class "smallIcon material-icons" ] ++ attrs) [ text name ]


menu : String -> String -> msg -> Html msg
menu iconName title action =
    div [ class "menu" ]
        [ icon [ onClick action ] iconName
        , span [] [ text title ]
        ]


container : String -> List (Html msg) -> Html msg
container identity =
    div [ id identity ]


widget : String -> Bool -> List (Html msg) -> Html msg
widget identity active =
    div
        [ id identity
        , class "widget"
        , style "visibility"
            (if active then
                "visible"

             else
                "hidden"
            )
        ]


drawer : Bool -> msg -> List (Html msg) -> Html msg
drawer show doClose children =
    div
        [ class "drawer"
        , style "left"
            (if show then
                "0px"

             else
                "-70%"
            )
        , style "display"
            (if show then
                "block"

             else
                "none"
            )
        ]
        [ div [ class "drawerLeft" ]
            [ div [ class "header" ] []
            , ul []
                [ li [] [ text "Om" ]
                , li [] [ text "Licenser" ]
                ]
            ]
        , div [ class "drawerRight", onClick doClose ] []
        ]


logo : Html msg
logo =
    img [ class "logo", src "logo.svg" ] []
