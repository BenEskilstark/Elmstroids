module UI exposing (..)

import Tuple exposing (..)
import String exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, attribute)
import Html.Events exposing (onClick)


fullscreen : Html msg -> Html msg
fullscreen content = div 
    [
        style "height" "100vh",
        style "overflow" "hidden",
        style "background-color" "black"
    ]
    [content]

centered : Html msg -> Html msg
centered content = div 
    [
        style "display" "flex",
        style "justify-content" "center",
        style "align-items" "center",
        style "height" "100%",
        style "flex-direction" "column"
    ] 
    [content]

column : List (Html msg) -> Html msg
column content = div 
    [
        style "display" "flex",
        style "align-items" "center",
        style "height" "100%",
        style "flex-direction" "column"
    ] 
    content


card : Html msg -> Html msg
card content = div 
    [ 
        style "margin" "15px", 
        style "padding" "8px",
        style "border" "1px solid gray",
        style "width" "500px",
        style "border-radius" "5px",
        style "background-color" "white"
    ] 
    [ content ]


title : String -> Html msg
title str = h2 [ style "margin" "0", style "text-align" "center" ] [ text str ]


type ClickType = Default | Accept | Reject
type alias Clickable msg = {label: String, msg: msg, clickType: ClickType}

optionCard : List (Clickable msg) -> Html msg -> Html msg
optionCard clickables content = card (
    span [] [
        div [
            style "margin-bottom" "10px", 
            style "border-bottom" "1px dashed black",
            style "padding-bottom" "5px"
        ] [ content ],
        div [ 
            style "display" "flex",
            style "gap" "10px",
            style "justify-content" "right"
        ] (List.map toButton clickables)
    ])

toButton : Clickable msg -> Html msg
toButton {label, msg, clickType} = button 
    (
        attribute "onblur" "" ::
        onClick msg ::
        case clickType of 
            Default -> []
            Accept -> [style "background-color" "chartreuse"]
            Reject -> [style "background-color" "salmon"]   
    )
    [ text label ] 


modal : List (Clickable msg) -> Html msg -> Html msg
modal clickables content  = div 
    [ 
        style "width" "100%", style "height" "100%",
        style "position" "absolute",
        style "top" "0px", style "left" "0px",
        style "background-color" "rgba(0, 0, 0, 0.65)",
        style "display" "flex", 
        style "justify-content" "center", style "align-items" "center"
    ] [ optionCard clickables content ]


quittableModal : List (Clickable msg) -> msg -> Html msg -> Html msg
quittableModal clickables close content = modal clickables (
    span [] [ div 
        [style "display" "flex", style "justify-content" "right"] 
        [span [style "font-size" "10px", onClick close] [text "❌"]
    ], 
        content]
    ) 