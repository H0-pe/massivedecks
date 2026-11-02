module Ui.IconButton exposing
    ( view
    , viewNoPropagation
    )

{-| A button that displays as a simple icon. The title doubles as its accessible name.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD


view : Html msg -> String -> Maybe msg -> Html msg
view =
    viewInternal HtmlE.onClick


viewNoPropagation : Html msg -> String -> Maybe msg -> Html msg
viewNoPropagation =
    viewInternal onClickNoPropagation


viewInternal : (msg -> Html.Attribute msg) -> Html msg -> String -> Maybe msg -> Html msg
viewInternal onClick icon title action =
    let
        actionAttr =
            case action of
                Just msg ->
                    msg |> onClick

                Nothing ->
                    HtmlA.disabled True
    in
    Html.button
        [ HtmlA.type_ "button"
        , HtmlA.class "ui-icon-button"
        , HtmlA.title title
        , HtmlA.attribute "aria-label" title
        , actionAttr
        ]
        [ icon ]


onClickNoPropagation : msg -> Html.Attribute msg
onClickNoPropagation msg =
    HtmlE.stopPropagationOn "click" (JsonD.succeed ( msg, True ))
