module Ui.Button exposing
    ( Density(..)
    , Type(..)
    , view
    , viewWithAttrs
    )

{-| A button with a label and an optional icon.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE


type Density
    = Padded
    | Dense


type Type
    = Standard
    | Outlined
    | Raised
    | Unelevated


view : Type -> Density -> String -> Maybe (Html msg) -> Maybe msg -> Html msg
view type_ density label icon action =
    viewWithAttrs type_ density label icon action []


viewWithAttrs : Type -> Density -> String -> Maybe (Html msg) -> Maybe msg -> List (Html.Attribute msg) -> Html msg
viewWithAttrs type_ density label icon action extraAttrs =
    let
        typeClass =
            case type_ of
                Standard ->
                    "text"

                Outlined ->
                    "outlined"

                Raised ->
                    "filled"

                Unelevated ->
                    "filled"

        actionAttr =
            case action of
                Just msg ->
                    msg |> HtmlE.onClick

                Nothing ->
                    HtmlA.disabled True
    in
    Html.button
        ([ HtmlA.type_ "button"
         , HtmlA.classList [ ( "ui-button", True ), ( typeClass, True ), ( "dense", density == Dense ) ]
         , actionAttr
         ]
            ++ extraAttrs
        )
        ((icon |> Maybe.map (\i -> [ Html.span [ HtmlA.class "ui-button-icon" ] [ i ] ]) |> Maybe.withDefault [])
            ++ [ Html.span [] [ Html.text label ] ]
        )
