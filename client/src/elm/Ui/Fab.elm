module Ui.Fab exposing
    ( Type(..)
    , view
    )

{-| A floating round action button.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE


type Type
    = Normal
    | Extended
    | Mini


view : Type -> String -> Html msg -> Maybe msg -> List (Html.Attribute msg) -> Html msg
view type_ title icon action attrs =
    let
        content =
            case type_ of
                Extended ->
                    [ Html.span [] [ title |> Html.text ] ]

                _ ->
                    []

        onClick =
            case action of
                Just msg ->
                    msg |> HtmlE.onClick

                Nothing ->
                    HtmlA.disabled True
    in
    Html.button
        (List.concat
            [ [ HtmlA.type_ "button"
              , HtmlA.classList [ ( "ui-fab", True ), ( "mini", type_ == Mini ), ( "extended", type_ == Extended ) ]
              , HtmlA.title title
              , HtmlA.attribute "aria-label" title
              , onClick
              ]
            , attrs
            ]
        )
        (icon :: content)
