module Ui.Switch exposing
    ( view
    , viewWithAttrs
    )

{-| An on/off switch: a native checkbox drawn as a toggle.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE


view : Html msg -> Bool -> Maybe (Bool -> msg) -> Html msg
view label checked action =
    viewWithAttrs label checked action []


viewWithAttrs : Html msg -> Bool -> Maybe (Bool -> msg) -> List (Html.Attribute msg) -> Html msg
viewWithAttrs label checked action attrs =
    let
        ( disabled, actionAttrs ) =
            case action of
                Just wrap ->
                    ( False, [ HtmlE.onCheck wrap ] )

                Nothing ->
                    ( True, [] )
    in
    Html.label (HtmlA.classList [ ( "ui-switch", True ), ( "disabled", disabled ) ] :: attrs)
        [ Html.input
            ([ HtmlA.type_ "checkbox"
             , HtmlA.attribute "role" "switch"
             , HtmlA.checked checked
             , HtmlA.disabled disabled
             ]
                ++ actionAttrs
            )
            []
        , Html.span [ HtmlA.class "ui-switch-track", HtmlA.attribute "aria-hidden" "true" ]
            [ Html.span [ HtmlA.class "ui-switch-thumb" ] [] ]
        , Html.span [ HtmlA.class "ui-switch-label" ] [ label ]
        ]
