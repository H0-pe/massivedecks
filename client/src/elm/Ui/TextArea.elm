module Ui.TextArea exposing (view)

{-| A multi-line text input.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA


view : List (Html.Attribute msg) -> List (Html msg) -> Html msg
view attrs children =
    Html.label [ HtmlA.class "ui-field" ] [ Html.textarea attrs children ]
