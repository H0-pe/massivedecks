module Ui.Slider exposing
    ( Model
    , view
    )

{-| A range slider.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Json


type alias Model msg =
    { min : Int
    , max : Int
    , step : Int
    , value : Int
    , onChange : Maybe (Int -> msg)
    , attrs : List (Html.Attribute msg)
    }


view : Model msg -> Html msg
view model =
    let
        toInt s =
            case String.toInt s of
                Just i ->
                    Json.succeed i

                Nothing ->
                    Json.fail "not a number"

        action =
            case model.onChange of
                Just wrap ->
                    Json.at [ "target", "value" ] Json.string |> Json.andThen toInt |> Json.map wrap |> HtmlE.on "input"

                Nothing ->
                    HtmlA.disabled True
    in
    Html.input
        ([ HtmlA.type_ "range"
         , HtmlA.class "ui-slider"
         , HtmlA.min (String.fromInt model.min)
         , HtmlA.max (String.fromInt model.max)
         , HtmlA.step (String.fromInt model.step)
         , HtmlA.value (String.fromInt model.value)
         , action
         ]
            ++ model.attrs
        )
        []
