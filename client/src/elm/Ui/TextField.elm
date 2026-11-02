module Ui.TextField exposing
    ( Type(..)
    , view
    , viewWithAttrs
    , viewWithFocus
    , viewWithReturn
    )

{-| A labelled text input.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Json


{-| The type of field.
-}
type Type
    = Text
    | Search
    | Tel
    | Url
    | Email
    | Password
    | Date
    | Month
    | Week
    | Time
    | DateTimeLocal
    | Number
    | Color


view : String -> Type -> String -> Maybe (String -> msg) -> Html msg
view label t value action =
    viewWithAttrs label t value action []


{-| A text field for the given type of data.
-}
viewWithFocus : String -> Type -> String -> Maybe (String -> msg) -> msg -> msg -> Html msg
viewWithFocus label t value action onFocus onBlur =
    viewWithAttrs label t value action [ onFocus |> HtmlE.onFocus, onBlur |> HtmlE.onBlur ]


viewWithReturn : String -> Type -> String -> Maybe (String -> msg) -> msg -> msg -> Html msg
viewWithReturn label t value action onReturn noOp =
    let
        onKeyPress k =
            if k == 13 then
                onReturn

            else
                noOp
    in
    viewWithAttrs label t value action [ HtmlE.keyCode |> Json.map onKeyPress |> HtmlE.on "keydown" ]


viewWithAttrs : String -> Type -> String -> Maybe (String -> msg) -> List (Html.Attribute msg) -> Html msg
viewWithAttrs label t value action attrs =
    let
        ( disabled, actionAttr ) =
            case action of
                Just wrap ->
                    ( False, HtmlE.onInput wrap )

                Nothing ->
                    ( True, HtmlA.disabled True )
    in
    Html.label [ HtmlA.classList [ ( "ui-field", True ), ( "disabled", disabled ) ] ]
        [ Html.span [ HtmlA.class "ui-field-label" ] [ Html.text label ]
        , Html.input
            ([ t |> typeString |> HtmlA.type_
             , HtmlA.value value
             , actionAttr
             ]
                ++ attrs
            )
            []
        ]



{- Private -}


typeString : Type -> String
typeString t =
    case t of
        Text ->
            "text"

        Search ->
            "search"

        Tel ->
            "tel"

        Url ->
            "url"

        Email ->
            "email"

        Password ->
            "password"

        Date ->
            "date"

        Month ->
            "month"

        Week ->
            "week"

        Time ->
            "time"

        DateTimeLocal ->
            "datetime-local"

        Number ->
            "number"

        Color ->
            "color"
