module Ui.Select exposing
    ( ItemModel
    , Model
    , view
    )

{-| A labelled drop-down: a native select, so phones show their own picker and long lists stay usable.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Json


view : Model id msg -> List (ItemModel id) -> Html msg
view model items =
    let
        viewItem item =
            Html.option
                [ item.id |> model.idToString |> HtmlA.value
                , model.selected == Just item.id |> HtmlA.selected
                ]
                [ Html.text (item.label ++ (item.secondary |> Maybe.map (\s -> " · " ++ s) |> Maybe.withDefault "")) ]

        onChange =
            Json.at [ "target", "value" ] Json.string |> Json.map (model.idFromString >> model.wrap) |> HtmlE.on "change"
    in
    Html.label [ HtmlA.classList [ ( "ui-field", True ), ( "ui-select", True ), ( "disabled", model.disabled ) ] ]
        [ Html.span [ HtmlA.class "ui-field-label" ] [ Html.text model.label ]
        , Html.select (HtmlA.disabled model.disabled :: onChange :: model.attrs) (items |> List.map viewItem)
        ]


{-| The things needed to render the select.
-}
type alias Model id msg =
    { label : String
    , idToString : id -> String
    , idFromString : String -> Maybe id
    , selected : Maybe id
    , wrap : Maybe id -> msg
    , disabled : Bool
    , fullWidth : Bool
    , attrs : List (Html.Attribute msg)
    }


{-| One option: its text and an optional detail shown after it.
-}
type alias ItemModel id =
    { id : id
    , label : String
    , secondary : Maybe String
    }
