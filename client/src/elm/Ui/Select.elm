module Ui.Select exposing
    ( ItemModel
    , Model
    , view
    )

{-| A labelled drop-down, drawn by the md-select element (src/ts/select.ts) so the open list matches the design too.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Json


view : Model id msg -> List (ItemModel id) -> Html msg
view model items =
    let
        viewItem item =
            Html.node "md-option"
                [ item.id |> model.idToString |> HtmlA.attribute "value"
                , HtmlA.attribute "label" item.label
                , item.secondary |> Maybe.withDefault "" |> HtmlA.attribute "detail"
                ]
                []

        onChange =
            Json.at [ "target", "value" ] Json.string |> Json.map (model.idFromString >> model.wrap) |> HtmlE.on "change"

        disabled =
            if model.disabled then
                [ HtmlA.attribute "disabled" "" ]

            else
                []
    in
    Html.node "md-select"
        (HtmlA.class "ui-select"
            :: HtmlA.attribute "label" model.label
            :: (model.selected |> Maybe.map model.idToString |> Maybe.withDefault "" |> HtmlA.attribute "value")
            :: onChange
            :: disabled
            ++ model.attrs
        )
        (items |> List.map viewItem)


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


{-| One option: its text and an optional detail shown under it.
-}
type alias ItemModel id =
    { id : id
    , label : String
    , secondary : Maybe String
    }
