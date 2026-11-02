module Ui.ListView exposing
    ( Action(..)
    , action
    , interactive
    , view
    , viewItem
    )

{-| A list of items, each optionally a button or a link, with an icon, a second line and something at the end.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE


view : List (Html.Attribute msg) -> List (Html msg) -> Html msg
view attributes children =
    Html.ul (HtmlA.class "ui-list" :: attributes) children


{-| What happens when you interact with the item.
-}
type Action msg
    = None
    | Link (Html msg -> Html msg)
    | Button (Maybe msg)


{-| A list item that is only sometimes enabled.
-}
interactive : msg -> Bool -> Action msg
interactive msg enabled =
    if enabled then
        msg |> Just |> Button

    else
        Nothing |> Button


{-| An action from a maybe, if it is enabled.
-}
action : Maybe msg -> Action msg
action =
    Button


{-| An item within a list.
-}
viewItem : Action msg -> Maybe (Html msg) -> Maybe (List (Html msg)) -> Maybe (List (Html msg)) -> List (Html msg) -> Html msg
viewItem action_ icon secondary meta children =
    let
        content =
            List.filterMap identity
                [ icon |> Maybe.map (\i -> Html.span [ HtmlA.class "ui-list-icon" ] [ i ])
                , Html.span [ HtmlA.class "ui-list-text" ]
                    (Html.span [ HtmlA.class "ui-list-primary" ] children
                        :: (secondary |> Maybe.map (\s -> [ Html.span [ HtmlA.class "ui-list-secondary" ] s ]) |> Maybe.withDefault [])
                    )
                    |> Just
                , meta |> Maybe.map (Html.span [ HtmlA.class "ui-list-meta" ])
                ]

        item =
            case action_ of
                Button (Just msg) ->
                    Html.button [ HtmlA.type_ "button", HtmlA.class "ui-list-item", HtmlE.onClick msg ] content

                Button Nothing ->
                    Html.button [ HtmlA.type_ "button", HtmlA.class "ui-list-item", HtmlA.disabled True ] content

                Link wrap ->
                    Html.span [ HtmlA.class "ui-list-item" ] content |> wrap

                None ->
                    Html.div [ HtmlA.class "ui-list-item" ] content
    in
    Html.li [ HtmlA.attribute "role" "none" ] [ item ]
