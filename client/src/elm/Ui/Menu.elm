module Ui.Menu exposing
    ( Corner(..)
    , State(..)
    , view
    )

{-| A pop-up menu next to the element that opens it. A click outside, Escape or picking an item closes it.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Json


{-| The corner of the anchor element the menu should position itself at.
-}
type Corner
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
    | TopStart
    | TopEnd
    | BottomStart
    | BottomEnd


{-| If the menu is visible or not.
-}
type State
    = Open
    | Closed


{-| View a pop-up menu.
-}
view : msg -> State -> Corner -> Html msg -> List (Html msg) -> Html msg
view onClose state corner anchor menuItems =
    let
        cornerClass =
            case corner of
                TopLeft ->
                    "top-start"

                TopStart ->
                    "top-start"

                TopRight ->
                    "top-end"

                TopEnd ->
                    "top-end"

                BottomLeft ->
                    "bottom-start"

                BottomStart ->
                    "bottom-start"

                BottomRight ->
                    "bottom-end"

                BottomEnd ->
                    "bottom-end"

        onEscape =
            HtmlE.keyCode
                |> Json.andThen
                    (\k ->
                        if k == 27 then
                            Json.succeed onClose

                        else
                            Json.fail "not escape"
                    )
                |> HtmlE.on "keydown"

        menu =
            case state of
                Open ->
                    [ Html.div [ HtmlA.class "ui-menu-backdrop", HtmlE.onClick onClose ] []

                    -- Picking an item also bubbles up here, which closes the menu.
                    , Html.ul [ HtmlA.class ("ui-menu " ++ cornerClass), HtmlA.attribute "role" "menu", HtmlE.onClick onClose ]
                        menuItems
                    ]

                Closed ->
                    []
    in
    Html.div [ HtmlA.class "ui-menu-anchor", onEscape ] (anchor :: menu)
