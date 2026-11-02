module Ui.Tabs exposing
    ( Model
    , TabModel
    , view
    )

{-| A row of pill buttons switching between sections.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE


{-| A model for a bar of tabs.
-}
type alias Model id msg =
    { selected : id
    , change : Maybe id -> msg
    , ids : List id
    , tab : id -> TabModel msg
    , equals : id -> id -> Bool
    , stacked : Bool
    }


{-| A model for a tab.
-}
type alias TabModel msg =
    { label : String
    , icon : Maybe (Html msg)
    }


{-| View a bar of tabs.
-}
view : Model id msg -> Html msg
view { selected, change, ids, tab, equals } =
    let
        viewTab id =
            let
                { label, icon } =
                    tab id

                isSelected =
                    equals selected id
            in
            Html.button
                [ HtmlA.type_ "button"
                , HtmlA.classList [ ( "selected", isSelected ) ]
                , HtmlA.attribute "aria-pressed"
                    (if isSelected then
                        "true"

                     else
                        "false"
                    )
                , Just id |> change |> HtmlE.onClick
                ]
                ((icon |> Maybe.map List.singleton |> Maybe.withDefault []) ++ [ Html.span [] [ Html.text label ] ])
    in
    Html.nav [ HtmlA.class "ui-tabs" ] (ids |> List.map viewTab)
