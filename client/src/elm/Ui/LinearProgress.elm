module Ui.LinearProgress exposing
    ( Progress(..)
    , view
    )

{-| A thin progress bar.
-}

import Html exposing (Html)
import Html.Attributes as HtmlA


{-| What progress to display.
-}
type
    Progress
    -- An unknown amount of progress.
    = Indeterminate
      -- A float progress from 0 (no progress) to 1 (complete).
    | Progress Float
      -- Complete.
    | Closed


view : Progress -> List (Html.Attribute msg) -> Html msg
view p attributes =
    let
        ( classes, width ) =
            case p of
                Indeterminate ->
                    ( "indeterminate", "100%" )

                Progress float ->
                    ( "", String.fromFloat (clamp 0 1 float * 100) ++ "%" )

                Closed ->
                    ( "closed", "0" )
    in
    Html.div (HtmlA.class ("ui-progress " ++ classes) :: HtmlA.attribute "role" "progressbar" :: attributes)
        [ Html.div [ HtmlA.class "ui-progress-bar", HtmlA.style "width" width ] [] ]
