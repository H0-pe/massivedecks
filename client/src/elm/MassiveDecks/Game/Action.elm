module MassiveDecks.Game.Action exposing (view)

import FontAwesome as Icon exposing (Icon)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import MassiveDecks.Game.Action.Model exposing (..)
import MassiveDecks.Game.Messages as Game exposing (Msg)
import MassiveDecks.Icon as Icon
import MassiveDecks.Model exposing (..)
import MassiveDecks.Strings as Strings exposing (MdString)
import MassiveDecks.Strings.Languages as Lang
import Material.Fab as Fab


actions : List Action
actions =
    [ PickCall, EditCall, Submit, TakeBack, Judge, Vote, Like, Advance ]


{-| Style for actions that are blocking the game from continuing until they are performed.
-}
blocking : List (Html.Attribute msg)
blocking =
    [ HtmlA.class "blocking" ]


{-| Style for an action that doesn't block the game.
-}
normal : List (Html.Attribute msg)
normal =
    [ HtmlA.class "normal" ]


view : (Msg -> msg) -> Shared -> Maybe Action -> Html msg
view wrap shared visible =
    Html.div [] (actions |> List.map (viewSingle wrap shared visible))


viewSingle : (Msg -> msg) -> Shared -> Maybe Action -> Action -> Html msg
viewSingle wrap shared visible action =
    let
        { icon, attrs, type_, title, onClick } =
            case action of
                PickCall ->
                    IconView Icon.accept blocking Fab.Normal Strings.PickCall Game.Submit

                EditCall ->
                    IconView Icon.edit blocking Fab.Normal Strings.WriteCall Game.WriteCall

                Submit ->
                    IconView Icon.accept blocking Fab.Normal Strings.SubmitPlay Game.Submit

                TakeBack ->
                    IconView Icon.undo normal Fab.Normal Strings.TakeBackPlay Game.TakeBack

                Judge ->
                    IconView Icon.win blocking Fab.Normal Strings.JudgePlay Game.Judge

                Vote ->
                    IconView Icon.accept blocking Fab.Normal Strings.VoteForPlay Game.Vote

                Like ->
                    IconView Icon.like normal Fab.Normal Strings.LikePlay Game.Like

                Advance ->
                    IconView Icon.skip blocking Fab.Normal Strings.AdvanceRound Game.AdvanceRound

                Discard ->
                    IconView Icon.discard normal Fab.Mini Strings.HouseRuleNeverHaveIEver Game.Discard
        label =
            title |> Lang.string shared

        exited =
            visible /= Just action
    in
    case shortLabel action of
        -- The actions the game waits on get a labelled button.
        Just short ->
            Html.button
                (HtmlA.classList [ ( "action", True ), ( "main-action", True ), ( "exited", exited ) ]
                    :: HtmlA.type_ "button"
                    :: HtmlA.title label
                    :: HtmlA.disabled exited
                    :: HtmlE.onClick (onClick |> wrap)
                    :: attrs
                )
                [ icon |> Icon.view, Html.span [] [ short |> Lang.html shared ] ]

        Nothing ->
            Fab.view
                type_
                label
                (icon |> Icon.view)
                (onClick |> wrap |> Just)
                (HtmlA.classList [ ( "action", True ), ( "exited", exited ) ] :: attrs)


{-| A short label shown on the button for the actions the game is waiting on.
-}
shortLabel : Action -> Maybe MdString
shortLabel action =
    case action of
        PickCall ->
            Just Strings.PickCallShort

        EditCall ->
            Just Strings.WriteCallShort

        Submit ->
            Just Strings.SubmitPlayShort

        Judge ->
            Just Strings.JudgePlayShort

        Vote ->
            Just Strings.VoteForPlayShort

        Advance ->
            Just Strings.AdvanceRoundShort

        _ ->
            Nothing


type alias IconView msg =
    { icon : Icon Icon.WithoutId
    , attrs : List (Html.Attribute msg)
    , type_ : Fab.Type
    , title : MdString
    , onClick : Game.Msg
    }
