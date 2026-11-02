module MassiveDecks.Pages.Lobby.Configure.Decks.Browser exposing (view)

{-| A browser for the decks the server offers: search, filter, and add or remove decks with one tap each, without
leaving the list.
-}

import FontAwesome as Icon
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import MassiveDecks.Card.Source as Source
import MassiveDecks.Card.Source.BuiltIn.Model as BuiltIn
import MassiveDecks.Card.Source.JsonAgainstHumanity.Model as JsonAgainstHumanity
import MassiveDecks.Card.Source.Model as Source
import MassiveDecks.Icon as Icon
import MassiveDecks.Model exposing (Shared)
import MassiveDecks.Pages.Lobby.Configure.Decks.Model exposing (..)
import MassiveDecks.Strings as Strings exposing (MdString)
import MassiveDecks.Strings.Languages as Lang
import MassiveDecks.Util.NeList as NeList
import Ui.Button as Button
import Ui.TextField as TextField


type alias Entry =
    { source : Source.External
    , name : String
    , detail : String
    , official : Bool
    }


view : (Msg -> msg) -> Shared -> Config -> Browser -> Html msg
view wrap shared existing browser =
    let
        isAdded entry =
            existing |> List.any (.source >> Source.equals entry.source)

        matchesFilter entry =
            case browser.filter of
                AllDecks ->
                    True

                OfficialDecks ->
                    entry.official

                CommunityDecks ->
                    not entry.official

                AddedDecks ->
                    isAdded entry

        query =
            browser.query |> String.trim |> String.toLower

        matchesQuery entry =
            query == "" || String.contains query (String.toLower entry.name)

        all =
            entries shared

        shown =
            all |> List.filter (\e -> matchesFilter e && matchesQuery e)

        addedCount =
            all |> List.filter isAdded |> List.length

        filterButton filter label =
            let
                selected =
                    filter == browser.filter
            in
            Html.button
                [ HtmlA.type_ "button"
                , HtmlA.classList [ ( "selected", selected ) ]
                , HtmlA.attribute "aria-pressed"
                    (if selected then
                        "true"

                     else
                        "false"
                    )
                , SetBrowserFilter filter |> wrap |> HtmlE.onClick
                ]
                [ label ]

        viewEntry entry =
            let
                added =
                    isAdded entry

                ( icon, label, style ) =
                    if added then
                        ( Icon.accept, Strings.DeckInGame, "filled" )

                    else
                        ( Icon.add, Strings.AddDeckShort, "outlined" )
            in
            Html.li [ HtmlA.classList [ ( "deck-browser-row", True ), ( "added", added ) ] ]
                [ Html.div [ HtmlA.class "deck-browser-info" ]
                    [ Html.span [ HtmlA.class "deck-browser-name" ] [ Html.text entry.name ]
                    , Html.span [ HtmlA.class "deck-browser-detail" ] [ Html.text entry.detail ]
                    ]
                , Html.button
                    [ HtmlA.type_ "button"
                    , HtmlA.class ("ui-button dense " ++ style)
                    , HtmlA.attribute "aria-pressed"
                        (if added then
                            "true"

                         else
                            "false"
                        )
                    , HtmlA.attribute "aria-label" (entry.name ++ ": " ++ (label |> Lang.string shared))
                    , Toggle entry.source |> wrap |> HtmlE.onClick
                    ]
                    [ Icon.view icon, Html.span [] [ label |> Lang.html shared ] ]
                ]

        list =
            if List.isEmpty shown then
                Html.p [ HtmlA.class "empty-info" ] [ Strings.NoMatchingDecks |> Lang.html shared ]

            else
                Html.ul [ HtmlA.class "deck-browser-list" ] (shown |> List.map viewEntry)

        title =
            Strings.DeckBrowserTitle
    in
    Html.div [ HtmlA.id "deck-browser" ]
        [ Html.div [ HtmlA.class "deck-browser-backdrop", CloseBrowser |> wrap |> HtmlE.onClick ] []
        , Html.div
            [ HtmlA.class "deck-browser-panel"
            , HtmlA.attribute "role" "dialog"
            , HtmlA.attribute "aria-modal" "true"
            , HtmlA.attribute "aria-label" (title |> Lang.string shared)
            ]
            [ Html.header []
                [ Html.h2 [] [ title |> Lang.html shared ]
                , Button.view Button.Raised
                    Button.Dense
                    (Strings.DeckBrowserDone |> Lang.string shared)
                    (Icon.accept |> Icon.view |> Just)
                    (CloseBrowser |> wrap |> Just)
                ]
            , TextField.viewWithAttrs
                (Strings.SearchDecks |> Lang.string shared)
                TextField.Search
                browser.query
                (SetBrowserQuery >> wrap |> Just)
                [ HtmlA.autofocus True ]
            , Html.nav [ HtmlA.class "ui-tabs" ]
                [ filterButton AllDecks (Strings.DeckFilterAll |> Lang.html shared)
                , filterButton OfficialDecks (Strings.DeckFilterOfficial |> Lang.html shared)
                , filterButton CommunityDecks (Strings.DeckFilterCommunity |> Lang.html shared)
                , filterButton AddedDecks
                    (Html.span []
                        [ Strings.DeckFilterAdded |> Lang.html shared
                        , Html.text (" · " ++ String.fromInt addedCount)
                        ]
                    )
                ]
            , Html.p [ HtmlA.class "deck-browser-count" ] [ Strings.DeckCount { count = List.length shown } |> Lang.html shared ]
            , list
            ]
        ]



{- Private -}


{-| Every deck the server offers: the built-in ones first, then JSON Against Humanity's (official ones first).
-}
entries : Shared -> List Entry
entries shared =
    let
        builtIn =
            shared.sources.builtIn
                |> Maybe.map (.decks >> NeList.toList >> List.map (fromBuiltIn shared))
                |> Maybe.withDefault []

        jah =
            shared.sources.jsonAgainstHumanity
                |> Maybe.map (.decks >> List.map (fromJsonAgainstHumanity shared))
                |> Maybe.withDefault []
    in
    builtIn ++ jah


fromBuiltIn : Shared -> BuiltIn.Deck -> Entry
fromBuiltIn shared deck =
    { source = Source.BuiltIn deck.id
    , name = deck.name
    , detail = Strings.DeckAuthor { author = deck.author } |> Lang.string shared
    , official = True
    }


fromJsonAgainstHumanity : Shared -> JsonAgainstHumanity.Deck -> Entry
fromJsonAgainstHumanity shared deck =
    { source = Source.JsonAgainstHumanity deck.id
    , name = deck.name
    , detail =
        if deck.calls + deck.responses > 0 then
            Strings.DeckCardCounts { calls = deck.calls, responses = deck.responses } |> Lang.string shared

        else
            ""
    , official = deck.official
    }
