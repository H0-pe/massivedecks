module MassiveDecks.Pages.Lobby.Tv exposing
    ( housekeeping
    , userName
    , view
    )

{-| TV mode: one device (usually a TV) is the shared screen for everyone in the room, players use their phones.

The TV starts the game itself under a fixed name, becomes a spectator, and hands control to the first human player
who joins (they become privileged, so they can configure and start the game from their phone).

This only applies to a user that joined as the TV. Normal games, e.g. played remotely, are unaffected, and anyone can
still open the TV view of a game they are in.

-}

import Dict exposing (Dict)
import FontAwesome as Icon
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Keyed as HtmlK
import MassiveDecks.Game as Game
import MassiveDecks.Game.Messages as Game
import MassiveDecks.Game.Model as Game
import MassiveDecks.Game.Round as Round
import MassiveDecks.Icon as Icon
import MassiveDecks.Model exposing (Shared)
import MassiveDecks.Pages.Lobby.Actions as Actions
import MassiveDecks.Pages.Lobby.GameCode as GameCode exposing (GameCode)
import MassiveDecks.Pages.Lobby.Model as Lobby exposing (Lobby)
import MassiveDecks.Pages.Lobby.Spectate as Spectate
import MassiveDecks.Pages.Lobby.Spectate.Stages.Postgame as Postgame
import MassiveDecks.Pages.Lobby.Spectate.Stages.Round as SpectateRound
import MassiveDecks.Pages.Route as Route
import MassiveDecks.Pages.Start.Route as Start
import MassiveDecks.Strings as Strings
import MassiveDecks.Strings.Languages as Lang
import MassiveDecks.User as User exposing (User)
import MassiveDecks.Util.Maybe as Maybe
import QRCode
import Set


{-| The name the TV joins under. It is also how a client recognises it is the TV.
-}
userName : String
userName =
    "📺 TV"


{-| Actions the TV needs to take to keep the room working: become a spectator, and make sure some human player can
run the game. Only does anything when the local user is the TV.
-}
housekeeping : Lobby.Model -> Cmd msg
housekeeping model =
    case model.lobbyAndConfigure |> Maybe.map .lobby of
        Just lobby ->
            let
                localId =
                    model.auth.claims.uid
            in
            case lobby.users |> Dict.get localId of
                Just local ->
                    if local.name == userName then
                        Cmd.batch [ becomeSpectator local, promoteHost localId local lobby ]

                    else
                        Cmd.none

                Nothing ->
                    Cmd.none

        Nothing ->
            Cmd.none


{-| The TV view of the lobby.
-}
view : (Game.Msg -> msg) -> Shared -> Lobby.Model -> List (Html msg)
view wrapGame shared model =
    let
        content =
            case model.lobbyAndConfigure |> Maybe.map .lobby of
                Just lobby ->
                    case lobby.game of
                        Just game ->
                            viewGame wrapGame shared model lobby game

                        Nothing ->
                            viewWaitingRoom shared model.route.gameCode lobby

                Nothing ->
                    [ Html.div [ HtmlA.class "tv-loading" ] [ Icon.loading |> Icon.view ] ]
    in
    [ Html.div [ HtmlA.id "tv" ] content ]



{- Private -}


becomeSpectator : User -> Cmd msg
becomeSpectator local =
    if local.role == User.Player then
        Actions.setUserRole Nothing User.Spectator

    else
        Cmd.none


{-| If no human other than the TV is privileged, promote the human who joined first. In audience mode people join as
spectators, so this deliberately doesn't require the host to be a player.
-}
promoteHost : User.Id -> User -> Lobby -> Cmd msg
promoteHost localId local lobby =
    let
        humans =
            lobby.users
                |> Dict.toList
                |> List.filter (\( id, u ) -> id /= localId && u.presence == User.Joined && u.control == User.Human)

        hasHost =
            humans |> List.any (\( _, u ) -> u.privilege == User.Privileged)

        firstHuman =
            humans
                |> List.sortBy (Tuple.first >> joinOrder)
                |> List.head
    in
    case ( local.privilege, hasHost, firstHuman ) of
        ( User.Privileged, False, Just ( id, _ ) ) ->
            Actions.setPrivilege id User.Privileged

        _ ->
            Cmd.none


{-| User ids are assigned in join order, as increasing numbers.
-}
joinOrder : User.Id -> Int
joinOrder id =
    String.toInt id |> Maybe.withDefault 0


players : Lobby -> List ( User.Id, User )
players lobby =
    lobby.users
        |> Dict.toList
        |> List.filter (\( _, u ) -> u.role == User.Player && u.presence == User.Joined)
        |> List.sortBy (Tuple.first >> joinOrder)


viewJoinInfo : Shared -> GameCode -> Html msg
viewJoinInfo shared gameCode =
    let
        qr =
            Route.externalUrl shared.origin (Route.Start { section = Start.Join (Just gameCode) })
                |> QRCode.fromStringWith QRCode.Low
                |> Result.map (QRCode.toSvg [] >> List.singleton)
                |> Result.withDefault []
    in
    Html.div [ HtmlA.class "tv-join" ]
        [ Html.div [ HtmlA.class "tv-join-text" ]
            [ Html.p [] [ Strings.TvJoinInstruction { url = Spectate.stripProtocol shared.origin } |> Lang.html shared ]
            , Html.p [ HtmlA.class "tv-code" ] [ Html.text (GameCode.toString gameCode) ]
            ]
        , Html.div [ HtmlA.class "tv-qr" ] qr
        ]


viewWaitingRoom : Shared -> GameCode -> Lobby -> List (Html msg)
viewWaitingRoom shared gameCode lobby =
    let
        joined =
            players lobby

        host =
            lobby.users
                |> Dict.toList
                |> List.filter (\( _, u ) -> u.privilege == User.Privileged && u.presence == User.Joined && u.control == User.Human && u.name /= userName)
                |> List.sortBy (Tuple.first >> joinOrder)
                |> List.head

        hint =
            case host of
                Just ( _, u ) ->
                    Strings.TvHostHint { name = u.name }

                Nothing ->
                    Strings.TvFirstPlayerHostHint

        viewPlayer ( id, u ) =
            ( id
            , Html.li [ HtmlA.classList [ ( "host", u.privilege == User.Privileged ) ] ]
                [ Html.text u.name ]
            )

        playerList =
            if List.isEmpty joined then
                Html.p [ HtmlA.class "tv-waiting" ] [ Strings.TvWaitingForPlayers |> Lang.html shared ]

            else
                HtmlK.ul [ HtmlA.class "tv-players" ] (joined |> List.map viewPlayer)
    in
    [ Html.div [ HtmlA.class "tv-waiting-room" ]
        [ Html.h1 [] [ Html.text lobby.config.name ]
        , viewJoinInfo shared gameCode
        , playerList
        , Html.p [ HtmlA.class "tv-hint" ] [ hint |> Lang.html shared ]
        ]
    ]


viewGame : (Game.Msg -> msg) -> Shared -> Lobby.Model -> Lobby -> Game.Model -> List (Html msg)
viewGame wrapGame shared model lobby game =
    let
        isPlayer id =
            lobby.users |> Dict.get id |> Maybe.map (\u -> u.role == User.Player) |> Maybe.withDefault False

        stage =
            case game.game.winner of
                Just winner ->
                    -- The client's player list includes spectators (like the TV itself); keep them off the podium.
                    Postgame.view shared lobby (game.game |> (\g -> { g | players = g.players |> Dict.filter (\id _ -> isPlayer id) })) winner

                Nothing ->
                    SpectateRound.view wrapGame shared lobby.config lobby.users game

        timeLeft =
            model.timeAnchor |> Maybe.andThen (\anchor -> Game.secondsLeft anchor game)

        timer =
            case ( game.game.winner, timeLeft |> Maybe.andThen (\s -> s |> Maybe.justIf (s > 0)) ) of
                ( Nothing, Just seconds ) ->
                    Html.div [ HtmlA.classList [ ( "tv-timer", True ), ( "urgent", seconds <= 10 ) ] ]
                        [ Html.text (String.fromInt seconds) ]

                _ ->
                    Html.text ""
    in
    [ Html.div [ HtmlA.id "spectate", HtmlA.class "tv-stage" ] stage
    , Html.aside [ HtmlA.class "tv-sidebar" ]
        [ timer
        , viewScores shared lobby game
        , Html.div [ HtmlA.class "tv-corner-join" ]
            [ Html.text (Spectate.stripProtocol shared.origin)
            , Html.span [ HtmlA.class "tv-code" ] [ Html.text (GameCode.toString model.route.gameCode) ]
            ]
        ]
    ]


viewScores : Shared -> Lobby -> Game.Model -> Html msg
viewScores shared lobby game =
    let
        round =
            game.game.round

        voting =
            lobby.config.rules.houseRules.godIsDead /= Nothing

        humanPlayers =
            lobby.users
                |> Dict.filter (\_ u -> u.role == User.Player && u.presence == User.Joined && u.control == User.Human)
                |> Dict.keys
                |> Set.fromList

        -- Who still has to act in this stage (play, or vote with "God Is Dead"), and who already has.
        ( isActive, participants, done ) =
            case round.stage of
                Round.P playing ->
                    ( True, round.players, playing.played )

                Round.J judging ->
                    if voting then
                        ( True, humanPlayers, judging.voted )

                    else
                        ( False, Set.empty, Set.empty )

                _ ->
                    ( False, Set.empty, Set.empty )

        votingStatus =
            case round.stage of
                Round.J judging ->
                    if voting then
                        Html.p [ HtmlA.class "tv-voting" ]
                            [ Strings.TvVoting { voted = Set.size judging.voted, total = Set.size humanPlayers } |> Lang.html shared ]

                    else
                        Html.text ""

                _ ->
                    Html.text ""

        -- The player order and the client's player list also contain spectators (like the TV itself).
        viewPlayer id =
            Maybe.map2 (viewScore id)
                (lobby.users |> Dict.get id |> Maybe.andThen (\u -> u |> Maybe.justIf (u.role == User.Player)))
                (game.game.players |> Dict.get id)

        gameOver =
            game.game.winner /= Nothing

        viewScore id user player =
            let
                status =
                    if gameOver then
                        Html.text ""

                    else if isActive && Set.member id done then
                        Icon.accept |> Icon.view

                    else if id == round.czar then
                        Icon.czar |> Icon.view

                    else
                        Html.text ""
            in
            ( id
            , Html.li
                [ HtmlA.classList
                    [ ( "czar", id == round.czar && not gameOver )
                    , ( "played", isActive && Set.member id done )
                    , ( "waiting", isActive && Set.member id participants && not (Set.member id done) )
                    , ( "away", user.presence == User.Left )
                    ]
                ]
                [ Html.span [ HtmlA.class "tv-status" ] [ status ]
                , Html.span [ HtmlA.class "tv-name" ] [ Html.text user.name ]
                , Html.span [ HtmlA.class "tv-score" ] [ Html.text (String.fromInt player.score) ]
                ]
            )

        ordered =
            game.game.playerOrder
                |> List.filterMap viewPlayer
    in
    Html.div [ HtmlA.class "tv-scores" ]
        [ votingStatus
        , Html.h2 [] [ Strings.TvScores |> Lang.html shared ]
        , HtmlK.ul [] ordered
        ]
