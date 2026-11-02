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
import MassiveDecks.Card.Call as Call
import MassiveDecks.Card.Model as Card
import MassiveDecks.Card.Parts as Parts
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


{-| The Massive Decks mark: two cards, one light, one dark, and the name.
-}
viewBrand : Html msg
viewBrand =
    Html.div [ HtmlA.class "tv-brand" ]
        [ Html.span [ HtmlA.class "tv-brand-card light" ] []
        , Html.span [ HtmlA.class "tv-brand-card dark" ] []
        , Html.span [ HtmlA.class "tv-brand-name" ] [ Html.text "Massive Decks" ]
        ]


{-| The game code with each character on its own tile, big enough to read from the sofa.
-}
viewCodeTiles : GameCode -> Html msg
viewCodeTiles gameCode =
    let
        code =
            GameCode.toString gameCode
    in
    Html.div [ HtmlA.class "tv-code-tiles", HtmlA.attribute "aria-label" code ]
        (code |> String.toList |> List.map (\c -> Html.span [] [ Html.text (String.fromChar c) ]))


initial : String -> String
initial name =
    name |> String.left 1 |> String.toUpper


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

        qr =
            Route.externalUrl shared.origin (Route.Start { section = Start.Join (Just gameCode) })
                |> QRCode.fromStringWith QRCode.Low
                |> Result.map (QRCode.toSvg [] >> List.singleton)
                |> Result.withDefault []

        viewPlayer ( id, u ) =
            let
                isHost =
                    u.privilege == User.Privileged
            in
            ( id
            , Html.li [ HtmlA.classList [ ( "host", isHost ) ] ]
                [ Html.span [ HtmlA.class "tv-chip-badge" ] [ Html.text (initial u.name) ]
                , Html.span [] [ Html.text u.name ]
                , if isHost then
                    Icon.view Icon.host

                  else
                    Html.text ""
                ]
            )

        waitingForMore =
            ( "waiting-for-more"
            , Html.li [ HtmlA.class "more" ]
                [ (if List.isEmpty joined then
                    Strings.TvWaitingForPlayers

                   else
                    Strings.TvWaitingForMore
                  )
                    |> Lang.html shared
                ]
            )
    in
    [ Html.div [ HtmlA.class "tv-waiting-room" ]
        [ Html.header [ HtmlA.class "tv-lobby-header" ]
            [ viewBrand
            , Html.span [ HtmlA.class "tv-lobby-name" ] [ Html.text lobby.config.name ]
            ]
        , Html.main_ [ HtmlA.class "tv-lobby-main" ]
            [ Html.section [ HtmlA.class "tv-lobby-join" ]
                [ Html.h1 []
                    [ Strings.TvHeadlinePhone |> Lang.html shared
                    , Html.br [] []
                    , Html.span [ HtmlA.class "accent" ] [ Strings.TvHeadlineFriendship |> Lang.html shared ]
                    ]
                , Html.p [ HtmlA.class "tv-join-text" ]
                    [ Strings.TvJoinInstruction { url = Spectate.stripProtocol shared.origin } |> Lang.html shared ]
                , viewCodeTiles gameCode
                ]
            , Html.section [ HtmlA.class "tv-lobby-qr" ]
                [ Html.div [ HtmlA.class "tv-qr" ] qr
                , Html.span [] [ Strings.TvScanToJoin |> Lang.html shared ]
                ]
            ]
        , Html.footer [ HtmlA.class "tv-lobby-footer" ]
            [ HtmlK.ul [ HtmlA.class "tv-players" ] ((joined |> List.map viewPlayer) ++ [ waitingForMore ])
            , Html.p [ HtmlA.class "tv-hint" ] [ hint |> Lang.html shared ]
            ]
        ]
    ]


viewGame : (Game.Msg -> msg) -> Shared -> Lobby.Model -> Lobby -> Game.Model -> List (Html msg)
viewGame _ shared model lobby game =
    case ( game.game.winner, game.game.round.stage ) of
        ( Just winner, _ ) ->
            viewStandings shared lobby game winner

        ( Nothing, Round.C complete ) ->
            viewResult shared lobby game complete

        ( Nothing, _ ) ->
            viewRound shared model lobby game


{-| The round counter and, when there is one, the score needed to win.
-}
viewRoundInfo : Shared -> Game.Model -> Html msg
viewRoundInfo shared game =
    let
        limit =
            game.game.rules.scoreLimit
                |> Maybe.map (\points -> [ Html.text " · ", Strings.TvFirstTo { points = points } |> Lang.html shared ])
                |> Maybe.withDefault []
    in
    Html.span [ HtmlA.class "tv-round-info" ]
        ((Strings.TvRound { round = List.length game.game.history + 1 } |> Lang.html shared) :: limit)


{-| One round on the big screen: the call large on the left, the answers in a grid on the right, everyone below.
-}
viewRound : Shared -> Lobby.Model -> Lobby -> Game.Model -> List (Html msg)
viewRound shared model lobby game =
    let
        round =
            game.game.round

        nameOf id =
            lobby.users |> Dict.get id |> Maybe.map .name |> Maybe.withDefault "?"

        czarName =
            nameOf round.czar

        voting =
            lobby.config.rules.houseRules.godIsDead /= Nothing

        humanPlayers =
            lobby.users
                |> Dict.filter (\_ u -> u.role == User.Player && u.presence == User.Joined && u.control == User.Human)
                |> Dict.keys
                |> Set.fromList

        timeLeft =
            model.timeAnchor
                |> Maybe.andThen (\anchor -> Game.secondsLeft anchor game)
                |> Maybe.andThen (\s -> s |> Maybe.justIf (s > 0))

        timer =
            case timeLeft of
                Just seconds ->
                    Html.span [ HtmlA.classList [ ( "tv-timer", True ), ( "urgent", seconds <= 10 ) ] ]
                        [ Icon.view Icon.waiting, Html.text (formatSeconds seconds) ]

                Nothing ->
                    Html.text ""

        letter index =
            Char.fromCode (65 + index) |> String.fromChar

        ( ( headline, sub ), call, answers ) =
            case round.stage of
                Round.S _ ->
                    ( ( Strings.TvCzarPicking { name = czarName }, Nothing )
                    , Call.viewUnknown shared []
                    , [ Html.div [ HtmlA.class "tv-waiting-note" ] [ Strings.TvCzarPicking { name = czarName } |> Lang.html shared ] ]
                    )

                Round.P playing ->
                    ( ( Strings.TvPickAnswers, Just (Strings.TvPlaying { played = Set.size playing.played, total = Set.size round.players }) )
                    , viewCall shared lobby Dict.empty playing.call
                    , round.players
                        |> Set.toList
                        |> List.map (\id -> viewBack (Set.member id playing.played))
                    )

                Round.R revealing ->
                    let
                        fills =
                            revealing.plays
                                |> List.filter (\p -> Just p.id == revealing.lastRevealed)
                                |> List.head
                                |> Maybe.andThen .responses
                                |> Maybe.map Parts.fillsFromPlay
                                |> Maybe.withDefault Dict.empty
                    in
                    ( ( Strings.TvRevealing { name = czarName }, Nothing )
                    , viewCall shared lobby fills revealing.call
                    , revealing.plays
                        |> List.map
                            (\p ->
                                case p.responses of
                                    Just responses ->
                                        viewAnswer (Just p.id == revealing.lastRevealed) Nothing responses

                                    Nothing ->
                                        viewBack True
                            )
                    )

                Round.J judging ->
                    ( if voting then
                        ( Strings.TvEveryoneVotes, Just (Strings.TvVoting { voted = Set.size judging.voted, total = Set.size humanPlayers }) )

                      else
                        ( Strings.TvCzarJudging { name = czarName }, Nothing )
                    , viewCall shared lobby Dict.empty judging.call
                    , judging.plays |> List.indexedMap (\i p -> viewAnswer False (Just (letter i)) p.responses)
                    )

                Round.C _ ->
                    -- Shown by viewResult instead.
                    ( ( Strings.TvGameOver, Nothing ), Html.text "", [] )

        columns =
            case List.length answers of
                n ->
                    if n <= 1 then
                        1

                    else if n <= 4 then
                        2

                    else if n <= 9 then
                        3

                    else
                        4
    in
    [ Html.div [ HtmlA.class "tv-round" ]
        [ Html.header [ HtmlA.class "tv-round-header" ]
            [ Html.div [ HtmlA.class "tv-round-title" ]
                [ Html.h1 [] [ headline |> Lang.html shared ]
                , sub |> Maybe.map (\s -> Html.span [ HtmlA.class "tv-round-sub" ] [ s |> Lang.html shared ]) |> Maybe.withDefault (Html.text "")
                ]
            , Html.div [ HtmlA.class "tv-round-meta" ]
                [ viewRoundInfo shared game
                , timer
                , Html.span [ HtmlA.class "tv-code-chip" ] [ Html.text (GameCode.toString model.route.gameCode) ]
                ]
            ]
        , Html.main_ [ HtmlA.class "tv-round-main" ]
            [ Html.div [ HtmlA.class "tv-call" ] [ call ]
            , Html.div
                [ HtmlA.classList [ ( "tv-answers", True ), ( "dense", List.length answers > 6 ) ]
                , HtmlA.style "grid-template-columns" ("repeat(" ++ String.fromInt columns ++ ", minmax(0, 1fr))")
                ]
                answers
            ]
        , viewPlayerStrip shared lobby game voting humanPlayers
        ]
    ]


{-| The end of a round: who won, the winning card in the call, how everyone voted, and the standings.
By now the finished round is already part of the history.
-}
viewResult : Shared -> Lobby -> Game.Model -> Round.Complete -> List (Html msg)
viewResult shared lobby game complete =
    let
        nameOf id =
            lobby.users |> Dict.get id |> Maybe.map .name |> Maybe.withDefault "?"

        voting =
            lobby.config.rules.houseRules.godIsDead /= Nothing

        -- The winner is a player, so find their play.
        isWinner p =
            p.playedBy == complete.winner

        plays =
            complete.playOrder
                |> List.filterMap (\playId -> complete.plays |> Dict.get playId)
                |> List.sortBy
                    (\p ->
                        ( if isWinner p then
                            0

                          else
                            1
                        , -(p.votes |> Maybe.withDefault 0)
                        )
                    )

        fills =
            plays |> List.filter isWinner |> List.head |> Maybe.map (.play >> Parts.fillsFromPlay) |> Maybe.withDefault Dict.empty

        mostVotes =
            plays |> List.filterMap .votes |> List.maximum |> Maybe.withDefault 0

        viewPlay p =
            let
                bar =
                    case p.votes of
                        Just votes ->
                            let
                                share =
                                    if mostVotes > 0 then
                                        toFloat votes / toFloat mostVotes * 100

                                    else
                                        0
                            in
                            [ Html.div [ HtmlA.class "tv-votes-bar" ]
                                [ Html.div [ HtmlA.class "tv-votes-fill", HtmlA.style "width" (String.fromFloat share ++ "%") ] [] ]
                            , Html.span [ HtmlA.class "tv-votes-count" ]
                                [ Strings.VoteCount { votes = votes } |> Lang.html shared, Html.text (" · " ++ nameOf p.playedBy) ]
                            ]

                        Nothing ->
                            [ Html.span [ HtmlA.class "tv-votes-count" ] [ Html.text (nameOf p.playedBy) ] ]
            in
            Html.li [ HtmlA.classList [ ( "winner", isWinner p ) ] ]
                [ Html.div [ HtmlA.class "tv-result-text" ]
                    [ Html.div [] (p.play |> List.map (\r -> Html.p [] [ Html.text r.body ]))
                    , if isWinner p then
                        Icon.view Icon.win

                      else
                        Html.text ""
                    ]
                , Html.div [ HtmlA.class "tv-votes" ] bar
                ]

        ranked =
            game.game.players
                |> Dict.toList
                |> List.filterMap
                    (\( id, p ) ->
                        lobby.users
                            |> Dict.get id
                            |> Maybe.andThen (\u -> ( u.name, p.score ) |> Maybe.justIf (u.role == User.Player))
                    )
                |> List.sortBy (\( _, score ) -> -score)

        topScore =
            ranked |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0

        viewStanding ( name, score ) =
            Html.span [ HtmlA.classList [ ( "leader", score == topScore && score > 0 ) ] ]
                [ Html.text (name ++ " " ++ String.fromInt score) ]
    in
    [ Html.div [ HtmlA.class "tv-result" ]
        [ Html.section [ HtmlA.class "tv-result-main" ]
            [ Html.div [ HtmlA.class "tv-result-title" ]
                [ Html.span [ HtmlA.class "tv-kicker" ] [ Strings.TvRoundWinnerLabel { round = max 1 (List.length game.game.history) } |> Lang.html shared ]
                , Html.h1 [] [ Strings.TvRoundWinner { name = nameOf complete.winner } |> Lang.html shared ]
                ]
            , Html.div [ HtmlA.class "tv-call winning" ] [ viewCall shared lobby fills complete.call ]
            ]
        , Html.section [ HtmlA.class "tv-result-side" ]
            [ Html.h2 []
                [ (if voting then
                    Strings.TvTheVotes

                   else
                    Strings.TvTheAnswers
                  )
                    |> Lang.html shared
                ]
            , Html.ol [ HtmlA.classList [ ( "tv-result-list", True ), ( "dense", List.length plays > 5 ) ] ] (plays |> List.map viewPlay)
            , Html.div [ HtmlA.class "tv-result-standings" ]
                (Html.span [ HtmlA.class "label" ] [ Strings.TvStandings |> Lang.html shared ] :: (ranked |> List.map viewStanding))
            ]
        ]
    ]


formatSeconds : Int -> String
formatSeconds seconds =
    String.fromInt (seconds // 60) ++ ":" ++ String.padLeft 2 '0' (String.fromInt (modBy 60 seconds))


viewCall : Shared -> Lobby -> Dict Int String -> Card.Call -> Html msg
viewCall shared lobby fills call =
    Call.viewFilled shared lobby.config Card.Front [] (always []) fills call


{-| A face-down answer: played (solid) or still waiting (dashed).
-}
viewBack : Bool -> Html msg
viewBack played =
    Html.div [ HtmlA.classList [ ( "tv-answer", True ), ( "back", True ), ( "waiting", not played ) ] ]
        [ Html.span [ HtmlA.class "tv-back-logo" ] [ Html.text "MD" ] ]


{-| An answer tile, optionally labelled (A, B, …) so people can talk about it. Multi-card plays show each card on its
own line.
-}
viewAnswer : Bool -> Maybe String -> List Card.Response -> Html msg
viewAnswer highlight label responses =
    Html.div [ HtmlA.classList [ ( "tv-answer", True ), ( "highlight", highlight ) ] ]
        [ Html.div [ HtmlA.class "tv-answer-text" ] (responses |> List.map (\r -> Html.p [] [ Html.text r.body ]))
        , label |> Maybe.map (\l -> Html.span [ HtmlA.class "tv-answer-label" ] [ Html.text l ]) |> Maybe.withDefault (Html.text "")
        ]


{-| Everyone in the game along the bottom: who still has to act, who is czar, and the scores.
-}
viewPlayerStrip : Shared -> Lobby -> Game.Model -> Bool -> Set.Set User.Id -> Html msg
viewPlayerStrip shared lobby game voting humanPlayers =
    let
        round =
            game.game.round

        ( isActive, participants, ( done, doneStatus ) ) =
            case round.stage of
                Round.P playing ->
                    ( True, round.players, ( playing.played, Strings.TvStatusPlayed ) )

                Round.J judging ->
                    if voting then
                        ( True, humanPlayers, ( judging.voted, Strings.TvStatusVoted ) )

                    else
                        ( False, Set.empty, ( Set.empty, Strings.TvStatusVoted ) )

                _ ->
                    ( False, Set.empty, ( Set.empty, Strings.TvStatusVoted ) )

        -- The player order and the client's player list also contain spectators (like the TV itself).
        viewPlayer id =
            Maybe.map2 (viewChip id)
                (lobby.users |> Dict.get id |> Maybe.andThen (\u -> u |> Maybe.justIf (u.role == User.Player)))
                (game.game.players |> Dict.get id)

        viewChip id user player =
            let
                isDone =
                    isActive && Set.member id done

                isWaiting =
                    isActive && Set.member id participants && not isDone

                -- When everyone votes there is no czar to show.
                isCzar =
                    id == round.czar && not voting

                badge =
                    if isDone then
                        Icon.view Icon.accept

                    else if isCzar then
                        Icon.view Icon.czar

                    else
                        Html.text (initial user.name)

                status =
                    if isDone then
                        Just doneStatus

                    else if isWaiting then
                        Just Strings.TvStatusThinking

                    else if isCzar then
                        Just Strings.TvStatusReading

                    else
                        Nothing
            in
            ( id
            , Html.li
                [ HtmlA.classList
                    [ ( "done", isDone )
                    , ( "waiting", isWaiting )
                    , ( "czar", isCzar )
                    , ( "away", user.presence == User.Left )
                    ]
                ]
                [ Html.span [ HtmlA.class "tv-chip-badge" ] [ badge ]
                , Html.span [ HtmlA.class "tv-chip-text" ]
                    [ Html.span [ HtmlA.class "tv-chip-name" ] [ Html.text user.name ]
                    , status |> Maybe.map (\s -> Html.span [ HtmlA.class "tv-chip-status" ] [ s |> Lang.html shared ]) |> Maybe.withDefault (Html.text "")
                    ]
                , Html.span [ HtmlA.class "tv-chip-score" ] [ Html.text (String.fromInt player.score) ]
                ]
            )
    in
    HtmlK.ul [ HtmlA.class "tv-strip" ] (game.game.playerOrder |> List.filterMap viewPlayer)


{-| The end of the game: everyone ranked by score, winners on top.
-}
viewStandings : Shared -> Lobby -> Game.Model -> Set.Set User.Id -> List (Html msg)
viewStandings shared lobby game winners =
    let
        ranked =
            game.game.players
                |> Dict.toList
                |> List.filterMap
                    (\( id, p ) ->
                        lobby.users
                            |> Dict.get id
                            |> Maybe.andThen (\u -> ( id, u, p ) |> Maybe.justIf (u.role == User.Player))
                    )
                |> List.sortBy (\( _, _, p ) -> -p.score)

        viewRow ( id, user, player ) =
            ( id
            , Html.li [ HtmlA.classList [ ( "winner", Set.member id winners ) ] ]
                [ Html.span [ HtmlA.class "tv-standing-badge" ]
                    [ if Set.member id winners then
                        Icon.view Icon.win

                      else
                        Html.text ""
                    ]
                , Html.span [ HtmlA.class "tv-standing-name" ] [ Html.text user.name ]
                , Html.span [ HtmlA.class "tv-standing-score" ] [ Html.text (String.fromInt player.score) ]
                ]
            )
    in
    [ Html.div [ HtmlA.class "tv-standings" ]
        [ Html.h1 [] [ Strings.TvGameOver |> Lang.html shared ]
        , HtmlK.ol [] (ranked |> List.map viewRow)
        ]
    ]
