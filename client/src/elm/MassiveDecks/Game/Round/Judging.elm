module MassiveDecks.Game.Round.Judging exposing (view)

import Dict exposing (Dict)
import MassiveDecks.Card.Model as Card
import MassiveDecks.Card.Parts as Parts
import MassiveDecks.Card.Play as Play
import MassiveDecks.Card.Response as Response
import MassiveDecks.Game.Action.Model as Action
import MassiveDecks.Game.Messages exposing (Msg(..))
import MassiveDecks.Game.Model exposing (RoundView)
import MassiveDecks.Game.Player as Player
import MassiveDecks.Game.Round as Round
import MassiveDecks.Game.Round.Plays as Plays
import MassiveDecks.Model exposing (Shared)
import MassiveDecks.Pages.Lobby.Configure.Model exposing (Config)
import MassiveDecks.Pages.Lobby.Model exposing (Auth)
import MassiveDecks.Strings as Strings
import MassiveDecks.User as User exposing (User)
import MassiveDecks.Util.Maybe as Maybe
import Set


view : (Msg -> msg) -> Auth -> Shared -> Dict User.Id User -> Config -> Round.Specific Round.Judging -> RoundView msg
view wrap auth shared users config round =
    let
        role =
            Player.role round auth.claims.uid

        stage =
            round.stage

        voting =
            config.rules.houseRules.godIsDead /= Nothing

        -- Everyone who could vote, to show progress. Computer players and spectators don't vote.
        voters =
            users
                |> Dict.values
                |> List.filter (\u -> u.role == User.Player && u.presence == User.Joined && u.control == User.Human)
                |> List.length

        canVoteFor play =
            Just play /= stage.likeDetail.played

        { action, msg, instruction, isCzar } =
            case ( voting, role ) of
                ( True, _ ) ->
                    { action =
                        stage.pick
                            |> Maybe.andThen (\p -> Action.Vote |> Maybe.justIf (canVoteFor p && stage.vote /= Just p))
                    , msg = \p -> p |> PickPlay |> wrap |> Maybe.justIf (canVoteFor p)
                    , instruction =
                        case stage.vote of
                            Just _ ->
                                Strings.VotedInstruction { voted = Set.size stage.voted, total = voters }

                            Nothing ->
                                Strings.VoteInstruction
                    , isCzar = False
                    }

                ( False, Player.RCzar ) ->
                    { action = Maybe.map (always Action.Judge) stage.pick
                    , msg = \p -> p |> PickPlay |> wrap |> Just
                    , instruction = Strings.RevealPlaysInstruction
                    , isCzar = True
                    }

                ( False, Player.RPlayer ) ->
                    let
                        canBeLiked play =
                            Just play /= stage.likeDetail.played && not (Set.member play stage.likeDetail.liked)
                    in
                    { action = Maybe.andThen (\p -> Maybe.justIf (canBeLiked p) Action.Like) stage.pick
                    , msg = \p -> p |> PickPlay |> wrap |> Maybe.justIf (canBeLiked p)
                    , instruction = Strings.WaitingForCzarInstruction
                    , isCzar = False
                    }

        picked =
            stage.plays
                |> List.filter (\play -> Just play.id == stage.pick)
                |> List.head
                |> Maybe.map (.responses >> Parts.fillsFromPlay)
                |> Maybe.withDefault Dict.empty

        details =
            stage.plays |> List.map (playDetails shared config msg)
    in
    { call = Just stage.call
    , instruction = Just instruction
    , action = action
    , content = details |> Plays.view shared users [ ( "judging", True ), ( "is-czar", isCzar ) ] stage.likeDetail.liked stage.pick
    , slotAttrs = always []
    , fillCallWith = picked
    , roundAttrs = []
    }



{- Private -}


playDetails : Shared -> Config -> (Play.Id -> Maybe msg) -> Play.Known -> Plays.Details msg
playDetails shared config msg { id, responses } =
    let
        maybeMsg =
            msg id

        cards =
            responses
                |> List.map (\r -> Response.view shared config Card.Front [] r)
    in
    Plays.Details id cards maybeMsg Nothing
