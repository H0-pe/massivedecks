import { InvalidActionError } from "../../../errors/validation.js";
import * as Event from "../../../event.js";
import * as RoundFinished from "../../../events/game-event/round-finished.js";
import type * as Play from "../../../games/cards/play.js";
import type * as Round from "../../../games/game/round.js";
import type { Player } from "../../../games/player.js";
import type * as Lobby from "../../../lobby.js";
import type { ConstrainedChange } from "../../../lobby/change.js";
import * as RoundStart from "../../../timeout/round-start.js";
import type * as Handler from "../../handler.js";
import type { Czar } from "../czar.js";
import * as Actions from "./../../actions.js";

/**
 * A user declares the winning play for a round.
 */
export interface Judge {
  action: "Judge";
  winner: Play.Id;
}

class JudgeAction extends Actions.Implementation<
  Czar,
  Judge,
  "Judge",
  Lobby.WithActiveGame
> {
  protected readonly name = "Judge";

  protected handle: Handler.Custom<Judge, Lobby.WithActiveGame> = (
    auth,
    lobby,
    action,
  ) => {
    const round = lobby.game.round;
    if (round.verifyStage<Round.Judging>(action, "Judging")) {
      if (lobby.game.rules.houseRules.godIsDead !== undefined) {
        throw new InvalidActionError(
          "With God Is Dead the winner is decided by vote, not by the czar.",
        );
      }
      const winningPlay = round.plays.find((play) => play.id === action.winner);
      if (winningPlay === undefined) {
        throw new InvalidActionError("Given play doesn't exist.");
      }
      return completeRound(lobby, round, winningPlay.id);
    } else {
      return {};
    }
  };
}

/**
 * Finish a judged round with the given winning play: hand out likes and the point, then move on.
 * Shared by the czar's judgement and by voting ("God Is Dead"), which also passes the vote counts.
 */
export const completeRound = (
  lobby: Lobby.WithActiveGame,
  round: Round.Judging,
  winner: Play.Id,
  votes?: { [play: string]: number },
): ConstrainedChange<Lobby.WithActiveGame> => {
  const game = lobby.game;
  let winningPlay = undefined;
  for (const play of round.plays) {
    if (play.likes.length > 0) {
      const player = game.players[play.playedBy] as Player;
      player.likes += play.likes.length;
    }
    if (play.id === winner) {
      winningPlay = play;
    }
  }
  if (winningPlay === undefined) {
    throw new InvalidActionError("Given play doesn't exist.");
  }
  const player = game.players[winningPlay.playedBy] as Player;
  player.score += 1;
  if (game.rules.houseRules.winnersPick) {
    game.rules.houseRules.winnersPick.roundWinner = winningPlay.playedBy;
  }
  const completedRound = round.advance(winningPlay.playedBy, votes);
  game.round = completedRound;
  game.history.splice(0, 0, completedRound.public());

  return {
    lobby,
    events: [Event.targetAll(RoundFinished.of(completedRound))],
    timeouts: [
      {
        timeout: RoundStart.of(),
        after: game.rules.stages.judging.after * 1000,
      },
    ],
  };
};

export const actions = new JudgeAction();
