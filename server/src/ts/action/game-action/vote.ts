import { InvalidActionError } from "../../errors/validation.js";
import * as Event from "../../event.js";
import * as VoteCast from "../../events/game-event/vote-cast.js";
import type * as Play from "../../games/cards/play.js";
import type * as Round from "../../games/game/round.js";
import * as GodIsDead from "../../games/rules/god-is-dead.js";
import type * as Lobby from "../../lobby.js";
import * as Actions from "../actions.js";
import type { GameAction } from "../game-action.js";
import type * as Handler from "../handler.js";

/**
 * A player votes for the best play ("God Is Dead" house rule). Voting again replaces the earlier vote.
 */
export interface Vote {
  action: "Vote";
  play: Play.Id;
}

class VoteActions extends Actions.Implementation<
  GameAction,
  Vote,
  "Vote",
  Lobby.WithActiveGame
> {
  protected readonly name = "Vote";

  protected handle: Handler.Custom<Vote, Lobby.WithActiveGame> = (
    auth,
    lobby,
    action,
  ) => {
    if (lobby.game.rules.houseRules.godIsDead === undefined) {
      throw new InvalidActionError("Voting is only used with God Is Dead.");
    }
    const round = lobby.game.round;
    if (round.verifyStage<Round.Judging>(action, "Judging")) {
      if (!GodIsDead.eligibleVoters(lobby).has(auth.uid)) {
        throw new InvalidActionError("Only players in the game can vote.");
      }
      const target = round.plays.find((play) => play.id === action.play);
      if (target === undefined) {
        throw new InvalidActionError("Given play doesn't exist.");
      }
      if (target.playedBy === auth.uid) {
        throw new InvalidActionError("You can't vote for your own play.");
      }
      round.votes.set(auth.uid, action.play);
      const decided = GodIsDead.tallyIfComplete(lobby);
      return {
        lobby,
        events: [
          Event.targetAll(VoteCast.of(auth.uid)),
          ...(decided.events ?? []),
        ],
        ...(decided.timeouts !== undefined ? { timeouts: decided.timeouts } : {}),
      };
    } else {
      return {};
    }
  };
}

export const actions = new VoteActions();
