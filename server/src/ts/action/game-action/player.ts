import type * as Action from "../../action.js";
import * as ActionExecutionError from "../../errors/action-execution-error.js";
import type * as Lobby from "../../lobby.js";
import type * as Token from "../../user/token.js";
import * as GameAction from "../game-action.js";
import * as Actions from "./../actions.js";
import * as Discard from "./player/discard.js";
import * as Fill from "./player/fill.js";
import * as Submit from "./player/submit.js";
import * as TakeBack from "./player/take-back.js";

/**
 * An action only players can perform.
 */
export type Player =
  | Submit.Submit
  | TakeBack.TakeBack
  | Fill.Fill
  | Discard.Discard;

class PlayerActions extends Actions.Group<
  Action.Action,
  Player,
  Lobby.WithActiveGame,
  Lobby.WithActiveGame
> {
  constructor() {
    super(Submit.actions, TakeBack.actions, Fill.actions, Discard.actions);
  }

  limit(
    auth: Token.Claims,
    lobby: Lobby.WithActiveGame,
    action: Player,
  ): lobby is Lobby.WithActiveGame {
    // With "God Is Dead" the czar only reads out the plays and plays like everyone else, but it must still be someone
    // in the round (not a spectator or the TV).
    if (lobby.game.rules.houseRules.godIsDead === undefined) {
      GameAction.expectRole(auth, action, lobby.game, "Player");
    } else if (!lobby.game.round.players.has(auth.uid)) {
      throw new ActionExecutionError.IncorrectPlayerRoleError(
        action,
        null,
        "Player",
      );
    }
    return true;
  }
}

export const actions = new PlayerActions();
