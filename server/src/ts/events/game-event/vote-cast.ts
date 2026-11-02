import type * as User from "../../user.js";

/**
 * Indicates a player has voted (with "God Is Dead"). Who they voted for stays secret until the round is decided.
 */
export interface VoteCast {
  event: "VoteCast";
  player: User.Id;
}

export const of = (player: User.Id): VoteCast => ({
  event: "VoteCast",
  player,
});
