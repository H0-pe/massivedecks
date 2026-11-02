import wu from "wu";

import { completeRound } from "../../action/game-action/czar/judge.js";
import type * as Lobby from "../../lobby.js";
import type { ConstrainedChange } from "../../lobby/change.js";
import type * as User from "../../user.js";
import type * as Play from "../cards/play.js";
import type * as Round from "../game/round.js";

/**
 * The players who may vote in the current round: human players in the game who are present. Computer players (Rando)
 * don't vote, and neither do spectators.
 */
export const eligibleVoters = (lobby: Lobby.WithActiveGame): Set<User.Id> => {
  const game = lobby.game;
  return new Set(
    wu(Object.entries(game.players))
      .filter(([id, player]) => {
        const user = lobby.users[id];
        return (
          user !== undefined &&
          user.role === "Player" &&
          user.presence === "Joined" &&
          user.control === "Human" &&
          player.presence === "Active"
        );
      })
      .map(([id]) => id),
  );
};

/**
 * Whether the given player has anything to vote for (you can't vote for your own play).
 */
export const canVote = (round: Round.Judging, voter: User.Id): boolean =>
  round.plays.some((play) => play.playedBy !== voter);

/**
 * Decide the round from the votes cast so far: most votes wins, a tie is decided at random, and with no votes at all a
 * random play wins.
 */
export const tally = (
  lobby: Lobby.WithActiveGame,
  round: Round.Judging,
): ConstrainedChange<Lobby.WithActiveGame> => {
  const counts: { [play: string]: number } = Object.fromEntries(
    round.plays.map((play) => [play.id, 0]),
  );
  for (const play of round.votes.values()) {
    if (counts[play] !== undefined) {
      counts[play] += 1;
    }
  }
  const most = Math.max(...Object.values(counts));
  const leaders: Play.Id[] = Object.entries(counts)
    .filter(([, count]) => count === most)
    .map(([play]) => play);
  const winner = leaders[Math.floor(Math.random() * leaders.length)] as Play.Id;
  return completeRound(lobby, round, winner, counts);
};

/**
 * Decide the round if everyone who can vote has voted.
 */
export const tallyIfComplete = (
  lobby: Lobby.WithActiveGame,
): ConstrainedChange<Lobby.WithActiveGame> => {
  const round = lobby.game.round;
  if (round.stage !== "Judging" || lobby.game.rules.houseRules.godIsDead === undefined) {
    return {};
  }
  const judging = round as Round.Judging;
  const outstanding = wu(eligibleVoters(lobby)).filter(
    (voter) => canVote(judging, voter) && !judging.votes.has(voter),
  );
  return outstanding.some(() => true) ? {} : tally(lobby, judging);
};
