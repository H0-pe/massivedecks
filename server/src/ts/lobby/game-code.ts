import { randomInt } from "node:crypto";

import Hashids from "hashids";

/**
 * A unique code for a lobby to identify it.
 * @minLength 2
 */
export type GameCode = string;

/**
 * A numeric id for a lobby. This is presented to the user as a game code.
 */
export type LobbyId = number;

// noinspection SpellCheckingInspection
/**
 * Game code methods.
 */
const hashIds = new Hashids(
  "massivedecks",
  2,
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
);

// No 0/O, 1/I/L: easy to read off a TV and type on a phone.
const randomAlphabet = "ABCDEFGHJKMNPQRSTUVWXYZ23456789";

/**
 * A random four character game code (~920,000 possibilities), so codes can't be guessed from each other.
 */
export const random = (): GameCode =>
  Array.from(
    { length: 4 },
    () => randomAlphabet[randomInt(randomAlphabet.length)],
  ).join("");

/**
 * Decode a game code to a lobby id.
 */
export const decode = (gameCode: GameCode): LobbyId =>
  Number(hashIds.decode(gameCode)[0]);

/**
 * Decode a lobby id to a game code.
 */
export const encode = (lobbyId: LobbyId): GameCode => hashIds.encode(lobbyId);
