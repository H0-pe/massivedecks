import wu from "wu";

import type * as Cache from "../../cache.js";
import { OutOfCardsError } from "../../errors/game-state-error.js";
import * as Util from "../../util.js";
import * as Card from "./card.js";

/**
 * Combine the cards of several decks, leaving out repeats: packs often share cards (e.g. a "main deck" and the
 * versions it collects, or the same deck from two sources), and the same card twice in one game isn't fun. Cards
 * count as the same when their text matches, ignoring case and punctuation. Blank custom cards are all kept.
 */
const combine = <C extends Card.Card>(
  sets: Iterable<Set<C>>,
  text: (card: C) => string,
): Set<C> => {
  const seen = new Set<string>();
  const result = new Set<C>();
  for (const set of sets) {
    for (const card of set) {
      if (Card.isCustom(card)) {
        result.add(card);
        continue;
      }
      const key = normalise(text(card));
      if (!seen.has(key)) {
        seen.add(key);
        result.add(card);
      }
    }
  }
  return result;
};

const normalise = (text: string): string =>
  text
    .toLowerCase()
    .replace(/[^\p{L}\p{N}_]+/gu, " ")
    .trim();

const callText = (call: Card.Call): string =>
  call.parts
    .map((line) =>
      line
        .map((part) =>
          typeof part === "string" ? part : Card.isSlot(part) ? "_" : part.text,
        )
        .join(""),
    )
    .join(" ");

const responseText = (response: Card.Response): string => response.text;

/**
 * A deck of cards.
 */
export abstract class Deck<C extends Card.BaseCard> {
  /**
   * The cards in the deck.
   */
  public readonly cards: C[];
  /**
   * Cards drawn from this deck that have since been discarded (used for reshuffling).
   */
  public readonly discarded: Set<C>;

  protected constructor(cards: C[], discarded: Iterable<C>) {
    this.cards = cards;
    this.discarded = new Set(discarded);
  }

  public discard(cards: Iterable<C>): void {
    for (const c of cards) {
      this.discardSingle(c);
    }
  }

  protected discardSingle(card: C): void {
    this.discarded.add(card);
  }

  public draw(cards: 1): [C];
  public draw(cards: 2): [C, C];
  public draw(cards: 3): [C, C, C];
  public draw(cards: number): C[];
  public draw(cards: number): C[] {
    const cardsLeft = this.cards.length;
    const toDraw = Math.min(cardsLeft, cards);
    const result = this.cards.splice(0, toDraw);
    if (toDraw < cards) {
      this.reshuffle();
      return [...result, ...this.draw(cards - toDraw)];
    } else {
      return result;
    }
  }

  public replace(...cards: [C]): [C];
  public replace(...cards: [C, C]): [C, C];
  public replace(...cards: [C, C, C]): [C, C, C];
  public replace(...cards: C[]): C[];
  public replace(...cards: C[]): C[] {
    this.discard(cards);
    return this.draw(cards.length);
  }

  protected reshuffle(): void {
    if (this.discarded.size < 1) {
      throw new OutOfCardsError();
    }
    this.cards.push(...Util.shuffled(this.discarded));
    this.discarded.clear();
  }

  public toJSON(): object {
    return {
      cards: this.cards,
      discarded: Array.from(this.discarded),
    };
  }
}

/**
 * A deck for call cards.
 */
export class Calls extends Deck<Card.Call> {
  public static fromTemplates(template: Iterable<Template<Card.Call>>): Calls {
    const deck = new Calls([], combine(template, callText));
    deck.reshuffle();
    return deck;
  }

  static fromJSON(deck: Deck<Card.Call>): Calls {
    return new Calls(deck.cards, deck.discarded);
  }
}

/**
 * A deck for response cards that resets them when they are discarded so they can't be identified or hold old data
 * (in the case of custom cards).
 */
export class Responses extends Deck<Card.Response> {
  protected override discardSingle(card: Card.Response): void {
    // We duplicate the card here so we don't damage any references to it hanging around elsewhere (e.g: history).
    this.discarded.add({
      ...card,
      id: Card.id(),
      ...(Card.isCustom(card) ? { text: "" } : {}),
    });
  }

  public static fromTemplates(
    template: Iterable<Template<Card.Response>>,
  ): Responses {
    const deck = new Responses([], combine(template, responseText));
    deck.reshuffle();
    return deck;
  }

  static fromJSON(deck: Deck<Card.Response>): Responses {
    return new Responses(deck.cards, deck.discarded);
  }
}

/**
 * The two decks needed for a game.
 */
export interface Decks {
  calls: Calls;
  responses: Responses;
}

/**
 * A template for a deck.
 */
export type Template<C extends Card.BaseCard> = Set<C>;

/**
 * Templates for the two decks needed for a game.
 */
export interface Templates extends Cache.Tagged {
  calls: Template<Card.Call>;
  responses: Template<Card.Response>;
}

export const decks = (templates: Iterable<Templates>): Decks => ({
  calls: Calls.fromTemplates(wu(templates).map((template) => template.calls)),
  responses: Responses.fromTemplates(
    wu(templates).map((template) => template.responses),
  ),
});
