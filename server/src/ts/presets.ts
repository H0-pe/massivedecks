import { promises as fs } from "fs";
import * as path from "path";

/**
 * Presets: saved lobby configurations shared by everyone using this server. The server doesn't interpret them beyond
 * checking their shape; the client applies them. They live in a JSON file so they survive restarts.
 */
export interface Preset {
  name: string;
  rules: object;
  public: boolean;
  audienceMode: boolean;
  decks: unknown[];
}

export class InvalidPresetError extends Error {}

const maxPresets = 100;
const maxNameLength = 60;
const maxPresetSize = 16 * 1024;

const isObject = (
  value: unknown,
): value is Partial<Record<keyof Preset, unknown>> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

export const validate = (value: unknown): Preset => {
  if (
    !isObject(value) ||
    typeof value.name !== "string" ||
    !isObject(value.rules) ||
    typeof value.public !== "boolean" ||
    typeof value.audienceMode !== "boolean" ||
    !Array.isArray(value.decks)
  ) {
    throw new InvalidPresetError("Malformed preset.");
  }
  const name = value.name.trim();
  if (name.length < 1 || name.length > maxNameLength) {
    throw new InvalidPresetError("Invalid preset name.");
  }
  const preset = {
    name,
    rules: value.rules,
    public: value.public,
    audienceMode: value.audienceMode,
    decks: value.decks,
  };
  if (JSON.stringify(preset).length > maxPresetSize) {
    throw new InvalidPresetError("Preset too large.");
  }
  return preset;
};

export class Store {
  private readonly file: string;
  private presets: Preset[];
  private writing: Promise<void> = Promise.resolve();

  private constructor(file: string, presets: Preset[]) {
    this.file = file;
    this.presets = presets;
  }

  public static async load(file: string): Promise<Store> {
    let presets: Preset[] = [];
    try {
      const parsed = JSON.parse((await fs.readFile(file)).toString());
      if (Array.isArray(parsed)) {
        presets = parsed.flatMap((p) => {
          try {
            return [validate(p)];
          } catch {
            return [];
          }
        });
      }
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== "ENOENT") {
        throw error;
      }
    }
    return new Store(file, presets);
  }

  public all(): Preset[] {
    return this.presets;
  }

  /**
   * Add or replace (by name) the given presets.
   */
  public async save(presets: Preset[]): Promise<Preset[]> {
    const names = new Set(presets.map((p) => p.name));
    const updated = [
      ...this.presets.filter((p) => !names.has(p.name)),
      ...presets,
    ];
    if (updated.length > maxPresets) {
      throw new InvalidPresetError("Too many presets.");
    }
    return await this.replace(updated);
  }

  public async delete(name: string): Promise<Preset[]> {
    return await this.replace(this.presets.filter((p) => p.name !== name));
  }

  private async replace(presets: Preset[]): Promise<Preset[]> {
    this.presets = presets;
    const content = JSON.stringify(presets);
    // Writes happen one after the other, each through a temporary file, so the file is never half-written.
    this.writing = this.writing
      .catch(() => undefined)
      .then(async () => {
        await fs.mkdir(path.dirname(this.file), { recursive: true });
        const temporary = `${this.file}.tmp`;
        await fs.writeFile(temporary, content);
        await fs.rename(temporary, this.file);
      });
    await this.writing;
    return presets;
  }
}
