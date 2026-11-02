/**
 * A drop-down in the app's own design. The platform's select list can't be styled, so this draws the field and the
 * list itself. Elm renders the options as <md-option value label detail> children and sets the "value" attribute; the
 * element never touches its children (Elm owns them) and draws everything in its shadow root. Picking an option
 * updates `value` and fires "change", like a native select.
 *
 * Long lists get a filter field. On narrow screens the list opens as a sheet at the bottom of the screen.
 */

interface Option {
  value: string;
  label: string;
  detail: string;
}

const style = `
:host {
  display: block;
  position: relative;
  min-width: 0;
  font-family: var(--md-font, sans-serif);
}

:host([disabled]) {
  pointer-events: none;
}

.field {
  all: unset;
  box-sizing: border-box;
  display: flex;
  align-items: center;
  gap: 0.75rem;
  width: 100%;
  min-height: 3.25rem;
  padding: 0.55rem 0.9rem 0.5rem;
  border-radius: 0.75rem;
  border: 2px solid transparent;
  background: var(--md-surface-raised);
  color: var(--md-text);
  cursor: pointer;
  transition: border-color 0.15s;
}

.field:hover {
  border-color: var(--md-line-strong);
}

.field:focus-visible,
:host([open]) .field {
  border-color: var(--md-primary);
}

:host([disabled]) .field {
  background: transparent;
  border: 2px dashed var(--md-line-strong);
  color: var(--md-text-muted);
}

.text {
  flex: 1;
  min-width: 0;
  display: flex;
  flex-direction: column;
  gap: 0.2rem;
}

.label {
  font-size: 0.8125rem;
  font-weight: 600;
  color: var(--md-text-muted);
}

.value {
  font-weight: 800;
  font-size: 1.0625rem;
  line-height: 1.3;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.value.empty {
  color: var(--md-text-faint);
  font-weight: 600;
}

.chevron {
  flex: none;
  width: 0.5rem;
  height: 0.5rem;
  margin: 0 0.25rem 0.25rem;
  border-right: 2px solid var(--md-text-muted);
  border-bottom: 2px solid var(--md-text-muted);
  transform: rotate(45deg);
  transition: transform 0.15s;
}

:host([open]) .chevron {
  transform: rotate(225deg);
  margin: 0.25rem 0.25rem 0;
}

.popup {
  position: absolute;
  z-index: 1000;
  left: 0;
  right: 0;
  top: calc(100% + 0.35rem);
  display: none;
  flex-direction: column;
  max-height: min(22rem, 60vh);
  box-sizing: border-box;
  padding: 0.4rem;
  border-radius: 0.9rem;
  border: 1px solid var(--md-line-strong);
  background: var(--md-surface-raised);
  box-shadow: 0 1rem 2.5rem rgba(0, 0, 0, 0.6);
}

:host([open]) .popup {
  display: flex;
}

:host([open][above]) .popup {
  top: auto;
  bottom: calc(100% + 0.35rem);
}

.filter {
  box-sizing: border-box;
  flex: none;
  margin: 0 0 0.4rem;
  padding: 0.6rem 0.75rem;
  border: 2px solid transparent;
  border-radius: 0.6rem;
  background: var(--md-background);
  color: var(--md-text);
  font: inherit;
  font-weight: 600;
  font-size: 1rem;
  outline: none;
}

.filter::-webkit-search-cancel-button {
  -webkit-appearance: none;
}

.filter:focus {
  border-color: var(--md-primary);
}

.list {
  overflow-y: auto;
  overscroll-behavior: contain;
  outline: none;
}

.option {
  display: flex;
  align-items: center;
  gap: 0.6rem;
  min-height: 2.75rem;
  box-sizing: border-box;
  padding: 0.45rem 0.75rem;
  border-radius: 0.6rem;
  color: var(--md-text);
  cursor: pointer;
}

.option .option-text {
  flex: 1;
  min-width: 0;
  display: flex;
  flex-direction: column;
}

.option .option-label {
  font-weight: 700;
}

.option .option-detail {
  font-size: 0.8125rem;
  color: var(--md-text-muted);
}

.option.active {
  background: var(--md-line);
}

.option[aria-selected="true"] {
  color: var(--md-primary);
}

.option .tick {
  flex: none;
  width: 0.35rem;
  height: 0.7rem;
  margin-right: 0.2rem;
  border-right: 2px solid currentColor;
  border-bottom: 2px solid currentColor;
  transform: rotate(45deg);
  visibility: hidden;
}

.option[aria-selected="true"] .tick {
  visibility: visible;
}

.none {
  padding: 0.75rem;
  color: var(--md-text-muted);
}

/* Phones: a sheet along the bottom of the screen, easy to reach and big enough for long lists. */
@media (max-width: 36rem) {
  :host([open]) .popup,
  :host([open][above]) .popup {
    position: fixed;
    left: 0.75rem;
    right: 0.75rem;
    top: auto;
    bottom: 0.75rem;
    max-height: 70vh;
  }

  .backdrop {
    position: fixed;
    inset: 0;
    z-index: 999;
    background: rgba(0, 0, 0, 0.6);
    display: none;
  }

  :host([open]) .backdrop {
    display: block;
  }
}
`;

const FILTER_FROM = 12;
let nextId = 0;

class MdSelect extends HTMLElement {
  static get observedAttributes() {
    return ["value", "label", "disabled"];
  }

  private readonly root: ShadowRoot;
  private readonly field: HTMLButtonElement;
  private readonly labelEl: HTMLElement;
  private readonly valueEl: HTMLElement;
  private readonly popup: HTMLElement;
  private readonly filterEl: HTMLInputElement;
  private readonly list: HTMLElement;
  private readonly observer: MutationObserver;
  private readonly uid = `md-select-${nextId++}`;
  private options: Option[] = [];
  private shown: Option[] = [];
  private active = -1;

  constructor() {
    super();
    this.root = this.attachShadow({ mode: "open" });
    this.root.innerHTML = `
      <style>${style}</style>
      <button class="field" type="button" aria-haspopup="listbox" aria-expanded="false">
        <span class="text"><span class="label"></span><span class="value"></span></span>
        <span class="chevron" aria-hidden="true"></span>
      </button>
      <div class="backdrop"></div>
      <div class="popup">
        <input class="filter" type="search" autocomplete="off" spellcheck="false" />
        <div class="list" role="listbox" tabindex="-1"></div>
      </div>`;
    this.field = this.root.querySelector(".field")!;
    this.labelEl = this.root.querySelector(".label")!;
    this.valueEl = this.root.querySelector(".value")!;
    this.popup = this.root.querySelector(".popup")!;
    this.filterEl = this.root.querySelector(".filter")!;
    this.list = this.root.querySelector(".list")!;
    this.list.id = `${this.uid}-list`;
    this.field.setAttribute("aria-controls", this.list.id);

    this.field.addEventListener("click", () => this.toggle());
    this.field.addEventListener("keydown", (e) => this.onFieldKey(e));
    this.filterEl.addEventListener("input", () => this.renderList());
    this.filterEl.addEventListener("keydown", (e) => this.onListKey(e));
    this.list.addEventListener("keydown", (e) => this.onListKey(e));
    this.root.querySelector(".backdrop")!.addEventListener("click", () => this.close(true));
    this.list.addEventListener("click", (e) => {
      const el = (e.target as HTMLElement).closest<HTMLElement>(".option");
      if (el?.dataset["value"] !== undefined) this.pick(el.dataset["value"]);
    });
    this.observer = new MutationObserver(() => this.readOptions());
  }

  connectedCallback() {
    this.observer.observe(this, { childList: true, subtree: true, attributes: true });
    this.readOptions();
    document.addEventListener("pointerdown", this.onOutside, true);
  }

  disconnectedCallback() {
    this.observer.disconnect();
    document.removeEventListener("pointerdown", this.onOutside, true);
  }

  attributeChangedCallback() {
    this.labelEl.textContent = this.getAttribute("label") ?? "";
    this.field.disabled = this.hasAttribute("disabled");
    this.renderValue();
  }

  get value(): string {
    return this.getAttribute("value") ?? "";
  }

  set value(v: string) {
    this.setAttribute("value", v);
  }

  /** Pick an option as if the user had: sets the value and fires "change". */
  select(value: string) {
    this.pick(value);
  }

  private readonly onOutside = (e: Event) => {
    if (this.hasAttribute("open") && !e.composedPath().includes(this)) this.close(false);
  };

  private readOptions() {
    this.options = Array.from(this.querySelectorAll("md-option")).map((o) => ({
      value: o.getAttribute("value") ?? "",
      label: o.getAttribute("label") ?? "",
      detail: o.getAttribute("detail") ?? "",
    }));
    this.renderValue();
    if (this.hasAttribute("open")) this.renderList();
  }

  private renderValue() {
    const current = this.options.find((o) => o.value === this.value);
    this.valueEl.textContent = current?.label ?? "—";
    this.valueEl.classList.toggle("empty", current === undefined);
  }

  private renderList() {
    const query = this.filterEl.value.trim().toLowerCase();
    this.shown = query === "" ? this.options : this.options.filter((o) => `${o.label} ${o.detail}`.toLowerCase().includes(query));
    this.list.replaceChildren(
      ...this.shown.map((o, i) => {
        const el = document.createElement("div");
        el.className = "option";
        el.id = `${this.uid}-${i}`;
        el.setAttribute("role", "option");
        el.setAttribute("aria-selected", String(o.value === this.value));
        el.dataset["value"] = o.value;
        const tick = document.createElement("span");
        tick.className = "tick";
        const text = document.createElement("span");
        text.className = "option-text";
        const label = document.createElement("span");
        label.className = "option-label";
        label.textContent = o.label;
        text.append(label);
        if (o.detail !== "") {
          const detail = document.createElement("span");
          detail.className = "option-detail";
          detail.textContent = o.detail;
          text.append(detail);
        }
        el.append(tick, text);
        el.addEventListener("pointermove", () => this.setActive(i, false));
        return el;
      }),
    );
    if (this.shown.length === 0) {
      const none = document.createElement("div");
      none.className = "none";
      none.textContent = "–";
      this.list.append(none);
    }
    const selected = this.shown.findIndex((o) => o.value === this.value);
    this.setActive(query === "" && selected >= 0 ? selected : 0, true);
  }

  private setActive(index: number, scroll: boolean) {
    this.active = Math.max(0, Math.min(index, this.shown.length - 1));
    this.list.querySelectorAll(".option").forEach((el, i) => el.classList.toggle("active", i === this.active));
    const el = this.list.querySelector<HTMLElement>(`#${this.uid}-${this.active}`);
    if (el) {
      this.list.setAttribute("aria-activedescendant", el.id);
      if (scroll) el.scrollIntoView({ block: "nearest" });
    }
  }

  private toggle() {
    if (this.hasAttribute("open")) this.close(true);
    else this.open();
  }

  private open() {
    if (this.hasAttribute("disabled")) return;
    const many = this.options.length >= FILTER_FROM;
    this.filterEl.hidden = !many;
    this.filterEl.value = "";
    // Open upwards when there isn't room below.
    const rect = this.getBoundingClientRect();
    this.toggleAttribute("above", window.innerHeight - rect.bottom < 260 && rect.top > window.innerHeight - rect.bottom);
    this.setAttribute("open", "");
    this.field.setAttribute("aria-expanded", "true");
    this.renderList();
    (many ? this.filterEl : this.list).focus({ preventScroll: true });
  }

  private close(refocus: boolean) {
    this.removeAttribute("open");
    this.field.setAttribute("aria-expanded", "false");
    if (refocus) this.field.focus({ preventScroll: true });
  }

  private pick(value: string) {
    this.close(true);
    if (value === this.value) return;
    this.value = value;
    this.dispatchEvent(new Event("change", { bubbles: true }));
  }

  private onFieldKey(e: KeyboardEvent) {
    if (["ArrowDown", "ArrowUp", "Enter", " "].includes(e.key)) {
      e.preventDefault();
      this.open();
    }
  }

  private onListKey(e: KeyboardEvent) {
    switch (e.key) {
      case "ArrowDown":
        e.preventDefault();
        this.setActive(this.active + 1, true);
        break;
      case "ArrowUp":
        e.preventDefault();
        this.setActive(this.active - 1, true);
        break;
      case "Home":
        if (e.target === this.list) {
          e.preventDefault();
          this.setActive(0, true);
        }
        break;
      case "End":
        if (e.target === this.list) {
          e.preventDefault();
          this.setActive(this.shown.length - 1, true);
        }
        break;
      case "Enter": {
        e.preventDefault();
        const option = this.shown[this.active];
        if (option) this.pick(option.value);
        break;
      }
      case "Escape":
        e.preventDefault();
        this.close(true);
        break;
      case "Tab":
        this.close(false);
        break;
    }
  }
}

customElements.define("md-select", MdSelect);
