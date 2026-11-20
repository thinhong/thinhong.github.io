// viz.js: the figure kit for interactive posts (Observable JS cells).
//
// Colours come from the theme: _theme/theme.scss defines every figure colour
// as a --viz-* CSS variable, in light and dark mode, and tokens() reads them.
// A figure never hard-codes a colour. The builders below make the card,
// buttons, segmented controls and legends that the theme styles (.viz-*).
//
// Use it from an OJS cell in a post folder (blog/<post>/index.qmd):
//   import { tokens, watchTheme, timeColor, timeGradient, motion, makeCard, button, segmented, legend, watchWidth } from "../../js/viz.js"
//
// Read the colours in a cell of their own, so every figure redraws when the
// reader switches between light and dark:
//   VIZ = Generators.observe((notify) => watchTheme(() => notify(tokens())))

export function tokens(el = document.documentElement) {
  const css = getComputedStyle(el);
  const v = (name) => css.getPropertyValue(`--viz-${name}`).trim();
  return {
    bg: v("bg"),
    ink: v("ink"),
    text: v("text"),
    muted: v("muted"),
    grey: v("grey"),
    faint: v("faint"),
    rule: v("rule"),
    wash: v("wash"),
    main: v("main"),
    ghost: v("ghost"),
    now: v("now"),
    attn: v("ghost"),
    // categorical colours for groups, always used in this order
    cat: [1, 2, 3, 4, 5].map((i) => v(`c${i}`)),
    // time: five steps of one blue, early to late
    time: [1, 2, 3, 4, 5].map((i) => v(`t${i}`)),
  };
}

// Calls back now and every time the page switches between light and dark.
// Returns a function that stops watching (Generators.observe calls it).
export function watchTheme(callback) {
  callback();
  const mo = new MutationObserver(() => callback());
  mo.observe(document.documentElement, { attributes: true, attributeFilter: ["data-theme"] });
  return () => mo.disconnect();
}

const hexToRgb = (h) => {
  const n = parseInt(h.replace("#", ""), 16);
  return [(n >> 16) & 255, (n >> 8) & 255, n & 255];
};

// The colour at position u (0 to 1) along the time steps, for example
// timeColor(t / T, VIZ.time) for a moment t in a run of length T.
export function timeColor(u, steps) {
  const x = Math.max(0, Math.min(1, u)) * (steps.length - 1);
  const i = Math.min(steps.length - 2, Math.floor(x));
  const f = x - i;
  const a = hexToRgb(steps[i]);
  const b = hexToRgb(steps[i + 1]);
  return `rgb(${a.map((c, k) => Math.round(c + (b[k] - c) * f)).join(",")})`;
}

// A horizontal gradient through the time steps, from x1 to x2 (in the svg's
// own units), added to svgNode. Returns "url(#id)" for a stroke or fill. Use
// it when time runs along the x axis: the line's colour then shows its time.
let gradientCount = 0;
export function timeGradient(svgNode, x1, x2, steps) {
  const ns = "http://www.w3.org/2000/svg";
  const id = `viz-time-${++gradientCount}`;
  let defs = svgNode.querySelector("defs");
  if (!defs) defs = svgNode.insertBefore(document.createElementNS(ns, "defs"), svgNode.firstChild);
  const g = document.createElementNS(ns, "linearGradient");
  g.setAttribute("id", id);
  g.setAttribute("gradientUnits", "userSpaceOnUse");
  g.setAttribute("x1", x1);
  g.setAttribute("x2", x2);
  g.setAttribute("y1", 0);
  g.setAttribute("y2", 0);
  steps.forEach((c, i) => {
    const s = document.createElementNS(ns, "stop");
    s.setAttribute("offset", `${(i / (steps.length - 1)) * 100}%`);
    s.setAttribute("stop-color", c);
    g.append(s);
  });
  defs.append(g);
  return `url(#${id})`;
}

// Animation length in ms, or 0 when the reader asks for reduced motion.
export const reducedMotion =
  typeof matchMedia === "function" && matchMedia("(prefers-reduced-motion: reduce)").matches;
export function motion(ms) {
  return reducedMotion ? 0 : ms;
}

function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text != null) node.textContent = text;
  return node;
}

// A figure card: title, one-line hint, a toolbar row and a body.
export function makeCard(title, hint) {
  const root = el("div", "viz-card");
  const toolbar = el("div", "viz-toolbar");
  const body = el("div", "viz-body");
  root.append(el("div", "viz-card-title", title), el("div", "viz-card-hint", hint), toolbar, body);
  return { root, toolbar, body };
}

export function button(label, onClick) {
  const b = el("button", "viz-btn", label);
  b.type = "button";
  b.onclick = onClick;
  return b;
}

// A row of options where exactly one is on. The returned element has
// .set(value) to change the choice without firing onChange.
export function segmented(options, value, onChange, label = null) {
  const seg = el("div", "viz-seg");
  seg.setAttribute("role", "group");
  if (label) seg.setAttribute("aria-label", label);
  const buttons = options.map((o) => {
    const b = el("button", null, String(o));
    b.type = "button";
    b.onclick = () => {
      if (o !== value) {
        set(o);
        onChange(o);
      }
    };
    seg.append(b);
    return b;
  });
  function set(v) {
    value = v;
    buttons.forEach((b, i) => {
      const on = options[i] === v;
      b.classList.toggle("on", on);
      b.setAttribute("aria-pressed", String(on));
    });
  }
  set(value);
  const wrap = el("div", "viz-field");
  if (label) wrap.append(el("span", "viz-field-label", label));
  wrap.append(seg);
  wrap.set = set;
  return wrap;
}

// items: [[label, colour], ...]
export function legend(items) {
  const wrap = el("div", "viz-legend");
  for (const [label, color] of items) {
    const s = el("span", null, label);
    s.style.setProperty("--c", color);
    wrap.append(s);
  }
  return wrap;
}

// Call back with the element's width whenever it changes by more than 1px.
// Pass OJS's `invalidation` promise so the observer stops when the cell re-runs.
export function watchWidth(node, callback, invalidation) {
  let last = -1;
  const ro = new ResizeObserver((entries) => {
    const w = Math.floor(entries[0].contentRect.width);
    if (w > 0 && Math.abs(w - last) > 1) {
      last = w;
      callback(w);
    }
  });
  ro.observe(node);
  if (invalidation) invalidation.then(() => ro.disconnect());
  return ro;
}
