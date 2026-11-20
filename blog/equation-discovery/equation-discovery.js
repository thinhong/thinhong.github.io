// equation-discovery.js
//
// Small, dependency-free code behind the interactive figures in the post
// "Equation discovery" (SINDy and global modelling).
//
// The functions follow the R code from the talk
// (https://github.com/quingzz/equation_discovery_ppt) and the GPoM package
// (https://cran.r-project.org/package=GPoM). They are short teaching
// versions, not replacements for those tools. The data are noise-free.

// ---------------------------------------------------------------------------
// Small helpers
// ---------------------------------------------------------------------------

export function mean(a) {
  let s = 0;
  for (let i = 0; i < a.length; i++) s += a[i];
  return s / a.length;
}

export function sd(a) {
  const m = mean(a);
  let s = 0;
  for (let i = 0; i < a.length; i++) s += (a[i] - m) ** 2;
  return Math.sqrt(s / (a.length - 1));
}

export function extent(v) {
  let lo = Infinity, hi = -Infinity;
  for (let i = 0; i < v.length; i++) {
    if (v[i] < lo) lo = v[i];
    if (v[i] > hi) hi = v[i];
  }
  return [lo, hi];
}

export function logspace(from, to, n) {
  const a = Math.log10(from);
  const b = Math.log10(to);
  return Array.from({ length: n }, (_, i) => 10 ** (a + ((b - a) * i) / (n - 1)));
}

// Indices 0, k, 2k, ... so that at most maxPoints are kept (for plotting).
export function thin(n, maxPoints) {
  const k = Math.max(1, Math.ceil(n / maxPoints));
  const idx = [];
  for (let i = 0; i < n; i += k) idx.push(i);
  return idx;
}

// ---------------------------------------------------------------------------
// Polynomial terms
// ---------------------------------------------------------------------------
// A term is a list of exponents, one per variable. For (x, y, z),
// [1, 0, 1] is xz and [0, 2, 0] is y². Terms are ordered by degree:
// 1, x, y, z, x², xy, xz, y², yz, z², ...

const SUP = { 2: "²", 3: "³", 4: "⁴" };

export function polyTerms(nVar, maxDeg, includeConst = true) {
  const terms = [];
  const collect = (prefix, remainingVars, degLeft) => {
    if (remainingVars === 1) {
      terms.push([...prefix, degLeft]);
      return;
    }
    for (let d = degLeft; d >= 0; d--) collect([...prefix, d], remainingVars - 1, degLeft - d);
  };
  for (let deg = includeConst ? 0 : 1; deg <= maxDeg; deg++) collect([], nVar, deg);
  return terms;
}

export function termLabel(powers, names = ["x", "y", "z"]) {
  let s = "";
  powers.forEach((p, i) => {
    if (p === 1) s += names[i];
    else if (p > 1) s += names[i] + SUP[p];
  });
  return s === "" ? "1" : s;
}

export function termTex(powers, names = ["x", "y", "z"]) {
  let s = "";
  powers.forEach((p, i) => {
    if (p === 1) s += names[i];
    else if (p > 1) s += `${names[i]}^{${p}}`;
  });
  return s === "" ? "1" : s;
}

export function termDegree(powers) {
  return powers.reduce((a, b) => a + b, 0);
}

// ---------------------------------------------------------------------------
// Simulation: classic fourth-order Runge-Kutta
// ---------------------------------------------------------------------------

export const LORENZ = { sigma: 10, rho: 28, beta: 8 / 3 };

export function lorenzRHS(p = LORENZ) {
  return (s, out) => {
    out[0] = p.sigma * (s[1] - s[0]);
    out[1] = s[0] * (p.rho - s[2]) - s[1];
    out[2] = s[0] * s[1] - p.beta * s[2];
  };
}

// f(state, out) writes the derivatives into out. Integrates from x0 with
// step h and saves every `saveEvery` steps. Stops early if a value becomes
// non-finite or larger than maxAbs.
export function rk4(f, x0, h, nSteps, { saveEvery = 1, maxAbs = 1e6 } = {}) {
  const n = x0.length;
  const nSave = Math.floor(nSteps / saveEvery) + 1;
  const t = new Float64Array(nSave);
  const X = Array.from({ length: n }, () => new Float64Array(nSave).fill(NaN));
  const s = Float64Array.from(x0);
  const k1 = new Float64Array(n), k2 = new Float64Array(n), k3 = new Float64Array(n), k4 = new Float64Array(n);
  const tmp = new Float64Array(n);
  for (let j = 0; j < n; j++) X[j][0] = s[j];
  let saved = 1;
  let status = "ok";
  for (let step = 1; step <= nSteps; step++) {
    f(s, k1);
    for (let j = 0; j < n; j++) tmp[j] = s[j] + 0.5 * h * k1[j];
    f(tmp, k2);
    for (let j = 0; j < n; j++) tmp[j] = s[j] + 0.5 * h * k2[j];
    f(tmp, k3);
    for (let j = 0; j < n; j++) tmp[j] = s[j] + h * k3[j];
    f(tmp, k4);
    let bad = false;
    for (let j = 0; j < n; j++) {
      s[j] += (h / 6) * (k1[j] + 2 * k2[j] + 2 * k3[j] + k4[j]);
      if (!Number.isFinite(s[j]) || Math.abs(s[j]) > maxAbs) bad = true;
    }
    if (bad) {
      status = "blew up";
      break;
    }
    if (step % saveEvery === 0) {
      t[saved] = step * h;
      for (let j = 0; j < n; j++) X[j][saved] = s[j];
      saved++;
    }
  }
  for (let i = saved; i < nSave; i++) t[i] = i * saveEvery * h;
  return { t, X, status, nValid: saved };
}

// The Lorenz system, sampled every dt time units.
export function simulateLorenz({ x0 = [10, 10, 10], dt = 0.01, tEnd = 100, substeps = 10, params = LORENZ } = {}) {
  const nOut = Math.round(tEnd / dt);
  const out = rk4(lorenzRHS(params), x0, dt / substeps, nOut * substeps, { saveEvery: substeps });
  return { t: out.t, X: out.X };
}

// ---------------------------------------------------------------------------
// Small dense linear algebra
// ---------------------------------------------------------------------------

// Solve A z = r for a small symmetric positive definite A (Cholesky), with
// Gaussian elimination as a fallback.
export function solveSmall(A, r) {
  const n = r.length;
  const L = Array.from({ length: n }, () => new Float64Array(n));
  let ok = true;
  for (let i = 0; i < n && ok; i++) {
    for (let j = 0; j <= i; j++) {
      let s = A[i][j];
      for (let k = 0; k < j; k++) s -= L[i][k] * L[j][k];
      if (i === j) {
        if (s <= 1e-14 * Math.abs(A[i][i])) {
          ok = false;
          break;
        }
        L[i][i] = Math.sqrt(s);
      } else {
        L[i][j] = s / L[j][j];
      }
    }
  }
  if (ok) {
    const y = new Float64Array(n);
    for (let i = 0; i < n; i++) {
      let s = r[i];
      for (let k = 0; k < i; k++) s -= L[i][k] * y[k];
      y[i] = s / L[i][i];
    }
    const z = new Float64Array(n);
    for (let i = n - 1; i >= 0; i--) {
      let s = y[i];
      for (let k = i + 1; k < n; k++) s -= L[k][i] * z[k];
      z[i] = s / L[i][i];
    }
    return z;
  }
  const M = A.map((row, i) => [...row, r[i]]);
  for (let i = 0; i < n; i++) M[i][i] += 1e-12 * Math.abs(M[i][i]) + 1e-300;
  for (let c = 0; c < n; c++) {
    let piv = c;
    for (let i = c + 1; i < n; i++) if (Math.abs(M[i][c]) > Math.abs(M[piv][c])) piv = i;
    [M[c], M[piv]] = [M[piv], M[c]];
    for (let i = c + 1; i < n; i++) {
      const f = M[i][c] / M[c][c];
      for (let k = c; k <= n; k++) M[i][k] -= f * M[c][k];
    }
  }
  const z = new Float64Array(n);
  for (let i = n - 1; i >= 0; i--) {
    let s = M[i][n];
    for (let k = i + 1; k < n; k++) s -= M[i][k] * z[k];
    z[i] = s / M[i][i];
  }
  return z;
}

// ---------------------------------------------------------------------------
// Derivatives with a Savitzky-Golay filter (as in GPoM::drvSucc)
// ---------------------------------------------------------------------------
// Fit a polynomial of degree `order` to each window of `win` points (win odd)
// by least squares. The fitted value at the window centre is the smoothed
// value, and the slope there is the derivative. The (win - 1) / 2 points at
// each end are dropped.

export function savitzkyGolayWeights(win, order, dt) {
  const m = (win - 1) / 2;
  const q = order + 1;
  const AtA = Array.from({ length: q }, () => new Float64Array(q));
  for (let k = -m; k <= m; k++)
    for (let i = 0; i < q; i++) for (let j = 0; j < q; j++) AtA[i][j] += k ** (i + j);
  const smooth = new Float64Array(win);
  const deriv = new Float64Array(win);
  for (let k = -m; k <= m; k++) {
    const a = Array.from({ length: q }, (_, j) => k ** j);
    const z = solveSmall(AtA, a);
    smooth[k + m] = z[0];
    deriv[k + m] = q > 1 ? z[1] / dt : 0;
  }
  return { smooth, deriv, m };
}

export function savitzkyGolay(v, dt, win = 9, order = 4) {
  const { smooth, deriv, m } = savitzkyGolayWeights(win, order, dt);
  const n = v.length - 2 * m;
  const s = new Float64Array(Math.max(n, 0));
  const d = new Float64Array(Math.max(n, 0));
  for (let i = 0; i < n; i++) {
    let a = 0, b = 0;
    for (let k = 0; k < win; k++) {
      a += smooth[k] * v[i + k];
      b += deriv[k] * v[i + k];
    }
    s[i] = a;
    d[i] = b;
  }
  return { smooth: s, deriv: d, offset: m };
}

// Smoothed states X and their derivatives dX.
export function prepareData(t, Xobs, { win = 9, order = 4 } = {}) {
  const dt = t[1] - t[0];
  const res = Xobs.map((v) => savitzkyGolay(v, dt, win, order));
  const m = res[0].offset;
  return { t: t.slice(m, t.length - m), X: res.map((r) => r.smooth), dX: res.map((r) => r.deriv), offset: m };
}

// ---------------------------------------------------------------------------
// Library of candidate terms, and least squares through the Gram matrix
// ---------------------------------------------------------------------------

// One column per term: the term evaluated at every time point.
export function evalTerms(terms, X) {
  const n = X[0].length;
  return terms.map((pw) => {
    const col = new Float64Array(n).fill(1);
    pw.forEach((p, j) => {
      if (p === 0) return;
      const v = X[j];
      for (let i = 0; i < n; i++) col[i] *= v[i] ** p;
    });
    return col;
  });
}

// G = Theta' Theta, and for each response y: b = Theta' y and yy = y' y.
export function gramSystem(cols, responses) {
  const p = cols.length;
  const n = cols[0].length;
  const G = Array.from({ length: p }, () => new Float64Array(p));
  for (let i = 0; i < p; i++) {
    for (let j = i; j < p; j++) {
      let s = 0;
      for (let k = 0; k < n; k++) s += cols[i][k] * cols[j][k];
      G[i][j] = s;
      G[j][i] = s;
    }
  }
  const eqs = responses.map((y) => {
    const b = new Float64Array(p);
    for (let i = 0; i < p; i++) {
      let s = 0;
      for (let k = 0; k < n; k++) s += cols[i][k] * y[k];
      b[i] = s;
    }
    let yy = 0;
    for (let k = 0; k < n; k++) yy += y[k] * y[k];
    return { b, yy };
  });
  return { G, eqs, n, p };
}

// Least squares on the columns in `active`. Returns a full-length coefficient
// vector with zeros for the other columns.
export function lsSubset(G, b, active) {
  const p = b.length;
  const coef = new Float64Array(p);
  if (active.length === 0) return coef;
  const d = active.map((i) => Math.sqrt(G[i][i]) || 1);
  const A = active.map((i, a) => Float64Array.from(active, (j, c) => G[i][j] / (d[a] * d[c])));
  const r = Float64Array.from(active, (i, a) => b[i] / d[a]);
  const z = solveSmall(A, r);
  active.forEach((i, a) => (coef[i] = z[a] / d[a]));
  return coef;
}

// Residual sum of squares ||y - Theta k||^2 = y'y - 2 k'b + k'G k
export function rss(G, b, yy, coef) {
  const p = b.length;
  let s = yy;
  for (let i = 0; i < p; i++) {
    if (coef[i] === 0) continue;
    s -= 2 * coef[i] * b[i];
    let g = 0;
    for (let j = 0; j < p; j++) if (coef[j] !== 0) g += G[i][j] * coef[j];
    s += coef[i] * g;
  }
  return Math.max(s, 0);
}

// ---------------------------------------------------------------------------
// SINDy: sequentially thresholded least squares (STLSQ)
// ---------------------------------------------------------------------------
// Same logic as sindy_alg() in the talk's R code and Code 1 in Brunton et al.
// (2016): fit least squares on the active terms, set the coefficients with
// |coefficient| < lambda to zero, refit on the remaining terms, and repeat
// until no coefficient is below lambda.
export function stlsq(G, b, yy, lambda, { maxIter = 20 } = {}) {
  const p = b.length;
  let act = Array.from({ length: p }, (_, i) => i);
  const history = [];
  for (let it = 0; it < maxIter; it++) {
    const coef = lsSubset(G, b, act);
    const below = act.filter((i) => Math.abs(coef[i]) < lambda);
    history.push({ active: act.slice(), coef, below });
    if (below.length === 0) break;
    act = act.filter((i) => Math.abs(coef[i]) >= lambda);
    if (act.length === 0) {
      history.push({ active: [], coef: new Float64Array(p), below: [] });
      break;
    }
  }
  const last = history[history.length - 1];
  return { coef: last.coef, active: last.active, history };
}

// The same run, cut into small steps for the step-by-step figure:
// "fit", "compare" (with lambda), "zero" (set the small ones to zero),
// then "fit" again, and so on. The last step is "stop" (nothing below
// lambda) or "empty" (every term was removed).
export function stlsqSteps(history) {
  const steps = [];
  history.forEach((h, k) => {
    if (h.active.length === 0) {
      steps.push({ kind: "empty", pass: k, active: [], coef: h.coef, below: [], prev: null });
      return;
    }
    const prev = k > 0 ? history[k - 1].coef : null;
    steps.push({ kind: "fit", pass: k, active: h.active, coef: h.coef, below: [], prev });
    if (h.below.length > 0) {
      steps.push({ kind: "compare", pass: k, active: h.active, coef: h.coef, below: h.below, prev: null });
      const kept = h.active.filter((i) => !h.below.includes(i));
      const coef = Float64Array.from(h.coef);
      h.below.forEach((i) => (coef[i] = 0));
      steps.push({ kind: "zero", pass: k, active: kept, coef, below: h.below, prev: null });
    } else {
      steps.push({ kind: "stop", pass: k, active: h.active, coef: h.coef, below: [], prev: null });
    }
  });
  return steps;
}

// ---------------------------------------------------------------------------
// GPoM: remove terms one at a time (as in autoGPoMoSearch)
// ---------------------------------------------------------------------------
// Start from the full polynomial. For each remaining term, set its
// coefficient to zero (no refit) and compute the residual. Remove the term
// whose removal increases the residual the least, refit, and repeat until one
// term is left. Every intermediate model is kept, so each equation gets a
// sequence of nested models (called a "ladder" in the code).
// Each step also stores `scores`: the relative increase of the residual
// (residual without the term / residual - 1) for every term in that model.
export function backwardElimination(G, b, yy) {
  const p = b.length;
  let act = Array.from({ length: p }, (_, i) => i);
  let coef = lsSubset(G, b, act);
  let r = rss(G, b, yy, coef);
  const ladder = [];
  for (;;) {
    const scores = new Map();
    let best = -1, bestR = Infinity;
    for (const i of act) {
      const k = Float64Array.from(coef);
      k[i] = 0;
      const ri = rss(G, b, yy, k);
      scores.set(i, ri / Math.max(r, 1e-300) - 1);
      if (ri < bestR) {
        bestR = ri;
        best = i;
      }
    }
    const step = { active: act.slice(), coef, rss: r, relErr: Math.sqrt(r / yy), scores, next: act.length > 1 ? best : null };
    ladder.push(step);
    if (act.length === 1) break;
    act = act.filter((i) => i !== best);
    coef = lsSubset(G, b, act);
    r = rss(G, b, yy, coef);
  }
  return ladder;
}

// All combinations of one model per equation (one step from each sequence)
// with a total number of terms in [nPmin, nPmax], in the same order as
// GPoM::findAllSets followed by a stable sort on the total number of terms.
// So "model 5" here is built the same way as "model 5" in gPoMo().
export function combineLadders(ladders, nPmin, nPmax) {
  const sizes = ladders.map((l) => l.map((s) => s.active.length));
  let sets = sizes[0].map((_, j) => [j]);
  for (let e = 1; e < ladders.length; e++) {
    const next = [];
    for (let j = 0; j < sizes[e].length; j++) for (const s of sets) next.push([...s, j]);
    sets = next;
  }
  const withN = sets
    .map((s) => ({ steps: s, nTerms: s.reduce((a, j, e) => a + sizes[e][j], 0) }))
    .filter((m) => m.nTerms >= nPmin && m.nTerms <= nPmax);
  const order = withN.map((m, i) => i).sort((a, b) => withN[a].nTerms - withN[b].nTerms || a - b);
  return order.map((i, k) => ({ id: k + 1, ...withN[i], perEq: withN[i].steps.map((j, e) => sizes[e][j]) }));
}

// ---------------------------------------------------------------------------
// Polynomial models: simulate and look at the behaviour
// ---------------------------------------------------------------------------

// coefs[e] is the coefficient vector of equation e over `terms`.
export function polyRHS(terms, coefs) {
  const nVar = terms[0].length;
  const maxDeg = Math.max(...terms.map(termDegree));
  const eqs = coefs.map((k) => {
    const idx = [], val = [];
    k.forEach((c, i) => {
      if (c !== 0) {
        idx.push(i);
        val.push(c);
      }
    });
    return { idx: Int32Array.from(idx), val: Float64Array.from(val) };
  });
  const used = Int32Array.from([...new Set(eqs.flatMap((q) => Array.from(q.idx)))]);
  const pw = Array.from({ length: nVar }, () => new Float64Array(maxDeg + 1));
  const tv = new Float64Array(terms.length);
  return (s, out) => {
    for (let j = 0; j < nVar; j++) {
      pw[j][0] = 1;
      for (let d = 1; d <= maxDeg; d++) pw[j][d] = pw[j][d - 1] * s[j];
    }
    for (let u = 0; u < used.length; u++) {
      const i = used[u];
      let v = 1;
      for (let j = 0; j < nVar; j++) if (terms[i][j] > 0) v *= pw[j][terms[i][j]];
      tv[i] = v;
    }
    for (let e = 0; e < eqs.length; e++) {
      const { idx, val } = eqs[e];
      let acc = 0;
      for (let q = 0; q < idx.length; q++) acc += val[q] * tv[idx[q]];
      out[e] = acc;
    }
  };
}

export function simulatePoly(terms, coefs, x0, { h = 0.01, tEnd = 100, saveEvery = 1, maxAbs = 1e4 } = {}) {
  return rk4(polyRHS(terms, coefs), x0, h, Math.round(tEnd / h), { saveEvery, maxAbs });
}

export function dataStats(X) {
  return X.map((v) => {
    const [min, max] = extent(v);
    return { mean: mean(v), sd: sd(v), min, max };
  });
}

// Long-run behaviour of a simulated run, with simple rules. GPoM's own test
// (autoGPoMoTest) labels models 0 (diverging, too far, or fixed point),
// -1 (period-1 cycle) or 1 (unclassified). These labels are a simpler
// teaching version of the same idea.
export function classifyRun(run, stats, { tooFar = 4 } = {}) {
  const n = run.nValid;
  if (run.status !== "ok" || n < run.t.length) return "diverges";
  const nVar = run.X.length;
  for (let j = 0; j < nVar; j++) {
    if (Math.abs(run.X[j][n - 1] - stats[j].mean) / stats[j].sd > tooFar) return "diverges";
  }
  // last 20% of the run: does the state still move?
  let moves = false;
  for (let j = 0; j < nVar; j++) {
    const [lo, hi] = extent(run.X[j].subarray(Math.floor(n * 0.8), n));
    if ((hi - lo) / stats[j].sd > 1e-2) moves = true;
  }
  if (!moves) return "fixed point";
  // last 40%: are the peaks of the most variable state all equal?
  let jMax = 0, best = -1;
  for (let j = 0; j < nVar; j++) {
    const s = sd(run.X[j].subarray(Math.floor(n * 0.6), n)) / stats[j].sd;
    if (s > best) {
      best = s;
      jMax = j;
    }
  }
  const v = run.X[jMax];
  const peaks = [];
  for (let i = Math.floor(n * 0.6) + 1; i < n - 1; i++) if (v[i] > v[i - 1] && v[i] >= v[i + 1]) peaks.push(v[i]);
  if (peaks.length >= 3 && (Math.max(...peaks) - Math.min(...peaks)) / stats[jMax].sd < 0.02) return "cycle";
  return "irregular";
}

// Overlap between where the model and the data spend their time in the
// (a, b) plane, from 0 (no overlap) to 1 (same picture). The plane is cut
// into bins x bins cells; the score adds up, over cells, the smaller of the
// two shares of time. The first 20% of each run is skipped. This score is
// not part of GPoM, which leaves the last judgement to the user.
export function makeOverlapScorer(dataX, a = 0, b = 2, { bins = 24, from = 0.2 } = {}) {
  const [lo0, hi0] = extent(dataX[a]);
  const [lo1, hi1] = extent(dataX[b]);
  const hist = (X, n) => {
    const H = new Float64Array(bins * bins);
    let count = 0;
    for (let i = Math.floor(n * from); i < n; i++) {
      const u = Math.floor(((X[a][i] - lo0) / (hi0 - lo0)) * bins);
      const w = Math.floor(((X[b][i] - lo1) / (hi1 - lo1)) * bins);
      if (!Number.isFinite(u) || !Number.isFinite(w)) continue;
      count++;
      if (u < 0 || u >= bins || w < 0 || w >= bins) continue;
      H[u * bins + w]++;
    }
    if (count > 0) for (let k = 0; k < H.length; k++) H[k] /= count;
    return H;
  };
  const P = hist(dataX, dataX[a].length);
  return (runX, nValid = runX[a].length) => {
    const Q = hist(runX, nValid);
    let s = 0;
    for (let k = 0; k < P.length; k++) s += Math.min(P[k], Q[k]);
    return s;
  };
}

export const OUTCOMES = [
  "reproduces the attractor",
  "irregular, elsewhere",
  "settles to a cycle",
  "settles to a point",
  "leaves the data range",
];

export function outcomeLabel(cls, score, cut = 0.5) {
  if (cls === "diverges") return OUTCOMES[4];
  if (cls === "fixed point") return OUTCOMES[3];
  if (cls === "cycle") return OUTCOMES[2];
  return score >= cut ? OUTCOMES[0] : OUTCOMES[1];
}

// Simulate one candidate from the first data point and describe it.
export function runCandidate(terms, coefs, dataX, { h = 0.02, tEnd = 100, keep = 1500, stats = null, scorer = null } = {}) {
  const st = stats || dataStats(dataX);
  const sc = scorer || makeOverlapScorer(dataX, 0, 2);
  const run = simulatePoly(terms, coefs, dataX.map((v) => v[0]), { h, tEnd });
  const cls = classifyRun(run, st);
  const score = cls === "diverges" ? 0 : sc(run.X, run.nValid);
  const idx = thin(run.nValid, keep);
  return {
    cls,
    score,
    outcome: outcomeLabel(cls, score),
    path: idx.map((i) => ({ x: run.X[0][i], y: run.X[1][i], z: run.X[2][i] })),
    run,
  };
}

// ---------------------------------------------------------------------------
// Printing
// ---------------------------------------------------------------------------

// TeX for one equation, for example "\frac{dx}{dt} = -10\,x + 10\,y"
export function equationTex(lhs, terms, coef, { digits = 3, names = ["x", "y", "z"] } = {}) {
  const parts = [];
  coef.forEach((c, i) => {
    if (c === 0) return;
    const a = Math.abs(c);
    let num;
    if (a >= 1000 || a < 1e-3) {
      const [m, e] = a.toExponential(1).split("e");
      num = `${m}\\times 10^{${Number(e)}}`;
    } else {
      num = Number(a.toPrecision(digits)).toString();
    }
    const t = termTex(terms[i], names);
    const body = t === "1" ? num : `${num}\\,${t}`;
    parts.push({ sign: c < 0 ? "-" : "+", body });
  });
  if (parts.length === 0) return `\\frac{d${lhs}}{dt} = 0`;
  let s = `\\frac{d${lhs}}{dt} = ${parts[0].sign === "-" ? "-" : ""}${parts[0].body}`;
  for (const q of parts.slice(1)) s += ` ${q.sign} ${q.body}`;
  return s;
}

// The true Lorenz coefficients written over a list of terms.
export function lorenzTrueCoefs(terms, p = LORENZ) {
  const find = (pw) => terms.findIndex((t) => t.join() === pw.join());
  const k = [new Float64Array(terms.length), new Float64Array(terms.length), new Float64Array(terms.length)];
  const set = (e, pw, v) => {
    const i = find(pw);
    if (i >= 0) k[e][i] = v;
  };
  set(0, [1, 0, 0], -p.sigma);
  set(0, [0, 1, 0], p.sigma);
  set(1, [1, 0, 0], p.rho);
  set(1, [0, 1, 0], -1);
  set(1, [1, 0, 1], -1);
  set(2, [0, 0, 1], -p.beta);
  set(2, [1, 1, 0], 1);
  return k;
}
