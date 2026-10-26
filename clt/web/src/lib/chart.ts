export interface Point {
  week: number;
  value: number;
}

export interface ChartOpts {
  yMin?: number; // fix the bottom of the axis (e.g. 0 for probabilities)
  yMax?: number; // fix the top of the axis (e.g. 1)
  ticks?: number; // roughly how many gridlines
  fmt: (v: number) => string;
}

// Round gridline values to 1, 2, or 5 times a power of ten
function niceTicks(lo: number, hi: number, count: number): number[] {
  const raw = (hi - lo) / count;
  const mag = 10 ** Math.floor(Math.log10(raw));
  const step = [1, 2, 5, 10].map((m) => m * mag).find((s) => s >= raw)!;
  const out: number[] = [];
  for (let v = Math.ceil(lo / step) * step; v <= hi + 1e-9; v += step) {
    out.push(Number(v.toFixed(10)));
  }
  return out;
}

// Builds everything an <svg> needs: the selected team as a highlighted line,
// every other team as a faint line for context.
export function buildChart(all: Record<string, Point[]>, team: string, opts: ChartOpts) {
  const W = 320;
  const H = 150;
  const padL = 34;
  const padR = 12;
  const padT = 10;
  const padB = 20;

  const allPoints = Object.values(all).flat();
  const weeks = [...new Set(allPoints.map((p) => p.week))].sort((a, b) => a - b);
  const minW = weeks[0];
  const maxW = weeks[weeks.length - 1];

  const values = allPoints.map((p) => p.value);
  let lo = opts.yMin ?? Math.min(...values);
  let hi = opts.yMax ?? Math.max(...values);
  const pad = (hi - lo) * 0.1 || 1;
  if (opts.yMin === undefined) lo -= pad;
  if (opts.yMax === undefined) hi += pad;

  const x = (w: number) =>
    weeks.length === 1
      ? padL + (W - padL - padR) / 2
      : padL + ((w - minW) / (maxW - minW)) * (W - padL - padR);
  const y = (v: number) => padT + (1 - (v - lo) / (hi - lo)) * (H - padT - padB);

  const path = (pts: Point[]) =>
    pts.map((p, i) => `${i ? "L" : "M"}${x(p.week).toFixed(1)},${y(p.value).toFixed(1)}`).join(" ");

  const mine = (all[team] ?? []).slice().sort((a, b) => a.week - b.week);
  const points = mine.map((p) => ({ ...p, cx: x(p.week), cy: y(p.value) }));
  const last = points[points.length - 1];

  return {
    W,
    H,
    padL,
    padR,
    grid: niceTicks(lo, hi, opts.ticks ?? 3).map((v) => ({ y: y(v), label: opts.fmt(v) })),
    weekLabels: weeks.map((w) => ({ x: x(w), label: String(w) })),
    others: Object.entries(all)
      .filter(([t]) => t !== team)
      .map(([, pts]) => path(pts.slice().sort((a, b) => a.week - b.week))),
    mine: path(mine),
    points,
    last,
  };
}
