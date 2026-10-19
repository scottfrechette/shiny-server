import fs from "node:fs";
import path from "node:path";

// Site lives in clt/web, data lives in clt/data. Astro runs builds from the
// project root, so process.cwd() is clt/web.
const DATA_ROOT = path.resolve(process.cwd(), "..", "data");

type Json = Record<string, any>;

function readJson<T = any>(rel: string): T | null {
  const file = path.join(DATA_ROOT, rel);
  if (!fs.existsSync(file)) return null;
  return JSON.parse(fs.readFileSync(file, "utf8")) as T;
}

export interface TeamRow {
  team: string;
  record: string;
  fvoa: number;
  fvoaRank: number;
  yahooRank: number;
  playoffPct: number | null; // fraction, 0.25 = 25%
  rankChange: number | null; // positive = moved up in FVOA rank vs last week
}

export interface Commentary {
  week: number;
  bullets: string[];
  commish_corner: string;
  awards?: { team: string; award: string; blurb: string }[];
}

export function loadHome() {
  const latest = readJson<{ season: number; week: number; path: string }>("latest.json");
  if (!latest) {
    throw new Error("clt/data/latest.json not found. Run git pull or the weekly workflow.");
  }

  const meta = readJson<Json>(`${latest.path}/meta.json`) ?? {};
  const rankings = readJson<Json[]>(`${latest.path}/rankings.json`) ?? [];
  const facts = readJson<{ teams: Json[] }>(`${latest.path}/facts.json`);
  const factsByTeam = new Map((facts?.teams ?? []).map((t) => [t.Team, t]));

  const rows: TeamRow[] = rankings
    .map((r) => {
      const f = factsByTeam.get(r.Team);
      return {
        team: String(r.Team),
        record: String(r.Record ?? ""),
        fvoa: Number(r.FVOA),
        fvoaRank: Number(r["FVOA Rank"]),
        yahooRank: Number(r["Yahoo Rank"]),
        playoffPct: f?.playoff_pct ?? null,
        rankChange: f?.rank_change ?? null,
      };
    })
    .sort((a, b) => a.fvoaRank - b.fvoaRank);

  // Use this week's commentary if it has been merged; otherwise fall back to
  // the most recent earlier week this season.
  let commentary: Commentary | null = null;
  for (let w = latest.week; w >= 1 && !commentary; w--) {
    const dir = `${latest.season}/week-${String(w).padStart(2, "0")}`;
    const c = readJson<Commentary>(`${dir}/commentary.json`);
    if (c) commentary = { ...c, week: w };
  }

  return { latest, generatedAt: meta.generated_at as string | undefined, rows, commentary };
}
