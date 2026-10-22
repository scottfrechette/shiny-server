import fs from "node:fs";
import path from "node:path";

// Site lives in clt/web, data lives in clt/data (build runs from clt/web)
const DATA_ROOT = path.resolve(process.cwd(), "..", "data");

function read<T>(file: string): T {
  if (!fs.existsSync(file)) {
    throw new Error(`Missing ${file}. Run git pull or the weekly pipeline.`);
  }
  return JSON.parse(fs.readFileSync(file, "utf8")) as T;
}

export function latestWeek() {
  return read<{ season: number; week: number; path: string }>(path.join(DATA_ROOT, "latest.json"));
}

// Load a JSON file from the latest week's snapshot folder
export function loadWeekFile<T = any>(name: string): T {
  return read<T>(path.join(DATA_ROOT, latestWeek().path, `${name}.json`));
}

// 0.256 -> "26%"; never shows a false 0% or 100%
export function pct(p: number | null | undefined): string {
  if (p === null || p === undefined) return "–";
  if (p < 0.005) return "<1%";
  if (p > 0.995) return ">99%";
  return `${Math.round(p * 100)}%`;
}

// 1 -> "1st", 22 -> "22nd"
export function ord(n: number): string {
  const s = ["th", "st", "nd", "rd"];
  const v = n % 100;
  return n + (s[(v - 20) % 10] || s[v] || s[0]);
}

export const signed = (v: number) => `${v >= 0 ? "+" : ""}${v.toFixed(2)}`;
export const int = (v: number) => Math.round(v).toLocaleString("en-US");

// Like loadWeekFile, but returns null when the file doesn't exist (for example
// when this week's commentary PR hasn't been merged yet)
export function tryLoadWeekFile<T = any>(name: string): T | null {
  const file = path.join(DATA_ROOT, latestWeek().path, `${name}.json`);
  return fs.existsSync(file) ? (JSON.parse(fs.readFileSync(file, "utf8")) as T) : null;
}
