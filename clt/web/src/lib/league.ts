import fs from "node:fs";
import path from "node:path";

// Shared with the R pipeline. Site lives in clt/web, settings in clt/league.json
// (the build runs from clt/web).
const file = path.resolve(process.cwd(), "..", "league.json");

export const league = JSON.parse(fs.readFileSync(file, "utf8")) as {
  league_name: string;
  short_name: string;
  takes_title: string;
  timezone: string;
  num_teams: number;
  playoff_spots: number;
};
