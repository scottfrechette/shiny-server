import { league } from "../lib/league";

// Built into /manifest.webmanifest. Replaces public/manifest.webmanifest, which
// must be deleted so the two don't collide.
export function GET() {
  return new Response(
    JSON.stringify({
      name: league.league_name,
      short_name: league.short_name,
      start_url: "/",
      display: "standalone",
      background_color: "#020617",
      theme_color: "#020617",
    }),
    { headers: { "Content-Type": "application/manifest+json" } },
  );
}
