// "PFinn" -> "pfinn". Used for /team/{slug} URLs.
export const slug = (name: string) =>
  name
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/(^-|-$)/g, "");
