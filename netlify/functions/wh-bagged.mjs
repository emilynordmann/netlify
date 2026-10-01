// Netlify function: read a Walkhighlands member's public Munro and Corbett maps
// and return the names of the hills they have ticked as climbed.
//
//   GET /.netlify/functions/wh-bagged?u=237287
//   -> { user, munros: [...names], corbetts: [...names], counts: {...} }
//
// The member's "hills climbed" must be set to public on Walkhighlands.
// The browser cannot read walkhighlands.co.uk directly (no CORS), hence this.

const UA = "emilynordmann.com munro-picker (contact: emilynordmann.com/contact)";

const ENT = { amp: "&", lt: "<", gt: ">", quot: '"', apos: "'", nbsp: " " };
function decode(s) {
  return s
    .replace(/&#(\d+);/g, (_, n) => String.fromCodePoint(+n))
    .replace(/&#x([0-9a-f]+);/gi, (_, n) => String.fromCodePoint(parseInt(n, 16)))
    .replace(/&([a-z]+);/gi, (m, k) => ENT[k.toLowerCase()] ?? m);
}

// One <tr> per hill; climbed rows carry images/tick.GIF, unclimbed box.GIF.
// The hill name is the link into /munros/ or /corbetts/.
export function parseMap(html, kind) {
  const linkRe = new RegExp(`<a[^>]*href="[^"]*/${kind}/[^"]*"[^>]*>([\\s\\S]*?)</a>`, "i");
  const out = [];
  for (const row of html.match(/<tr[\s\S]*?<\/tr>/gi) || []) {
    if (!/tick\.gif/i.test(row)) continue;
    const m = row.match(linkRe);
    if (!m) continue;
    const name = decode(m[1].replace(/<[^>]+>/g, "")).replace(/\s+/g, " ").trim();
    if (name) out.push(name);
  }
  return [...new Set(out)];
}

export function parseMeta(html) {
  const t = html.match(/<title>([^<]*)<\/title>/i);
  const user = t ? decode(t[1]).split(/ - /).pop().replace(/•.*$/, "").trim() : null;
  const c = html.match(/climbed\s+(\d+)\s+out of\s+(\d+)\s+(Munros|Corbetts)/i);
  return { user, climbed: c ? +c[1] : null, total: c ? +c[2] : null };
}

async function get(url) {
  const r = await fetch(url, { headers: { "user-agent": UA, accept: "text/html" } });
  if (!r.ok) throw new Error(`Walkhighlands returned ${r.status}`);
  return r.text();
}

const json = (body, status = 200, extra = {}) =>
  new Response(JSON.stringify(body), {
    status,
    headers: {
      "content-type": "application/json; charset=utf-8",
      "access-control-allow-origin": "*",
      ...extra,
    },
  });

export default async (req) => {
  // accept a bare id or a pasted map URL (whose own ?u= would split the query)
  const raw = new URL(req.url).searchParams.getAll("u").join(" ");
  const u = (raw.match(/(?:^|\bu=|\s)(\d+)\b/) || [, ""])[1];
  if (!u) return json({ error: "Missing Walkhighlands user id (u=...)" }, 400);

  try {
    const [mh, ch] = await Promise.all([
      get(`https://www.walkhighlands.co.uk/Forum/munros.php?u=${u}`),
      get(`https://www.walkhighlands.co.uk/Forum/corbetts.php?mode=map&u=${u}`),
    ]);
    const meta = parseMeta(mh), cmeta = parseMeta(ch);
    const munros = parseMap(mh, "munros");
    const corbetts = parseMap(ch, "corbetts");
    if (!meta.user && !munros.length && !corbetts.length)
      return json({ error: "No map found for that user id, or the member's hills are not public." }, 404);
    return json(
      { user: meta.user, munros, corbetts,
        counts: { munros: meta.climbed, munrosTotal: meta.total, corbetts: cmeta.climbed, corbettsTotal: cmeta.total } },
      200,
      { "cache-control": "public, max-age=600" }
    );
  } catch (e) {
    return json({ error: String(e.message || e) }, 502);
  }
};
