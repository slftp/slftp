# Plan: Optimizing the `ROUTES` phase

## Current observation

After the `AddSites` optimizations, `ROUTES` is now the remaining dominant phase in the race timeline:

- Median: ~12 ms
- Average: ~80 ms
- p95: ~500 ms
- Max: ~900 ms

`ROUTES` is the timestamp set at the very end of `kb_AddB`, after `FireRules` has run for every source site and `RoutesText` has been generated.

## What happens between `SITE_RULES` and `ROUTES`

1. **`FireRules` loop** (`kb.pas`)
   - Iterates over every `TPazoSite` in `PazoSitesList`.
   - For each source site, iterates over `ps.speed_from.Routes`.
   - For every route:
     - `p.FindSite(...)` to get destination `TPazoSite`.
     - `FindSiteByName('', dstps.Name)` to get destination `TSite`.
     - Possibly `FireRuleSet(p, dstps)`.
     - `CalculateRank(...)`.
     - `ps.AddDestination(...)`.
   - The entire loop holds `kb_lock`.

2. **`RoutesText` generation** (`pazo.pas`)
   - Iterates over all `PazoSitesList`.
   - For each site, iterates over `destinations`.
   - Builds the IRC/route announcement string.
   - Called twice: once for the timeline marker and once for `irc_SendROUTEINFOS`.

3. **`irc_SendROUTEINFOS`**
   - Sends the route announcement to IRC.

## Hypotheses for slowness

| Hypothesis | Why it could be slow | How to verify |
|------------|----------------------|---------------|
| `FireRules` loop is CPU-heavy | Many sites × many routes × rule evaluations | Add per-site and per-route timing markers |
| `FindSiteByName` in inner loop | Called for every route, even though `TPazoSite` already exists | Add timing around `FindSiteByName` calls |
| `FireRuleSet` per destination | Rule parsing/evaluation might be expensive | Add timing around `FireRuleSet` calls |
| `kb_lock` serializes everything | All source sites processed sequentially under one lock | Compare total FireRules time with number of sites |
| `RoutesText` string building | Iterates over all destinations twice | Add timing around both `RoutesText` calls |
| `irc_SendROUTEINFOS` blocks | Network/IRC output might stall | Add timing around `irc_SendROUTEINFOS` |

## Logging plan

Add fine-grained timeline markers in `kb.pas` and `pazo.pas`:

- `FIRE_SITE_START` / `FIRE_SITE_DONE` — per source site in `FireRules` loop.
- `FIRE_ROUTE` — per route inside `FireRules`.
- `FIRE_RANK` — time spent in `CalculateRank` + `AddDestination`.
- `ROUTES_TEXT_START` / `ROUTES_TEXT_DONE` — both calls to `RoutesText`.
- `IRC_ROUTE_SEND` — time spent in `irc_SendROUTEINFOS`.

This will show which sub-phase dominates `ROUTES`.

## Optimization options

### Option 1 — Cache `TSite` references in `TPazoSite`

Each `FireRules` call does `FindSiteByName('', ps.Name)` and `FindSiteByName('', dstps.Name)`. `TPazoSite` already has a reference to its `TSite` indirectly, but it is not stored. Add:

```pascal
fSite: TSite;
```

to `TPazoSite`, initialized in `Create`, and use it instead of `FindSiteByName`.

**Pros:** Removes repeated dictionary lookups.  
**Cons:** Requires careful lifetime management.

### Option 2 — Pre-build a destination lookup map

Inside `FireRules`, build a temporary `TDictionary<String, TPazoSite>` for `PazoSitesList` once per release, so `p.FindSite(...)` becomes O(1) without scanning the list.

**Pros:** Fast route-to-destination lookup.  
**Cons:** Extra memory per `FireRules` call.

### Option 3 — Parallelize `FireRules` per source site

Remove the per-site `kb_lock` around `FireRules` if the operations are read-only on shared state, and process source sites in parallel.

**Pros:** Scales with number of cores/sites.  
**Cons:** High risk; `FireRules` modifies `p.srcsite`, `p.dstsite`, and destination lists. Needs careful review.

### Option 4 — Avoid duplicate `RoutesText` work

`RoutesText` is called twice: once for the timeline marker and once for IRC. Cache the result after the first call and reuse it if destinations have not changed.

**Pros:** Simple and safe.  
**Cons:** Only helps if `RoutesText` itself is slow.

### Option 5 — Lazy IRC sending

Move `irc_SendROUTEINFOS` out of the critical path if it blocks.

**Pros:** Removes network latency from `ROUTES`.  
**Cons:** Changes behavior; routes might be announced slightly later.

## Recommended next step

1. Implement the logging plan (Option 0).
2. Deploy and collect data.
3. Based on the data, pick the most impactful optimization.

Most likely candidates after logging:
- Option 1 if `FindSiteByName` dominates.
- Option 4 if `RoutesText` dominates.
- Option 2 if `p.FindSite(...)` dominates.
