# Plan: Site/Section Cache Optimizations

This plan covers two hot paths identified in the race timeline:

1. `TPazoSite.DelaySetup` (~25 ms / release)
2. `TPazoSite.Create` / `TSite.Speed_From` (~28 ms / release)

Both are called for every candidate site while a release is being set up, so even small per-call savings multiply by the number of sites.

---

## 1. `delayleech` / `delayupload` caching

### Current behavior

```pascal
procedure TPazoSite.DelaySetup;
begin
  delay_leech := fSite.delayleech[rls.section];
  delay_upload := fSite.delayupload[rls.section];
end;
```

Each `delayleech[section]` call does:

```pascal
fMinValue := GetDelayLeechMin(section);       // RCInteger('delayleech-<section>-min')
if fMinValue <= 0 then
  fMinValue := GetDelayLeechMin('global');    // RCInteger('delayleech-global-min')
fMaxValue := GetDelayLeechMax(section);       // RCInteger('delayleech-<section>-max')
if fMaxValue <= 0 then
  fMaxValue := GetDelayLeechMax('global');    // RCInteger('delayleech-global-max')
if fMaxValue > 0 then
  Result := RandomRange(fMinValue, fMaxValue);
```

That is up to 4 `RCInteger` calls per access. `RCInteger` already uses `FSettingsCacheDict`, but each call still costs:

- dictionary lookup by string key
- key construction (`'delayleech-' + section + '-min'`)
- a `RandomRange` call when the value is finally built

With ~8 candidate sites this is ~64 lookups per release.

### Goal

Cache the **final computed delay value** per site and section, so that subsequent accesses for the same section are a single dictionary lookup.

### Options

#### Option 1.1 — Dedicated per-section cache (recommended)

Add to `TSite`:

```pascal
fDelayCacheCS: TSlCriticalSection2;
fDelayLeechCache: TDictionary<String, Integer>;
fDelayUploadCache: TDictionary<String, Integer>;
```

Change `GetDelayLeech` / `GetDelayUpload` to:

1. Enter `fDelayCacheCS`.
2. Create the dictionary lazily.
3. Return the cached value if the section key exists.
4. Otherwise compute the value as before, store it, then return it.

Invalidate in the setters:

```pascal
procedure TSite.SetDelayLeechMin(const aSection: String; const Value: integer);
begin
  WCInteger('delayleech-' + aSection + '-min', Value);
  if aSection = 'global' then
    InvalidateDelayLeechCache('')   // global affects every section -> clear all
  else
    InvalidateDelayLeechCache(aSection);
end;
```

Same pattern for `SetDelayLeechMax`, `SetDelayUploadMin`, `SetDelayUploadMax`.

**Pros:**
- Simple and localized to `sitesunit.pas`.
- Reuses the existing `TDictionary<String, Integer>` pattern already used for `sitesDict`.
- One lock protects both dictionaries.
- Only computes `RandomRange` once per section per site.

**Cons:**
- Adds two dictionaries and one critical section per site.
- Need to remember invalidation in every delay setter.

#### Option 1.2 — No invalidation, cache forever

Same as Option 1.1, but never clear the cache when a setter is called.

**Pros:**
- Even simpler; no setter changes.

**Cons:**
- Changing delay values at runtime would require a restart to take effect.
- Likely unacceptable for a live bot.

#### Option 1.3 — Cache only inside `DelaySetup`

Instead of caching inside `TSite`, cache the computed values in `TPazoSite.DelaySetup` (e.g. store them in `TPazoSite` fields).

**Pros:**
- Keeps `TSite` untouched.

**Cons:**
- `TPazoSite` is created per release, so the cache is lost immediately afterwards.
- No real gain unless the same `TPazoSite` calls `DelaySetup` repeatedly, which it does not appear to do.

### Recommendation for delays

**Option 1.1**. It is the minimal change that actually helps across releases and remains correct when settings change.

### Files to touch for delays

- `sitesunit.pas` only.

---

## 2. `Speed_From` caching

### Current behavior

`TSite` already maintains `fSpeedFromCache: TList<TSpeedFromRouteInfo>`, built once by `UpdateSpeedFromCache`. However, `GetSpeed_From` returns a **copy** of that list:

```pascal
Result := TList<TSpeedFromRouteInfo>.Create(self.fSpeedFromCache);
```

`TPazoSite.Create` stores this copy:

```pascal
speed_from := fSite.Speed_From;
```

So every release pays for the list copy for every source site.

### Goal

Avoid copying the route list for every `TPazoSite` while preserving correctness.

### Options

#### Option 2.1 — Share an immutable route list (recommended)

Replace the mutable `TList<TSpeedFromRouteInfo>` cache with an immutable wrapper or array:

```pascal
type
  TSpeedFromRouteList = class
  public
    Routes: TArray<TSpeedFromRouteInfo>;
    constructor Create(const aRoutes: TArray<TSpeedFromRouteInfo>);
  end;
```

In `TSite`:

```pascal
fSpeedFromCache: TSpeedFromRouteList;
```

`UpdateSpeedFromCache` builds a `TArray<TSpeedFromRouteInfo>`, sorts it once, wraps it in `TSpeedFromRouteList`, and atomically assigns it to `fSpeedFromCache` under `fSpeedFromCS`.

`GetSpeed_From` returns the shared reference:

```pascal
function TSite.GetSpeed_From: TSpeedFromRouteList;
begin
  if fSpeedFromCache = nil then ... build ...;
  Result := fSpeedFromCache;
end;
```

In `TPazoSite`:

```pascal
speed_from: TSpeedFromRouteList;   // reference, not owned
```

Do **not** free `speed_from` in `TPazoSite.Destroy`. Update consumers to iterate `speed_from.Routes`.

**Pros:**
- Removes the per-release copy completely.
- Readers need no lock while iterating because the list never changes after creation.
- Atomic swap in `UpdateSpeedFromCache` keeps thread safety simple.

**Cons:**
- Touches `pazo.pas` and every consumer of `speed_from`.
- Need to verify that no caller modifies the list.

#### Option 2.2 — Lazy copy in `TPazoSite`

Keep `TSite.GetSpeed_From` returning a copy, but do not copy in `TPazoSite.Create`. Instead store `fSpeedFromSource: TSite` and fetch the list only when first accessed.

**Pros:**
- Minimal change; `sitesunit.pas` stays the same.
- Moves the copy cost out of `AddSites` / `TPazoSite.Create`.

**Cons:**
- Does not reduce total work, only defers it.
- Adds complexity to `TPazoSite`.

#### Option 2.3 — Section-scoped filtered cache

If consumers only need a subset of routes for a specific section, add:

```pascal
fSectionSpeedFromCache: TDictionary<String, TSpeedFromRouteList>;
```

Cache the filtered result per `(source site, section)`.

**Pros:**
- Could help if filtering is expensive and repeated.

**Cons:**
- Overkill unless section-specific filtering exists and is hot.
- Adds another cache to invalidate.

### Recommendation for speed-from

**Option 2.1** if no caller mutates `speed_from`. If mutation is found during implementation, fall back to **Option 2.2**.

### Files to touch for speed-from

- `sitesunit.pas`: `fSpeedFromCache`, `GetSpeed_From`, `UpdateSpeedFromCache`, invalidation.
- `pazo.pas`: `TPazoSite.speed_from`, `TPazoSite.Create`, `TPazoSite.Destroy`, consumers.

---

## 3. Suggested order of work

1. Implement **Option 1.1** (`delayleech`/`delayupload` cache). It is small, low-risk, and gives a measurable reduction in `DelaySetup` time.
2. Build, deploy, and verify the timeline improvement.
3. Implement **Option 2.1** (`Speed_From` immutable shared cache). It is larger but removes the second hot path.
4. Build, deploy, and verify again.

---

## 4. Risks / things to verify

- `RandomRange` is only called once per cached section. This means the delay value becomes stable for a site/section until the cache is invalidated. This is desirable for races (consistent behavior) but should be documented.
- Global delay changes must invalidate all section entries.
- `Speed_From` shared reference must remain valid while `TPazoSite` uses it. `TPazoSite` already holds a reference to `TSite`, so lifetime is safe; only atomic rebuild matters.
- Confirm that `speed_from` is never modified after `TPazoSite.Create` before switching to Option 2.1.

---

## 5. Expected outcome

- `DelaySetup` should drop from ~25 ms to well under 5 ms per release after the first access per site/section.
- `TPazoSite.Create` should lose the `Speed_From` copy cost, leaving `TDirList.Create` as the remaining dominant cost to investigate next.
