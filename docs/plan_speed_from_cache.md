# Plan: Site/Section Speed_From Cache Optimization

## Current State

- `TSite.fSpeedFromCache` already caches the parsed `speed-from-*` routes for a source site.
- `TSite.GetSpeed_From` returns a **new copy** of that list on every call:
  ```pascal
  Result := TList<TSpeedFromRouteInfo>.Create(self.fSpeedFromCache);
  ```
- `TPazoSite.Create` calls `speed_from := fSite.Speed_From`, so every release pays for this copy.
- `UpdateSpeedFromCache` parses all `speed-from-*` entries from `site-<name>` once, which is acceptable if it happens rarely, but the per-call copy is paid for every `TPazoSite`.

## Goal

Eliminate the per-`TPazoSite` copy of `speed_from` while keeping correctness and thread safety.

## Proposed Implementation

### Option A: Share immutable cache reference (recommended)

Change `TSite.fSpeedFromCache` from `TList<TSpeedFromRouteInfo>` to a reference-counted/immutable container so callers can share it without copying.

1. Introduce a thin wrapper or use an interface/`TInterfacedObject` for the cached route list:
   ```pascal
   type
     TSpeedFromRouteList = class
       Routes: TArray<TSpeedFromRouteInfo>;
       constructor Create(const aRoutes: TArray<TSpeedFromRouteInfo>);
     end;
   ```
   The array is sorted once in `UpdateSpeedFromCache` and never modified after creation.

2. Replace `fSpeedFromCache: TList<TSpeedFromRouteInfo>` with:
   ```pascal
   fSpeedFromCache: TSpeedFromRouteList;
   ```

3. Change `GetSpeed_From` to return the shared reference:
   ```pascal
   function TSite.GetSpeed_From: TSpeedFromRouteList;
   begin
     if self.fSpeedFromCache = nil then
     begin
       self.fSpeedFromCS.Enter('GetSpeed_From');
       try
         if self.fSpeedFromCache = nil then
           self.UpdateSpeedFromCache;
       finally
         self.fSpeedFromCS.Leave;
       end;
     end;
     Result := self.fSpeedFromCache;
   end;
   ```

4. Update `TPazoSite`:
   - Change `speed_from: TList<TSpeedFromRouteInfo>` to `speed_from: TSpeedFromRouteList`.
   - Do **not** free `speed_from` in `TPazoSite.Destroy` because it is owned by `TSite`.
   - Update all call sites that iterate `speed_from` to use `speed_from.Routes` (or add an enumerator/helper).

5. Thread safety:
   - `UpdateSpeedFromCache` rebuilds the cache under `fSpeedFromCS` and atomically assigns the new reference.
   - Readers see either the old or the new immutable list, never a partially built one.
   - No locks are held while iterating the shared routes.

6. Invalidation:
   - Whenever a `speed-from-*` value is written, call `UpdateSpeedFromCache` or clear `fSpeedFromCache` so the next reader rebuilds it.
   - Existing migration code already calls `UpdateSpeedFromCache` after changes.

### Option B: Lazy per-TPazoSite copy (minimal change)

Keep the copy semantics but defer it until `speed_from` is actually used.

1. In `TPazoSite` store `fSpeedFromSource: TSite` instead of the copied list.
2. Add a getter `speed_from` that calls `fSpeedFromSource.Speed_From` on first access.
3. This does not reduce the total work but moves it out of `TPazoSite.Create`, improving `AddSites` latency.

### Option C: Section-scoped filtered cache

If the route list is later filtered by section, cache the filtered result per `(source site, section)`:

1. Add `fSectionSpeedFromCache: TDictionary<String, TSpeedFromRouteList>` to `TSite`.
2. Provide `GetSpeed_FromForSection(const aSection: String)`.
3. Clear section caches when `fSpeedFromCache` is rebuilt.

This is only useful if a section-specific subset is repeatedly requested. It is not needed if the whole list is always consumed.

## Recommended Next Step

Implement **Option A** because it removes the per-release copy completely with a small, localized change. If route mutation by callers is discovered during the change, switch to Option B as a safe fallback.

## Files to Touch

- `sitesunit.pas`: `TSite.fSpeedFromCache`, `GetSpeed_From`, `UpdateSpeedFromCache`, speed-from writers.
- `pazo.pas`: `TPazoSite.speed_from`, `TPazoSite.Create`, `TPazoSite.Destroy`, and all consumers of `speed_from`.

## Validation

- Build with `make slftp`.
- Deploy and compare `create=` timing in `[RACETIMELINE]` before/after.
- Verify no access violations under load; the shared immutable list must not be freed while readers use it.
