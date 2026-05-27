import type { FileEntry } from '../api/client';
import type { CbftpPathEntry } from '../api/cbftpClient';

export type BrowserDirSortBy = 'name' | 'modified';
export type BrowserSortDir = 'asc' | 'desc';

export function parseBrowserModifiedMs(entry: FileEntry | CbftpPathEntry): number | null {
  const record = entry as unknown as Record<string, unknown>;
  const candidates = [
    record.mtime,
    record.mtime_ms,
    record.modified,
    record.modified_at,
    record.date,
    record.last_modified,
    record.time,
    record.timestamp,
  ];

  for (const value of candidates) {
    if (typeof value === 'number' && Number.isFinite(value)) {
      return value < 1_000_000_000_000 ? value * 1000 : value;
    }
    if (typeof value !== 'string' || value.trim() === '') continue;

    const ms = Date.parse(value);
    if (!Number.isNaN(ms) && ms > 0) {
      const d = new Date(ms);
      if (d.getFullYear() !== 2001 || value.includes('2001')) return ms;
    }

    const now = new Date();
    const currentYear = now.getFullYear();

    const mUnixTime = value.match(/^([A-Z][a-z]{2})\s+(\d{1,2})\s+(\d{2}):(\d{2})$/);
    if (mUnixTime) {
      const monthIndex = 'JanFebMarAprMayJunJulAugSepOctNovDec'.indexOf(mUnixTime[1]) / 3;
      const day = parseInt(mUnixTime[2], 10);
      const hour = parseInt(mUnixTime[3], 10);
      const minute = parseInt(mUnixTime[4], 10);
      if (monthIndex >= 0) {
        const d = new Date(currentYear, monthIndex, day, hour, minute);
        if (d.getTime() > now.getTime() + 86400000) d.setFullYear(currentYear - 1);
        return d.getTime();
      }
    }

    const mUnixYear = value.match(/^([A-Z][a-z]{2})\s+(\d{1,2})\s+(\d{4})$/);
    if (mUnixYear) {
      const monthIndex = 'JanFebMarAprMayJunJulAugSepOctNovDec'.indexOf(mUnixYear[1]) / 3;
      const day = parseInt(mUnixYear[2], 10);
      const year = parseInt(mUnixYear[3], 10);
      if (monthIndex >= 0) return new Date(year, monthIndex, day).getTime();
    }

    const mFtp = value.match(/^(\d{2})-(\d{2})\s+(\d{2}):(\d{2})$/);
    if (mFtp) {
      const month = Number(mFtp[1]);
      const day = Number(mFtp[2]);
      const hour = Number(mFtp[3]);
      const minute = Number(mFtp[4]);
      if ([month, day, hour, minute].every((n) => Number.isFinite(n))) {
        const d = new Date(currentYear, month - 1, day, hour, minute, 0, 0);
        if (d.getTime() > now.getTime() + 86400000) d.setFullYear(currentYear - 1);
        if (!Number.isNaN(d.getTime())) return d.getTime();
      }
    }
  }

  return null;
}

export function sortBrowserDirs<T extends FileEntry | CbftpPathEntry>(
  dirs: T[],
  sortBy: BrowserDirSortBy = 'modified',
  sortDir: BrowserSortDir = 'desc',
): T[] {
  return [...dirs].sort((a, b) => {
    let cmp = 0;

    if (sortBy === 'name') {
      cmp = a.name.localeCompare(b.name, undefined, { sensitivity: 'base' });
    } else {
      const am = parseBrowserModifiedMs(a);
      const bm = parseBrowserModifiedMs(b);

      if (am !== null && bm !== null) cmp = am - bm;
      else if (am !== null && bm === null) cmp = -1;
      else if (am === null && bm !== null) cmp = 1;
      else cmp = 0;
    }

    if (cmp !== 0) return sortDir === 'asc' ? cmp : -cmp;
    return a.name.localeCompare(b.name, undefined, { sensitivity: 'base' });
  });
}
