export interface ParsedAnnounce {
  botNick: string;
  eventType: string;
  releaseName: string;
}

const NEWDIR_KEYWORDS = 'NEW,NEWDIR,MKDIR,NEW DIR,NEW RACE';

/**
 * Parse IRC announce text to extract bot nick, event type, and release name
 */
export function parseIrcAnnounce(text: string): ParsedAnnounce | null {
  // Remove timestamp prefix if present: (HH:MM:SS)
  let cleaned = text.replace(/^\(\d{2}:\d{2}:\d{2}\)\s*/, '').trim();

  // Extract bot nick from (@BotNick) or (@|BotNick|)
  const botMatch = cleaned.match(/^\(@\|?([^|)]+)\|?\)/);
  if (!botMatch) {
    return null;
  }

  const botNick = botMatch[1];
  cleaned = cleaned.substring(botMatch[0].length).trim();

  let eventType = 'PRE';
  let releaseName = '';

  // Format 1: IF style - :: [ Tag ] :: [Section] :: [ Release ] :: [ Site ]
  // Example: :: [ New ] :: [MP3] :: [ 1219/Tymo-Obsessed_To_You-SINGLE-WEB-2025-XTC_iNT ] :: [ DEfCoN1 ]
  const ifMatch = cleaned.match(/^::\s*\[\s*([^\]]+)\s*\]\s*::\s*\[[^\]]+\]\s*::\s*\[\s*(?:\d+\/)?([^\]]+)\s*\]/);
  if (ifMatch) {
    const eventTag = ifMatch[1].trim();
    releaseName = ifMatch[2].trim();
    eventType = mapEventType(eventTag);
    return { botNick, eventType, releaseName };
  }

  // Format 2: CZ style - [Section] [Event] Release by User
  // Example: [MUSIC] [MKDIR] Tymo-Under_The_Stars_(Extended_Mix)-SINGLE-WEB-2025-XTC_iNT by aCe·(No Tagline Set)
  const czMatch = cleaned.match(/^\[[^\]]+\]\s*\[([^\]]+)\]\s+([^\s]+)/);
  if (czMatch) {
    const eventTag = czMatch[1].trim();
    releaseName = czMatch[2].trim();
    eventType = mapEventType(eventTag);
    return { botNick, eventType, releaseName };
  }

  const completeMatch = cleaned.match(/^COMPLETE:\s+-[^-]+-\s+([^\s]+)/i);
  if (completeMatch) {
    releaseName = completeMatch[1].trim();
    eventType = 'COMPLETE';
    return { botNick, eventType, releaseName };
  }

  // Format 3: CZ NUKE style - [NUKE] Release in [Section] was nuked...
  // Example: [NUKE] Black.Phone.2.2025.MULTi.2160p.UHD.BluRay.H265-NERO in [MOVIES] was nuked 10x
  const czNukeMatch = cleaned.match(/^\[NUKE\]\s+([^\s]+)\s+in\s+\[/);
  if (czNukeMatch) {
    releaseName = czNukeMatch[1].trim();
    eventType = 'NUKE';
    return { botNick, eventType, releaseName };
  }

  // Format 4: DL style - ( Section )-( Event )-( Release )-( Details )
  // Example: ( TV-720P )-( NEW RACE )-( Dora.2024.S04E25.GERMAN.DL.720p.WEB.h264-SCHOKOBONS )-( started by aCe/iND )
  const dlMatch = cleaned.match(/^\(\s*[^)]+\s*\)-\(\s*([^)]+)\s*\)-\(\s*([^)]+)\s*\)/);
  if (dlMatch) {
    const eventTag = dlMatch[1].trim();
    releaseName = dlMatch[2].trim();
    eventType = mapEventType(eventTag);
    return { botNick, eventType, releaseName };
  }

  return null;
}

/**
 * Map announce event tags to slftp event types
 */
function mapEventType(eventTag: string): string {
  const tag = eventTag.toUpperCase();

  // Direct matches
  if (tag === 'PRE') return 'PRE';
  if (tag === 'ADDPRE') return 'ADDPRE';
  if (tag === 'COMPLETE') return 'COMPLETE';
  if (tag === 'NUKE') return 'NUKE';
  if (tag === 'REQUEST') return 'REQUEST';

  // Map to NEWDIR
  if (tag === 'NEWDIR' || tag === 'MKDIR' || tag === 'NEW' || tag === 'NEW RACE') {
    return 'NEWDIR';
  }

  // Default to PRE
  return 'PRE';
}

/**
 * Convert release name to a catchlist pattern
 * Replaces the group tag with a wildcard
 */
export function releaseToPattern(releaseName: string): string {
  // Match common release patterns: Name-GROUP or Name.GROUP
  // Example: Tymo-Obsessed_To_You-SINGLE-WEB-2025-XTC_iNT -> Tymo-Obsessed_To_You-SINGLE-WEB-2025-*

  // Pattern: everything before the last dash/dot followed by uppercase letters/numbers
  const match = releaseName.match(/^(.+?)[-.]([A-Z0-9_]+(?:_[A-Z0-9]+)*)$/);

  if (match) {
    return `${match[1]}-*`;
  }

  // If no group tag found, return as-is with wildcard at end
  return `${releaseName}*`;
}

export function parsedAnnounceToWords(parsed: ParsedAnnounce): string {
  if (parsed.eventType === 'NEWDIR') {
    return NEWDIR_KEYWORDS;
  }

  if (parsed.eventType === 'COMPLETE') {
    return 'COMPLETE';
  }

  if (!parsed.releaseName) {
    return '';
  }

  return releaseToPattern(parsed.releaseName);
}
