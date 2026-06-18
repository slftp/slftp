import { Alert, Badge, Button, Card, Checkbox, Group, Loader, ScrollArea, Stack, Table, Text, TextInput, Title, Tooltip, Modal, ActionIcon, Breadcrumbs, Center, Pill } from '@mantine/core';
import { IconAlertCircle, IconRefresh, IconSearch, IconPlus, IconBook, IconFolderOpen, IconArrowUp, IconChevronUp, IconChevronDown, IconSelector, IconLink, IconTrash, IconCopy } from '@tabler/icons-react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useMemo, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { apiClient } from '../api/client';
import type { Issue, IssuesSummary } from '../api/client';
import { getCbftpPath, type CbftpPathEntry } from '../api/cbftpClient';
import { notifications } from '@mantine/notifications';
import { sortBrowserDirs, type BrowserDirSortBy, type BrowserSortDir } from '../utils/browserDates';

function parseMaybeJsonArray(value: unknown): any[] {
  if (Array.isArray(value)) return value;
  if (typeof value === 'string') {
    try {
      const parsed = JSON.parse(value);
      return Array.isArray(parsed) ? parsed : [];
    } catch {
      return [];
    }
  }
  return [];
}

function formatWindowSeconds(seconds: number): string {
  if (!Number.isFinite(seconds) || seconds <= 0) return '';
  if (seconds % 86400 === 0) return `${seconds / 86400}d`;
  if (seconds % 3600 === 0) return `${seconds / 3600}h`;
  if (seconds % 60 === 0) return `${seconds / 60}m`;
  return `${seconds}s`;
}

type IssueFilterField = 'type' | 'section' | 'release' | 'site' | 'reason' | 'event';

function tokenizeFilterQuery(query: string): string[] {
  const tokens: string[] = [];
  let current = '';
  let inQuotes = false;

  for (let i = 0; i < query.length; i++) {
    const ch = query[i];
    if (ch === '"') {
      inQuotes = !inQuotes;
      continue;
    }
    if (!inQuotes && /\s/.test(ch)) {
      if (current) tokens.push(current);
      current = '';
      continue;
    }
    current += ch;
  }
  if (current) tokens.push(current);

  return tokens.map((t) => t.trim()).filter(Boolean);
}

function parseIssueFilter(query: string): { free: string[]; fields: Partial<Record<IssueFilterField, string>> } {
  const tokens = tokenizeFilterQuery(query);
  const fields: Partial<Record<IssueFilterField, string>> = {};
  const free: string[] = [];

  const normalizeKey = (key: string): IssueFilterField | null => {
    const k = key.trim().toLowerCase();
    if (k === 'type' || k === 'issuetype' || k === 'issue') return 'type';
    if (k === 'section') return 'section';
    if (k === 'release' || k === 'releasename') return 'release';
    if (k === 'site' || k === 'sitename') return 'site';
    if (k === 'reason') return 'reason';
    if (k === 'event' || k === 'kbevent') return 'event';
    return null;
  };

  for (const token of tokens) {
    const idx = token.indexOf(':');
    if (idx > 0) {
      const key = normalizeKey(token.slice(0, idx));
      const value = token.slice(idx + 1).trim();
      if (key && value) {
        fields[key] = value;
        continue;
      }
    }
    free.push(token);
  }

  return { free, fields };
}

function upsertFilterField(prev: string, field: IssueFilterField, value: string): string {
  const tokens = tokenizeFilterQuery(prev).filter((t) => {
    const idx = t.indexOf(':');
    if (idx <= 0) return true;
    const key = t.slice(0, idx).trim().toLowerCase();
    if (field === 'type' && (key === 'type' || key === 'issuetype' || key === 'issue')) return false;
    if (field === 'section' && key === 'section') return false;
    if (field === 'release' && (key === 'release' || key === 'releasename')) return false;
    if (field === 'site' && (key === 'site' || key === 'sitename')) return false;
    if (field === 'reason' && key === 'reason') return false;
    if (field === 'event' && (key === 'event' || key === 'kbevent')) return false;
    return true;
  });

  tokens.push(`${field}:${value}`);
  return tokens.join(' ').trim();
}

function toggleBrowserSort(currentBy: BrowserDirSortBy, currentDir: BrowserSortDir, nextBy: BrowserDirSortBy): { by: BrowserDirSortBy; dir: BrowserSortDir } {
  if (currentBy !== nextBy) return { by: nextBy, dir: nextBy === 'modified' ? 'desc' : 'asc' };
  return { by: currentBy, dir: currentDir === 'asc' ? 'desc' : 'asc' };
}

function browserSortIndicator(active: boolean, dir: BrowserSortDir) {
  if (!active) return <IconSelector size="0.9rem" />;
  return dir === 'asc' ? <IconChevronUp size="0.9rem" /> : <IconChevronDown size="0.9rem" />;
}

export function Issues() {
  const BROWSER_INITIAL_RENDER_LIMIT = 1000;
  const BROWSER_RENDER_LIMIT_STEP = 1000;
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const [filter, setFilter] = useState('');
  const [addSectionModalOpened, setAddSectionModalOpened] = useState(false);
  const [selectedIssue, setSelectedIssue] = useState<Issue | null>(null);
  const [sectionPath, setSectionPath] = useState('');
  const [addDropRule, setAddDropRule] = useState(false);

  // Browser state
  const [browserOpen, setBrowserOpen] = useState(false);
  const [browserPath, setBrowserPath] = useState('/');
  const [browserSortBy, setBrowserSortBy] = useState<BrowserDirSortBy>('modified');
  const [browserSortDir, setBrowserSortDir] = useState<BrowserSortDir>('desc');
  const [browserRenderLimit, setBrowserRenderLimit] = useState<number>(BROWSER_INITIAL_RENDER_LIMIT);

  const { data: summary, isLoading: summaryLoading, refetch: refetchSummary } = useQuery({
    queryKey: ['issuesSummary'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiIssuesService/GetSummary', { WindowSeconds: 24 * 3600 });
      if (res.data?.result && Array.isArray(res.data.result)) return res.data.result[0] as IssuesSummary;
      return res.data as IssuesSummary;
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const { data, isLoading, error, refetch, isFetching } = useQuery({
    queryKey: ['issuesList'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiIssuesService/GetIssues', { Limit: 500, SinceUnix: 0, TypesCsv: '' });
      if (res.data?.result && Array.isArray(res.data.result)) {
        const list = res.data.result[0];
        const issues = list?.Issues ? parseMaybeJsonArray(list.Issues) : [];
        return issues as Issue[];
      }
      return [] as Issue[];
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const clearIssuesMutation = useMutation({
    mutationFn: async () => {
      await apiClient.post('/ApiIssuesService/ClearIssues');
    },
    onSuccess: () => {
      queryClient.setQueryData(['issuesList'], []);
      refetchSummary();
      notifications.show({
        title: 'Success',
        message: 'Issues cleared.',
        color: 'green'
      });
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.message || 'Failed to clear issues',
        color: 'red'
      });
    },
  });

  async function copyToClipboard(text: string): Promise<void> {
    if (navigator.clipboard && window.isSecureContext) {
      await navigator.clipboard.writeText(text);
      return;
    }

    // Fallback for non-HTTPS contexts or blocked clipboard API
    const textarea = document.createElement('textarea');
    textarea.value = text;
    textarea.style.position = 'fixed';
    textarea.style.left = '-9999px';
    textarea.style.top = '-9999px';
    textarea.setAttribute('readonly', '');
    document.body.appendChild(textarea);
    textarea.focus();
    textarea.select();

    try {
      const success = document.execCommand('copy');
      if (!success) {
        throw new Error('document.execCommand("copy") returned false');
      }
    } finally {
      document.body.removeChild(textarea);
    }
  }

  const handleCopyIssues = async () => {
    if (!filtered.length) {
      notifications.show({
        title: 'Nothing to copy',
        message: 'The issues table is empty.',
        color: 'orange',
      });
      return;
    }

    const clean = (value: string | undefined) =>
      String(value ?? '').replace(/[\t\n\r]/g, ' ');

    const formatTime = (ts: number | undefined) => {
      if (!ts) return '';
      const d = new Date(ts * 1000);
      return `${d.toLocaleDateString()} ${d.toLocaleTimeString()}`;
    };

    const header = ['Time', 'Type', 'Section', 'Release', 'Site', 'Event', 'Reason'];
    const rows = filtered.map((i) => [
      formatTime(i.TsUnix),
      clean(i.IssueType),
      clean(i.Section),
      clean(i.ReleaseName),
      clean(i.SiteName),
      clean(i.KbEvent),
      clean(i.Reason),
    ]);

    const tsv = [header.join('\t'), ...rows.map((r) => r.join('\t'))].join('\n');

    try {
      await copyToClipboard(tsv);
      notifications.show({
        title: 'Copied',
        message: `${filtered.length} issue${filtered.length !== 1 ? 's' : ''} copied to clipboard.`,
        color: 'green',
      });
    } catch (err) {
      notifications.show({
        title: 'Error',
        message: err instanceof Error ? err.message : 'Failed to copy to clipboard.',
        color: 'red',
      });
    }
  };

  const { data: cbftpBrowserData, isLoading: browserLoading, isRefetching: browserRefetching, error: browserError } = useQuery({
    queryKey: ['issues-browser-cbftp', selectedIssue?.SiteName, browserPath],
    queryFn: async () => {
      if (!selectedIssue?.SiteName) return [] as CbftpPathEntry[];
      return getCbftpPath(selectedIssue.SiteName, browserPath, 60);
    },
    enabled: !!selectedIssue?.SiteName && browserOpen,
  });

  const { data: siteSectionsData } = useQuery({
    queryKey: ['siteSections', selectedIssue?.SiteName],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSitesService/GetSiteSections', { SiteName: selectedIssue?.SiteName });
      const raw = res.data?.result ?? res.data;
      return parseMaybeJsonArray(Array.isArray(raw) ? raw[0] : raw) as { section: string; dir: string }[];
    },
    enabled: !!selectedIssue?.SiteName && addSectionModalOpened,
  });

  const sectionPathSuggestions = useMemo(() => {
    if (!siteSectionsData || siteSectionsData.length === 0) return [];
    // Collect unique dirs from the site's existing sections as path hints
    const dirSet = new Map<string, string[]>();
    for (const entry of siteSectionsData) {
      if (!entry.dir) continue;
      const sections = dirSet.get(entry.dir) || [];
      sections.push(entry.section);
      dirSet.set(entry.dir, sections);
    }
    return Array.from(dirSet.entries())
      .map(([dir, sections]) => ({ dir, sections }))
      .sort((a, b) => a.dir.localeCompare(b.dir));
  }, [siteSectionsData]);

  const issues = Array.isArray(data) ? data : [];

  const addSectionMutation = useMutation({
    mutationFn: async ({ siteName, section, path, issueIds, dropRule }: { siteName: string; section: string; path: string; issueIds: number[]; dropRule: boolean }) => {
      await apiClient.post('/ApiSitesService/SetSiteSection', {
        SiteName: siteName,
        Section: section,
        Dir: path
      });
      if (dropRule) {
        await apiClient.post('/ApiSitesService/AddSiteDropRule', {
          SiteName: siteName,
          Section: section
        });
      }
      // Delete all issues for this site + section combination
      for (const issueId of issueIds) {
        await apiClient.post('/ApiIssuesService/DeleteIssue', { IssueId: issueId });
      }
      return issueIds.length;
    },
    onSuccess: (deletedCount) => {
      notifications.show({
        title: 'Success',
        message: `Section added successfully. ${deletedCount} issue${deletedCount !== 1 ? 's' : ''} removed.`,
        color: 'green'
      });
      setAddSectionModalOpened(false);
      setSectionPath('');
      setAddDropRule(false);
      setSelectedIssue(null);
      refetch();
      refetchSummary();
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.message || 'Failed to add section',
        color: 'red'
      });
    },
  });

  const handleOpenAddSection = (issue: Issue) => {
    setSelectedIssue(issue);
    setSectionPath('');
    setAddSectionModalOpened(true);
  };

  const handleAddSection = () => {
    if (!selectedIssue || !sectionPath.trim()) return;

    // Find all issues with the same site + section combination
    const matchingIssueIds = issues
      .filter(issue =>
        issue.SiteName === selectedIssue.SiteName &&
        issue.Section === selectedIssue.Section
      )
      .map(issue => issue.Id);

    addSectionMutation.mutate({
      siteName: selectedIssue.SiteName,
      section: selectedIssue.Section,
      path: sectionPath.trim(),
      issueIds: matchingIssueIds,
      dropRule: addDropRule,
    });
  };

  const normalizePath = (value: string) => {
    let p = value.trim();
    if (!p) return '';
    if (!p.startsWith('/')) p = '/' + p;
    if (p.length > 1 && p.endsWith('/')) p = p.slice(0, -1);
    return p;
  };

  const navigateBrowserPath = (value: string) => {
    const next = normalizePath(value);
    setBrowserPath(next || '/');
    setBrowserRenderLimit(BROWSER_INITIAL_RENDER_LIMIT);
  };

  const handleBrowserRefresh = () => {
    if (!selectedIssue?.SiteName) return;
    queryClient.invalidateQueries({ queryKey: ['issues-browser-cbftp', selectedIssue.SiteName, browserPath] });
  };

  const openBrowser = () => {
    setBrowserPath('/');
    setBrowserRenderLimit(BROWSER_INITIAL_RENDER_LIMIT);
    setBrowserOpen(true);
  };

  const handleBrowserSort = (nextBy: BrowserDirSortBy) => {
    const next = toggleBrowserSort(browserSortBy, browserSortDir, nextBy);
    setBrowserSortBy(next.by);
    setBrowserSortDir(next.dir);
  };

  const browserDirs = useMemo(() => {
    const files = cbftpBrowserData || [];
    return sortBrowserDirs(
      files.filter((f) => (f as CbftpPathEntry).type === 'DIR' || (f as CbftpPathEntry).type === 'LINK'),
      browserSortBy,
      browserSortDir
    ) as CbftpPathEntry[];
  }, [cbftpBrowserData, browserSortBy, browserSortDir]);
  const visibleBrowserDirs = useMemo(() => browserDirs.slice(0, browserRenderLimit), [browserDirs, browserRenderLimit]);
  const hasHiddenBrowserDirs = browserDirs.length > visibleBrowserDirs.length;

  const breadcrumbItems = useMemo(() => {
    const parts = browserPath === '/' ? [] : browserPath.split('/').filter(Boolean);
    if (parts.length === 0) {
      return [{ label: '/', path: '/' }];
    }
    return parts.map((part, idx) => ({
      label: '/' + part,
      path: '/' + parts.slice(0, idx + 1).join('/'),
    }));
  }, [browserPath]);

  const filtered = useMemo(() => {
    const q = filter.trim();
    if (!q) return issues;

    const parsed = parseIssueFilter(q);
    const freeTokens = parsed.free.map((t) => t.toLowerCase());
    const fields = Object.fromEntries(
      Object.entries(parsed.fields).map(([k, v]) => [k, (v || '').toLowerCase()])
    ) as Partial<Record<IssueFilterField, string>>;

    return issues.filter((i) => {
      const issueFields: Record<IssueFilterField, string> = {
        type: (i.IssueType || '').toLowerCase(),
        section: (i.Section || '').toLowerCase(),
        release: (i.ReleaseName || '').toLowerCase(),
        site: (i.SiteName || '').toLowerCase(),
        reason: (i.Reason || '').toLowerCase(),
        event: (i.KbEvent || '').toLowerCase(),
      };

      for (const [k, v] of Object.entries(fields) as Array<[IssueFilterField, string]>) {
        if (!v) continue;
        if (!issueFields[k].includes(v)) return false;
      }

      if (freeTokens.length === 0) return true;
      const hay = `${issueFields.type} ${issueFields.section} ${issueFields.release} ${issueFields.site} ${issueFields.reason} ${issueFields.event}`;
      return freeTokens.every((t) => hay.includes(t));
    });
  }, [issues, filter]);

  const typeBadgeStyle = (t: string) => {
    const u = (t || '').toUpperCase();
    if (u === 'SKIP') return { background: 'rgba(251, 191, 36, 0.25)', border: '1px solid rgba(251, 191, 36, 0.5)', color: '#fbbf24' };
    if (u === 'DONT_MATCH' || u === 'DONTMATCH') return { background: 'rgba(248, 113, 113, 0.25)', border: '1px solid rgba(248, 113, 113, 0.5)', color: '#f87171' };
    if (u === 'MISSING_SECTION' || u === 'MISSING_SECTION_DIR') return { background: 'rgba(251, 146, 60, 0.25)', border: '1px solid rgba(251, 146, 60, 0.5)', color: '#fb923c' };
    if (u === 'NUKE') return { background: 'rgba(168, 85, 247, 0.25)', border: '1px solid rgba(168, 85, 247, 0.5)', color: '#a855f7' };
    return { background: 'rgba(100, 116, 139, 0.25)', border: '1px solid rgba(100, 116, 139, 0.5)', color: '#94a3b8' };
  };

  return (
    <Stack>
      <Group justify="space-between" align="center">
        <Title order={2}>Issues</Title>
        <Group gap="xs">
          <Button leftSection={<IconCopy size="1rem" />} onClick={handleCopyIssues} variant="light" color="blue">
            Copy
          </Button>
          <Button leftSection={<IconTrash size="1rem" />} variant="light" color="gray" onClick={() => clearIssuesMutation.mutate()} loading={clearIssuesMutation.isPending}>
            Clear
          </Button>
          <Button leftSection={<IconRefresh size="1rem" />} onClick={() => refetch()} loading={isFetching} variant="light">
            Refresh
          </Button>
        </Group>
      </Group>

      <Group>
        <Card withBorder radius="md" p="sm" style={{ flex: 1 }}>
          {summaryLoading || !summary ? (
            <Group justify="center"><Loader size="sm" /></Group>
          ) : (
            <Group justify="space-between">
              <Group gap="xs">
                <Badge 
                  variant="light" 
                  style={{ 
                    cursor: 'pointer',
                    background: 'rgba(100, 116, 139, 0.25)',
                    border: '1px solid rgba(100, 116, 139, 0.5)',
                    color: '#94a3b8'
                  }} 
                  onClick={() => setFilter('')}
                >
                  Total: {summary.Total}
                </Badge>
                <Badge
                  variant="light"
                  style={{ 
                    cursor: 'pointer',
                    background: 'rgba(251, 191, 36, 0.25)',
                    border: '1px solid rgba(251, 191, 36, 0.5)',
                    color: '#fbbf24'
                  }}
                  onClick={() => setFilter((prev) => upsertFilterField(prev, 'type', 'SKIP'))}
                >
                  Skip: {summary.Skip}
                </Badge>
                <Badge
                  variant="light"
                  style={{ 
                    cursor: 'pointer',
                    background: 'rgba(248, 113, 113, 0.25)',
                    border: '1px solid rgba(248, 113, 113, 0.5)',
                    color: '#f87171'
                  }}
                  onClick={() => setFilter((prev) => upsertFilterField(prev, 'type', 'DONT_MATCH'))}
                >
                  DontMatch: {summary.DontMatch}
                </Badge>
                <Badge
                  variant="light"
                  style={{
                    cursor: 'pointer',
                    background: 'rgba(251, 146, 60, 0.25)',
                    border: '1px solid rgba(251, 146, 60, 0.5)',
                    color: '#fb923c'
                  }}
                  onClick={() => setFilter((prev) => upsertFilterField(prev, 'type', 'MISSING_SECTION'))}
                >
                  MissingSection: {summary.MissingSection}
                </Badge>
                <Badge
                  variant="light"
                  style={{ 
                    cursor: 'pointer',
                    background: 'rgba(168, 85, 247, 0.25)',
                    border: '1px solid rgba(168, 85, 247, 0.5)',
                    color: '#a855f7'
                  }}
                  onClick={() => setFilter((prev) => upsertFilterField(prev, 'type', 'NUKE'))}
                >
                  Nuke: {summary.Nuke}
                </Badge>
              </Group>
              <Text size="xs" c="dimmed">Window: {formatWindowSeconds(summary.WindowSeconds)}</Text>
            </Group>
          )}
        </Card>

        <TextInput
          placeholder='Filter: free text tokens, or e.g. type:MISSING_SECTION site:SomeSite'
          leftSection={<IconSearch size="1rem" />}
          rightSection={
            <Tooltip
              label={'Filter syntax: use `field:value` (e.g. `type:MISSING_SECTION site:MySite`).\nMultiple tokens are combined with AND.\nFields: type:, site:, section:, release:, reason:, event:.\nValues with spaces: wrap in "quotes".'}
              withArrow
              withinPortal
            >
              <IconAlertCircle size="1rem" style={{ opacity: 0.6 }} />
            </Tooltip>
          }
          value={filter}
          onChange={(e) => setFilter(e.currentTarget.value)}
          style={{ width: 340 }}
        />
      </Group>

      {error && (
        <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
          {error.message}
        </Alert>
      )}

      <Card withBorder radius="md" p={0}>
        {isLoading && !data ? (
          <Group justify="center" p="md"><Loader size="md" /></Group>
        ) : (
          <ScrollArea h="calc(100vh - 260px)">
            <Table striped highlightOnHover withTableBorder style={{ tableLayout: 'auto' }}>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th style={{ width: 1, whiteSpace: 'nowrap' }}>Time</Table.Th>
                  <Table.Th style={{ minWidth: 90, whiteSpace: 'nowrap' }}>Type</Table.Th>
                  <Table.Th>Section</Table.Th>
                  <Table.Th>Release</Table.Th>
                  <Table.Th>Site</Table.Th>
                  <Table.Th>Event</Table.Th>
                  <Table.Th>Reason</Table.Th>
                  <Table.Th style={{ width: 80 }}>Actions</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {filtered.map((i) => (
                  <Table.Tr key={`${i.Id}`}>
                    <Table.Td style={{ whiteSpace: 'nowrap' }}>
                      <Text size="xs" style={{ whiteSpace: 'nowrap' }}>
                        {i.TsUnix
                          ? `${new Date(i.TsUnix * 1000).toLocaleDateString()} ${new Date(i.TsUnix * 1000).toLocaleTimeString()}`
                          : ''}
                      </Text>
                    </Table.Td>
                    <Table.Td style={{ minWidth: 90, whiteSpace: 'nowrap' }}>
                      <Badge
                        variant="light"
                        size="sm"
                        style={{ 
                          whiteSpace: 'nowrap', 
                          maxWidth: 'none',
                          ...typeBadgeStyle(i.IssueType)
                        }}
                      >
                        {i.IssueType}
                      </Badge>
                    </Table.Td>
                    <Table.Td><Text size="xs">{i.Section}</Text></Table.Td>
                    <Table.Td>
                      <Tooltip label={i.ReleaseName} withArrow withinPortal disabled={!i.ReleaseName || i.ReleaseName.length <= 60}>
                        <Text
                          size="xs"
                          style={{
                            maxWidth: '600px',
                            overflow: 'hidden',
                            textOverflow: 'ellipsis',
                            whiteSpace: 'nowrap',
                            direction: 'rtl',
                            textAlign: 'left',
                            unicodeBidi: 'plaintext'
                          }}
                        >
                          {i.ReleaseName}
                        </Text>
                      </Tooltip>
                    </Table.Td>
                    <Table.Td><Text size="xs">{i.SiteName}</Text></Table.Td>
                    <Table.Td><Text size="xs" c="dimmed">{i.KbEvent}</Text></Table.Td>
                    <Table.Td><Text size="xs">{i.Reason}</Text></Table.Td>
                    <Table.Td>
                      <Group gap="xs">
                        {i.IssueType?.toUpperCase() === 'MISSING_SECTION' && (
                          <Tooltip label="Add missing section">
                            <ActionIcon
                              variant="light"
                              color="green"
                              size="sm"
                              onClick={() => handleOpenAddSection(i)}
                            >
                              <IconPlus size="1rem" />
                            </ActionIcon>
                          </Tooltip>
                        )}
                        {(i.IssueType?.toUpperCase() === 'SKIP' || i.IssueType?.toUpperCase() === 'DONT_MATCH' || i.IssueType?.toUpperCase() === 'DONTMATCH') && i.SiteName && (
                          <Tooltip label="Go to rules for this site">
                            <ActionIcon
                              variant="light"
                              color="blue"
                              size="sm"
                              onClick={() => navigate(`/rules?site=${encodeURIComponent(i.SiteName)}`)}
                            >
                              <IconBook size="1rem" />
                            </ActionIcon>
                          </Tooltip>
                        )}
                      </Group>
                    </Table.Td>
                  </Table.Tr>
                ))}
                {filtered.length === 0 && (
                  <Table.Tr>
                    <Table.Td colSpan={8}>
                      <Text size="sm" c="dimmed" ta="center" p="md">
                        No issues found.
                      </Text>
                    </Table.Td>
                  </Table.Tr>
                )}
              </Table.Tbody>
            </Table>
          </ScrollArea>
        )}
      </Card>

      <Modal
        opened={addSectionModalOpened}
        onClose={() => {
          setAddSectionModalOpened(false);
          setSectionPath('');
          setAddDropRule(false);
          setSelectedIssue(null);
        }}
        title="Add Missing Section"
        size="md"
      >
        <Stack gap="md">
          <TextInput
            label="Site"
            value={selectedIssue?.SiteName || ''}
            disabled
          />
          <TextInput
            label="Section"
            value={selectedIssue?.Section || ''}
            disabled
          />
          <TextInput
            label="Release"
            value={selectedIssue?.ReleaseName || ''}
            disabled
          />
          <TextInput
            label="Section Path"
            placeholder="e.g. /TV-720P-BLURAY-DE/"
            value={sectionPath}
            onChange={(e) => setSectionPath(e.currentTarget.value)}
            autoFocus
            rightSection={
              <ActionIcon variant="subtle" onClick={openBrowser} aria-label="Browse">
                <IconFolderOpen size="1rem" />
              </ActionIcon>
            }
            onKeyDown={(e) => {
              if (e.key === 'Enter' && sectionPath.trim()) {
                handleAddSection();
              }
            }}
          />
          {sectionPathSuggestions.length > 0 && (
            <Stack gap={4}>
              <Text size="xs" c="dimmed">Existing paths on this site:</Text>
              <Group gap="xs">
                {sectionPathSuggestions.map((s) => (
                  <Tooltip
                    key={s.dir}
                    label={s.sections.join(', ')}
                    withArrow
                    withinPortal
                  >
                    <Pill
                      style={{ cursor: 'pointer' }}
                      onClick={() => setSectionPath(s.dir)}
                    >
                      {s.dir}
                    </Pill>
                  </Tooltip>
                ))}
              </Group>
            </Stack>
          )}
          <Tooltip
            label="Typical use case: affil-only sections"
            withArrow
          >
            <Checkbox
              label="Add drop rule (if default then DROP)"
              checked={addDropRule}
              onChange={(e) => setAddDropRule(e.currentTarget.checked)}
            />
          </Tooltip>
          <Group justify="flex-end" mt="md">
            <Button variant="light" onClick={() => setAddSectionModalOpened(false)}>
              Cancel
            </Button>
            <Button
              onClick={handleAddSection}
              loading={addSectionMutation.isPending}
              disabled={!sectionPath.trim()}
            >
              Add Section
            </Button>
          </Group>
        </Stack>
      </Modal>

      <Modal opened={browserOpen} onClose={() => setBrowserOpen(false)} title={`Browse ${selectedIssue?.SiteName}`} size="xl">
        <Stack gap="sm">
          <Group justify="space-between">
            <Group gap="xs">
              <ActionIcon
                variant="light"
                onClick={() => navigateBrowserPath(browserPath.split('/').slice(0, -1).join('/') || '/')}
                aria-label="Up one level"
              >
                <IconArrowUp size="1rem" />
              </ActionIcon>
              <ActionIcon
                variant="light"
                onClick={handleBrowserRefresh}
                aria-label="Refresh"
              >
                <IconRefresh size="1rem" />
              </ActionIcon>
            </Group>
            <Button
              leftSection={<IconPlus size="1rem" />}
              onClick={() => {
                setSectionPath(browserPath);
                setBrowserOpen(false);
              }}
            >
              Use this path
            </Button>
          </Group>

          <Breadcrumbs separator={null}>
            {breadcrumbItems.map((item) => (
              <Button
                key={item.path}
                variant="subtle"
                size="compact-sm"
                onClick={() => navigateBrowserPath(item.path)}
              >
                {item.label}
              </Button>
            ))}
          </Breadcrumbs>

          {(browserLoading || browserRefetching) && (
            <Center h={120}><Loader size="md" /></Center>
          )}

          {!browserLoading && browserError && (
            <Alert color="red" title="Browser error">
              {browserError instanceof Error ? browserError.message : 'Failed to load directory.'}
            </Alert>
          )}

          {!browserLoading && !browserError && (
            <Table striped highlightOnHover withTableBorder withColumnBorders>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th
                    style={{ cursor: 'pointer', userSelect: 'none' }}
                    onClick={() => handleBrowserSort('name')}
                  >
                    <Group gap={4} wrap="nowrap">
                      <Text size="sm" fw={600}>Directory</Text>
                      {browserSortIndicator(browserSortBy === 'name', browserSortDir)}
                    </Group>
                  </Table.Th>
                  <Table.Th
                    style={{ cursor: 'pointer', userSelect: 'none', whiteSpace: 'nowrap' }}
                    onClick={() => handleBrowserSort('modified')}
                  >
                    <Group gap={4} wrap="nowrap">
                      <Text size="sm" fw={600}>Modified</Text>
                      {browserSortIndicator(browserSortBy === 'modified', browserSortDir)}
                    </Group>
                  </Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {browserDirs.length === 0 && (
                  <Table.Tr>
                    <Table.Td colSpan={2}>
                      <Text size="sm" c="dimmed">No folders or symlinks in this path.</Text>
                    </Table.Td>
                  </Table.Tr>
                )}
                {visibleBrowserDirs.map((dir) => (
                  <Table.Tr key={dir.name} style={{ cursor: 'pointer' }} onClick={() => navigateBrowserPath(`${browserPath === '/' ? '' : browserPath}/${dir.name}`)}>
                    <Table.Td>
                      <Group gap="xs">
                        {dir.type === 'LINK' ? <IconLink size="1rem" /> : <IconFolderOpen size="1rem" />}
                        <Tooltip label={dir.type === 'LINK' && dir.link_target ? `${dir.name} -> ${dir.link_target}` : dir.name} withArrow withinPortal>
                          <Text fw={600}>{dir.name}</Text>
                        </Tooltip>
                      </Group>
                    </Table.Td>
                    <Table.Td>
                      <Text size="sm" c="dimmed">{(dir as any).last_modified || (dir as any).date || '-'}</Text>
                    </Table.Td>
                  </Table.Tr>
                ))}
                {hasHiddenBrowserDirs && (
                  <Table.Tr>
                    <Table.Td colSpan={2}>
                      <Group justify="space-between" wrap="nowrap">
                        <Text size="sm" c="dimmed">
                          Showing {visibleBrowserDirs.length} of {browserDirs.length} entries
                        </Text>
                        <Button
                          size="xs"
                          variant="light"
                          onClick={() => setBrowserRenderLimit((prev) => Math.min(prev + BROWSER_RENDER_LIMIT_STEP, browserDirs.length))}
                        >
                          Load {Math.min(BROWSER_RENDER_LIMIT_STEP, browserDirs.length - visibleBrowserDirs.length)} more
                        </Button>
                      </Group>
                    </Table.Td>
                  </Table.Tr>
                )}
              </Table.Tbody>
            </Table>
          )}
        </Stack>
      </Modal>
    </Stack>
  );
}
