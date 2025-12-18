import { useEffect, useMemo, useState } from 'react';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { apiClient, fetchBrowserPath } from '../api/client';
import {
  ActionIcon,
  Alert,
  Anchor,
  Badge,
  Breadcrumbs,
  Center,
  Checkbox,
  Divider,
  Group,
  Loader,
  Paper,
  Select,
  Stack,
  Table,
  Text,
  TextInput,
  ThemeIcon,
  Tooltip,
} from '@mantine/core';
import {
  IconArrowRight,
  IconArrowUp,
  IconChevronDown,
  IconChevronUp,
  IconFile,
  IconFolder,
  IconFolderOpen,
  IconInfoCircle,
  IconRefresh,
  IconSelector,
  IconX,
} from '@tabler/icons-react';
import type { BrowserResponse, FileEntry } from '../api/client';

type SitesListItem = { name: string };
type GetSitesResponse = {
  Sites?: string | SitesListItem[];
  result?: Array<{ Sites?: string | SitesListItem[] }>;
};

type SortBy = 'name' | 'uid' | 'size' | 'modified';
type SortDir = 'asc' | 'desc';

interface FileBrowserPaneProps {
  site?: string | null;
  path?: string;
  onSiteChange?: (site: string | null) => void;
  onPathChange?: (path: string) => void;
  onSelectionChange?: (files: FileEntry[]) => void;
}

function _parseModifiedMs(aFile: FileEntry): number | null {
  const record = aFile as unknown as Record<string, unknown>;
  const candidates = [
    record.mtime,
    record.mtime_ms,
    record.modified,
    record.modified_at,
    record.date,
    record.time,
    record.timestamp,
  ];

  for (const value of candidates) {
    if (typeof value === 'number' && Number.isFinite(value)) {
      return value < 1_000_000_000_000 ? value * 1000 : value;
    }
    if (typeof value === 'string' && value.trim() !== '') {
      const ms = Date.parse(value);
      if (!Number.isNaN(ms)) return ms;

      // Common FTP-style date formats that are not reliably parsed by Date.parse
      // e.g. "12-18 01:45" (MM-DD HH:MM) -> assume current year, local time
      const m1 = value.match(/^(\d{2})-(\d{2})\s+(\d{2}):(\d{2})$/);
      if (m1) {
        const year = new Date().getFullYear();
        const month = Number(m1[1]);
        const day = Number(m1[2]);
        const hour = Number(m1[3]);
        const minute = Number(m1[4]);
        if ([month, day, hour, minute].every((n) => Number.isFinite(n))) {
          const d = new Date(year, month - 1, day, hour, minute, 0, 0);
          if (!Number.isNaN(d.getTime())) return d.getTime();
        }
      }
    }
  }

  return null;
}

function _formatModified(aMs: number | null): string {
  if (!aMs) return '—';
  try {
    return new Intl.DateTimeFormat(undefined, {
      year: '2-digit',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
    }).format(new Date(aMs));
  } catch {
    return new Date(aMs).toLocaleString();
  }
}

function _toggleSort(aCurrentBy: SortBy, aCurrentDir: SortDir, aNextBy: SortBy): { by: SortBy; dir: SortDir } {
  if (aCurrentBy !== aNextBy) return { by: aNextBy, dir: 'asc' };
  return { by: aCurrentBy, dir: aCurrentDir === 'asc' ? 'desc' : 'asc' };
}

function _sortIndicator(aActive: boolean, aDir: SortDir) {
  if (!aActive) return <IconSelector size="0.9rem" />;
  return aDir === 'asc' ? <IconChevronUp size="0.9rem" /> : <IconChevronDown size="0.9rem" />;
}

function _splitFilename(aName: string): { base: string; ext: string } {
  const idx = aName.lastIndexOf('.');
  if (idx <= 0 || idx === aName.length - 1) return { base: aName, ext: '' };
  return { base: aName.slice(0, idx), ext: aName.slice(idx) };
}

export function FileBrowserPane({
  site: propSite = null,
  path: propPath = '/',
  onSiteChange,
  onPathChange,
  onSelectionChange,
}: FileBrowserPaneProps) {
  const [internalSite, setInternalSite] = useState<string | null>(propSite);
  const [internalPath, setInternalPath] = useState<string>(propPath);
  const [inputPath, setInputPath] = useState<string>(propPath);
  const [selectedFiles, setSelectedFiles] = useState<Set<string>>(new Set());
  const [sortBy, setSortBy] = useState<SortBy>('modified');
  const [sortDir, setSortDir] = useState<SortDir>('desc');
  const queryClient = useQueryClient();

  useEffect(() => {
    if (propSite !== undefined) setInternalSite(propSite);
  }, [propSite]);

  useEffect(() => {
    if (propPath !== undefined) {
      setInternalPath(propPath);
      setInputPath(propPath);
    }
  }, [propPath]);

  const handleSiteChange = (val: string | null) => {
    setInternalSite(val);
    onSiteChange?.(val);
    const newPath = '/';
    setInternalPath(newPath);
    onPathChange?.(newPath);
    setSelectedFiles(new Set());
  };

  const handlePathNavigate = (newPath: string) => {
    let p = newPath.startsWith('/') ? newPath : '/' + newPath;
    if (p.length > 1 && p.endsWith('/')) p = p.slice(0, -1);
    setInternalPath(p);
    onPathChange?.(p);
    setSelectedFiles(new Set());
  };

  const { data: sitesData, error: sitesError, isLoading: sitesLoading } = useQuery<SitesListItem[]>({
    queryKey: ['sitesList'],
    queryFn: async () => {
      const res = await apiClient.post<GetSitesResponse>('/ApiSitesService/GetSites', { Filter: '' });
      const responseData = (Array.isArray(res.data?.result) ? res.data.result[0] : res.data) ?? {};
      const rawSites = responseData.Sites;

      const normalize = (value: unknown): SitesListItem[] => {
        if (!Array.isArray(value)) return [];
        return value.filter((s): s is SitesListItem => {
          return typeof s === 'object' && s !== null && 'name' in s && typeof (s as { name?: unknown }).name === 'string';
        });
      };

      if (typeof rawSites === 'string') {
        try {
          return normalize(JSON.parse(rawSites) as unknown);
        } catch {
          return [];
        }
      }

      return normalize(rawSites);
    },
  });

  const siteOptions = sitesData ? sitesData.map((s) => ({ value: s.name, label: s.name })) : [];

  const { data: browserData, isLoading, error, isRefetching } = useQuery({
    queryKey: ['browser', internalSite, internalPath],
    queryFn: async (): Promise<BrowserResponse | null> => {
      if (!internalSite) return null;
      return fetchBrowserPath(internalSite, internalPath);
    },
    enabled: !!internalSite,
    refetchInterval: (query) => {
      const data = query.state.data;
      return (data?.status === 'pending' ? 1000 : false);
    },
  });

  useEffect(() => {
    if (!onSelectionChange || !browserData?.files) return;
    const selectedEntries = browserData.files.filter((f) => selectedFiles.has(f.name));
    onSelectionChange(selectedEntries);
  }, [selectedFiles, browserData?.files, onSelectionChange]);

  const handleRefresh = () => {
    if (!internalSite) return;
    fetchBrowserPath(internalSite, internalPath, true).then(() => {
      queryClient.invalidateQueries({ queryKey: ['browser', internalSite, internalPath] });
    });
  };

  const handleUp = () => {
    if (internalPath === '/') return;
    const parts = internalPath.split('/').filter((p) => p);
    parts.pop();
    handlePathNavigate(parts.length > 0 ? '/' + parts.join('/') : '/');
  };

  const clearSelection = () => {
    setSelectedFiles(new Set());
  };

  const toggleSelection = (fileName: string) => {
    const newSet = new Set(selectedFiles);
    if (newSet.has(fileName)) newSet.delete(fileName);
    else newSet.add(fileName);
    setSelectedFiles(newSet);
  };

  const formatSize = (bytes: number) => {
    if (bytes === 0) return '0 B';
    const k = 1024;
    const sizes = ['B', 'KB', 'MB', 'GB', 'TB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
  };

  const pathParts = internalPath.split('/').filter((p) => p);
  const selectedCount = selectedFiles.size;
  const canNavigateUp = internalPath !== '/';

  const filesRaw: FileEntry[] = browserData?.status === 'ready' && browserData.files ? browserData.files : [];
  const files = useMemo(() => {
    return filesRaw.filter((f) => {
      const name = (f as unknown as { name?: unknown }).name;
      if (typeof name !== 'string') return false;
      if (name.trim() === '') return false;
      if (name.startsWith('.')) return false;

      const size = (f as unknown as { size?: unknown }).size;
      if (typeof size !== 'number' || !Number.isFinite(size)) return false;
      return true;
    });
  }, [filesRaw]);

  const sortedFiles = useMemo(() => {
    const decorated = files.map((f) => ({ f, modifiedMs: _parseModifiedMs(f) }));
    decorated.sort((a, b) => {
      if (a.f.is_dir !== b.f.is_dir) return a.f.is_dir ? -1 : 1;

      let cmp = 0;
      if (sortBy === 'name') {
        cmp = a.f.name.localeCompare(b.f.name, undefined, { sensitivity: 'base' });
      } else if (sortBy === 'uid') {
        const au = (a.f.user || '').toString();
        const bu = (b.f.user || '').toString();
        cmp = au.localeCompare(bu, undefined, { sensitivity: 'base' });
      } else if (sortBy === 'size') {
        const as = a.f.is_dir ? -1 : a.f.size;
        const bs = b.f.is_dir ? -1 : b.f.size;
        cmp = as - bs;
      } else if (sortBy === 'modified') {
        const am = a.modifiedMs;
        const bm = b.modifiedMs;
        if (am === null && bm === null) cmp = 0;
        else if (am === null) cmp = 1;
        else if (bm === null) cmp = -1;
        else cmp = am - bm;
      } else {
        cmp = 0;
      }

      if (cmp !== 0) return sortDir === 'asc' ? cmp : -cmp;
      return a.f.name.localeCompare(b.f.name, undefined, { sensitivity: 'base' });
    });
    return decorated;
  }, [files, sortBy, sortDir]);

  return (
    <Stack gap="sm" h="100%" mih={0}>
      <Paper withBorder p="sm" radius="md">
        <Stack gap="xs">
          <Group gap="xs" justify="space-between" align="flex-end" wrap="nowrap">
            <Select
              placeholder={sitesLoading ? 'Loading...' : 'Select Site'}
              data={siteOptions}
              value={internalSite}
              onChange={handleSiteChange}
              searchable
              disabled={sitesLoading}
              rightSection={sitesLoading ? <Loader size="xs" /> : null}
              style={{ flex: 1 }}
            />

            <Group gap="xs" wrap="nowrap">
              <Tooltip label="Go up" withArrow withinPortal>
                <ActionIcon
                  variant="light"
                  size="lg"
                  onClick={handleUp}
                  disabled={!internalSite || !canNavigateUp}
                >
                  <IconArrowUp size="1.1rem" />
                </ActionIcon>
              </Tooltip>

              <Tooltip label="Refresh" withArrow withinPortal>
                <ActionIcon
                  variant="light"
                  size="lg"
                  onClick={handleRefresh}
                  loading={isRefetching || (browserData?.status === 'pending')}
                  disabled={!internalSite}
                >
                  <IconRefresh size="1.1rem" />
                </ActionIcon>
              </Tooltip>
            </Group>
          </Group>

          <Group gap="xs" wrap="nowrap">
            <TextInput
              value={inputPath}
              onChange={(e) => setInputPath(e.currentTarget.value)}
              onKeyDown={(e) => {
                if (e.key === 'Enter') handlePathNavigate(inputPath);
              }}
              style={{ flex: 1 }}
              leftSection={<IconFolderOpen size="0.9rem" />}
              rightSection={
                <Tooltip label="Go" withArrow withinPortal>
                  <ActionIcon size="md" variant="subtle" onClick={() => handlePathNavigate(inputPath)}>
                    <IconArrowRight size="1rem" />
                  </ActionIcon>
                </Tooltip>
              }
            />

            <Group gap="xs" wrap="nowrap">
              <Badge variant="light" color={selectedCount > 0 ? 'blue' : 'gray'}>
                {selectedCount} selected
              </Badge>
              <Tooltip label="Clear selection" withArrow withinPortal>
                <ActionIcon size="md" variant="subtle" onClick={clearSelection} disabled={selectedCount === 0}>
                  <IconX size="1rem" />
                </ActionIcon>
              </Tooltip>
            </Group>
          </Group>

          <Group gap="xs" wrap="nowrap">
            <Breadcrumbs separator=">" style={{ fontSize: '0.8rem', flex: 1, minWidth: 0 }}>
              <Anchor component="button" onClick={() => handlePathNavigate('/')}>
                root
              </Anchor>
              {pathParts.map((part, index) => {
                const p = '/' + pathParts.slice(0, index + 1).join('/');
                return (
                  <Anchor key={p} component="button" onClick={() => handlePathNavigate(p)}>
                    {part}
                  </Anchor>
                );
              })}
            </Breadcrumbs>
            <Tooltip label="Ctrl/Cmd-click toggles selection" withArrow withinPortal>
              <ThemeIcon variant="light" color="gray" size="md">
                <IconInfoCircle size="1rem" />
              </ThemeIcon>
            </Tooltip>
          </Group>
        </Stack>
      </Paper>

      {sitesError && <Alert color="red" p="xs">{(sitesError as Error).message}</Alert>}
      {error && <Alert color="red" p="xs">{(error as Error).message}</Alert>}
      {browserData?.status === 'error' && <Alert color="red" p="xs">{browserData.message}</Alert>}

      <Paper withBorder radius="md" style={{ flex: 1, minHeight: 0, display: 'flex', flexDirection: 'column' }}>
        {(isLoading || (browserData?.status === 'pending')) ? (
          <Center style={{ flex: 1 }}>
            <Stack align="center" gap="xs">
              <Loader size="md" />
              <Text size="sm" c="dimmed">Loading...</Text>
            </Stack>
          </Center>
        ) : browserData?.status === 'ready' && browserData.files ? (
          <Table.ScrollContainer
            minWidth={0}
            maxHeight="100%"
            scrollAreaProps={{ offsetScrollbars: true, type: 'auto' }}
            style={{ flex: 1 }}
          >
            <Table
              striped
              highlightOnHover
              stickyHeader
              layout="fixed"
              horizontalSpacing="sm"
              verticalSpacing="xs"
              styles={{
                tr: {
                  '&[data-selected]': {
                    backgroundColor: 'var(--mantine-primary-color-light)',
                  },
                },
                td: { whiteSpace: 'nowrap' },
                th: { whiteSpace: 'nowrap' },
              }}
            >
              <Table.Thead>
                <Table.Tr>
                  <Table.Th style={{ width: 30 }} />
                  <Table.Th
                    onClick={() => {
                      const next = _toggleSort(sortBy, sortDir, 'name');
                      setSortBy(next.by);
                      setSortDir(next.dir);
                    }}
                    style={{ cursor: 'pointer' }}
                  >
                    <Group gap={6} wrap="nowrap">
                      <Text size="sm" fw={600}>Name</Text>
                      {_sortIndicator(sortBy === 'name', sortDir)}
                    </Group>
                  </Table.Th>
                  <Table.Th
                    onClick={() => {
                      const next = _toggleSort(sortBy, sortDir, 'uid');
                      setSortBy(next.by);
                      setSortDir(next.dir);
                    }}
                    style={{ width: 90, cursor: 'pointer', textAlign: 'right' }}
                  >
                    <Group gap={6} wrap="nowrap" justify="flex-end">
                      <Text size="sm" fw={600}>UID</Text>
                      {_sortIndicator(sortBy === 'uid', sortDir)}
                    </Group>
                  </Table.Th>
                  <Table.Th
                    onClick={() => {
                      const next = _toggleSort(sortBy, sortDir, 'size');
                      setSortBy(next.by);
                      setSortDir(next.dir);
                    }}
                    style={{ width: 110, cursor: 'pointer', textAlign: 'right' }}
                  >
                    <Group gap={6} wrap="nowrap" justify="flex-end">
                      <Text size="sm" fw={600}>Size</Text>
                      {_sortIndicator(sortBy === 'size', sortDir)}
                    </Group>
                  </Table.Th>
                  <Table.Th
                    onClick={() => {
                      const next = _toggleSort(sortBy, sortDir, 'modified');
                      setSortBy(next.by);
                      setSortDir(next.dir);
                    }}
                    style={{ width: 150, cursor: 'pointer' }}
                  >
                    <Group gap={6} wrap="nowrap">
                      <Text size="sm" fw={600}>Modified</Text>
                      {_sortIndicator(sortBy === 'modified', sortDir)}
                    </Group>
                  </Table.Th>
                </Table.Tr>
              </Table.Thead>

              <Table.Tbody>
                {internalPath !== '/' && (
                  <Table.Tr style={{ cursor: 'pointer' }} onClick={handleUp}>
                    <Table.Td colSpan={5}>
                      <Group gap="xs">
                        <ThemeIcon color="gray" variant="light" size="sm">
                          <IconArrowUp size="0.9rem" />
                        </ThemeIcon>
                        <Text fw={600} size="sm">..</Text>
                      </Group>
                    </Table.Td>
                  </Table.Tr>
                )}

                {sortedFiles.map(({ f, modifiedMs }, idx) => (
                  <Table.Tr
                    key={`${f.name}-${idx}`}
                    style={{ cursor: f.is_dir ? 'pointer' : 'default', userSelect: 'none' }}
                    data-selected={selectedFiles.has(f.name) || undefined}
                    onClick={(e) => {
                      if (e.ctrlKey || e.metaKey) {
                        toggleSelection(f.name);
                        return;
                      }
                      if (f.is_dir) {
                        handlePathNavigate(internalPath + (internalPath === '/' ? '' : '/') + f.name);
                      } else {
                        toggleSelection(f.name);
                      }
                    }}
                  >
                    <Table.Td>
                      <Checkbox
                        checked={selectedFiles.has(f.name)}
                        onChange={() => toggleSelection(f.name)}
                        onClick={(e) => e.stopPropagation()}
                        size="xs"
                      />
                    </Table.Td>
                    <Table.Td>
                      <Group gap="xs" wrap="nowrap" style={{ minWidth: 0 }}>
                        <ThemeIcon color={f.is_dir ? 'blue' : 'gray'} variant="light" size="sm">
                          {f.is_dir ? <IconFolder size="0.8rem" /> : <IconFile size="0.8rem" />}
                        </ThemeIcon>
                        <Tooltip label={f.name} withArrow withinPortal>
                          {f.is_dir ? (
                            <Text size="sm" fw={600} truncate style={{ minWidth: 0, flex: 1 }}>
                              {f.name}
                            </Text>
                          ) : (
                            (() => {
                              const parts = _splitFilename(f.name);
                              return (
                                <Text
                                  size="sm"
                                  fw={400}
                                  component="span"
                                  style={{ minWidth: 0, flex: 1, display: 'flex' }}
                                >
                                  <span style={{ minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                                    {parts.base}
                                  </span>
                                  {parts.ext && (
                                    <span style={{ flex: '0 0 auto', whiteSpace: 'nowrap' }}>
                                      {parts.ext}
                                    </span>
                                  )}
                                </Text>
                              );
                            })()
                          )}
                        </Tooltip>
                      </Group>
                    </Table.Td>
                    <Table.Td>
                      {(() => {
                        const uid = (f.user || '').toString();
                        const display = uid.length > 6 ? `${uid.slice(0, 6)}…` : (uid || '—');
                        return (
                          <Tooltip label={uid || '—'} withArrow withinPortal>
                            <Text size="sm" c="dimmed" ta="right">
                              {display}
                            </Text>
                          </Tooltip>
                        );
                      })()}
                    </Table.Td>
                    <Table.Td>
                      <Text size="sm" c="dimmed" ta="right">
                        {f.is_dir ? '—' : formatSize(f.size)}
                      </Text>
                    </Table.Td>
                    <Table.Td>
                      <Text size="sm" c="dimmed">
                        {_formatModified(modifiedMs)}
                      </Text>
                    </Table.Td>
                  </Table.Tr>
                ))}
              </Table.Tbody>
            </Table>
          </Table.ScrollContainer>
        ) : (
          <Center style={{ flex: 1 }}>
            <Stack align="center" gap={4} p="md">
              <ThemeIcon variant="light" size="lg" color="gray">
                <IconFolderOpen size="1.2rem" />
              </ThemeIcon>
              <Text fw={600}>Select a site</Text>
              <Text size="sm" c="dimmed" ta="center">
                Choose a site to browse directories and run commands.
              </Text>
            </Stack>
          </Center>
        )}

        <Divider />
        <Group p="xs" justify="space-between">
          <Text size="xs" c="dimmed">
            {internalSite ? `${internalSite}:${internalPath}` : '—'}
          </Text>
          <Text size="xs" c="dimmed">
            {browserData?.status === 'ready' && browserData.files ? `${browserData.files.length} entries` : ''}
          </Text>
        </Group>
      </Paper>
    </Stack>
  );
}
