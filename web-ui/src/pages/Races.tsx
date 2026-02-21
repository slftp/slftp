import { Alert, Badge, Button, Card, Group, Loader, Pagination, ScrollArea, Stack, Switch, Table, Text, TextInput, Title } from '@mantine/core';
import { IconAlertCircle, IconArrowLeft, IconChevronDown, IconChevronUp, IconRefresh, IconSearch } from '@tabler/icons-react';
import { useQuery } from '@tanstack/react-query';
import { useEffect, useMemo, useState, type CSSProperties, type ReactNode } from 'react';
import { useNavigate, useParams } from 'react-router-dom';
import { apiClient } from '../api/client';

type RaceLine = {
  Id: number;
  TsUnix: number;
  SrcSite: string;
  DstSite: string;
  Section: string;
  Release: string;
  FileName: string;
  SizeBytes: number;
};

type ReleaseSummary = {
  Release: string;
  Section: string;
  EarliestTsUnix: number;
  LatestTsUnix: number;
  TransferCount: number;
  TotalSizeBytes: number;
};

type ReleaseSortField = 'EarliestTsUnix' | 'LatestTsUnix' | 'Release' | 'Section' | 'TransferCount' | 'TotalSizeBytes';
type LineSortField = 'TsUnix' | 'SrcSite' | 'DstSite' | 'Section' | 'FileName' | 'SizeBytes';
type SortDirection = 'asc' | 'desc';

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

function formatBytes(bytes?: number): string {
  if (!bytes || !Number.isFinite(bytes) || bytes <= 0) return '';
  const units = ['B', 'KiB', 'MiB', 'GiB', 'TiB'];
  let value = bytes;
  let idx = 0;
  while (value >= 1024 && idx < units.length - 1) {
    value /= 1024;
    idx++;
  }
  return `${value.toFixed(idx === 0 ? 0 : 2)} ${units[idx]}`;
}

function formatTs(tsUnix?: number): string {
  if (!tsUnix) return '';
  const d = new Date(tsUnix * 1000);
  return `${d.toLocaleDateString()} ${d.toLocaleTimeString()}`;
}

function renderSectionBadge(section?: string) {
  const value = (section || '').trim();
  if (!value) {
    return (
      <Badge color="red" variant="filled" size="xs" radius="sm">
        NO SECTION
      </Badge>
    );
  }

  return (
    <Badge
      size="xs"
      variant="light"
      radius="sm"
      styles={{
        root: {
          background: 'var(--nav-hover-bg)',
          border: '1px solid var(--nav-active-border)',
          color: 'var(--primary-light)',
        },
      }}
    >
      {value}
    </Badge>
  );
}

export function Races() {
  const navigate = useNavigate();
  const { releaseName } = useParams<{ releaseName?: string }>();
  const selectedRelease = useMemo(() => {
    if (!releaseName) return '';
    try {
      return decodeURIComponent(releaseName);
    } catch {
      return releaseName;
    }
  }, [releaseName]);
  const isReleaseView = selectedRelease.length > 0;

  const [autoRefresh, setAutoRefresh] = useState(true);
  const [page, setPage] = useState(1);
  const [releasePage, setReleasePage] = useState(1);
  const [releaseFilter, setReleaseFilter] = useState('');
  const [lineFilter, setLineFilter] = useState('');

  const [releaseSortField, setReleaseSortField] = useState<ReleaseSortField>('LatestTsUnix');
  const [releaseSortDirection, setReleaseSortDirection] = useState<SortDirection>('desc');
  const [lineSortField, setLineSortField] = useState<LineSortField>('TsUnix');
  const [lineSortDirection, setLineSortDirection] = useState<SortDirection>('desc');

  const pageSize = 500;
  const maxPages = 5;

  const {
    data,
    isLoading,
    error,
    refetch,
    isFetching,
  } = useQuery({
    queryKey: ['races', page],
    enabled: !isReleaseView,
    queryFn: async () => {
      const res = await apiClient.post('/ApiRacesService/GetRaces', { Page: page, PageSize: pageSize, SinceUnix: 0 });
      let result = res.data;
      if (res.data?.result && Array.isArray(res.data.result)) {
        result = res.data.result[0];
      }
      let parsed: any = result;
      if (typeof result === 'string') {
        try {
          parsed = JSON.parse(result);
        } catch {
          parsed = {};
        }
      }
      const items = parsed?.items ? parseMaybeJsonArray(parsed.items) : parseMaybeJsonArray(parsed);
      return items as RaceLine[];
    },
    refetchInterval: autoRefresh && !isReleaseView ? 30000 : false,
    refetchOnWindowFocus: false,
  });

  const {
    data: releaseData,
    isLoading: releaseLoading,
    error: releaseError,
    refetch: refetchRelease,
    isFetching: releaseFetching,
  } = useQuery({
    queryKey: ['releaseTransfers', selectedRelease, releasePage],
    enabled: isReleaseView,
    queryFn: async () => {
      const res = await apiClient.post('/ApiRacesService/GetReleaseTransfers', {
        Release: selectedRelease,
        Page: releasePage,
        PageSize: pageSize,
        SinceUnix: 0,
      });
      let result = res.data;
      if (res.data?.result && Array.isArray(res.data.result)) {
        result = res.data.result[0];
      }
      let parsed: any = result;
      if (typeof result === 'string') {
        try {
          parsed = JSON.parse(result);
        } catch {
          parsed = {};
        }
      }
      const items = parsed?.items ? parseMaybeJsonArray(parsed.items) : parseMaybeJsonArray(parsed);
      return items as RaceLine[];
    },
    refetchInterval: autoRefresh && isReleaseView ? 30000 : false,
    refetchOnWindowFocus: false,
  });

  const races = Array.isArray(data) ? data : [];
  const releaseTransfers = Array.isArray(releaseData) ? releaseData : [];
  const shouldCheckReleaseNextPage = isReleaseView && releaseTransfers.length === pageSize && releasePage < maxPages;

  const { data: releaseNextPageData, isFetching: releaseNextPageFetching } = useQuery({
    queryKey: ['releaseTransfersNext', selectedRelease, releasePage],
    enabled: shouldCheckReleaseNextPage,
    queryFn: async () => {
      const res = await apiClient.post('/ApiRacesService/GetReleaseTransfers', {
        Release: selectedRelease,
        Page: releasePage + 1,
        PageSize: pageSize,
        SinceUnix: 0,
      });
      let result = res.data;
      if (res.data?.result && Array.isArray(res.data.result)) {
        result = res.data.result[0];
      }
      let parsed: any = result;
      if (typeof result === 'string') {
        try {
          parsed = JSON.parse(result);
        } catch {
          parsed = {};
        }
      }
      const items = parsed?.items ? parseMaybeJsonArray(parsed.items) : parseMaybeJsonArray(parsed);
      return items as RaceLine[];
    },
    refetchOnWindowFocus: false,
  });
  const hasReleaseNextPage = Array.isArray(releaseNextPageData) && releaseNextPageData.length > 0;

  const releaseTotalPages = useMemo(() => {
    if (!isReleaseView) return maxPages;
    if (releaseLoading) return Math.max(1, releasePage);

    if (releaseTransfers.length === 0) {
      if (releasePage > 1) return releasePage - 1;
      return 1;
    }

    if (releaseTransfers.length < pageSize) return releasePage;
    if (releaseNextPageFetching) return Math.min(releasePage + 1, maxPages);
    return hasReleaseNextPage ? Math.min(releasePage + 1, maxPages) : releasePage;
  }, [isReleaseView, releaseLoading, releasePage, releaseTransfers.length, releaseNextPageFetching, hasReleaseNextPage]);

  useEffect(() => {
    if (!isReleaseView) return;
    if (releasePage <= releaseTotalPages) return;
    setReleasePage(releaseTotalPages);
  }, [isReleaseView, releasePage, releaseTotalPages]);

  const releaseSummaries = useMemo(() => {
    const byRelease = new Map<string, ReleaseSummary>();

    for (const row of races) {
      const name = (row.Release || '').trim();
      if (!name) continue;

      const ts = row.TsUnix || 0;

      const existing = byRelease.get(name);
      if (!existing) {
        byRelease.set(name, {
          Release: name,
          Section: row.Section || '',
          EarliestTsUnix: ts,
          LatestTsUnix: ts,
          TransferCount: 1,
          TotalSizeBytes: row.SizeBytes || 0,
        });
        continue;
      }

      existing.TransferCount += 1;
      existing.TotalSizeBytes += row.SizeBytes || 0;
      if (ts < existing.EarliestTsUnix) {
        existing.EarliestTsUnix = ts;
      }
      if (ts > existing.LatestTsUnix) {
        existing.LatestTsUnix = ts;
        existing.Section = row.Section || existing.Section;
      }
    }

    return Array.from(byRelease.values());
  }, [races]);

  const filteredReleases = useMemo(() => {
    const q = releaseFilter.trim().toLowerCase();
    let result = releaseSummaries;

    if (q) {
      result = releaseSummaries.filter((r) => {
        const release = (r.Release || '').toLowerCase();
        const section = (r.Section || '').toLowerCase();
        return release.includes(q) || section.includes(q);
      });
    }

    return [...result].sort((a, b) => {
      let av: string | number = '';
      let bv: string | number = '';

      switch (releaseSortField) {
        case 'EarliestTsUnix':
          av = a.EarliestTsUnix || 0;
          bv = b.EarliestTsUnix || 0;
          break;
        case 'LatestTsUnix':
          av = a.LatestTsUnix || 0;
          bv = b.LatestTsUnix || 0;
          break;
        case 'Release':
          av = (a.Release || '').toLowerCase();
          bv = (b.Release || '').toLowerCase();
          break;
        case 'Section':
          av = (a.Section || '').toLowerCase();
          bv = (b.Section || '').toLowerCase();
          break;
        case 'TransferCount':
          av = a.TransferCount || 0;
          bv = b.TransferCount || 0;
          break;
        case 'TotalSizeBytes':
          av = a.TotalSizeBytes || 0;
          bv = b.TotalSizeBytes || 0;
          break;
      }

      if (av < bv) return releaseSortDirection === 'asc' ? -1 : 1;
      if (av > bv) return releaseSortDirection === 'asc' ? 1 : -1;
      return 0;
    });
  }, [releaseSummaries, releaseFilter, releaseSortField, releaseSortDirection]);

  const filteredTransfers = useMemo(() => {
    const q = lineFilter.trim().toLowerCase();
    let result = releaseTransfers;

    if (q) {
      result = releaseTransfers.filter((r) => {
        const fileName = (r.FileName || '').toLowerCase();
        const srcSite = (r.SrcSite || '').toLowerCase();
        const dstSite = (r.DstSite || '').toLowerCase();
        const section = (r.Section || '').toLowerCase();
        return fileName.includes(q) || srcSite.includes(q) || dstSite.includes(q) || section.includes(q);
      });
    }

    return [...result].sort((a, b) => {
      let av: string | number = '';
      let bv: string | number = '';

      switch (lineSortField) {
        case 'TsUnix':
          av = a.TsUnix || 0;
          bv = b.TsUnix || 0;
          break;
        case 'SrcSite':
          av = (a.SrcSite || '').toLowerCase();
          bv = (b.SrcSite || '').toLowerCase();
          break;
        case 'DstSite':
          av = (a.DstSite || '').toLowerCase();
          bv = (b.DstSite || '').toLowerCase();
          break;
        case 'Section':
          av = (a.Section || '').toLowerCase();
          bv = (b.Section || '').toLowerCase();
          break;
        case 'FileName':
          av = (a.FileName || '').toLowerCase();
          bv = (b.FileName || '').toLowerCase();
          break;
        case 'SizeBytes':
          av = a.SizeBytes || 0;
          bv = b.SizeBytes || 0;
          break;
      }

      if (av < bv) return lineSortDirection === 'asc' ? -1 : 1;
      if (av > bv) return lineSortDirection === 'asc' ? 1 : -1;
      return 0;
    });
  }, [releaseTransfers, lineFilter, lineSortField, lineSortDirection]);

  const handleReleaseSort = (field: ReleaseSortField) => {
    if (releaseSortField === field) {
      setReleaseSortDirection(releaseSortDirection === 'asc' ? 'desc' : 'asc');
      return;
    }
    setReleaseSortField(field);
    setReleaseSortDirection('asc');
  };

  const handleLineSort = (field: LineSortField) => {
    if (lineSortField === field) {
      setLineSortDirection(lineSortDirection === 'asc' ? 'desc' : 'asc');
      return;
    }
    setLineSortField(field);
    setLineSortDirection('asc');
  };

  const ReleaseSortHeader = ({
    field,
    children,
    style,
  }: {
    field: ReleaseSortField;
    children: ReactNode;
    style?: CSSProperties;
  }) => (
    <Table.Th
      style={{ ...style, cursor: 'pointer', userSelect: 'none' }}
      onClick={() => handleReleaseSort(field)}
    >
      <Group gap={4} wrap="nowrap">
        {children}
        {releaseSortField === field && (releaseSortDirection === 'asc' ? <IconChevronUp size={14} /> : <IconChevronDown size={14} />)}
      </Group>
    </Table.Th>
  );

  const LineSortHeader = ({
    field,
    children,
    style,
  }: {
    field: LineSortField;
    children: ReactNode;
    style?: CSSProperties;
  }) => (
    <Table.Th
      style={{ ...style, cursor: 'pointer', userSelect: 'none' }}
      onClick={() => handleLineSort(field)}
    >
      <Group gap={4} wrap="nowrap">
        {children}
        {lineSortField === field && (lineSortDirection === 'asc' ? <IconChevronUp size={14} /> : <IconChevronDown size={14} />)}
      </Group>
    </Table.Th>
  );

  const refresh = () => {
    if (isReleaseView) {
      void refetchRelease();
      return;
    }
    void refetch();
  };

  return (
    <Stack>
      <Group justify="space-between" align="center">
        <Stack gap={2}>
          <Title order={2}>{isReleaseView ? 'Race Release' : 'Races'}</Title>
          {isReleaseView && (
            <Text size="sm" c="dimmed">{selectedRelease}</Text>
          )}
        </Stack>
        <Group>
          <Switch
            label="Auto-refresh (30s)"
            checked={autoRefresh}
            onChange={(e) => setAutoRefresh(e.currentTarget.checked)}
          />
          <Button leftSection={<IconRefresh size="1rem" />} onClick={refresh} loading={isReleaseView ? releaseFetching : isFetching} variant="light">
            Refresh
          </Button>
        </Group>
      </Group>

      {!isReleaseView ? (
        <>
          <Group>
            <TextInput
              placeholder="Search releases or section..."
              leftSection={<IconSearch size="1rem" />}
              value={releaseFilter}
              onChange={(e) => setReleaseFilter(e.currentTarget.value)}
              style={{ width: 360 }}
            />
            <Pagination total={maxPages} value={page} onChange={setPage} size="sm" />
            <Text size="xs" c="dimmed">
              {releaseFilter ? `Showing ${filteredReleases.length} matching releases (Total: ${releaseSummaries.length})` : `Total: ${releaseSummaries.length} releases`}
            </Text>
            <Text size="xs" c="dimmed">
              Page {page}/{maxPages} · {pageSize}/page
            </Text>
          </Group>

          {error && (
            <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
              {(error as any).message || 'Failed to load races'}
            </Alert>
          )}

          <Card withBorder radius="md" p={0}>
            {isLoading && !data ? (
              <Group justify="center" p="md"><Loader size="md" /></Group>
            ) : (
              <ScrollArea h="calc(100vh - 220px)">
                <Table striped highlightOnHover withTableBorder style={{ tableLayout: 'auto' }}>
                  <Table.Thead>
                    <Table.Tr>
                      <ReleaseSortHeader field="Release">Release</ReleaseSortHeader>
                      <ReleaseSortHeader field="EarliestTsUnix" style={{ width: 1, whiteSpace: 'nowrap' }}>Start</ReleaseSortHeader>
                      <ReleaseSortHeader field="LatestTsUnix" style={{ width: 1, whiteSpace: 'nowrap' }}>End</ReleaseSortHeader>
                      <ReleaseSortHeader field="Section" style={{ width: 140 }}>Section</ReleaseSortHeader>
                      <ReleaseSortHeader field="TransferCount" style={{ width: 120 }}>Transfers</ReleaseSortHeader>
                      <ReleaseSortHeader field="TotalSizeBytes" style={{ width: 140 }}>Size</ReleaseSortHeader>
                    </Table.Tr>
                  </Table.Thead>
                  <Table.Tbody>
                    {filteredReleases.map((r) => (
                      <Table.Tr key={r.Release}>
                        <Table.Td>
                          <Text
                            size="xs"
                            fw={600}
                            style={{ cursor: 'pointer' }}
                            onClick={() => {
                              setReleasePage(1);
                              setLineFilter('');
                              navigate(`/races/${encodeURIComponent(r.Release)}`);
                            }}
                          >
                            {r.Release}
                          </Text>
                        </Table.Td>
                        <Table.Td style={{ whiteSpace: 'nowrap' }}>
                          <Text size="xs" style={{ whiteSpace: 'nowrap' }}>
                            {formatTs(r.EarliestTsUnix)}
                          </Text>
                        </Table.Td>
                        <Table.Td style={{ whiteSpace: 'nowrap' }}>
                          <Text size="xs" style={{ whiteSpace: 'nowrap' }}>
                            {formatTs(r.LatestTsUnix)}
                          </Text>
                        </Table.Td>
                        <Table.Td>
                          {renderSectionBadge(r.Section)}
                        </Table.Td>
                        <Table.Td>
                          <Text size="xs">{r.TransferCount}</Text>
                        </Table.Td>
                        <Table.Td>
                          <Text size="xs">{formatBytes(r.TotalSizeBytes)}</Text>
                        </Table.Td>
                      </Table.Tr>
                    ))}
                  </Table.Tbody>
                </Table>
              </ScrollArea>
            )}
          </Card>
        </>
      ) : (
        <>
          <Group justify="space-between" align="center">
            <Button
              variant="light"
              leftSection={<IconArrowLeft size="1rem" />}
              onClick={() => navigate('/races')}
            >
              Back to Releases
            </Button>
            <Pagination total={releaseTotalPages} value={releasePage} onChange={setReleasePage} size="sm" />
          </Group>

          <Group>
            <TextInput
              placeholder="Search files, section or sites..."
              leftSection={<IconSearch size="1rem" />}
              value={lineFilter}
              onChange={(e) => setLineFilter(e.currentTarget.value)}
              style={{ width: 360 }}
            />
            <Text size="xs" c="dimmed">
              {lineFilter ? `Showing ${filteredTransfers.length} matching lines (Total: ${releaseTransfers.length})` : `Total: ${releaseTransfers.length} lines`}
            </Text>
            <Text size="xs" c="dimmed">
              Page {releasePage}/{releaseTotalPages} · {pageSize}/page
            </Text>
          </Group>

          {releaseError && (
            <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
              {(releaseError as any).message || 'Failed to load release transfers'}
            </Alert>
          )}

          <Card withBorder radius="md" p={0}>
            {releaseLoading && !releaseData ? (
              <Group justify="center" p="md"><Loader size="md" /></Group>
            ) : (
              <ScrollArea h="calc(100vh - 300px)">
                <Table striped highlightOnHover withTableBorder style={{ tableLayout: 'auto' }}>
                  <Table.Thead>
                    <Table.Tr>
                      <LineSortHeader field="TsUnix" style={{ width: 1, whiteSpace: 'nowrap' }}>Time</LineSortHeader>
                      <LineSortHeader field="SrcSite">From → To</LineSortHeader>
                      <LineSortHeader field="Section" style={{ width: 120 }}>Section</LineSortHeader>
                      <LineSortHeader field="FileName">File</LineSortHeader>
                      <LineSortHeader field="SizeBytes" style={{ width: 140 }}>Size</LineSortHeader>
                    </Table.Tr>
                  </Table.Thead>
                  <Table.Tbody>
                    {filteredTransfers.map((t) => (
                      <Table.Tr key={`rel-${t.Id}`}>
                        <Table.Td style={{ whiteSpace: 'nowrap' }}>
                          <Text size="xs" style={{ whiteSpace: 'nowrap' }}>
                            {formatTs(t.TsUnix)}
                          </Text>
                        </Table.Td>
                        <Table.Td>
                          <Group gap={6} wrap="nowrap">
                            <Badge
                              variant="light"
                              styles={{
                                root: { maxWidth: 'none', whiteSpace: 'nowrap', textOverflow: 'clip', minWidth: '50px', textAlign: 'center', background: 'rgba(99, 102, 241, 0.35)', border: '1px solid rgba(99, 102, 241, 0.6)', color: '#fff' },
                                label: { overflow: 'visible', textOverflow: 'clip' },
                              }}
                            >
                              {(t.SrcSite && t.SrcSite.trim()) ? t.SrcSite.trim() : 'unknown'}
                            </Badge>
                            <Text size="xs" c="dimmed">→</Text>
                            <Badge
                              variant="light"
                              styles={{
                                root: { maxWidth: 'none', whiteSpace: 'nowrap', textOverflow: 'clip', minWidth: '50px', textAlign: 'center', background: 'rgba(168, 85, 247, 0.35)', border: '1px solid rgba(168, 85, 247, 0.6)', color: '#fff' },
                                label: { overflow: 'visible', textOverflow: 'clip' },
                              }}
                            >
                              {(t.DstSite && t.DstSite.trim()) ? t.DstSite.trim() : 'unknown'}
                            </Badge>
                          </Group>
                        </Table.Td>
                        <Table.Td>
                          {renderSectionBadge(t.Section)}
                        </Table.Td>
                        <Table.Td>
                          <Text size="xs" style={{ fontFamily: 'monospace' }}>{t.FileName || ''}</Text>
                        </Table.Td>
                        <Table.Td>
                          <Text size="xs">{formatBytes(t.SizeBytes)}</Text>
                        </Table.Td>
                      </Table.Tr>
                    ))}
                  </Table.Tbody>
                </Table>
              </ScrollArea>
            )}
          </Card>
        </>
      )}
    </Stack>
  );
}
