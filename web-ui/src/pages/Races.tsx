import { Alert, Badge, Button, Card, Group, Loader, Modal, Pagination, ScrollArea, Stack, Table, Text, TextInput, Title, Switch } from '@mantine/core';
import { IconAlertCircle, IconRefresh, IconSearch, IconChevronDown, IconChevronUp } from '@tabler/icons-react';
import { useQuery } from '@tanstack/react-query';
import { useMemo, useState } from 'react';
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

type SortField = 'TsUnix' | 'SrcSite' | 'DstSite' | 'Section' | 'Release' | 'FileName' | 'SizeBytes';
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

export function Races() {
  const [filter, setFilter] = useState('');
  const [autoRefresh, setAutoRefresh] = useState(true);
  const [page, setPage] = useState(1);
  const pageSize = 500;
  const maxPages = 5;
  const [selectedRelease, setSelectedRelease] = useState<string | null>(null);
  const [releasePage, setReleasePage] = useState(1);
  const [sortField, setSortField] = useState<SortField>('TsUnix');
  const [sortDirection, setSortDirection] = useState<SortDirection>('desc');

  const { data, isLoading, error, refetch, isFetching } = useQuery({
    queryKey: ['races', page],
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
    refetchInterval: autoRefresh ? 30000 : false,
    refetchOnWindowFocus: false,
  });

  const { data: releaseData, isLoading: releaseLoading, error: releaseError, refetch: refetchRelease, isFetching: releaseFetching } = useQuery({
    queryKey: ['releaseTransfers', selectedRelease, releasePage],
    enabled: !!selectedRelease,
    queryFn: async () => {
      const res = await apiClient.post('/ApiRacesService/GetReleaseTransfers', {
        Release: selectedRelease,
        Page: releasePage,
        PageSize: pageSize,
        SinceUnix: 0
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
    refetchInterval: autoRefresh && !!selectedRelease ? 30000 : false,
    refetchOnWindowFocus: false,
  });

  const races = Array.isArray(data) ? data : [];
  const releaseTransfers = Array.isArray(releaseData) ? releaseData : [];

  const filtered = useMemo(() => {
    const q = filter.trim().toLowerCase();
    let result = races;
    
    if (q) {
      result = races.filter((r) => {
        const release = (r.Release || '').toLowerCase();
        const fileName = (r.FileName || '').toLowerCase();
        const srcSite = (r.SrcSite || '').toLowerCase();
        const dstSite = (r.DstSite || '').toLowerCase();
        const section = (r.Section || '').toLowerCase();
        
        // Search in each field separately for better matching
        return release.includes(q) || 
               fileName.includes(q) || 
               srcSite.includes(q) || 
               dstSite.includes(q) || 
               section.includes(q);
      });
    }
    
    // Sort the results
    result = [...result].sort((a, b) => {
      let aVal: string | number = '';
      let bVal: string | number = '';
      
      switch (sortField) {
        case 'TsUnix':
          aVal = a.TsUnix || 0;
          bVal = b.TsUnix || 0;
          break;
        case 'SrcSite':
          aVal = (a.SrcSite || '').toLowerCase();
          bVal = (b.SrcSite || '').toLowerCase();
          break;
        case 'DstSite':
          aVal = (a.DstSite || '').toLowerCase();
          bVal = (b.DstSite || '').toLowerCase();
          break;
        case 'Section':
          aVal = (a.Section || '').toLowerCase();
          bVal = (b.Section || '').toLowerCase();
          break;
        case 'Release':
          aVal = (a.Release || '').toLowerCase();
          bVal = (b.Release || '').toLowerCase();
          break;
        case 'FileName':
          aVal = (a.FileName || '').toLowerCase();
          bVal = (b.FileName || '').toLowerCase();
          break;
        case 'SizeBytes':
          aVal = a.SizeBytes || 0;
          bVal = b.SizeBytes || 0;
          break;
      }
      
      if (aVal < bVal) return sortDirection === 'asc' ? -1 : 1;
      if (aVal > bVal) return sortDirection === 'asc' ? 1 : -1;
      return 0;
    });
    
    return result;
  }, [races, filter, sortField, sortDirection]);

  const handleSort = (field: SortField) => {
    if (sortField === field) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      setSortField(field);
      setSortDirection('asc');
    }
  };

  const SortHeader = ({ field, children, style }: { field: SortField; children: React.ReactNode; style?: React.CSSProperties }) => (
    <Table.Th 
      style={{ ...style, cursor: 'pointer', userSelect: 'none' }} 
      onClick={() => handleSort(field)}
    >
      <Group gap={4} wrap="nowrap">
        {children}
        {sortField === field && (
          sortDirection === 'asc' ? <IconChevronUp size={14} /> : <IconChevronDown size={14} />
        )}
      </Group>
    </Table.Th>
  );

  const formatBytes = (bytes?: number): string => {
    if (!bytes || !Number.isFinite(bytes) || bytes <= 0) return '';
    const units = ['B', 'KiB', 'MiB', 'GiB', 'TiB'];
    let v = bytes;
    let idx = 0;
    while (v >= 1024 && idx < units.length - 1) {
      v /= 1024;
      idx++;
    }
    return `${v.toFixed(idx === 0 ? 0 : 2)} ${units[idx]}`;
  };

  return (
    <Stack>
      <Group justify="space-between" align="center">
        <Title order={2}>Races</Title>
        <Group>
          <Switch
            label="Auto-refresh (30s)"
            checked={autoRefresh}
            onChange={(e) => setAutoRefresh(e.currentTarget.checked)}
          />
          <Button leftSection={<IconRefresh size="1rem" />} onClick={() => refetch()} loading={isFetching} variant="light">
            Refresh
          </Button>
        </Group>
      </Group>

      <Group>
        <TextInput
          placeholder="Search..."
          leftSection={<IconSearch size="1rem" />}
          value={filter}
          onChange={(e) => setFilter(e.currentTarget.value)}
          style={{ width: 360 }}
        />
        <Pagination total={maxPages} value={page} onChange={setPage} size="sm" />
        <Text size="xs" c="dimmed">
          {filter ? `Showing ${filtered.length} matching lines (Total: ${races.length})` : `Total: ${races.length} lines`}
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
                  <SortHeader field="TsUnix" style={{ width: 1, whiteSpace: 'nowrap' }}>Time</SortHeader>
                  <SortHeader field="SrcSite">From → To</SortHeader>
                  <SortHeader field="Release">Release / File</SortHeader>
                  <SortHeader field="Section" style={{ width: 120 }}>Section</SortHeader>
                  <SortHeader field="SizeBytes" style={{ width: 140 }}>Size</SortHeader>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {filtered.map((r) => (
                  <Table.Tr key={`${r.Id}`}>
                    <Table.Td style={{ whiteSpace: 'nowrap' }}>
                      <Text size="xs" style={{ whiteSpace: 'nowrap' }}>
                        {r.TsUnix
                          ? `${new Date(r.TsUnix * 1000).toLocaleDateString()} ${new Date(r.TsUnix * 1000).toLocaleTimeString()}`
                          : ''}
                      </Text>
                    </Table.Td>
                    <Table.Td>
                      <Group gap={6} wrap="nowrap">
                        <Badge variant="light" style={{ maxWidth: 'none', whiteSpace: 'nowrap', background: 'rgba(99, 102, 241, 0.35)', border: '1px solid rgba(99, 102, 241, 0.6)', color: '#fff' }}>
                          {r.SrcSite || '—'}
                        </Badge>
                        <Text size="xs" c="dimmed">→</Text>
                        <Badge variant="light" style={{ maxWidth: 'none', whiteSpace: 'nowrap', background: 'rgba(168, 85, 247, 0.35)', border: '1px solid rgba(168, 85, 247, 0.6)', color: '#fff' }}>
                          {r.DstSite || '—'}
                        </Badge>
                      </Group>
                    </Table.Td>
                    <Table.Td>
                      <Stack gap={2}>
                        <Text
                          size="xs"
                          fw={600}
                          style={{ cursor: 'pointer' }}
                          onClick={() => {
                            setSelectedRelease(r.Release || '');
                            setReleasePage(1);
                          }}
                        >
                          {r.Release || ''}
                        </Text>
                        <Text size="xs" c="dimmed" style={{ fontFamily: 'monospace' }}>{r.FileName || ''}</Text>
                      </Stack>
                    </Table.Td>
                    <Table.Td>
                      <Text size="xs">{r.Section || ''}</Text>
                    </Table.Td>
                    <Table.Td>
                      <Text size="xs">{formatBytes(r.SizeBytes)}</Text>
                    </Table.Td>
                  </Table.Tr>
                ))}
              </Table.Tbody>
            </Table>
          </ScrollArea>
        )}
      </Card>

      <Modal
        opened={!!selectedRelease}
        onClose={() => setSelectedRelease(null)}
        title={`Transfers for: ${selectedRelease || ''}`}
        size="90%"
      >
        <Stack>
          <Group justify="space-between" align="center">
            <Pagination total={maxPages} value={releasePage} onChange={setReleasePage} size="sm" />
            <Group>
              <Button
                leftSection={<IconRefresh size="1rem" />}
                onClick={() => refetchRelease()}
                loading={releaseFetching}
                variant="light"
              >
                Refresh
              </Button>
            </Group>
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
                      <Table.Th style={{ width: 1, whiteSpace: 'nowrap' }}>Time</Table.Th>
                      <Table.Th>From → To</Table.Th>
                      <Table.Th style={{ width: 120 }}>Section</Table.Th>
                      <Table.Th>File</Table.Th>
                      <Table.Th style={{ width: 140 }}>Size</Table.Th>
                    </Table.Tr>
                  </Table.Thead>
                  <Table.Tbody>
                    {releaseTransfers.map((t) => (
                      <Table.Tr key={`rel-${t.Id}`}>
                        <Table.Td style={{ whiteSpace: 'nowrap' }}>
                          <Text size="xs" style={{ whiteSpace: 'nowrap' }}>
                            {t.TsUnix
                              ? `${new Date(t.TsUnix * 1000).toLocaleDateString()} ${new Date(t.TsUnix * 1000).toLocaleTimeString()}`
                              : ''}
                          </Text>
                        </Table.Td>
                        <Table.Td>
                          <Group gap={6} wrap="nowrap">
                            <Badge
                              variant="light"
                              styles={{
                                root: { maxWidth: 'none', whiteSpace: 'nowrap', textOverflow: 'clip', minWidth: '50px', textAlign: 'center', background: 'rgba(99, 102, 241, 0.35)', border: '1px solid rgba(99, 102, 241, 0.6)', color: '#fff' },
                                label: { overflow: 'visible', textOverflow: 'clip' }
                              }}
                            >
                              {(t.SrcSite && t.SrcSite.trim()) ? t.SrcSite.trim() : 'unknown'}
                            </Badge>
                            <Text size="xs" c="dimmed">→</Text>
                            <Badge
                              variant="light"
                              styles={{
                                root: { maxWidth: 'none', whiteSpace: 'nowrap', textOverflow: 'clip', minWidth: '50px', textAlign: 'center', background: 'rgba(168, 85, 247, 0.35)', border: '1px solid rgba(168, 85, 247, 0.6)', color: '#fff' },
                                label: { overflow: 'visible', textOverflow: 'clip' }
                              }}
                            >
                              {(t.DstSite && t.DstSite.trim()) ? t.DstSite.trim() : 'unknown'}
                            </Badge>
                          </Group>
                        </Table.Td>
                        <Table.Td>
                          <Text size="xs">{t.Section || ''}</Text>
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
        </Stack>
      </Modal>
    </Stack>
  );
}
