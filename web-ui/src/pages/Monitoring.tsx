import { useEffect, useMemo, useRef, useState } from 'react';
import {
  Alert,
  Badge,
  Card,
  Center,
  Group,
  Loader,
  ScrollArea,
  Stack,
  Table,
  Text,
  TextInput,
  Title,
} from '@mantine/core';
import { useQuery } from '@tanstack/react-query';
import { IconAlertCircle, IconSearch } from '@tabler/icons-react';
import { fetchSlotsRuntime, type SiteSlotsRuntime, type SlotRuntime } from '../api/client';

const STREAM_RETRY_MS = 50;
const STREAM_TIMEOUT_MS = 15000;
const FALLBACK_REFRESH_MS = 1000;

type StreamMode = 'stream' | 'fallback';

function parseSseMessage(raw: string): { event: string; data: string } | null {
  const lines = raw.split(/\r?\n/);
  let eventName = 'message';
  const dataLines: string[] = [];

  for (const line of lines) {
    if (line.startsWith('event:')) {
      eventName = line.slice(6).trim();
    } else if (line.startsWith('data:')) {
      dataLines.push(line.slice(5).trimStart());
    }
  }

  if (dataLines.length === 0) return null;
  return { event: eventName, data: dataLines.join('\n') };
}

export function Monitoring() {
  const [filter, setFilter] = useState('');
  const [isVisible, setIsVisible] = useState<boolean>(document.visibilityState === 'visible');
  const [streamMode, setStreamMode] = useState<StreamMode>('stream');
  const [sitesData, setSitesData] = useState<SiteSlotsRuntime[] | null>(null);
  const [lastUpdateAt, setLastUpdateAt] = useState<number | null>(null);
  const lastHashRef = useRef('');

  useEffect(() => {
    const onVisibilityChange = () => setIsVisible(document.visibilityState === 'visible');
    document.addEventListener('visibilitychange', onVisibilityChange);
    return () => document.removeEventListener('visibilitychange', onVisibilityChange);
  }, []);

  useEffect(() => {
    if (!isVisible) return;

    let stopped = false;
    let retryTimer: number | null = null;

    const apiBase = (import.meta.env.VITE_API_BASE_URL || '/api').replace(/\/+$/, '');

    const openStream = async () => {
      if (stopped) return;

      try {
        const token = localStorage.getItem('apiToken');
        const params = new URLSearchParams();
        if (lastHashRef.current) params.set('hash', lastHashRef.current);
        params.set('timeout_ms', String(STREAM_TIMEOUT_MS));

        const response = await fetch(`${apiBase}/sites/slots/stream?${params.toString()}`, {
          method: 'GET',
          cache: 'no-store',
          headers: token ? { Authorization: `Bearer ${token}` } : undefined,
        });

        if (!response.ok) {
          throw new Error(`Stream HTTP ${response.status}`);
        }

        const raw = await response.text();
        const message = parseSseMessage(raw);
        if (message?.data) {
          const payload = JSON.parse(message.data) as { hash?: string; slots?: SiteSlotsRuntime[] };
          if (payload.hash) {
            lastHashRef.current = String(payload.hash);
          }
          if (message.event === 'slots' && Array.isArray(payload.slots)) {
            setSitesData(payload.slots);
            setLastUpdateAt(Date.now());
          }
        }

        setStreamMode('stream');
      } catch {
        setStreamMode('fallback');
      } finally {
        if (!stopped && isVisible) {
          retryTimer = window.setTimeout(openStream, STREAM_RETRY_MS);
        }
      }
    };

    openStream();

    return () => {
      stopped = true;
      if (retryTimer !== null) {
        window.clearTimeout(retryTimer);
      }
    };
  }, [isVisible]);

  const fallbackQuery = useQuery({
    queryKey: ['slots-runtime-fallback'],
    queryFn: () => fetchSlotsRuntime(''),
    enabled: streamMode === 'fallback' && isVisible,
    refetchInterval: streamMode === 'fallback' && isVisible ? FALLBACK_REFRESH_MS : false,
    refetchOnWindowFocus: false,
    staleTime: 0,
  });

  useEffect(() => {
    if (streamMode === 'fallback' && fallbackQuery.data) {
      setSitesData(fallbackQuery.data);
      setLastUpdateAt(Date.now());
    }
  }, [streamMode, fallbackQuery.data]);

  const data = sitesData || [];

  const filteredSites = useMemo(() => {
    const list = data.slice().sort((a, b) => a.site.localeCompare(b.site));
    const term = filter.trim().toLowerCase();

    return list
      .map((site) => {
        const slots = (site.slots || [])
          .slice()
          .sort((a, b) => a.slot - b.slot)
          .filter((slot) => {
            if (!term) return true;
            return (
              site.site.toLowerCase().includes(term) ||
              String(slot.slot).includes(term) ||
              (slot.action || '').toLowerCase().includes(term) ||
              slot.task.toLowerCase().includes(term)
            );
          });
        return { ...site, slots };
      })
      .filter((site) => site.slots.length > 0 || site.locked);
  }, [data, filter]);

  const isLoading = !sitesData && (streamMode === 'stream' || fallbackQuery.isLoading);
  const hasError = streamMode === 'fallback' && !sitesData && !!fallbackQuery.error;

  if (isLoading) {
    return (
      <Center h={300}>
        <Loader size="lg" />
      </Center>
    );
  }

  if (hasError) {
    return (
      <Alert color="red" icon={<IconAlertCircle size="1.1rem" />} title="Monitoring unavailable">
        {(fallbackQuery.error as Error).message}
      </Alert>
    );
  }

  return (
    <Stack gap="md">
      <Group justify="space-between" align="flex-end">
        <div>
          <Title order={3}>Monitoring</Title>
          <Text size="sm" c="dimmed">
            Slot number and current action ({streamMode === 'stream' ? 'SSE stream' : 'fallback polling'})
          </Text>
        </div>
        <Group gap="xs">
          <Badge color={streamMode === 'stream' ? 'green' : 'yellow'} variant="light">
            {streamMode === 'stream' ? 'stream' : 'fallback'}
          </Badge>
          <Badge variant="light">
            Last update: {lastUpdateAt ? new Date(lastUpdateAt).toLocaleTimeString() : '-'}
          </Badge>
        </Group>
      </Group>

      <Group>
        <TextInput
          leftSection={<IconSearch size="1rem" />}
          placeholder="Filter site/slot/action"
          value={filter}
          onChange={(event) => setFilter(event.currentTarget.value)}
          style={{ flex: 1, minWidth: 260 }}
        />
      </Group>

      {filteredSites.length === 0 ? (
        <Text size="sm" c="dimmed">No slots match the current filter.</Text>
      ) : (
        filteredSites.map((site) => (
          <Card key={site.site} withBorder>
            <Stack gap="xs">
              <Group justify="space-between">
                <Group gap="xs">
                  <Title order={5}>{site.site}</Title>
                  {site.locked && <Badge color="yellow">lock timeout</Badge>}
                </Group>
                <Text size="sm" c="dimmed">
                  {site.slots.length} slots
                </Text>
              </Group>

              <ScrollArea>
                <Table highlightOnHover withTableBorder withColumnBorders>
                  <Table.Thead>
                    <Table.Tr>
                      <Table.Th style={{ width: 72 }}>Slot</Table.Th>
                      <Table.Th>Action</Table.Th>
                    </Table.Tr>
                  </Table.Thead>
                  <Table.Tbody>
                    {site.slots.map((slot: SlotRuntime) => (
                      <Table.Tr key={`${site.site}-${slot.slot}`}>
                        <Table.Td style={{ width: 72, whiteSpace: 'nowrap' }}>
                          <Text size="sm" ff="monospace">{slot.slot}</Text>
                        </Table.Td>
                        <Table.Td>
                          <Text
                            size="sm"
                            ff="monospace"
                            style={{ whiteSpace: 'normal', overflowWrap: 'anywhere' }}
                          >
                            {slot.action?.trim() ? slot.action : '-'}
                          </Text>
                        </Table.Td>
                      </Table.Tr>
                    ))}
                  </Table.Tbody>
                </Table>
              </ScrollArea>
            </Stack>
          </Card>
        ))
      )}
    </Stack>
  );
}
