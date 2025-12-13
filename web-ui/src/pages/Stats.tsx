import { ActionIcon, Alert, Badge, Card, Center, Group, Loader, SegmentedControl, Select, SimpleGrid, Stack, Switch, Table, Text, Title, Tooltip } from '@mantine/core';
import { IconAlertCircle, IconRefresh } from '@tabler/icons-react';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { useMemo, useState } from 'react';
import { apiClient } from '../api/client';

type Period = 'DAY' | 'MONTH' | 'YEAR';

type RaceDirEntry = {
  site: string;
  bytes: number;
  files: number;
};

type RaceSiteStats = {
  name: string;
  inBytes: number;
  outBytes: number;
  inFiles: number;
  outFiles: number;
  inBySite?: RaceDirEntry[];
  outBySite?: RaceDirEntry[];
};

type RaceStatsResponse = {
  enabled: boolean;
  site: string;
  period: Period;
  sqlPeriod?: string;
  detailed: boolean;
  error?: string;
  sites?: RaceSiteStats[];
  totalBytes?: number;
  totalFiles?: number;
};

const formatBytes = (value: number) => {
  if (!Number.isFinite(value)) return '-';
  const abs = Math.abs(value);
  const units = ['B', 'KB', 'MB', 'GB', 'TB', 'PB'];
  let unitIndex = 0;
  let n = abs;
  while (n >= 1024 && unitIndex < units.length - 1) {
    n /= 1024;
    unitIndex += 1;
  }
  const sign = value < 0 ? '-' : '';
  const digits = unitIndex === 0 ? 0 : 2;
  return `${sign}${n.toFixed(digits)} ${units[unitIndex]}`;
};

export function Stats() {
  const queryClient = useQueryClient();
  const [site, setSite] = useState<string>('*');
  const [period, setPeriod] = useState<Period>('DAY');
  const [detailed, setDetailed] = useState(false);

  const { data: sitesList } = useQuery({
    queryKey: ['sites-for-stats'],
    queryFn: async (): Promise<string[]> => {
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: '' });

      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }

      const rawSites = responseData?.Sites;
      if (!rawSites) return [];

      try {
        const parsed = typeof rawSites === 'string' ? JSON.parse(rawSites) : rawSites;
        if (!Array.isArray(parsed)) return [];
        return parsed.map((s: any) => s?.name).filter((n: any) => typeof n === 'string' && n.length > 0);
      } catch {
        return [];
      }
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const siteOptions = useMemo(() => {
    const names = Array.from(new Set(sitesList ?? [])).sort((a, b) => a.localeCompare(b));
    return [
      { value: '*', label: 'All sites (*)' },
      ...names.map((name) => ({ value: name, label: name })),
    ];
  }, [sitesList]);

  const { data, isLoading, error, isFetching } = useQuery({
    queryKey: ['race-stats', site, period, detailed],
    queryFn: async (): Promise<RaceStatsResponse> => {
      const res = await apiClient.post('/ApiStatsService/GetRaceStats', { SiteName: site, Period: period, Detailed: detailed });

      let payload: any = res.data;
      if (payload?.result && Array.isArray(payload.result)) {
        payload = payload.result[0];
      }
      if (typeof payload === 'string') {
        payload = JSON.parse(payload);
      }
      return payload as RaceStatsResponse;
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const statsSites = data?.sites ?? [];
  const totals = useMemo(() => {
    const totalInBytes = statsSites.reduce((acc, s) => acc + (s.inBytes ?? 0), 0);
    const totalOutBytes = statsSites.reduce((acc, s) => acc + (s.outBytes ?? 0), 0);
    const totalInFiles = statsSites.reduce((acc, s) => acc + (s.inFiles ?? 0), 0);
    const totalOutFiles = statsSites.reduce((acc, s) => acc + (s.outFiles ?? 0), 0);
    return {
      totalInBytes,
      totalOutBytes,
      totalInFiles,
      totalOutFiles,
      totalBytes: totalInBytes + totalOutBytes,
      totalFiles: totalInFiles + totalOutFiles,
    };
  }, [statsSites]);

  const sortedSites = useMemo(() => {
    const copy = [...statsSites];
    copy.sort((a, b) => (b.inBytes + b.outBytes) - (a.inBytes + a.outBytes));
    return copy;
  }, [statsSites]);

  const selectedSite = site === '*' ? null : statsSites[0] ?? null;

  if (isLoading) return <Center h={400}><Loader size="xl" /></Center>;

  if (error) return (
    <Alert icon={<IconAlertCircle size="1rem" />} title="Connection Error" color="red">
      Could not load stats from slftp API.
      <br />
      Error: {error.message}
    </Alert>
  );

  if (!data?.enabled) return (
    <Alert icon={<IconAlertCircle size="1rem" />} title="Stats Disabled" color="yellow">
      {data?.error || 'Stats are disabled.'}
    </Alert>
  );

  return (
    <Stack>
      <Title order={2}>Stats</Title>

      <Card withBorder radius="md" padding="lg">
        <Group justify="space-between" align="flex-end" wrap="wrap">
          <Group wrap="wrap">
            <Select
              label="Site"
              data={siteOptions}
              value={site}
              onChange={(v) => setSite(v || '*')}
              searchable
              w={240}
            />

            <div>
              <Text size="sm" fw={500} mb={4}>Period</Text>
              <SegmentedControl
                value={period}
                onChange={(v) => setPeriod(v as Period)}
                data={[
                  { value: 'DAY', label: 'Day' },
                  { value: 'MONTH', label: 'Month' },
                  { value: 'YEAR', label: 'Year' },
                ]}
              />
            </div>

            <Switch
              label="Detailed"
              checked={detailed}
              onChange={(e) => setDetailed(e.currentTarget.checked)}
              mt={24}
            />
          </Group>

          <Tooltip label="Refresh">
            <ActionIcon
              variant="default"
              size="lg"
              loading={isFetching}
              onClick={() => queryClient.invalidateQueries({ queryKey: ['race-stats'] })}
              aria-label="Refresh stats"
            >
              <IconRefresh size="1.2rem" />
            </ActionIcon>
          </Tooltip>
        </Group>

        <Text size="xs" c="dimmed" mt="sm">
          Note: Results depend on server settings (e.g. `slftp.ini` → `[stats] delete_after_days` retention and `[stats] min_filesize` filtering).
          If `delete_after_days` is set, `MONTH`/`YEAR` may include less data than expected.
        </Text>

        {data?.error && (
          <Alert mt="md" icon={<IconAlertCircle size="1rem" />} title="Stats Error" color="yellow">
            {data.error}
          </Alert>
        )}
      </Card>

      <SimpleGrid cols={{ base: 1, sm: 3 }}>
        <Card withBorder radius="md" padding="lg">
          <Text size="xs" c="dimmed" fw={700} tt="uppercase">Total In</Text>
          <Text fw={700} size="xl">{formatBytes(totals.totalInBytes)}</Text>
          <Text size="sm" c="dimmed">{totals.totalInFiles} files</Text>
        </Card>
        <Card withBorder radius="md" padding="lg">
          <Text size="xs" c="dimmed" fw={700} tt="uppercase">Total Out</Text>
          <Text fw={700} size="xl">{formatBytes(totals.totalOutBytes)}</Text>
          <Text size="sm" c="dimmed">{totals.totalOutFiles} files</Text>
        </Card>
        <Card withBorder radius="md" padding="lg">
          <Text size="xs" c="dimmed" fw={700} tt="uppercase">Total In + Out</Text>
          <Text fw={700} size="xl">{formatBytes(totals.totalBytes)}</Text>
          <Group gap="xs">
            <Text size="sm" c="dimmed">{totals.totalFiles} files</Text>
            <Badge variant="light">{statsSites.length} sites</Badge>
            {data?.sqlPeriod && <Badge variant="light">{data.sqlPeriod}</Badge>}
          </Group>
        </Card>
      </SimpleGrid>

      {site === '*' && (
        <Card withBorder radius="md" padding="lg">
          <Group justify="space-between" mb="sm">
            <Title order={4}>Sites Overview</Title>
            <Text size="sm" c="dimmed">Sorted by total traffic</Text>
          </Group>

          <Table striped highlightOnHover withTableBorder withColumnBorders>
            <Table.Thead>
              <Table.Tr>
                <Table.Th>Site</Table.Th>
                <Table.Th style={{ textAlign: 'right' }}>Total</Table.Th>
                <Table.Th style={{ textAlign: 'right' }}>In</Table.Th>
                <Table.Th style={{ textAlign: 'right' }}>Out</Table.Th>
                <Table.Th style={{ textAlign: 'right' }}>Files (in/out)</Table.Th>
              </Table.Tr>
            </Table.Thead>
            <Table.Tbody>
              {sortedSites.map((s) => {
                const totalBytes = s.inBytes + s.outBytes;
                return (
                  <Table.Tr key={s.name}>
                    <Table.Td>{s.name}</Table.Td>
                    <Table.Td style={{ textAlign: 'right' }}>{formatBytes(totalBytes)}</Table.Td>
                    <Table.Td style={{ textAlign: 'right' }}>{formatBytes(s.inBytes)}</Table.Td>
                    <Table.Td style={{ textAlign: 'right' }}>{formatBytes(s.outBytes)}</Table.Td>
                    <Table.Td style={{ textAlign: 'right' }}>{s.inFiles}/{s.outFiles}</Table.Td>
                  </Table.Tr>
                );
              })}
            </Table.Tbody>
          </Table>
        </Card>
      )}

      {selectedSite && (
        <Card withBorder radius="md" padding="lg">
          <Group justify="space-between" mb="sm">
            <Title order={4}>Site Details: {selectedSite.name}</Title>
            <Badge variant="light">{period}</Badge>
          </Group>
          <SimpleGrid cols={{ base: 1, sm: 2 }}>
            <Card withBorder radius="md" padding="md">
              <Text size="xs" c="dimmed" fw={700} tt="uppercase">In</Text>
              <Text fw={700} size="lg">{formatBytes(selectedSite.inBytes)}</Text>
              <Text size="sm" c="dimmed">{selectedSite.inFiles} files</Text>
            </Card>
            <Card withBorder radius="md" padding="md">
              <Text size="xs" c="dimmed" fw={700} tt="uppercase">Out</Text>
              <Text fw={700} size="lg">{formatBytes(selectedSite.outBytes)}</Text>
              <Text size="sm" c="dimmed">{selectedSite.outFiles} files</Text>
            </Card>
          </SimpleGrid>
        </Card>
      )}

      {selectedSite && detailed && (
        <SimpleGrid cols={{ base: 1, sm: 2 }}>
          <Card withBorder radius="md" padding="lg">
            <Title order={5} mb="sm">Inbound (from)</Title>
            <Table striped highlightOnHover withTableBorder>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Site</Table.Th>
                  <Table.Th style={{ textAlign: 'right' }}>Traffic</Table.Th>
                  <Table.Th style={{ textAlign: 'right' }}>Files</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {(selectedSite.inBySite ?? []).map((e) => (
                  <Table.Tr key={e.site}>
                    <Table.Td>{e.site}</Table.Td>
                    <Table.Td style={{ textAlign: 'right' }}>{formatBytes(e.bytes)}</Table.Td>
                    <Table.Td style={{ textAlign: 'right' }}>{e.files}</Table.Td>
                  </Table.Tr>
                ))}
                {(selectedSite.inBySite ?? []).length === 0 && (
                  <Table.Tr>
                    <Table.Td colSpan={3}><Text c="dimmed">No data</Text></Table.Td>
                  </Table.Tr>
                )}
              </Table.Tbody>
            </Table>
          </Card>

          <Card withBorder radius="md" padding="lg">
            <Title order={5} mb="sm">Outbound (to)</Title>
            <Table striped highlightOnHover withTableBorder>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Site</Table.Th>
                  <Table.Th style={{ textAlign: 'right' }}>Traffic</Table.Th>
                  <Table.Th style={{ textAlign: 'right' }}>Files</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {(selectedSite.outBySite ?? []).map((e) => (
                  <Table.Tr key={e.site}>
                    <Table.Td>{e.site}</Table.Td>
                    <Table.Td style={{ textAlign: 'right' }}>{formatBytes(e.bytes)}</Table.Td>
                    <Table.Td style={{ textAlign: 'right' }}>{e.files}</Table.Td>
                  </Table.Tr>
                ))}
                {(selectedSite.outBySite ?? []).length === 0 && (
                  <Table.Tr>
                    <Table.Td colSpan={3}><Text c="dimmed">No data</Text></Table.Td>
                  </Table.Tr>
                )}
              </Table.Tbody>
            </Table>
          </Card>
        </SimpleGrid>
      )}
    </Stack>
  );
}
