import { Alert, Badge, Button, Card, Group, Loader, Stack, Switch, Table, Text, TextInput, Title, Autocomplete, ActionIcon, Tooltip, Tabs } from '@mantine/core';
import { IconAlertCircle, IconPlayerPlay, IconWand, IconBolt, IconCpu, IconSettings } from '@tabler/icons-react';
import { useMutation, useQuery } from '@tanstack/react-query';
import { useMemo, useState } from 'react';
import { apiClient } from '../api/client';
import { SpeedTest } from './SpeedTest';
import { ConfigEditor } from './ConfigEditor';

type RecentRelease = {
  ReleaseName: string;
  Section: string;
  Added: number;
  PazoId: number;
  Ready: boolean;
  Stopped: boolean;
};

type SimulatorSiteResult = {
  Sitename: string;
  Section: string;
  Allowed: boolean;
  Reason: string;
  RuleAction: string;
  IsAffil: boolean;
  HasSection: boolean;
  SiteDown: boolean;
  PretimeOk: boolean;
};

type SimulatorRouteResult = {
  SourceSite: string;
  DestinationSite: string;
  Rank: number;
  RouteWeight: number;
};

type SimulatorResponse = {
  success: boolean;
  error?: string;
  simulation?: {
    Releasename: string;
    Section: string;
    TotalSites: number;
    AllowedSites: number;
    ErrorMessage: string;
    Sites: SimulatorSiteResult[] | string;
    Routes: SimulatorRouteResult[] | string;
  };
};

function parseMaybeJsonArray<T = any>(value: unknown): T[] {
  if (Array.isArray(value)) return value as T[];
  if (typeof value === 'string') {
    try {
      const parsed = JSON.parse(value);
      return Array.isArray(parsed) ? (parsed as T[]) : [];
    } catch {
      return [];
    }
  }
  return [];
}

function ReleaseSimulator() {
  const [section, setSection] = useState('');
  const [releaseName, setReleaseName] = useState('');
  const [simulatePre, setSimulatePre] = useState(false);
  const [filter, setFilter] = useState('');

  const { data: recentReleasesData } = useQuery({
    queryKey: ['recent-releases'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSystemService/GetRecentReleases', { Limit: 50 });
      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }
      const rawReleases = responseData.Releases;
      if (!rawReleases) return [];
      try {
        if (typeof rawReleases === 'string') {
          return JSON.parse(rawReleases) as RecentRelease[];
        }
        if (Array.isArray(rawReleases)) {
          return rawReleases as RecentRelease[];
        }
      } catch {
        return [];
      }
      return [];
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const simulateMutation = useMutation({
    mutationFn: async () => {
      const res = await apiClient.post('/ApiSimulatorService/Simulate', { Section: section, ReleaseName: releaseName, SimulatePre: simulatePre });
      if (res.data?.result && Array.isArray(res.data.result)) return res.data.result[0] as SimulatorResponse;
      return res.data as SimulatorResponse;
    },
  });

  const detectSectionMutation = useMutation({
    mutationFn: async (rlsName: string) => {
      const res = await apiClient.post('/ApiSimulatorService/DetectSection', { ReleaseName: rlsName });
      if (res.data?.result && Array.isArray(res.data.result)) return res.data.result[0];
      return res.data;
    },
    onSuccess: (data) => {
      if (data?.success && data?.section) {
        setSection(data.section);
      }
    },
  });

  const sim = simulateMutation.data?.simulation;
  const sites = useMemo(() => parseMaybeJsonArray<SimulatorSiteResult>(sim?.Sites), [sim?.Sites]);
  const routes = useMemo(() => parseMaybeJsonArray<SimulatorRouteResult>(sim?.Routes), [sim?.Routes]);

  const recentReleases = recentReleasesData || [];
  const releaseOptions = useMemo(() => {
    const names = recentReleases.map(r => r.ReleaseName);
    return [...new Set(names)];
  }, [recentReleases]);

  const handleReleaseSelect = (value: string) => {
    const release = recentReleases.find(r => r.ReleaseName === value);
    if (release) {
      setReleaseName(release.ReleaseName);
      setSection(release.Section);
    }
  };

  const filteredSites = useMemo(() => {
    const q = filter.trim().toLowerCase();
    const filtered = !q ? sites : sites.filter((s) => `${s.Sitename} ${s.RuleAction} ${s.Reason}`.toLowerCase().includes(q));
    return filtered.sort((a, b) => a.Sitename.localeCompare(b.Sitename));
  }, [sites, filter]);

  return (
    <Stack>
      <Card withBorder radius="md" p="md">
        <Stack gap="md">
          <Autocomplete
            label="Recent Releases (last 50)"
            placeholder="Select a recent release..."
            data={releaseOptions}
            onOptionSubmit={handleReleaseSelect}
            limit={50}
            maxDropdownHeight={300}
          />

          <Group align="end">
            <Group align="end" gap="xs" style={{ flex: 1 }}>
              <TextInput
                label="Section"
                placeholder="e.g. TV-DVDR-DE"
                value={section}
                onChange={(e) => setSection(e.currentTarget.value)}
                style={{ flex: 1 }}
              />
              <Tooltip label="Detect section from release name">
                <ActionIcon
                  variant="light"
                  color="blue"
                  size="lg"
                  onClick={() => releaseName.trim() && detectSectionMutation.mutate(releaseName)}
                  loading={detectSectionMutation.isPending}
                  disabled={!releaseName.trim()}
                  style={{ marginBottom: 1 }}
                >
                  <IconWand size="1.125rem" />
                </ActionIcon>
              </Tooltip>
            </Group>
            <TextInput
              label="Release"
              placeholder="ReleaseName..."
              value={releaseName}
              onChange={(e) => setReleaseName(e.currentTarget.value)}
              style={{ flex: 3 }}
            />
            <Switch
              label="Simulate PRE"
              checked={simulatePre}
              onChange={(e) => setSimulatePre(e.currentTarget.checked)}
            />
            <Button
              leftSection={<IconPlayerPlay size="1rem" />}
              onClick={() => simulateMutation.mutate()}
              loading={simulateMutation.isPending}
              disabled={!section.trim() || !releaseName.trim()}
            >
              Run
            </Button>
          </Group>
        </Stack>
      </Card>

      {simulateMutation.isPending && (
        <Group justify="center" p="md"><Loader size="md" /></Group>
      )}

      {simulateMutation.isError && (
        <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
          {(simulateMutation.error as any)?.message || 'Failed to run simulator'}
        </Alert>
      )}

      {simulateMutation.data && simulateMutation.data.success === false && (
        <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
          {simulateMutation.data.error || 'Simulation failed'}
        </Alert>
      )}

      {sim && (
        <>
          <Card withBorder radius="md" p="sm">
            <Group justify="space-between">
              <Group gap="xs">
                <Badge color="gray" variant="light">Total sites: {sim.TotalSites}</Badge>
                <Badge color="teal" variant="light">Allowed: {sim.AllowedSites}</Badge>
                <Badge color="violet" variant="light">{simulatePre ? 'PRE' : 'NEWDIR'}</Badge>
              </Group>
              <Text size="xs" c="dimmed">{sim.Section} · {sim.Releasename}</Text>
            </Group>
          </Card>

          <Group>
            <TextInput
              placeholder="Filter sites (sitename/reason/action)..."
              value={filter}
              onChange={(e) => setFilter(e.currentTarget.value)}
              style={{ width: 360 }}
            />
          </Group>

          <Card withBorder radius="md" p="md">
            <Table striped highlightOnHover withTableBorder>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Site</Table.Th>
                  <Table.Th>Allowed</Table.Th>
                  <Table.Th>Rule</Table.Th>
                  <Table.Th>Reason</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {filteredSites.map((s) => (
                  <Table.Tr key={`${s.Sitename}`}>
                    <Table.Td><Text size="sm">{s.Sitename}</Text></Table.Td>
                    <Table.Td>
                      <Badge color={s.Allowed ? 'teal' : 'gray'} variant="light">{s.Allowed ? 'ALLOW' : 'NO'}</Badge>
                    </Table.Td>
                    <Table.Td><Text size="sm" c="dimmed">{s.RuleAction}</Text></Table.Td>
                    <Table.Td><Text size="sm">{s.Reason}</Text></Table.Td>
                  </Table.Tr>
                ))}
                {filteredSites.length === 0 && (
                  <Table.Tr>
                    <Table.Td colSpan={4}>
                      <Text size="sm" c="dimmed" ta="center" p="md">No sites.</Text>
                    </Table.Td>
                  </Table.Tr>
                )}
              </Table.Tbody>
            </Table>
          </Card>

          <Card withBorder radius="md" p="md">
            <Title order={4}>Routes ({routes.length})</Title>
            {routes.length === 0 ? (
              <Text size="sm" c="dimmed">No routes.</Text>
            ) : (
              <Table striped highlightOnHover withTableBorder>
                <Table.Thead>
                  <Table.Tr>
                    <Table.Th>From</Table.Th>
                    <Table.Th>To</Table.Th>
                    <Table.Th>Weight</Table.Th>
                    <Table.Th>Rank</Table.Th>
                  </Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                  {routes.map((r, idx) => (
                    <Table.Tr key={`${r.SourceSite}-${r.DestinationSite}-${idx}`}>
                      <Table.Td><Text size="sm">{r.SourceSite}</Text></Table.Td>
                      <Table.Td><Text size="sm">{r.DestinationSite}</Text></Table.Td>
                      <Table.Td><Text size="sm" c="dimmed">{r.RouteWeight}</Text></Table.Td>
                      <Table.Td><Text size="sm" c="dimmed">{r.Rank}</Text></Table.Td>
                    </Table.Tr>
                  ))}
                </Table.Tbody>
              </Table>
            )}
          </Card>
        </>
      )}
    </Stack>
  );
}

export function Tools() {
  const [activeTab, setActiveTab] = useState<string | null>('simulator');

  return (
    <Stack>
      <Title order={2}>Tools</Title>

      <Tabs value={activeTab} onChange={setActiveTab}>
        <Tabs.List>
          <Tabs.Tab value="simulator" leftSection={<IconCpu size="0.8rem" />}>
            Release Simulator
          </Tabs.Tab>
          <Tabs.Tab value="speedtest" leftSection={<IconBolt size="0.8rem" />}>
            Speedtests
          </Tabs.Tab>
          <Tabs.Tab value="config" leftSection={<IconSettings size="0.8rem" />}>
            Config Editor
          </Tabs.Tab>
        </Tabs.List>

        <Tabs.Panel value="simulator" pt="xs">
          <ReleaseSimulator />
        </Tabs.Panel>

        <Tabs.Panel value="speedtest" pt="xs">
          <SpeedTest />
        </Tabs.Panel>

        <Tabs.Panel value="config" pt="xs">
          <ConfigEditor />
        </Tabs.Panel>
      </Tabs>
    </Stack>
  );
}
