import { Alert, Badge, Button, Card, Group, Loader, Stack, Switch, Table, Text, TextInput, Title, Autocomplete, ActionIcon, Tooltip, Tabs, Textarea, Modal } from '@mantine/core';
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

type DetectSectionResponse = {
  success: boolean;
  section?: string;
  error?: string;
  message?: string;
  debug?: DetectSectionDebug;
};

type DetectSectionDebug = {
  release?: string;
  inputDirect?: string;
  sectionDirect?: string;
  usedReplace?: boolean;
  replaceChanged?: boolean;
  resolution?: string;
  inputAfterReplace?: string;
  sectionAfterReplace?: string;
  sectionBeforeMapping?: string;
  sectionAfterMapping?: string;
  mappingChanged?: boolean;
  compactTrace?: string;
  trace?: string;
};

type SectionDetectionResult = {
  ReleaseName: string;
  Section: string;
  Error?: string;
  Success: boolean;
  Debug?: DetectSectionDebug;
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

async function detectSectionByReleaseName(rlsName: string): Promise<DetectSectionResponse> {
  const res = await apiClient.post('/ApiSimulatorService/DetectSection', { ReleaseName: rlsName });
  if (res.data?.result && Array.isArray(res.data.result)) return res.data.result[0] as DetectSectionResponse;
  return res.data as DetectSectionResponse;
}

function ReleaseSimulator() {
  const [section, setSection] = useState('');
  const [releaseName, setReleaseName] = useState('');
  const [simulatePre, setSimulatePre] = useState(false);
  const [filter, setFilter] = useState('');
  const [detailsOpen, setDetailsOpen] = useState(false);
  const [detailsRelease, setDetailsRelease] = useState('');
  const [detailsSection, setDetailsSection] = useState('');
  const [detailsDebug, setDetailsDebug] = useState<DetectSectionDebug | undefined>(undefined);

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
    mutationFn: detectSectionByReleaseName,
    onSuccess: (data) => {
      if (data?.success && data?.section) {
        setSection(data.section);
      }
    },
  });

  const detectSectionDetailsMutation = useMutation({
    mutationFn: detectSectionByReleaseName,
    onSuccess: (data, rlsName) => {
      setDetailsRelease(rlsName);
      setDetailsSection(data?.section || '');
      setDetailsDebug(data?.debug);
      setDetailsOpen(true);
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
              <Tooltip label="Show how this section was built">
                <Button
                  variant="subtle"
                  size="compact-sm"
                  px={0}
                  onClick={() => sim.Releasename && detectSectionDetailsMutation.mutate(sim.Releasename)}
                  loading={detectSectionDetailsMutation.isPending}
                >
                  {sim.Section} · {sim.Releasename}
                </Button>
              </Tooltip>
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

      <Modal
        opened={detailsOpen}
        onClose={() => setDetailsOpen(false)}
        title="Section Build Details"
        size="80%"
        yOffset="2vh"
      >
        <Stack gap="xs">
          <Text size="sm"><b>Release:</b> {detailsRelease || '-'}</Text>
          <Text size="sm"><b>Final Section:</b> {detailsSection || '-'}</Text>
          <Table withTableBorder withColumnBorders>
            <Table.Tbody>
              <Table.Tr>
                <Table.Td w={180}><Text size="sm">Direct input</Text></Table.Td>
                <Table.Td><Text size="sm" ff="monospace">{detailsDebug?.inputDirect || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Direct section</Text></Table.Td>
                <Table.Td><Text size="sm">{detailsDebug?.sectionDirect || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Tried replace fallback</Text></Table.Td>
                <Table.Td><Text size="sm">{detailsDebug?.usedReplace ? 'yes' : 'no'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Replace changed input</Text></Table.Td>
                <Table.Td><Text size="sm">{detailsDebug?.usedReplace ? (detailsDebug?.replaceChanged ? 'yes' : 'no') : '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Resolution mode</Text></Table.Td>
                <Table.Td><Text size="sm">{detailsDebug?.resolution || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Input after replace</Text></Table.Td>
                <Table.Td><Text size="sm" ff="monospace">{detailsDebug?.inputAfterReplace || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Section after replace</Text></Table.Td>
                <Table.Td><Text size="sm">{detailsDebug?.sectionAfterReplace || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Before mapping</Text></Table.Td>
                <Table.Td><Text size="sm">{detailsDebug?.sectionBeforeMapping || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">After mapping</Text></Table.Td>
                <Table.Td><Text size="sm">{detailsDebug?.sectionAfterMapping || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Mapping changed</Text></Table.Td>
                <Table.Td><Text size="sm">{detailsDebug?.mappingChanged ? 'yes' : 'no'}</Text></Table.Td>
              </Table.Tr>
            </Table.Tbody>
          </Table>

          {detailsDebug?.compactTrace && (
            <Textarea
              label="Compact Trace"
              readOnly
              value={detailsDebug.compactTrace}
              autosize
              minRows={6}
              maxRows={16}
            />
          )}

          {detailsDebug?.trace && (
            <details>
              <summary>Full Trace</summary>
              <Textarea
                label=""
                readOnly
                value={detailsDebug.trace}
                autosize
                minRows={6}
                maxRows={14}
                mt="xs"
              />
            </details>
          )}
        </Stack>
      </Modal>
    </Stack>
  );
}

function SectionsSimulator() {
  const [releaseInput, setReleaseInput] = useState('');
  const [selectedResult, setSelectedResult] = useState<SectionDetectionResult | null>(null);

  const releases = useMemo(() => {
    const names = releaseInput
      .split(/\r?\n/)
      .map((name) => name.trim())
      .filter((name) => name.length > 0);
    return [...new Set(names)];
  }, [releaseInput]);

  const detectSectionsMutation = useMutation({
    mutationFn: async (releaseNames: string[]) => {
      const result = await Promise.all(releaseNames.map(async (releaseName) => {
        try {
          const response = await detectSectionByReleaseName(releaseName);
          if (response?.success && response.section) {
            return {
              ReleaseName: releaseName,
              Section: response.section,
              Success: true,
              Debug: response.debug,
            } as SectionDetectionResult;
          }

          return {
            ReleaseName: releaseName,
            Section: '',
            Error: response?.error || response?.message || 'No section detected',
            Success: false,
            Debug: response.debug,
          } as SectionDetectionResult;
        } catch (error: any) {
          return {
            ReleaseName: releaseName,
            Section: '',
            Error: error?.message || 'Request failed',
            Success: false,
            Debug: undefined,
          } as SectionDetectionResult;
        }
      }));

      return result;
    },
  });

  const results = detectSectionsMutation.data || [];
  const detectedCount = results.filter((r) => r.Success).length;

  return (
    <Stack>
      <Card withBorder radius="md" p="md">
        <Stack gap="md">
          <Textarea
            label="Releases"
            placeholder="One release per line..."
            value={releaseInput}
            onChange={(e) => setReleaseInput(e.currentTarget.value)}
            autosize
            minRows={8}
            maxRows={18}
          />

          <Group justify="space-between" align="center">
            <Text size="sm" c="dimmed">
              {releases.length} unique release{releases.length === 1 ? '' : 's'}
            </Text>
            <Button
              leftSection={<IconWand size="1rem" />}
              onClick={() => detectSectionsMutation.mutate(releases)}
              loading={detectSectionsMutation.isPending}
              disabled={releases.length === 0}
            >
              Detect Sections
            </Button>
          </Group>
        </Stack>
      </Card>

      {detectSectionsMutation.isPending && (
        <Group justify="center" p="md"><Loader size="md" /></Group>
      )}

      {detectSectionsMutation.isError && (
        <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
          {(detectSectionsMutation.error as any)?.message || 'Failed to detect sections'}
        </Alert>
      )}

      {results.length > 0 && (
        <>
          <Card withBorder radius="md" p="sm">
            <Group gap="xs">
              <Badge color="gray" variant="light">Total: {results.length}</Badge>
              <Badge color="teal" variant="light">Detected: {detectedCount}</Badge>
              <Badge color="red" variant="light">Failed: {results.length - detectedCount}</Badge>
            </Group>
          </Card>

          <Card withBorder radius="md" p="md">
            <Table striped highlightOnHover withTableBorder>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Release</Table.Th>
                  <Table.Th>Section</Table.Th>
                  <Table.Th>Status</Table.Th>
                  <Table.Th>Error</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {results.map((result) => (
                  <Table.Tr key={result.ReleaseName}>
                    <Table.Td>
                      <Button variant="subtle" size="compact-sm" px={0} onClick={() => setSelectedResult(result)}>
                        {result.ReleaseName}
                      </Button>
                    </Table.Td>
                    <Table.Td><Text size="sm">{result.Section || '-'}</Text></Table.Td>
                    <Table.Td>
                      <Badge color={result.Success ? 'teal' : 'red'} variant="light">
                        {result.Success ? 'OK' : 'FAILED'}
                      </Badge>
                    </Table.Td>
                    <Table.Td><Text size="sm" c={result.Success ? 'dimmed' : 'red'}>{result.Error || '-'}</Text></Table.Td>
                  </Table.Tr>
                ))}
              </Table.Tbody>
            </Table>
          </Card>
        </>
      )}

      <Modal
        opened={selectedResult !== null}
        onClose={() => setSelectedResult(null)}
        title="Section Build Details"
        size="80%"
        yOffset="2vh"
      >
        <Stack gap="xs">
          <Text size="sm"><b>Release:</b> {selectedResult?.ReleaseName || '-'}</Text>
          <Text size="sm"><b>Final Section:</b> {selectedResult?.Section || '-'}</Text>
          <Table withTableBorder withColumnBorders>
            <Table.Tbody>
              <Table.Tr>
                <Table.Td w={180}><Text size="sm">Direct input</Text></Table.Td>
                <Table.Td><Text size="sm" ff="monospace">{selectedResult?.Debug?.inputDirect || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Direct section</Text></Table.Td>
                <Table.Td><Text size="sm">{selectedResult?.Debug?.sectionDirect || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Tried replace fallback</Text></Table.Td>
                <Table.Td><Text size="sm">{selectedResult?.Debug?.usedReplace ? 'yes' : 'no'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Replace changed input</Text></Table.Td>
                <Table.Td><Text size="sm">{selectedResult?.Debug?.usedReplace ? (selectedResult?.Debug?.replaceChanged ? 'yes' : 'no') : '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Resolution mode</Text></Table.Td>
                <Table.Td><Text size="sm">{selectedResult?.Debug?.resolution || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Input after replace</Text></Table.Td>
                <Table.Td><Text size="sm" ff="monospace">{selectedResult?.Debug?.inputAfterReplace || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Section after replace</Text></Table.Td>
                <Table.Td><Text size="sm">{selectedResult?.Debug?.sectionAfterReplace || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Before mapping</Text></Table.Td>
                <Table.Td><Text size="sm">{selectedResult?.Debug?.sectionBeforeMapping || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">After mapping</Text></Table.Td>
                <Table.Td><Text size="sm">{selectedResult?.Debug?.sectionAfterMapping || '-'}</Text></Table.Td>
              </Table.Tr>
              <Table.Tr>
                <Table.Td><Text size="sm">Mapping changed</Text></Table.Td>
                <Table.Td><Text size="sm">{selectedResult?.Debug?.mappingChanged ? 'yes' : 'no'}</Text></Table.Td>
              </Table.Tr>
            </Table.Tbody>
          </Table>

          {selectedResult?.Debug?.compactTrace && (
            <Textarea
              label="Compact Trace"
              readOnly
              value={selectedResult.Debug.compactTrace}
              autosize
              minRows={6}
              maxRows={16}
            />
          )}

          {selectedResult?.Debug?.trace && (
            <details>
              <summary>Full Trace</summary>
              <Textarea
                label=""
                readOnly
                value={selectedResult.Debug.trace}
                autosize
                minRows={6}
                maxRows={14}
                mt="xs"
              />
            </details>
          )}
        </Stack>
      </Modal>
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
          <Tabs.Tab value="sections-simulator" leftSection={<IconWand size="0.8rem" />}>
            Sections Simulator
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

        <Tabs.Panel value="sections-simulator" pt="xs">
          <SectionsSimulator />
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
