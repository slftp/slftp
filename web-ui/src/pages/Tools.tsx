import { Alert, Badge, Button, Card, Group, Loader, Stack, Switch, Table, Text, TextInput, Title, Autocomplete, ActionIcon, Tooltip, Tabs, Textarea, Modal } from '@mantine/core';
import { IconAlertCircle, IconPlayerPlay, IconWand, IconBolt, IconCpu, IconSettings, IconUpload, IconDeviceFloppy, IconListCheck, IconCheck, IconX, IconBan } from '@tabler/icons-react';
import { useMutation, useQuery } from '@tanstack/react-query';
import { useMemo, useState, useRef } from 'react';
import { apiClient, batchTestSections, saveSectionTesterData, loadSectionTesterData, type SectionTestItem } from '../api/client';
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
    Skipped: boolean;
    SkipReason: string;
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

      {sim?.Skipped && (
        <Alert icon={<IconBan size="1rem" />} title="Skipped" color="orange">
          {sim.SkipReason}
        </Alert>
      )}

      {sim && !sim.Skipped && (
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

function parseReleaseText(content: string): SectionTestItem[] {
  const lines = content.split('\n');
  const items: SectionTestItem[] = [];
  
  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith('#')) continue;
    
    // Parse format: "Release.Name-GRP SECTION"
    const lastSpaceIdx = trimmed.lastIndexOf(' ');
    if (lastSpaceIdx > 0) {
      const releaseName = trimmed.substring(0, lastSpaceIdx).trim();
      const section = trimmed.substring(lastSpaceIdx + 1).trim();
      if (releaseName && section) {
        items.push({ name: releaseName, section: section.toUpperCase() });
      }
    }
  }
  
  return items;
}

function SectionsSimulator() {
  const [content, setContent] = useState<string>('');
  const [parsedItems, setParsedItems] = useState<SectionTestItem[]>([]);
  const fileInputRef = useRef<HTMLInputElement>(null);

  // Load saved data on mount
  useQuery({
    queryKey: ['section-tester-data'],
    queryFn: async () => {
      const data = await loadSectionTesterData();
      setContent(data);
      setParsedItems(parseReleaseText(data));
      return data;
    },
    refetchOnWindowFocus: false,
  });

  // Save mutation
  const saveMutation = useMutation({
    mutationFn: saveSectionTesterData,
  });

  // Test mutation
  const testMutation = useMutation({
    mutationFn: async () => {
      const items = parseReleaseText(content);
      if (items.length === 0) {
        throw new Error('No valid releases found in text');
      }
      setParsedItems(items);
      return batchTestSections(items);
    },
  });

  const handleContentChange = (value: string) => {
    setContent(value);
    setParsedItems(parseReleaseText(value));
  };

  const handleSave = () => {
    saveMutation.mutate(content);
  };

  const handleRunTest = () => {
    testMutation.mutate();
  };

  const handleFileUpload = (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (!file) return;

    const reader = new FileReader();
    reader.onload = (e) => {
      const text = e.target?.result as string;
      setContent(text);
      setParsedItems(parseReleaseText(text));
    };
    reader.readAsText(file);
    
    if (fileInputRef.current) {
      fileInputRef.current.value = '';
    }
  };

  const results = testMutation.data;
  const hasResults = results && results.success && results.results.length > 0;

  return (
    <Stack gap="md">
      {/* Header with stats and buttons */}
      <Card withBorder radius="md" p="sm">
        <Group justify="space-between" wrap="wrap">
          <Group gap="xs">
            <Badge color="gray" variant="light">Total: {parsedItems.length}</Badge>
            {hasResults && (
              <>
                <Badge color="teal" variant="light">Matched: {results.stats.matched}</Badge>
                <Badge color="red" variant="light">Failed: {results.stats.failed}</Badge>
              </>
            )}
          </Group>
          <Group gap="xs">
            <input
              type="file"
              accept=".txt"
              style={{ display: 'none' }}
              ref={fileInputRef}
              onChange={handleFileUpload}
            />
            <Tooltip label="Upload Release.txt file">
              <ActionIcon
                variant="light"
                color="blue"
                size="lg"
                onClick={() => fileInputRef.current?.click()}
              >
                <IconUpload size="1.125rem" />
              </ActionIcon>
            </Tooltip>
            <Tooltip label="Save test data">
              <ActionIcon
                variant="light"
                color="green"
                size="lg"
                onClick={handleSave}
                loading={saveMutation.isPending}
              >
                <IconDeviceFloppy size="1.125rem" />
              </ActionIcon>
            </Tooltip>
            <Button
              leftSection={<IconPlayerPlay size="1rem" />}
              onClick={handleRunTest}
              loading={testMutation.isPending}
              disabled={parsedItems.length === 0}
            >
              Run Test
            </Button>
          </Group>
        </Group>
      </Card>

      {/* Content textarea */}
      <Card withBorder radius="md" p="md">
        <Stack gap="xs">
          <Group justify="space-between">
            <Text size="sm" fw={500}>Release.txt Content</Text>
            <Text size="xs" c="dimmed">Format: ReleaseName Section (one per line)</Text>
          </Group>
          <Textarea
            value={content}
            onChange={(e) => handleContentChange(e.currentTarget.value)}
            placeholder="Example:\nRelease.Name-GRP MP3\nAnother.Release-GRP TV-DVDR-DE"
            minRows={8}
            maxRows={15}
            autosize
            styles={{
              input: {
                fontFamily: 'monospace',
                fontSize: '0.9rem',
              },
            }}
          />
        </Stack>
      </Card>

      {/* Loading state */}
      {testMutation.isPending && (
        <Group justify="center" p="md">
          <Loader size="md" />
          <Text size="sm" c="dimmed">Testing sections...</Text>
        </Group>
      )}

      {/* Error state */}
      {testMutation.isError && (
        <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
          {(testMutation.error as any)?.message || 'Failed to run section test'}
        </Alert>
      )}

      {/* Results table */}
      {hasResults && (
        <Card withBorder radius="md" p="md">
          <Stack gap="md">
            <Group justify="space-between">
              <Title order={4}>Test Results</Title>
              <Group gap="xs">
                <Badge color="teal" leftSection={<IconCheck size="0.8rem" />}>
                  {results.stats.matched} matched
                </Badge>
                <Badge color="red" leftSection={<IconX size="0.8rem" />}>
                  {results.stats.failed} failed
                </Badge>
              </Group>
            </Group>

            <Table striped highlightOnHover withTableBorder>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Release</Table.Th>
                  <Table.Th>Expected Section</Table.Th>
                  <Table.Th>Detected Section</Table.Th>
                  <Table.Th style={{ width: 100 }}>Status</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {results.results.map((result, idx) => (
                  <Table.Tr 
                    key={idx}
                    style={{
                      backgroundColor: result.matched 
                        ? 'rgba(0, 150, 0, 0.05)' 
                        : 'rgba(255, 0, 0, 0.05)',
                      borderLeft: `4px solid ${result.matched ? '#099268' : '#fa5252'}`,
                    }}
                  >
                    <Table.Td>
                      <Text size="sm" style={{ fontFamily: 'monospace' }}>
                        {result.releaseName}
                      </Text>
                    </Table.Td>
                    <Table.Td>
                      <Text size="sm" fw={500}>
                        {result.expectedSection}
                      </Text>
                    </Table.Td>
                    <Table.Td>
                      <Text size="sm" c={result.matched ? 'dimmed' : 'red'}>
                        {result.detectedSection || '(none)'}
                      </Text>
                    </Table.Td>
                    <Table.Td>
                      {result.matched ? (
                        <Badge color="teal" size="sm" leftSection={<IconCheck size="0.7rem" />}>
                          OK
                        </Badge>
                      ) : (
                        <Badge color="red" size="sm" leftSection={<IconX size="0.7rem" />}>
                          FAIL
                        </Badge>
                      )}
                    </Table.Td>
                  </Table.Tr>
                ))}
                {results.results.length === 0 && (
                  <Table.Tr>
                    <Table.Td colSpan={4}>
                      <Text size="sm" c="dimmed" ta="center" p="md">
                        No results to display
                      </Text>
                    </Table.Td>
                  </Table.Tr>
                )}
              </Table.Tbody>
            </Table>
          </Stack>
        </Card>
      )}

      {/* Save success notification */}
      {saveMutation.isSuccess && (
        <Alert icon={<IconCheck size="1rem" />} color="green" variant="light">
          Test data saved successfully
        </Alert>
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
          <Tabs.Tab value="sections-simulator" leftSection={<IconListCheck size="0.8rem" />}>
            Section Tester
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
