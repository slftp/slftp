import { ActionIcon, Alert, Badge, Button, Card, Code, Grid, Group, Loader, MultiSelect, Paper, ScrollArea, Select, Stack, Table, Tabs, Text, Title, Tooltip, useMantineColorScheme } from '@mantine/core';
import { useForm } from '@mantine/form';
import { IconArrowRight, IconRefresh, IconTrash, IconActivity, IconCheck, IconX, IconClock, IconGrid3x3, IconAlertCircle, IconFileOff } from '@tabler/icons-react';
import { useMutation, useQuery } from '@tanstack/react-query';
import { useEffect, useRef, useState } from 'react';
import { apiClient } from '../api/client';

type Site = {
  name: string;
  status: string;
};

type SpeedTestResult = {
  source: string;
  destination: string;
  speed: string;
  amount: string;
  time: string;
  success: boolean;
  message: string;
  status: 'running' | 'success' | 'failed' | 'no_file';
  startTime: string;
  endTime: string;
};

type SpeedTestLogResponse = {
  status: string;
  message?: string;
  results?: SpeedTestResult[]; // JSON string of SpeedTestResult[]
};

export function SpeedTest() {
  const { colorScheme } = useMantineColorScheme();
  const [activeTab, setActiveTab] = useState<string | null>('local');
  const [testId, setTestId] = useState<string | null>(() => {
    // Load testId from localStorage on mount
    return localStorage.getItem('speedtest-current-id');
  });
  const [testType, setTestType] = useState<string | null>(() => {
    const stored = localStorage.getItem('speedtest-current-type');
    if (stored && ['local', 'out', 'in', 'cleanup', 'matrix'].includes(stored)) {
      return stored;
    }
    return null;
  });
  const [logs, setLogs] = useState<string[]>([]);
  const [testStatus, setTestStatus] = useState<string>('');
  const [results, setResults] = useState<SpeedTestResult[]>([]);
  const [matrixActive, setMatrixActive] = useState<string[]>([]);
  const [matrixInitialized, setMatrixInitialized] = useState<boolean>(false);

  const viewport = useRef<HTMLDivElement>(null);

  // Save testId to localStorage whenever it changes
  useEffect(() => {
    if (testId) {
      localStorage.setItem('speedtest-current-id', testId);
    } else {
      localStorage.removeItem('speedtest-current-id');
    }
  }, [testId]);

  useEffect(() => {
    if (testType) {
      localStorage.setItem('speedtest-current-type', testType);
    } else {
      localStorage.removeItem('speedtest-current-type');
    }
  }, [testType]);

  const { data: sitesData } = useQuery({
    queryKey: ['sites'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: '*' });
      let data = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        data = res.data.result[0];
      }
      const rawSites = data.Sites;
      if (typeof rawSites === 'string') {
        return JSON.parse(rawSites) as Site[];
      }
      return (rawSites as Site[]) || [];
    },
    refetchOnWindowFocus: false,
  });

  const sites = (sitesData || []).filter(s => s.status === 'UP').map(s => s.name).sort();
  const allSites = (sitesData || []).map(s => s.name).sort();

  // Get sites without SPEEDTEST section
  const { data: speedTestSitesData } = useQuery({
    queryKey: ['speedtest-sites'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSpeedService/GetSpeedTestSites', {});
      let data = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        data = res.data.result[0];
      }
      if (typeof data === 'string') return JSON.parse(data) as string[];
      return (data as string[]) || [];
    },
    refetchOnWindowFocus: false,
  });

  const speedTestSites = speedTestSitesData || [];
  const sitesWithoutSpeedTest = allSites.filter(s => !speedTestSites.includes(s) && s !== 'SLFTP');

  // Log Polling
  const { data: logData } = useQuery({
    queryKey: ['speedtest-log', testId],
    queryFn: async () => {
      if (!testId) return [];
      const res = await apiClient.post('/ApiSpeedService/GetTestLog', { TestId: testId });
      let data = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        data = res.data.result[0];
      }
      if (typeof data === 'string') return JSON.parse(data) as string[];
      return data as string[];
    },
    enabled: !!testId,
    refetchInterval: (testStatus === 'finished' || testStatus === 'error' || testStatus === 'aborted') ? false : 1000,
  });

  const { data: statusData } = useQuery({
    queryKey: ['speedtest-status', testId],
    queryFn: async () => {
      if (!testId) return null;
      const res = await apiClient.post('/ApiSpeedService/GetTestStatus', { TestId: testId });
      let data = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        data = res.data.result[0];
      }
      if (typeof data === 'string') return JSON.parse(data) as SpeedTestLogResponse;
      return data as SpeedTestLogResponse;
    },
    enabled: !!testId,
    refetchInterval: (testStatus === 'finished' || testStatus === 'error' || testStatus === 'aborted') ? false : 1000,
  });

  useEffect(() => {
    if (logData) {
      setLogs(logData);
      scrollToBottom();
    }
  }, [logData]);

  useEffect(() => {
    if (statusData) {
      setTestStatus(statusData.status);
      if (statusData.results) {
        setResults(statusData.results);
      }
    }
  }, [statusData]);

  useEffect(() => {
    if (!matrixInitialized && speedTestSites.length > 0) {
      setMatrixActive(speedTestSites);
      setMatrixInitialized(true);
    }
  }, [matrixInitialized, speedTestSites]);

  const scrollToBottom = () => {
    if (viewport.current) {
      viewport.current.scrollTo({ top: viewport.current.scrollHeight, behavior: 'smooth' });
    }
  };

  const startTestMutation = useMutation({
    mutationFn: async (vars: { type: 'local' | 'out' | 'in' | 'cleanup' | 'matrix', params?: any }) => {
      let endpoint = '';
      if (vars.type === 'local') endpoint = '/ApiSpeedService/TestSpeedLocal';
      if (vars.type === 'out') endpoint = '/ApiSpeedService/TestSpeedOut';
      if (vars.type === 'in') endpoint = '/ApiSpeedService/TestSpeedIn';
      if (vars.type === 'cleanup') endpoint = '/ApiSpeedService/TestSpeedCleanup';
      if (vars.type === 'matrix') endpoint = '/ApiSpeedService/TestSpeedMatrix';

      const res = await apiClient.post(endpoint, vars.params || {});
      let data = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        data = res.data.result[0];
      }
      return data as string; // Test ID
    },
    onSuccess: (id, vars) => {
      setTestId(id);
      setTestType(vars.type);
      setLogs([]);
      setResults([]);
      setTestStatus('running');
    },
    onError: (err: any) => {
      setLogs(prev => [...prev, `ERROR STARTING TEST: ${err.message}`]);
    }
  });

  const localForm = useForm({
    initialValues: { site: '' },
    validate: { site: (val: string) => !val ? 'Select a site' : null }
  });

  const outForm = useForm({
    initialValues: { source: '', dests: [] as string[] },
    validate: { 
      source: (val: string) => !val ? 'Select source' : null,
      dests: (val: string[]) => val.length === 0 ? 'Select destination(s)' : null
    }
  });

  const inForm = useForm({
    initialValues: { dest: '', sources: [] as string[] },
    validate: { 
      dest: (val: string) => !val ? 'Select destination' : null,
      sources: (val: string[]) => val.length === 0 ? 'Select source(s)' : null
    }
  });
  
  const cleanupForm = useForm({
    initialValues: { sites: [] as string[] }
  });

  const abortTestMutation = useMutation({
    mutationFn: async () => {
      if (!testId) return false;
      const res = await apiClient.post('/ApiSpeedService/AbortSpeedTest', { TestId: testId });
      let data = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        data = res.data.result[0];
      }
      return data as boolean;
    },
    onSuccess: (ok) => {
      if (!ok) {
        setLogs(prev => [...prev, 'ERROR ABORTING TEST']);
      }
    },
    onError: (err: any) => {
      setLogs(prev => [...prev, `ERROR ABORTING TEST: ${err.message}`]);
    }
  });

  const handleStartLocal = (values: typeof localForm.values) => {
    startTestMutation.mutate({ type: 'local', params: { SiteName: values.site } });
  };

  const handleStartOut = (values: typeof outForm.values) => {
    startTestMutation.mutate({ 
      type: 'out', 
      params: { SourceSite: values.source, DestSites: values.dests.join(' ') } 
    });
  };

  const handleStartIn = (values: typeof inForm.values) => {
    startTestMutation.mutate({ 
      type: 'in', 
      params: { DestSite: values.dest, SourceSites: values.sources.join(' ') } 
    });
  };
  
  const handleCleanup = (values: typeof cleanupForm.values) => {
    startTestMutation.mutate({
      type: 'cleanup',
      params: { Sites: values.sites.join(' ') }
    });
  };

  const toggleMatrixSite = (site: string) => {
    setMatrixActive(prev => (
      prev.includes(site) ? prev.filter(s => s !== site) : [...prev, site]
    ));
  };

  const handleStartMatrix = () => {
    const activeSites = matrixActive.filter(site => speedTestSites.includes(site));
    startTestMutation.mutate({
      type: 'matrix',
      params: {
        IncludeSites: activeSites.length > 0 ? activeSites.join(' ') : '',
        ExcludeSites: ''
      }
    });
  };

  const handleAbortMatrix = () => {
    abortTestMutation.mutate();
  };

  const formatSpeed = (speedKbps: string): { value: string; unit: string } => {
    const speed = parseFloat(speedKbps);
    if (isNaN(speed)) return { value: '-', unit: '' };

    if (speed >= 1024 * 1024) {
      return { value: (speed / 1024 / 1024).toFixed(1), unit: 'GB/s' };
    } else if (speed >= 1024) {
      return { value: (speed / 1024).toFixed(1), unit: 'MB/s' };
    }
    return { value: speed.toFixed(1), unit: 'kB/s' };
  };

  const formatAmount = (amountStr: string): string => {
    const match = amountStr.match(/([\d.]+)\s*MB/i);
    if (!match) return amountStr;

    const mb = parseFloat(match[1]);
    if (isNaN(mb)) return amountStr;

    if (mb >= 1024) {
      return `${(mb / 1024).toFixed(2)} GB`;
    }
    return `${mb.toFixed(1)} MB`;
  };

  const getStatusBadge = (result: SpeedTestResult) => {
    if (result.status === 'running') {
      return <Badge color="blue" leftSection={<Loader size="xs" color="white" />}>Running</Badge>;
    }
    if (result.status === 'success') {
      return <Badge color="green" leftSection={<IconCheck size={14} />}>Success</Badge>;
    }
    if (result.status === 'no_file') {
      return <Badge color="yellow" leftSection={<IconFileOff size={14} />}>No File</Badge>;
    }
    return <Badge color="red" leftSection={<IconX size={14} />}>Failed</Badge>;
  };

  // Build matrix data structure
  const buildMatrix = () => {
    const sites = new Set<string>();
    results.forEach(r => {
      sites.add(r.source);
      sites.add(r.destination);
    });

    const siteArray = Array.from(sites).sort();
    const matrix: Record<string, Record<string, SpeedTestResult | null>> = {};

    siteArray.forEach(src => {
      matrix[src] = {};
      siteArray.forEach(dst => {
        matrix[src][dst] = null;
      });
    });

    results.forEach(r => {
      if (matrix[r.source]) {
        matrix[r.source][r.destination] = r;
      }
    });

    return { sites: siteArray, matrix };
  };

  const matrixData = buildMatrix();

  return (
    <Stack>
      <Tabs value={activeTab} onChange={setActiveTab}>
        <Tabs.List>
          <Tabs.Tab value="local" leftSection={<IconActivity size="0.8rem" />}>Local Speedtest</Tabs.Tab>
          <Tabs.Tab value="out" leftSection={<IconArrowRight size="0.8rem" />}>Outbound (Site-to-Site)</Tabs.Tab>
          <Tabs.Tab value="in" leftSection={<IconArrowRight size="0.8rem" style={{ transform: 'rotate(180deg)' }} />}>Inbound (Site-to-Site)</Tabs.Tab>
          <Tabs.Tab value="matrix" leftSection={<IconGrid3x3 size="0.8rem" />}>Matrix Test</Tabs.Tab>
          <Tabs.Tab value="cleanup" leftSection={<IconTrash size="0.8rem" />}>Cleanup</Tabs.Tab>
        </Tabs.List>

        <Card withBorder radius="md" mt="md" p="md">
          <Tabs.Panel value="local">
            <form onSubmit={localForm.onSubmit(handleStartLocal)}>
              <Stack>
                <Select
                  label="Select Site"
                  placeholder="Choose a site"
                  data={sites}
                  searchable
                  {...localForm.getInputProps('site')}
                />
                <Button type="submit" loading={startTestMutation.isPending} disabled={!!testId && testStatus === 'running'}>
                  Start Local Speedtest
                </Button>
              </Stack>
            </form>
          </Tabs.Panel>

          <Tabs.Panel value="out">
            <form onSubmit={outForm.onSubmit(handleStartOut)}>
              <Stack>
                <Select
                  label="Source Site"
                  description="Site where the file originates"
                  data={sites}
                  searchable
                  {...outForm.getInputProps('source')}
                />
                <MultiSelect
                  label="Destination Site(s)"
                  description="Sites to send the file to"
                  data={sites}
                  searchable
                  {...outForm.getInputProps('dests')}
                />
                <Button type="submit" loading={startTestMutation.isPending} disabled={!!testId && testStatus === 'running'}>
                  Start Outbound Test
                </Button>
              </Stack>
            </form>
          </Tabs.Panel>

          <Tabs.Panel value="in">
            <form onSubmit={inForm.onSubmit(handleStartIn)}>
              <Stack>
                <Select
                  label="Destination Site"
                  description="Site receiving the file"
                  data={sites}
                  searchable
                  {...inForm.getInputProps('dest')}
                />
                <MultiSelect
                  label="Source Site(s)"
                  description="Sites sending the file"
                  data={sites}
                  searchable
                  {...inForm.getInputProps('sources')}
                />
                <Button type="submit" loading={startTestMutation.isPending} disabled={!!testId && testStatus === 'running'}>
                  Start Inbound Test
                </Button>
              </Stack>
            </form>
          </Tabs.Panel>
          
          <Tabs.Panel value="matrix">
            <Stack>
              <Text>
                Tests all sites with SPEEDTEST section against each other. This will create a full connectivity matrix showing which sites can transfer to which.
              </Text>
              <Stack gap="xs">
                <Group justify="space-between">
                  <Text fw={600} size="sm">Active sites</Text>
                  <Group gap="xs">
                    <Button
                      size="xs"
                      variant="subtle"
                      onClick={() => setMatrixActive(speedTestSites)}
                    >
                      Select all
                    </Button>
                    <Button
                      size="xs"
                      variant="subtle"
                      onClick={() => setMatrixActive([])}
                    >
                      Clear
                    </Button>
                  </Group>
                </Group>
                <Group gap="xs" wrap="wrap">
                  {speedTestSites.map(site => {
                    const active = matrixActive.includes(site);
                    return (
                      <Button
                        key={site}
                        size="xs"
                        variant={active ? 'filled' : 'light'}
                        color={active ? 'green' : 'gray'}
                        onClick={() => toggleMatrixSite(site)}
                      >
                        {site}
                      </Button>
                    );
                  })}
                </Group>
                <Text size="xs" c="dimmed">
                  If none are selected, all SPEEDTEST sites will be included.
                </Text>
              </Stack>
              <Button
                onClick={handleStartMatrix}
                loading={startTestMutation.isPending}
                disabled={!!testId && testStatus === 'running'}
                leftSection={<IconGrid3x3 size="1rem" />}
              >
                Start Matrix Test
              </Button>
              {testType === 'matrix' && testId && testStatus === 'running' && (
                <Button
                  color="red"
                  variant="light"
                  onClick={handleAbortMatrix}
                  loading={abortTestMutation.isPending}
                  leftSection={<IconX size="1rem" />}
                >
                  Abort Matrix Test
                </Button>
              )}
            </Stack>
          </Tabs.Panel>

          <Tabs.Panel value="cleanup">
            <form onSubmit={cleanupForm.onSubmit(handleCleanup)}>
              <Stack>
                <MultiSelect
                  label="Select Site(s)"
                  description="Sites to clean speedtest files from (leave empty for all)"
                  data={allSites}
                  searchable
                  {...cleanupForm.getInputProps('sites')}
                />
                <Button type="submit" color="red" loading={startTestMutation.isPending} disabled={!!testId && testStatus === 'running'}>
                  Cleanup Files
                </Button>
              </Stack>
            </form>
          </Tabs.Panel>
        </Card>
      </Tabs>

      {sitesWithoutSpeedTest.length > 0 && (
        <Alert icon={<IconAlertCircle size="1rem" />} title="Sites without SPEEDTEST section" color="yellow" variant="light">
          <Text size="sm">
            The following sites do not have a SPEEDTEST section configured: <strong>{sitesWithoutSpeedTest.join(', ')}</strong>
          </Text>
        </Alert>
      )}

      {matrixData.sites.length > 1 && (
        <Paper shadow="xs" p="md" withBorder>
          <Title order={5} mb="md">Connectivity Matrix</Title>
          <ScrollArea>
            <Table withTableBorder withColumnBorders>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th style={{ minWidth: '100px' }}>From \ To</Table.Th>
                  {matrixData.sites.map(dst => (
                    <Table.Th key={dst} style={{ minWidth: '80px', textAlign: 'center' }}>
                      <Text size="xs" fw={700}>{dst}</Text>
                    </Table.Th>
                  ))}
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {matrixData.sites.map(src => (
                  <Table.Tr key={src}>
                    <Table.Td style={{ fontWeight: 700 }}>{src}</Table.Td>
                    {matrixData.sites.map(dst => {
                      const result = matrixData.matrix[src][dst];
                      const speed = result ? formatSpeed(result.speed) : null;

                      if (src === dst) {
                        return <Table.Td
                          key={dst}
                          style={{
                            backgroundColor: colorScheme === 'dark' ? 'var(--mantine-color-dark-6)' : 'var(--mantine-color-gray-3)',
                            textAlign: 'center'
                          }}
                        >
                          <Text c={colorScheme === 'dark' ? 'gray.5' : 'gray.7'} fw={700}>-</Text>
                        </Table.Td>;
                      }

                      if (!result) {
                        return <Table.Td
                          key={dst}
                          style={{
                            backgroundColor: colorScheme === 'dark' ? 'var(--mantine-color-dark-7)' : 'var(--mantine-color-gray-0)',
                            textAlign: 'center'
                          }}
                        >
                          <Text size="xs" c="dimmed">Pending</Text>
                        </Table.Td>;
                      }

                      if (result.status === 'running') {
                        return <Tooltip key={dst} label="Running...">
                          <Table.Td style={{
                            backgroundColor: colorScheme === 'dark' ? 'var(--mantine-color-blue-9)' : 'var(--mantine-color-blue-1)',
                            textAlign: 'center'
                          }}>
                            <Loader size="xs" color={colorScheme === 'dark' ? 'blue.3' : 'blue.7'} />
                          </Table.Td>
                        </Tooltip>;
                      }

                      if (result.status === 'success') {
                        return <Tooltip key={dst} label={`${speed?.value} ${speed?.unit} - ${result.amount}`}>
                          <Table.Td style={{
                            backgroundColor: colorScheme === 'dark' ? 'var(--mantine-color-green-9)' : 'var(--mantine-color-green-3)',
                            textAlign: 'center',
                            cursor: 'pointer'
                          }}>
                            <Text size="xs" fw={700} c="black">{speed?.value}</Text>
                            <Text size="xs" c="dark.9">{speed?.unit}</Text>
                          </Table.Td>
                        </Tooltip>;
                      }

                      if (result.status === 'no_file') {
                        return <Tooltip key={dst} label={result.message || 'No speedtest file found'}>
                          <Table.Td style={{
                            backgroundColor: colorScheme === 'dark' ? 'var(--mantine-color-yellow-9)' : 'var(--mantine-color-yellow-3)',
                            textAlign: 'center',
                            cursor: 'pointer'
                          }}>
                            <IconFileOff size={16} color="black" />
                          </Table.Td>
                        </Tooltip>;
                      }

                      return <Tooltip key={dst} label={result.message || 'Failed'}>
                        <Table.Td style={{
                          backgroundColor: colorScheme === 'dark' ? 'var(--mantine-color-red-9)' : 'var(--mantine-color-red-3)',
                          textAlign: 'center',
                          cursor: 'pointer'
                        }}>
                          <IconX size={16} color="black" />
                        </Table.Td>
                      </Tooltip>;
                    })}
                  </Table.Tr>
                ))}
              </Table.Tbody>
            </Table>
          </ScrollArea>
        </Paper>
      )}

      <Grid gutter="md">
        <Grid.Col span={{ base: 12, md: 8 }}>
          <Paper shadow="xs" p="md" withBorder style={{ height: '100%' }}>
            <Group justify="space-between" mb="md">
              <Title order={5}>Transfer Results</Title>
              {testId && (
                <ActionIcon
                  variant="light"
                  onClick={() => {
                    setTestId(null);
                    setTestType(null);
                    setResults([]);
                    setLogs([]);
                    setTestStatus('');
                    localStorage.removeItem('speedtest-current-id');
                    localStorage.removeItem('speedtest-current-type');
                  }}
                  title="Clear current test"
                >
                  <IconRefresh size="1rem" />
                </ActionIcon>
              )}
            </Group>

            {activeTab === 'cleanup' && testStatus === 'running' && results.length === 0 ? (
              <Alert icon={<IconAlertCircle size="1rem" />} color="blue" variant="light">
                <Text size="sm">
                  Cleanup tasks have been added to the queue. You can click the <IconRefresh size="0.9rem" style={{ display: 'inline', verticalAlign: 'middle' }} /> button above to start a new test, or switch tabs to view the log.
                </Text>
              </Alert>
            ) : results.length > 0 ? (
              <ScrollArea h={500}>
                <Table striped highlightOnHover>
                  <Table.Thead>
                    <Table.Tr>
                      <Table.Th>Status</Table.Th>
                      <Table.Th>Source</Table.Th>
                      <Table.Th>Destination</Table.Th>
                      <Table.Th>Speed</Table.Th>
                      <Table.Th>Amount</Table.Th>
                      <Table.Th style={{ width: 1, whiteSpace: 'nowrap' }}>Time</Table.Th>
                      <Table.Th>Started</Table.Th>
                    </Table.Tr>
                  </Table.Thead>
                  <Table.Tbody>
                    {results.map((r, i) => {
                      const speed = formatSpeed(r.speed);
                      const bgColor = r.status === 'no_file' ?
                        'rgba(255, 193, 7, 0.1)' :
                        r.status === 'failed' ?
                          'rgba(255, 0, 0, 0.05)' :
                          undefined;
                      return (
                        <Table.Tr key={i} style={{ backgroundColor: bgColor }}>
                          <Table.Td>{getStatusBadge(r)}</Table.Td>
                          <Table.Td><Text fw={500}>{r.source}</Text></Table.Td>
                          <Table.Td><Text fw={500}>{r.destination}</Text></Table.Td>
                          <Table.Td>
                            {r.speed ? (
                              <Group gap={4}>
                                <Text size="lg" fw={700} c="blue">{speed.value}</Text>
                                <Text size="sm" c="dimmed">{speed.unit}</Text>
                              </Group>
                            ) : (
                              <Text c="dimmed">-</Text>
                            )}
                          </Table.Td>
                          <Table.Td>
                            {r.amount ? (
                              <Text fw={600}>{formatAmount(r.amount)}</Text>
                            ) : (
                              <Text c="dimmed">-</Text>
                            )}
                          </Table.Td>
                          <Table.Td style={{ whiteSpace: 'nowrap' }}>{r.time || '-'}</Table.Td>
                          <Table.Td style={{ whiteSpace: 'nowrap' }}>
                            <Group gap={4} style={{ whiteSpace: 'nowrap' }}>
                              <IconClock size={14} />
                              <Text size="xs" style={{ whiteSpace: 'nowrap' }}>{r.startTime}</Text>
                            </Group>
                          </Table.Td>
                        </Table.Tr>
                      );
                    })}
                  </Table.Tbody>
                </Table>
              </ScrollArea>
            ) : (
              <Text c="dimmed" ta="center" mt="xl">No transfers yet. Start a speedtest to see results here.</Text>
            )}
          </Paper>
        </Grid.Col>

        <Grid.Col span={{ base: 12, md: 4 }}>
          <Paper shadow="xs" p="md" withBorder style={{ height: '100%' }}>
            <Group justify="space-between" mb="xs">
              <Title order={5}>Log</Title>
              <Text size="sm" c={testStatus === 'running' ? 'blue' : testStatus === 'error' ? 'red' : testStatus === 'aborted' ? 'yellow' : 'green'}>
                {testStatus || 'Idle'}
              </Text>
            </Group>
            <ScrollArea h={500} viewportRef={viewport} type="auto">
              <Code block style={{ minHeight: '100%', fontSize: '0.75rem' }}>
                {logs.length > 0 ? logs.map((l, i) => (
                  <div key={i}>{l}</div>
                )) : <Text c="dimmed">No logs yet...</Text>}
              </Code>
            </ScrollArea>
          </Paper>
        </Grid.Col>
      </Grid>
    </Stack>
  );
}
