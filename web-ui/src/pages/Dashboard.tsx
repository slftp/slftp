import { 
  SimpleGrid, 
  Card, 
  Text, 
  Title, 
  Group, 
  ThemeIcon, 
  Center, 
  Stack, 
  Loader, 
  Alert, 
  Badge, 
  Table, 
  ScrollArea, 
  Modal, 
  Progress, 
  Tooltip,
  Box,
  Grid,
} from '@mantine/core';
import { 
  IconClock, 
  IconAlertCircle, 
  IconRocket, 
  IconInfoCircle, 
  IconAlertTriangle,
  IconServer,
  IconActivity,
  IconCpu,
  IconHash,
} from '@tabler/icons-react';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '../api/client';
import type { SystemStatus, IssuesSummary } from '../api/client';
import { useMemo, useState } from 'react';
import { StatCard, MiniStatCard } from '../components/StatCard';

interface ReleaseInfo {
  ReleaseName: string;
  Section: string;
  Added: number;
  PazoId: number;
  Ready: boolean;
  Stopped: boolean;
  QueueNumber: number;
  Sites: string[];
}

interface ReleaseSiteDetail {
  SiteName: string;
  Complete: boolean;
  FileCount: number;
  TotalFiles: number;
  FilesRacedByMe: number;
  Percent: number;
  Status: string;
  StartedTime: number;
  CompletedTime: number;
}

interface ReleaseDetails {
  ReleaseName: string;
  Section: string;
  Added: string;
  PazoId: number;
  Ready: boolean;
  Stopped: boolean;
  QueueNumber: number;
  SiteDetails: ReleaseSiteDetail[];
  TotalFiles: number;
  ErrorReason: string;
}

export function Dashboard() {
  const [selectedPazoId, setSelectedPazoId] = useState<number | null>(null);
  const [modalOpened, setModalOpened] = useState(false);

  const { data, isLoading, error } = useQuery({
    queryKey: ['systemStatus'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSystemService/GetStatus');
      if (res.data && res.data.result && Array.isArray(res.data.result)) {
         return res.data.result[0] as SystemStatus;
      }
      return res.data as SystemStatus;
    },
    refetchInterval: 5000,
    refetchOnWindowFocus: false,
  });

  const { data: releasesData, isLoading: releasesLoading } = useQuery({
    queryKey: ['recentReleases'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSystemService/GetRecentReleases', { Limit: 15 });
      if (res.data && res.data.result && Array.isArray(res.data.result)) {
        const releasesList = res.data.result[0];
        if (releasesList && releasesList.Releases) {
          const releases = (() => {
            if (Array.isArray(releasesList.Releases)) return releasesList.Releases;
            if (typeof releasesList.Releases === 'string') {
              try {
                const parsed = JSON.parse(releasesList.Releases);
                return Array.isArray(parsed) ? parsed : [];
              } catch {
                return [];
              }
            }
            return [];
          })();
          return { releases, total: releasesList.Total };
        }
      }
      return { releases: [], total: 0 };
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const { data: issuesSummary } = useQuery({
    queryKey: ['issuesSummary'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiIssuesService/GetSummary', { WindowSeconds: 24 * 3600 });
      if (res.data && res.data.result && Array.isArray(res.data.result)) {
        return res.data.result[0] as IssuesSummary;
      }
      return res.data as IssuesSummary;
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const { data: releaseDetails, isLoading: detailsLoading } = useQuery({
    queryKey: ['releaseDetails', selectedPazoId],
    queryFn: async () => {
      if (!selectedPazoId) return null;
      const res = await apiClient.post('/ApiSystemService/GetReleaseDetails', { PazoId: selectedPazoId });
      if (res.data && res.data.result && Array.isArray(res.data.result)) {
        const details = res.data.result[0];
        if (details && details.SiteDetails) {
          const siteDetails = (() => {
            if (Array.isArray(details.SiteDetails)) return details.SiteDetails;
            if (typeof details.SiteDetails === 'string') {
              try {
                const parsed = JSON.parse(details.SiteDetails);
                return Array.isArray(parsed) ? parsed : [];
              } catch {
                return [];
              }
            }
            return [];
          })();
          return { ...details, SiteDetails: siteDetails } as ReleaseDetails;
        }
      }
      return null;
    },
    enabled: selectedPazoId !== null && modalOpened,
  });



  const { visibleSiteDetails, maxSiteFiles } = useMemo(() => {
    const allSites = (releaseDetails?.SiteDetails ?? []).filter((site) =>
      site.SiteName.toLowerCase() !== 'slftp' &&
      site.Status !== 'Not Allowed' &&
      site.Status !== 'Not Allowed (Present)'
    );

    const sortedSites = [...allSites].sort((a, b) => {
      if (a.CompletedTime === 0 && b.CompletedTime === 0) return 0;
      if (a.CompletedTime === 0) return 1;
      if (b.CompletedTime === 0) return -1;
      return a.CompletedTime - b.CompletedTime;
    });

    const maxFiles = sortedSites.reduce((max, site) => Math.max(max, site.FileCount), 0);

    return {
      visibleSiteDetails: sortedSites,
      maxSiteFiles: maxFiles,
    };
  }, [releaseDetails]);

  if (isLoading) return (
    <Center h={300}>
      <Stack align="center" gap="sm">
        <Loader size="lg" color="brand" />
        <Text size="sm" c="dimmed">Loading system status...</Text>
      </Stack>
    </Center>
  );
  
  if (error) return (
    <Alert 
      icon={<IconAlertCircle size="1.25rem" />} 
      title="Connection Error" 
      color="red"
      radius="lg"
      style={{
        background: 'rgba(255, 77, 77, 0.1)',
        border: '1px solid rgba(255, 77, 77, 0.3)',
      }}
    >
      Could not connect to slftp API. Is the server running on port 8089?
      <br />
      Error: {error.message}
    </Alert>
  );

  const stats = data!;

  const formatUptime = (seconds: number) => {
    const d = Math.floor(seconds / (3600 * 24));
    const h = Math.floor((seconds % (3600 * 24)) / 3600);
    const m = Math.floor((seconds % 3600) / 60);
    if (d > 0) return `${d}d ${h}h ${m}m`;
    return `${h}h ${m}m`;
  };
  const formatLoadAvg = (avg1: number, avg5: number, avg15: number) =>
    `${avg1.toFixed(2)}, ${avg5.toFixed(2)}, ${avg15.toFixed(2)}`;

  const uptimeStr = formatUptime(stats.Uptime);
  const totalSites = stats.SitesCount;
  const sitesUpPct = totalSites > 0 ? (stats.SitesUp / totalSites) * 100 : 0;

  return (
    <Stack gap="lg">
      {/* Header Section */}
      <Box>
        <Group justify="space-between" align="flex-end" mb={4}>
          <div>
            <Text size="xs" c="dimmed" mb={2}>Welcome back,</Text>
            <Title 
              order={4}
              style={{
                background: 'linear-gradient(135deg, #fff 0%, #a3aed0 100%)',
                WebkitBackgroundClip: 'text',
                WebkitTextFillColor: 'transparent',
              }}
            >
              System Overview
            </Title>
          </div>
          <Badge
            size="sm"
            radius="md"
            style={{
              background: 'var(--nav-active-bg)',
              border: '1px solid var(--nav-active-border)',
            }}
          >
            v{stats.Version}
          </Badge>
        </Group>
      </Box>

      {/* Main Stats Grid */}
      <SimpleGrid cols={{ base: 1, sm: 2, lg: 4 }} spacing="md">
        <StatCard
          title="Uptime"
          value={uptimeStr}
          subtitle={`Version ${stats.Version}`}
          icon={<IconClock size="1.4rem" stroke={1.5} color="white" />}
          variant="gradient"
        />

        <StatCard
          title="Sites Online"
          value={`${stats.SitesUp} / ${stats.SitesCount}`}
          subtitle={stats.SitesDown > 0 ? `${stats.SitesDown} sites offline` : 'All systems operational'}
          icon={<IconServer size="1.4rem" stroke={1.5} color="white" />}
          iconColor="#00ff88"
          iconGradient="linear-gradient(135deg, #00ff88 0%, #00d4ff 100%)"
          trend={{ value: sitesUpPct, isPositive: sitesUpPct > 90, showSign: false }}
        />

        <StatCard
          title="Active Tasks"
          value={stats.ActiveTasks}
          subtitle={`Queue: ${stats.QueueSize} | Dir/sec now: ${stats.DirlistPerSecond?.toFixed(1) ?? 0} | peak: ${stats.DirlistPerSecondMax?.toFixed(1) ?? 0}`}
          icon={<IconActivity size="1.4rem" stroke={1.5} color="white" />}
          iconColor="#9b59b6"
          iconGradient="linear-gradient(135deg, #9b59b6 0%, #e74c3c 100%)"
        />

        <StatCard
          title="Issues (24h)"
          value={issuesSummary?.Total ?? 0}
          subtitle={`Skip: ${issuesSummary?.Skip ?? 0} · Missing: ${issuesSummary?.MissingSection ?? 0}`}
          icon={<IconAlertTriangle size="1.4rem" stroke={1.5} color="white" />}
          iconColor="#ffb547"
          iconGradient="linear-gradient(135deg, #ffb547 0%, #ff6b6b 100%)"
        />
      </SimpleGrid>

      {/* Mini Stats Row */}
      <SimpleGrid cols={{ base: 2, sm: 3, lg: 6 }} spacing="sm">
        {stats.LoadAvgAvailable && (
          <MiniStatCard
            title="Load Avg"
            value={formatLoadAvg(stats.LoadAvgCurrent1 ?? 0, stats.LoadAvgCurrent5 ?? 0, stats.LoadAvgCurrent15 ?? 0)}
            icon={<IconCpu size="1rem" />}
            color="#ff8c42"
          />
        )}
        {stats.LoadAvgAvailable && (
          <MiniStatCard
            title="Load Avg Peak"
            value={formatLoadAvg(stats.LoadAvgPeak1 ?? 0, stats.LoadAvgPeak5 ?? 0, stats.LoadAvgPeak15 ?? 0)}
            icon={<IconCpu size="1rem" />}
            color="#e74c3c"
          />
        )}
        <MiniStatCard
          title="Queue Peak"
          value={stats.QueueSizeMax ?? 0}
          icon={<IconHash size="1rem" />}
          color="#9b59b6"
        />
        {stats.CpuLoadAvailable && (
          <MiniStatCard
            title="CPU Load"
            value={`${stats.CpuLoadCurrent}%`}
            icon={<IconCpu size="1rem" />}
            color={stats.CpuLoadCurrent > 80 ? '#e74c3c' : stats.CpuLoadCurrent > 60 ? '#ffb547' : '#00ff88'}
          />
        )}
        {stats.CpuLoadAvailable && stats.PerformanceLevel > 0 && (
          <MiniStatCard
            title="Perf Level"
            value={stats.PerformanceLevel}
            icon={<IconActivity size="1rem" />}
            color={stats.PerformanceLevel >= 7 ? '#00ff88' : stats.PerformanceLevel >= 4 ? '#ffb547' : '#ff6b6b'}
          />
        )}
      </SimpleGrid>

      {/* Recent Releases Table */}
      <Card
        radius="xl"
        padding="lg"
        style={{
          background: 'var(--gradient-card)',
          backdropFilter: 'blur(10px)',
          border: '1px solid var(--border)',
          boxShadow: 'var(--shadow)',
        }}
      >
        <Group justify="space-between" mb="md">
          <Group gap="sm">
            <ThemeIcon
              size={36}
              radius="lg"
              style={{
                background: 'var(--primary-gradient)',
                boxShadow: 'var(--shadow-sm)',
              }}
            >
              <IconRocket size="1rem" stroke={2} color="white" />
            </ThemeIcon>
            <Box>
              <Title order={5} style={{ color: '#fff', marginBottom: 2 }}>Recent Releases</Title>
              <Text size="xs" c="dimmed">Latest racing activity</Text>
            </Box>
          </Group>
          {releasesData && (
            <Badge
              size="sm"
              radius="md"
              style={{
                background: 'var(--nav-hover-bg)',
                border: '1px solid var(--nav-active-border)',
                color: 'var(--primary-light)',
              }}
            >
              {releasesData.total} total
            </Badge>
          )}
        </Group>

        {releasesLoading ? (
          <Center h={150}>
            <Loader color="brand" size="md" />
          </Center>
        ) : releasesData && releasesData.releases.length > 0 ? (
          <ScrollArea type="auto" offsetScrollbars>
            <div style={{ minWidth: 800 }}>
              <Table 
                style={{ tableLayout: 'fixed' }}
                styles={{
                  thead: {
                    background: 'rgba(30, 41, 59, 0.8)',
                  },
                  th: {
                    color: 'var(--text-secondary)',
                    fontWeight: 600,
                    fontSize: '10px',
                    textTransform: 'uppercase',
                    letterSpacing: '0.08em',
                    padding: '10px 12px',
                  },
                  td: {
                    borderBottom: '1px solid var(--border)',
                    padding: '10px 12px',
                  },
                  tr: {
                    transition: 'background-color 0.12s linear',
                    '&:hover': {
                      background: 'var(--table-row-hover)',
                    },
                  },
                }}
              >
                <Table.Thead>
                  <Table.Tr>
                    <Table.Th style={{ width: '42%' }}>Release</Table.Th>
                    <Table.Th style={{ width: '14%' }}>Section</Table.Th>
                    <Table.Th style={{ width: '24%' }}>Sites</Table.Th>
                    <Table.Th style={{ width: '10%' }}>Status</Table.Th>
                    <Table.Th style={{ width: '10%' }}>Queue #</Table.Th>
                  </Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                  {releasesData.releases.map((release: ReleaseInfo, idx: number) => {
                    const visibleSites = (release.Sites ?? []).filter((site) => site.toLowerCase() !== 'slftp');

                    return (
                      <Table.Tr
                        key={release.PazoId || idx}
                        style={{ 
                          cursor: 'pointer',
                          background: idx % 2 === 1 ? 'var(--table-row-odd)' : 'transparent',
                        }}
                        onClick={() => {
                          setSelectedPazoId(release.PazoId);
                          setModalOpened(true);
                        }}
                      >
                          <Table.Td>
                            <Tooltip label={release.ReleaseName} position="top-start" withArrow>
                              <Text 
                                size="sm" 
                                fw={500} 
                                style={{ 
                                  fontFamily: 'monospace',
                                  color: '#fff',
                                }} 
                                truncate
                              >
                                {release.ReleaseName}
                              </Text>
                            </Tooltip>
                          </Table.Td>
                          <Table.Td>
                            {!release.Section || release.Section === '' ? (
                              <Badge 
                                color="red" 
                                variant="filled" 
                                size="xs"
                                radius="sm"
                              >
                                NO SECTION
                              </Badge>
                            ) : (
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
                                {release.Section}
                              </Badge>
                            )}
                          </Table.Td>
                          <Table.Td>
                            <Group gap={4} wrap="wrap">
                              {visibleSites.length > 0 ? (
                                visibleSites.slice(0, 4).map((site, i) => (
                                  <Badge 
                                    key={i} 
                                    size="xs" 
                                    variant="light"
                                    radius="sm"
                                    styles={{
                                      root: {
                                        background: 'rgba(34, 197, 94, 0.12)',
                                        border: '1px solid rgba(34, 197, 94, 0.25)',
                                        color: '#22c55e',
                                      },
                                    }}
                                  >
                                    {site}
                                  </Badge>
                                ))
                              ) : (
                                <Text size="xs" c="dimmed">No sites</Text>
                              )}
                              {visibleSites.length > 4 && (
                                <Badge 
                                  size="xs" 
                                  variant="light"
                                  radius="sm"
                                  c="dimmed"
                                >
                                  +{visibleSites.length - 4}
                                </Badge>
                              )}
                            </Group>
                          </Table.Td>
                          <Table.Td>
                            {release.Ready ? (
                              <Badge 
                                size="xs" 
                                radius="sm"
                                style={{
                                  background: 'rgba(0, 255, 136, 0.15)',
                                  border: '1px solid rgba(0, 255, 136, 0.3)',
                                  color: '#00ff88',
                                }}
                              >
                                Ready
                              </Badge>
                            ) : release.Stopped ? (
                              <Badge 
                                size="xs" 
                                radius="sm"
                                style={{
                                  background: 'rgba(255, 77, 77, 0.15)',
                                  border: '1px solid rgba(255, 77, 77, 0.3)',
                                  color: '#ff4d4d',
                                }}
                              >
                                Stopped
                              </Badge>
                            ) : (
                              <Badge 
                                size="xs" 
                                radius="sm"
                                style={{
                                  background: 'rgba(255, 181, 71, 0.15)',
                                  border: '1px solid rgba(255, 181, 71, 0.3)',
                                  color: '#ffb547',
                                }}
                              >
                                Racing
                              </Badge>
                            )}
                          </Table.Td>
                          <Table.Td>
                            <Text size="sm" c="dimmed">#{release.QueueNumber}</Text>
                          </Table.Td>
                      </Table.Tr>
                    );
                  })}
                </Table.Tbody>
              </Table>
            </div>
          </ScrollArea>
        ) : (
          <Center h={120}>
            <Stack align="center" gap="xs">
              <ThemeIcon size={40} radius="xl" color="gray" variant="light">
                <IconRocket size="1.25rem" />
              </ThemeIcon>
              <Text size="sm" c="dimmed">No recent releases</Text>
            </Stack>
          </Center>
        )}
      </Card>

      {/* Release Details Modal */}
      <Modal
        opened={modalOpened}
        onClose={() => {
          setModalOpened(false);
          setSelectedPazoId(null);
        }}
        title={
          <Group gap="sm">
            <ThemeIcon
              size={36}
              radius="lg"
              style={{
                background: 'var(--primary-gradient)',
              }}
            >
              <IconInfoCircle size="1rem" color="white" />
            </ThemeIcon>
            <Box>
              <Title order={5} style={{ marginBottom: 0 }}>Release Details</Title>
              <Text size="xs" c="dimmed">Detailed racing information</Text>
            </Box>
          </Group>
        }
        size="1200px"
        radius="xl"
        styles={{
          content: {
            background: 'var(--bg-secondary)',
            backdropFilter: 'blur(12px)',
            border: '1px solid var(--border)',
          },
          header: {
            background: 'transparent',
            borderBottom: '1px solid var(--border)',
            padding: '16px 24px',
          },
          body: {
            padding: '20px 24px',
          },
        }}
      >
        {detailsLoading ? (
          <Center h={250}>
            <Loader size="md" color="brand" />
          </Center>
        ) : releaseDetails ? (
          <Stack gap="md">
            {/* Release Info Card */}
            <Card
              radius="lg"
              padding="md"
              style={{
                background: 'var(--gradient-card)',
                border: '1px solid var(--border)',
              }}
            >
              <Grid gutter="md">
                <Grid.Col span={{ base: 12, md: 6 }}>
                  <Stack gap={6}>
                    <Group gap="sm" wrap="nowrap">
                      <Text size="xs" c="dimmed" w={70}>Release</Text>
                      <Text 
                        size="sm" 
                        fw={500} 
                        style={{ 
                          fontFamily: 'monospace',
                          color: '#fff',
                        }}
                        truncate
                      >
                        {releaseDetails.ReleaseName}
                      </Text>
                    </Group>
                    <Group gap="sm">
                      <Text size="xs" c="dimmed" w={70}>Section</Text>
                      <Badge
                        size="xs"
                        radius="sm"
                        styles={{
                          root: {
                            background: 'var(--nav-hover-bg)',
                            border: '1px solid var(--nav-active-border)',
                            color: 'var(--primary-light)',
                          },
                        }}
                      >
                        {releaseDetails.Section}
                      </Badge>
                    </Group>
                    <Group gap="sm">
                      <Text size="xs" c="dimmed" w={70}>Total Files</Text>
                      <Text size="sm" fw={500}>{releaseDetails.TotalFiles} files</Text>
                    </Group>
                  </Stack>
                </Grid.Col>
                <Grid.Col span={{ base: 12, md: 6 }}>
                  <Stack gap={6}>
                    <Group gap="sm">
                      <Text size="xs" c="dimmed" w={70}>Status</Text>
                      {releaseDetails.Ready ? (
                        <Badge
                          size="xs"
                          radius="sm"
                          style={{
                            background: 'rgba(0, 255, 136, 0.15)',
                            border: '1px solid rgba(0, 255, 136, 0.3)',
                            color: '#00ff88',
                          }}
                        >
                          Ready
                        </Badge>
                      ) : releaseDetails.Stopped ? (
                        <Badge
                          size="xs"
                          radius="sm"
                          style={{
                            background: 'rgba(255, 77, 77, 0.15)',
                            border: '1px solid rgba(255, 77, 77, 0.3)',
                            color: '#ff4d4d',
                          }}
                        >
                          Stopped
                        </Badge>
                      ) : (
                        <Badge
                          size="xs"
                          radius="sm"
                          style={{
                            background: 'rgba(255, 181, 71, 0.15)',
                            border: '1px solid rgba(255, 181, 71, 0.3)',
                            color: '#ffb547',
                          }}
                        >
                          Racing
                        </Badge>
                      )}
                    </Group>
                    <Group gap="sm">
                      <Text size="xs" c="dimmed" w={70}>Queue #</Text>
                      <Text size="sm">#{releaseDetails.QueueNumber}</Text>
                    </Group>
                    {releaseDetails.Added && (
                      <Group gap="sm">
                        <Text size="xs" c="dimmed" w={70}>Added</Text>
                        <Text size="xs" style={{ fontFamily: 'monospace' }}>
                          {new Date(releaseDetails.Added).toLocaleString()}
                        </Text>
                      </Group>
                    )}
                  </Stack>
                </Grid.Col>
              </Grid>
            </Card>

            {/* Sites Table */}
            <Box>
              <Title order={6} mb="sm" style={{ color: '#fff' }}>
                Site Details ({visibleSiteDetails.length} sites)
              </Title>
              
              <Table
                striped
                highlightOnHover
                styles={{
                  thead: {
                    background: 'rgba(17, 28, 68, 0.8)',
                  },
                  th: {
                    color: '#a3aed0',
                    fontWeight: 600,
                    fontSize: '10px',
                    textTransform: 'uppercase',
                    letterSpacing: '0.08em',
                    padding: '10px 12px',
                  },
                  td: {
                    borderBottom: '1px solid rgba(255, 255, 255, 0.04)',
                    padding: '10px 12px',
                  },
                }}
              >
                <Table.Thead>
                  <Table.Tr>
                    <Table.Th>Site</Table.Th>
                    <Table.Th>Files</Table.Th>
                    <Table.Th style={{ width: '25%' }}>Progress</Table.Th>
                    <Table.Th>Status</Table.Th>
                    <Table.Th>Duration</Table.Th>
                  </Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                  {visibleSiteDetails.map((site, idx) => {
                    const progress = site.Complete ? 100 : maxSiteFiles > 0 ? (site.FileCount / maxSiteFiles) * 100 : 0;

                    return (
                      <Table.Tr key={idx}>
                        <Table.Td>
                          <Text fw={500} size="sm">{site.SiteName}</Text>
                        </Table.Td>
                        <Table.Td>
                          <Stack gap={2}>
                            {site.Complete ? (
                              <Badge
                                size="xs"
                                radius="sm"
                                styles={{
                                  root: {
                                    background: 'rgba(0, 255, 136, 0.15)',
                                    border: '1px solid rgba(0, 255, 136, 0.3)',
                                    color: '#00ff88',
                                  },
                                }}
                              >
                                {site.FileCount} files
                              </Badge>
                            ) : (
                              <Text size="xs">
                                {site.FileCount} / {maxSiteFiles}
                              </Text>
                            )}
                            {site.FilesRacedByMe > 0 && (
                              <Text size="11px" c="var(--primary-light)" fw={500}>
                                {site.FilesRacedByMe} uploaded
                              </Text>
                            )}
                          </Stack>
                        </Table.Td>
                        <Table.Td>
                          <Stack gap={4}>
                            <Progress
                              value={progress}
                              size="xs"
                              radius="xl"
                              styles={{
                                root: {
                                  background: 'rgba(255, 255, 255, 0.08)',
                                },
                                section: {
                                  background: site.Complete 
                                    ? '#00ff88' 
                                    : site.FileCount === 0 
                                      ? 'var(--text-muted)' 
                                      : 'var(--primary-gradient)',
                                },
                              }}
                            />
                            <Text size="11px" c="dimmed" ta="right">
                              {progress.toFixed(0)}%
                            </Text>
                          </Stack>
                        </Table.Td>
                        <Table.Td>
                          <Badge
                            size="xs"
                            radius="sm"
                            styles={{
                              root: {
                                background: site.Complete
                                  ? 'rgba(0, 255, 136, 0.15)'
                                  : site.Status.includes('Not Allowed')
                                    ? 'rgba(255, 77, 77, 0.15)'
                                    : site.Status === 'Pre'
                                      ? 'var(--nav-hover-bg)'
                                      : 'rgba(112, 126, 174, 0.15)',
                                border: site.Complete
                                  ? '1px solid rgba(0, 255, 136, 0.3)'
                                  : site.Status.includes('Not Allowed')
                                    ? '1px solid rgba(255, 77, 77, 0.3)'
                                    : site.Status === 'Pre'
                                      ? '1px solid var(--nav-active-border)'
                                      : '1px solid rgba(112, 126, 174, 0.3)',
                                color: site.Complete
                                  ? '#00ff88'
                                  : site.Status.includes('Not Allowed')
                                    ? '#ff4d4d'
                                    : site.Status === 'Pre'
                                      ? 'var(--primary-light)'
                                      : 'var(--text-secondary)',
                              },
                            }}
                          >
                            {site.Status}
                          </Badge>
                        </Table.Td>
                        <Table.Td>
                          {site.StartedTime > 0 && site.CompletedTime > 0 ? (
                            <Text size="xs" fw={500} style={{ fontFamily: 'monospace' }}>
                              {((site.CompletedTime - site.StartedTime) / 1000).toFixed(2)}s
                            </Text>
                          ) : (
                            <Text size="xs" c="dimmed">-</Text>
                          )}
                        </Table.Td>
                      </Table.Tr>
                    );
                  })}
                </Table.Tbody>
              </Table>
            </Box>
          </Stack>
        ) : (
          <Center h={150}>
            <Text size="sm" c="dimmed">No details available</Text>
          </Center>
        )}
      </Modal>
    </Stack>
  );
}
