import { SimpleGrid, Card, Text, Title, Group, ThemeIcon, RingProgress, Center, Stack, Loader, Alert, Badge, Table, ScrollArea, Modal, Progress, Tooltip } from '@mantine/core';
import { IconClock, IconListCheck, IconAlertCircle, IconRocket, IconInfoCircle, IconAlertTriangle } from '@tabler/icons-react';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '../api/client';
import type { SystemStatus, IssuesSummary } from '../api/client';
import { useState } from 'react';

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
      const res = await apiClient.post('/ApiSystemService/GetRecentReleases', { Limit: 10 });
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
    refetchInterval: 5000,
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
    refetchInterval: 5000,
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

  if (isLoading) return <Center h={400}><Loader size="xl" /></Center>;
  
  if (error) return (
    <Alert icon={<IconAlertCircle size="1rem" />} title="Connection Error" color="red">
      Could not connect to slftp API. Is the server running on port 8089?
      <br />
      Error: {error.message}
    </Alert>
  );

  const stats = data!;

  // Format uptime (seconds to readable)
  const formatUptime = (seconds: number) => {
    const d = Math.floor(seconds / (3600 * 24));
    const h = Math.floor((seconds % (3600 * 24)) / 3600);
    const m = Math.floor((seconds % 3600) / 60);
    if (d > 0) return `${d}d ${h}h ${m}m`;
    return `${h}h ${m}m`;
  };

  const uptimeStr = formatUptime(stats.Uptime);
  const totalSites = stats.SitesCount;
  const sitesUpPct = totalSites > 0 ? (stats.SitesUp / totalSites) * 100 : 0;

  return (
    <Stack>
      <Title order={2}>System Status</Title>
      
      <SimpleGrid cols={{ base: 1, sm: 2, lg: 4 }}>
        
        {/* Uptime Card */}
        <Card withBorder padding="lg" radius="md">
          <Group justify="space-between">
            <div>
              <Text size="xs" c="dimmed" fw={700} tt="uppercase">
                Uptime
              </Text>
              <Text fw={700} size="xl">
                {uptimeStr}
              </Text>
              <Text size="xs" c="dimmed">
                Version: {stats.Version}
              </Text>
            </div>
            <ThemeIcon color="blue" variant="light" size={48} radius="md">
              <IconClock size="1.8rem" stroke={1.5} />
            </ThemeIcon>
          </Group>
        </Card>

        {/* Sites Status Card */}
        <Card withBorder padding="lg" radius="md">
          <Group justify="space-between">
            <div>
              <Text size="xs" c="dimmed" fw={700} tt="uppercase">
                Sites Online
              </Text>
              <Text fw={700} size="xl">
                {stats.SitesUp} / {stats.SitesCount}
              </Text>
              <Text size="xs" c={stats.SitesDown > 0 ? 'red' : 'dimmed'}>
                {stats.SitesDown} Offline
              </Text>
            </div>
            <RingProgress
              size={55}
              thickness={6}
              roundCaps
              sections={[{ value: sitesUpPct, color: sitesUpPct > 90 ? 'teal' : 'orange' }]}
            />
          </Group>
        </Card>

        {/* Queue Card */}
        <Card withBorder padding="lg" radius="md">
          <Group justify="space-between">
            <div>
              <Text size="xs" c="dimmed" fw={700} tt="uppercase">
                Active Tasks
              </Text>
              <Text fw={700} size="xl">
                {stats.ActiveTasks}
              </Text>
              <Group gap="xs">
                <Text size="xs" c="dimmed">
                  Queue Size: {stats.QueueSize}
                </Text>
                <Text size="xs" c="dimmed">
                  ·
                </Text>
                <Text size="xs" c="blue" fw={500}>
                  {stats.DirlistPerSecond?.toFixed(1) ?? 0} dir/s (max: {stats.DirlistPerSecondMax?.toFixed(1) ?? 0})
                </Text>
              </Group>
            </div>
            <ThemeIcon color="grape" variant="light" size={48} radius="md">
              <IconListCheck size="1.8rem" stroke={1.5} />
            </ThemeIcon>
          </Group>
        </Card>

        {/* Issues Card */}
        <Card withBorder padding="lg" radius="md">
          <Group justify="space-between">
            <div>
              <Text size="xs" c="dimmed" fw={700} tt="uppercase">
                Issues (24h)
              </Text>
              <Text fw={700} size="xl">
                {issuesSummary?.Total ?? 0}
              </Text>
              <Text size="xs" c="dimmed">
                Skip: {issuesSummary?.Skip ?? 0} · DontMatch: {issuesSummary?.DontMatch ?? 0} · Missing: {issuesSummary?.MissingSection ?? 0} · Nuke: {issuesSummary?.Nuke ?? 0}
              </Text>
            </div>
            <ThemeIcon color="yellow" variant="light" size={48} radius="md">
              <IconAlertTriangle size="1.8rem" stroke={1.5} />
            </ThemeIcon>
          </Group>
        </Card>

      </SimpleGrid>

      {/* Recent Releases */}
      <Card withBorder padding="lg" radius="md" mt="xl">
        <Group justify="space-between" mb="md">
          <Group>
            <ThemeIcon color="violet" variant="light" size={32} radius="md">
              <IconRocket size="1.2rem" stroke={1.5} />
            </ThemeIcon>
            <Title order={3}>Recent Releases</Title>
          </Group>
          {releasesData && (
            <Badge size="lg" variant="light">
              {releasesData.total} total
            </Badge>
          )}
        </Group>

	        {releasesLoading ? (
	          <Center h={200}><Loader size="md" /></Center>
	        ) : releasesData && releasesData.releases.length > 0 ? (
	          <ScrollArea type="always" offsetScrollbars>
	            <div style={{ minWidth: 900 }}>
	            <Table striped highlightOnHover style={{ tableLayout: 'fixed' }}>
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
	                {releasesData.releases.map((release: ReleaseInfo, idx: number) => (
                  <Table.Tr
                    key={release.PazoId || idx}
                    style={{ cursor: 'pointer' }}
                    onClick={() => {
                      setSelectedPazoId(release.PazoId);
                      setModalOpened(true);
	                    }}
	                  >
	                    <Table.Td>
	                      <Tooltip label={release.ReleaseName} position="top-start" withArrow>
	                        <Text size="sm" fw={500} style={{ fontFamily: 'monospace' }} truncate>
	                          {release.ReleaseName}
	                        </Text>
	                      </Tooltip>
	                    </Table.Td>
	                    <Table.Td>
                        {!release.Section || release.Section === '' ? (
                          <Badge color="red" variant="filled" size="xs">
                            SECTION NOT SET
                          </Badge>
                        ) : (
                          <Badge size="sm" variant="dot">
                            <Text span size="xs" truncate style={{ maxWidth: 140 }}>
                              {release.Section}
                            </Text>
                          </Badge>
                        )}
	                    </Table.Td>
	                    <Table.Td>
	                      <Group gap="xs" wrap="wrap">
	                        {release.Sites && release.Sites.length > 0 ? (
	                          release.Sites.filter(s => s.toLowerCase() !== 'slftp').slice(0, 3).map((site, i) => (
	                            <Badge key={i} size="sm" variant="light" color="blue">
	                              {site}
	                            </Badge>
                          ))
                        ) : (
                          <Text size="xs" c="dimmed">No sites</Text>
                        )}
                        {release.Sites && release.Sites.filter(s => s.toLowerCase() !== 'slftp').length > 3 && (
                          <Badge size="sm" variant="light" color="gray">
                            +{release.Sites.filter(s => s.toLowerCase() !== 'slftp').length - 3}
                          </Badge>
                        )}
                      </Group>
                    </Table.Td>
                    <Table.Td>
                      {release.Ready ? (
                        <Badge size="sm" color="green">Ready</Badge>
                      ) : release.Stopped ? (
                        <Badge size="sm" color="red">Stopped</Badge>
                      ) : (
                        <Badge size="sm" color="yellow">Racing</Badge>
                      )}
                    </Table.Td>
                    <Table.Td>
                      <Text size="sm" c="dimmed">#{release.QueueNumber}</Text>
	                    </Table.Td>
	                  </Table.Tr>
	                ))}
	              </Table.Tbody>
	            </Table>
	            </div>
	          </ScrollArea>
	        ) : (
	          <Center h={150}>
	            <Text c="dimmed">No recent releases</Text>
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
          <Group>
            <ThemeIcon color="violet" variant="light">
              <IconInfoCircle size="1.2rem" />
            </ThemeIcon>
            <Title order={3}>Release Details</Title>
          </Group>
        }
        size="1400px"
      >
        {detailsLoading ? (
          <Center h={300}><Loader size="lg" /></Center>
	        ) : releaseDetails ? (
	          <Stack gap="md">
	            <Card withBorder padding="md">
	              <Stack gap="xs">
	                <Group gap="sm" align="flex-start" wrap="nowrap">
	                  <Text size="sm" c="dimmed" w={90}>Release</Text>
	                  <Text size="sm" fw={600} style={{ fontFamily: 'monospace', flex: 1, wordBreak: 'break-word' }}>
	                    {releaseDetails.ReleaseName}
	                  </Text>
	                </Group>
	                <Group gap="sm" align="center" wrap="nowrap">
	                  <Text size="sm" c="dimmed" w={90}>Section</Text>
	                  <Badge>{releaseDetails.Section}</Badge>
	                </Group>
	                <Group gap="sm" align="center" wrap="nowrap">
	                  <Text size="sm" c="dimmed" w={90}>Total Files</Text>
	                  <Badge variant="light">{releaseDetails.TotalFiles} files</Badge>
	                </Group>
	                <Group gap="sm" align="center" wrap="nowrap">
	                  <Text size="sm" c="dimmed" w={90}>Status</Text>
	                  {releaseDetails.Ready ? (
	                    <Badge color="green">Ready</Badge>
	                  ) : releaseDetails.Stopped ? (
	                    <Badge color="red">Stopped</Badge>
	                  ) : (
	                    <Badge color="yellow">Racing</Badge>
	                  )}
	                </Group>
	                <Group gap="sm" align="center" wrap="nowrap">
	                  <Text size="sm" c="dimmed" w={90}>Queue #</Text>
	                  <Text size="sm">#{releaseDetails.QueueNumber}</Text>
	                </Group>
	                {releaseDetails.Added && (
	                  <Group gap="sm" align="center" wrap="nowrap">
	                    <Text size="sm" c="dimmed" w={90}>Added</Text>
	                    <Text size="sm" style={{ fontFamily: 'monospace' }}>
	                      {new Date(releaseDetails.Added).toLocaleString()}
	                    </Text>
	                  </Group>
	                )}
	              </Stack>
	            </Card>

            <Title order={4}>Sites ({releaseDetails.SiteDetails.filter(s =>
              s.SiteName.toLowerCase() !== 'slftp' &&
              s.Status !== 'Not Allowed' &&
              s.Status !== 'Not Allowed (Present)'
            ).length})</Title>

            <Table striped highlightOnHover>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Site</Table.Th>
                  <Table.Th>Files</Table.Th>
                  <Table.Th>Progress</Table.Th>
                  <Table.Th>Status</Table.Th>
                  <Table.Th>Started</Table.Th>
                  <Table.Th>Completed</Table.Th>
                  <Table.Th>Duration</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {(() => {
                  // Filter out slftp site and "Not Allowed" sites
                  const allSites = releaseDetails.SiteDetails.filter(s =>
                    s.SiteName.toLowerCase() !== 'slftp' &&
                    s.Status !== 'Not Allowed' &&
                    s.Status !== 'Not Allowed (Present)'
                  );

                  // Sort by CompletedTime (ascending - fastest first)
                  const visibleSites = [...allSites].sort((a, b) => {
                    // Sites without completion time go to the end
                    if (a.CompletedTime === 0 && b.CompletedTime === 0) return 0;
                    if (a.CompletedTime === 0) return 1;
                    if (b.CompletedTime === 0) return -1;
                    return a.CompletedTime - b.CompletedTime;
                  });

                  // Calculate max files across all visible sites for relative progress
                  const maxFiles = Math.max(...visibleSites.map(s => s.FileCount));

                  // Find fastest completion time (smallest CompletedTime > 0)
                  const fastestCompletedTime = Math.min(...visibleSites.filter(s => s.CompletedTime > 0).map(s => s.CompletedTime));

                  // Find fastest start time (smallest StartedTime > 0)
                  const fastestStartedTime = Math.min(...visibleSites.filter(s => s.StartedTime > 0).map(s => s.StartedTime));

                  return visibleSites.map((site, idx) => {
                    // Use Complete flag for 100%, otherwise relative to max
                    const progress = site.Complete ? 100 : maxFiles > 0 ? (site.FileCount / maxFiles) * 100 : 0;

                    return (
                      <Table.Tr key={idx}>
                        <Table.Td>
                          <Text fw={500}>{site.SiteName}</Text>
                        </Table.Td>
                        <Table.Td>
                          <Stack gap={4}>
                            {site.Complete ? (
                              <Badge size="sm" variant="light" color="green">
                                {site.FileCount} files (Complete)
                              </Badge>
                            ) : (
                              <Text size="sm" fw={500}>
                                {site.FileCount} / {maxFiles} files
                              </Text>
                            )}
                            {site.FilesRacedByMe > 0 && (
                              <Text size="xs" c="blue" fw={500}>
                                {site.FilesRacedByMe} uploaded by me
                              </Text>
                            )}
                          </Stack>
                        </Table.Td>
                        <Table.Td style={{ width: '30%' }}>
                          <Stack gap="xs">
                            <Progress
                              value={progress}
                              color={site.Complete ? 'green' : site.FileCount === 0 ? 'gray' : 'blue'}
                              size="md"
                            />
                            <Text size="xs" c="dimmed" ta="right">
                              {progress.toFixed(1)}%
                            </Text>
                          </Stack>
                        </Table.Td>
                        <Table.Td>
                          <Badge
                            size="sm"
                            color={
                              site.Complete ? 'green' :
                              site.Status.includes('Not Allowed') ? 'red' :
                              site.Status === 'Pre' ? 'blue' :
                              'gray'
                            }
                          >
                            {site.Status}
                          </Badge>
                        </Table.Td>
                        <Table.Td>
                          {site.StartedTime > 0 ? (
                            <Stack gap={4}>
                              <Text size="xs" style={{ fontFamily: 'monospace' }}>
                                {new Date(site.StartedTime).toLocaleString(undefined, {
                                  year: 'numeric',
                                  month: '2-digit',
                                  day: '2-digit',
                                  hour: '2-digit',
                                  minute: '2-digit',
                                  second: '2-digit',
                                  fractionalSecondDigits: 3
                                } as any)}
                              </Text>
                              {fastestStartedTime && site.StartedTime > fastestStartedTime && (
                                <Text size="xs" c="orange" fw={500}>
                                  +{((site.StartedTime - fastestStartedTime) / 1000).toFixed(3)}s
                                </Text>
                              )}
                              {fastestStartedTime && site.StartedTime === fastestStartedTime && (
                                <Text size="xs" c="green" fw={600}>
                                  FASTEST
                                </Text>
                              )}
                            </Stack>
                          ) : (
                            <Text size="xs" c="dimmed">-</Text>
                          )}
                        </Table.Td>
                        <Table.Td>
                          {site.CompletedTime > 0 ? (
                            <Stack gap={4}>
                              <Text size="xs" style={{ fontFamily: 'monospace' }}>
                                {new Date(site.CompletedTime).toLocaleString(undefined, {
                                  year: 'numeric',
                                  month: '2-digit',
                                  day: '2-digit',
                                  hour: '2-digit',
                                  minute: '2-digit',
                                  second: '2-digit',
                                  fractionalSecondDigits: 3
                                } as any)}
                              </Text>
                              {fastestCompletedTime && site.CompletedTime > fastestCompletedTime && (
                                <Text size="xs" c="orange" fw={500}>
                                  +{((site.CompletedTime - fastestCompletedTime) / 1000).toFixed(3)}s
                                </Text>
                              )}
                              {fastestCompletedTime && site.CompletedTime === fastestCompletedTime && (
                                <Text size="xs" c="green" fw={600}>
                                  FASTEST
                                </Text>
                              )}
                            </Stack>
                          ) : (
                            <Text size="xs" c="dimmed">-</Text>
                          )}
                        </Table.Td>
                        <Table.Td>
                          {site.StartedTime > 0 && site.CompletedTime > 0 ? (
                            <Text size="xs" fw={500} style={{ fontFamily: 'monospace' }}>
                              {((site.CompletedTime - site.StartedTime) / 1000).toFixed(3)}s
                            </Text>
                          ) : (
                            <Text size="xs" c="dimmed">-</Text>
                          )}
                        </Table.Td>
                      </Table.Tr>
                    );
                  });
                })()}
              </Table.Tbody>
            </Table>
          </Stack>
        ) : (
          <Center h={200}>
            <Text c="dimmed">No details available</Text>
          </Center>
        )}
      </Modal>
    </Stack>
  );
}
