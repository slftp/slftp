import { SimpleGrid, Card, Text, Title, Group, ThemeIcon, RingProgress, Center, Stack, Loader, Alert, Badge, Table, ScrollArea, Modal, Progress } from '@mantine/core';
import { IconClock, IconListCheck, IconAlertCircle, IconRocket, IconInfoCircle } from '@tabler/icons-react';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '../api/client';
import type { SystemStatus } from '../api/client';
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
    refetchInterval: 10000,
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
      
      <SimpleGrid cols={{ base: 1, sm: 3 }}>
        
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
              <Text size="xs" c="dimmed">
                Queue Size: {stats.QueueSize}
              </Text>
            </div>
            <ThemeIcon color="grape" variant="light" size={48} radius="md">
              <IconListCheck size="1.8rem" stroke={1.5} />
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
          <ScrollArea>
            <Table striped highlightOnHover>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Release</Table.Th>
                  <Table.Th>Section</Table.Th>
                  <Table.Th>Sites</Table.Th>
                  <Table.Th>Status</Table.Th>
                  <Table.Th>Queue #</Table.Th>
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
                      <Text size="sm" fw={500} style={{ fontFamily: 'monospace' }}>
                        {release.ReleaseName}
                      </Text>
                    </Table.Td>
                    <Table.Td>
                      <Badge size="sm" variant="dot">
                        {release.Section}
                      </Badge>
                    </Table.Td>
                    <Table.Td>
                      <Group gap="xs">
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
        size="xl"
      >
        {detailsLoading ? (
          <Center h={300}><Loader size="lg" /></Center>
        ) : releaseDetails ? (
          <Stack gap="md">
            <Card withBorder padding="md">
              <Stack gap="xs">
                <Group justify="space-between">
                  <Text size="sm" c="dimmed">Release</Text>
                  <Text size="sm" fw={600} style={{ fontFamily: 'monospace' }}>
                    {releaseDetails.ReleaseName}
                  </Text>
                </Group>
                <Group justify="space-between">
                  <Text size="sm" c="dimmed">Section</Text>
                  <Badge>{releaseDetails.Section}</Badge>
                </Group>
                <Group justify="space-between">
                  <Text size="sm" c="dimmed">Total Files</Text>
                  <Badge variant="light">{releaseDetails.TotalFiles} files</Badge>
                </Group>
                <Group justify="space-between">
                  <Text size="sm" c="dimmed">Status</Text>
                  {releaseDetails.Ready ? (
                    <Badge color="green">Ready</Badge>
                  ) : releaseDetails.Stopped ? (
                    <Badge color="red">Stopped</Badge>
                  ) : (
                    <Badge color="yellow">Racing</Badge>
                  )}
                </Group>
                <Group justify="space-between">
                  <Text size="sm" c="dimmed">Queue #</Text>
                  <Text size="sm">#{releaseDetails.QueueNumber}</Text>
                </Group>
                {releaseDetails.ErrorReason && (
                  <Group justify="space-between">
                    <Text size="sm" c="dimmed">Error</Text>
                    <Text size="sm" c="red">{releaseDetails.ErrorReason}</Text>
                  </Group>
                )}
              </Stack>
            </Card>

            <Title order={4}>Sites ({releaseDetails.SiteDetails.filter(s => s.SiteName.toLowerCase() !== 'slftp').length})</Title>

            <Table striped highlightOnHover>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Site</Table.Th>
                  <Table.Th>Files</Table.Th>
                  <Table.Th>Progress</Table.Th>
                  <Table.Th>Status</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {(() => {
                  // Filter out slftp site
                  const visibleSites = releaseDetails.SiteDetails.filter(s => s.SiteName.toLowerCase() !== 'slftp');
                  // Calculate max files across all visible sites for relative progress
                  const maxFiles = Math.max(...visibleSites.map(s => s.FileCount));

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
                        <Table.Td style={{ width: '35%' }}>
                          <Stack gap="xs">
                            <Progress
                              value={progress}
                              color={site.Complete ? 'green' : site.FileCount === 0 ? 'gray' : 'blue'}
                              size="lg"
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
