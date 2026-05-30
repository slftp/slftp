import { useState, useMemo } from 'react';
import { 
  Alert, 
  Badge, 
  Button, 
  Card, 
  Group, 
  Loader, 
  Modal, 
  Stack, 
  Text, 
  Title, 
  Progress, 
  ThemeIcon, 
  ScrollArea, 
  SimpleGrid, 
  Paper, 
  Table, 
  Tooltip, 
  Center, 
  Checkbox 
} from '@mantine/core';
import { useDisclosure } from '@mantine/hooks';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { 
  IconAlertTriangle, 
  IconArrowRight, 
  IconCheck, 
  IconX, 
  IconRefresh, 
  IconCloudUpload, 
  IconFolders, 
  IconPlus, 
  IconEdit 
} from '@tabler/icons-react';
import { getSites as getCbftpSites, getSiteSections as getCbftpSiteSections, updateSite as updateCbftpSite } from '../../api/cbftpClient';
import type { SiteSection } from '../../api/cbftpClient';
import { apiClient } from '../../api/client';

interface SlftpSection {
  section: string;
  dir: string;
}

interface SectionSyncStatus {
  name: string;
  slftpPath: string;
  cbftpPath: string | null;
  status: 'MATCH' | 'MISMATCH' | 'MISSING_IN_CBFTP';
}

interface SiteComparison {
  siteName: string;
  sections: SectionSyncStatus[];
  needsSync: boolean;
  existsInCbftp: boolean;
}

interface SyncResult {
  site: string;
  status: 'success' | 'skipped' | 'error';
  message: string;
  changes: number;
}

export function SectionSync() {
  const queryClient = useQueryClient();
  const [selectedSites, setSelectedSites] = useState<Set<string>>(new Set());
  
  const [confirmModalOpened, { open: openConfirmModal, close: closeConfirmModal }] = useDisclosure(false);
  const [isSyncing, setIsSyncing] = useState(false);
  const [isFinished, setIsFinished] = useState(false);
  
  const [syncResults, setSyncResults] = useState<SyncResult[]>([]);
  const [syncProgress, setSyncProgress] = useState(0);

  const { data: slftpSites, isLoading: loadingSlftp } = useQuery({
    queryKey: ['slftp-sites-sections-sync'],
    queryFn: async (): Promise<SiteComparison[]> => {
      // 1. Get sites from slftp
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: '' });
      let rawSitesData = res.data.result?.[0] || res.data;
      let sitesList = rawSitesData.Sites || [];
      
      if (typeof sitesList === 'string') {
        try { sitesList = JSON.parse(sitesList); } catch { sitesList = []; }
      }

      // 2. Get cbftp site list
      const cbftpNames = await getCbftpSites().catch(() => [] as string[]);
      const cbftpNamesSet = new Set(cbftpNames);

      const comparisons: SiteComparison[] = [];
      for (const site of sitesList) {
        const name = site.name || site.Name;
        if (!name || name.toLowerCase() === 'slftp') continue;
        
        try {
          // Fetch slftp sections
          const sectionsRes = await apiClient.post('/ApiSitesService/GetSiteSections', { SiteName: name });
          let slSections = sectionsRes.data.result?.[0] || sectionsRes.data;
          if (typeof slSections === 'string') slSections = JSON.parse(slSections);
          const slftpSections: SlftpSection[] = Array.isArray(slSections) ? slSections : [];

          // Fetch cbftp sections
          let cbftpSections: SiteSection[] = [];
          const existsInCbftp = cbftpNamesSet.has(name);
          if (existsInCbftp) {
            cbftpSections = await getCbftpSiteSections(name).catch(() => []);
          }

          const cbftpSectionMap = new Map<string, string>();
          cbftpSections.forEach(s => cbftpSectionMap.set(s.name, s.path));

          const sectionStatuses: SectionSyncStatus[] = slftpSections.map(ss => {
            const cbftpPath = cbftpSectionMap.get(ss.section) ?? null;
            let status: SectionSyncStatus['status'] = 'MATCH';
            if (cbftpPath === null) status = 'MISSING_IN_CBFTP';
            else if (cbftpPath !== ss.dir) status = 'MISMATCH';
            return { name: ss.section, slftpPath: ss.dir, cbftpPath, status };
          });

          comparisons.push({
            siteName: name,
            sections: sectionStatuses,
            needsSync: sectionStatuses.some(s => s.status !== 'MATCH'),
            existsInCbftp
          });
        } catch (e) {
          console.error(`Error processing site ${name}:`, e);
          comparisons.push({ siteName: name, sections: [], needsSync: false, existsInCbftp: cbftpNamesSet.has(name) });
        }
      }
      return comparisons.sort((a, b) => {
        if (a.needsSync && !b.needsSync) return -1;
        if (!a.needsSync && b.needsSync) return 1;
        return a.siteName.localeCompare(b.siteName);
      });
    },
    staleTime: 60000,
  });

  const sitesNeedingSync = useMemo(() =>
    (slftpSites || []).filter(c => c.needsSync && c.existsInCbftp),
    [slftpSites]
  );

  const syncMutation = useMutation({
    mutationFn: async () => {
      setIsSyncing(true);
      setIsFinished(false);
      setSyncResults([]);
      setSyncProgress(0);
      
      const sitesToProcess = Array.from(selectedSites);
      const total = sitesToProcess.length;
      const results: SyncResult[] = [];

      for (let i = 0; i < total; i++) {
        const siteName = sitesToProcess[i];
        const comparison = slftpSites?.find(c => c.siteName === siteName);
        
        if (!comparison || !comparison.existsInCbftp) {
          results.push({ site: siteName, status: 'skipped', message: 'Site not found in cbftp', changes: 0 });
        } else {
          try {
            const updatedSections = comparison.sections.map(s => ({
              name: s.name,
              path: s.slftpPath
            }));
            await updateCbftpSite(siteName, { sections: updatedSections });
            const changes = comparison.sections.filter(s => s.status !== 'MATCH').length;
            results.push({ site: siteName, status: 'success', message: `Synced ${changes} section(s)`, changes });
          } catch (err: any) {
            results.push({ site: siteName, status: 'error', message: err.message || 'Unknown error', changes: 0 });
          }
        }
        setSyncResults([...results]);
        setSyncProgress(((i + 1) / total) * 100);
      }
      return results;
    },
    onSuccess: () => {
      setIsSyncing(false);
      setIsFinished(true);
      queryClient.invalidateQueries({ queryKey: ['slftp-sites-sections-sync'] });
    },
  });

  const handleSyncSelected = () => {
    if (selectedSites.size === 0) return;
    openConfirmModal();
  };

  const handleSelectAll = () => {
    if (selectedSites.size === sitesNeedingSync.length) {
      setSelectedSites(new Set());
    } else {
      setSelectedSites(new Set(sitesNeedingSync.map(c => c.siteName)));
    }
  };

  const handleToggleSite = (siteName: string) => {
    const newSelected = new Set(selectedSites);
    if (newSelected.has(siteName)) newSelected.delete(siteName);
    else newSelected.add(siteName);
    setSelectedSites(newSelected);
  };

  const handleCloseModal = () => {
    closeConfirmModal();
    if (isFinished) setSelectedSites(new Set());
  };

  const summaryStats = useMemo(() => {
    const totalSites = (slftpSites || []).length;
    const needSyncCount = sitesNeedingSync.length;
    const inSyncCount = totalSites - needSyncCount;
    return { totalSites, needSyncCount, inSyncCount };
  }, [slftpSites, sitesNeedingSync]);

  if (loadingSlftp) return <Center h={200}><Loader /></Center>;

  return (
    <Stack gap="lg">
      <SimpleGrid cols={3}>
        <Paper withBorder p="md" radius="md">
          <Text size="xs" c="dimmed" tt="uppercase" fw={700}>Total slftp Sites</Text>
          <Text fw={700} size="xl">{summaryStats.totalSites}</Text>
        </Paper>
        <Paper withBorder p="md" radius="md">
          <Text size="xs" c="dimmed" tt="uppercase" fw={700}>In Sync</Text>
          <Text fw={700} size="xl" c="green">{summaryStats.inSyncCount}</Text>
        </Paper>
        <Paper withBorder p="md" radius="md">
          <Text size="xs" c="dimmed" tt="uppercase" fw={700}>Need Sync</Text>
          <Text fw={700} size="xl" c={summaryStats.needSyncCount > 0 ? "orange" : "gray"}>
            {summaryStats.needSyncCount}
          </Text>
        </Paper>
      </SimpleGrid>

      <Card withBorder shadow="sm" radius="md" padding="lg">
        <Stack gap="md">
          <Group justify="space-between">
            <Group>
              <ThemeIcon size="xl" radius="md" variant="light" color="blue">
                <IconFolders size="1.5rem" />
              </ThemeIcon>
              <div>
                <Title order={4}>Section Comparison</Title>
                <Text size="sm" c="dimmed">Detailed path comparison between slftp and cbftp.</Text>
              </div>
            </Group>
            <Group>
              <Button 
                variant="light" 
                leftSection={<IconRefresh size={16} />} 
                onClick={() => queryClient.invalidateQueries({ queryKey: ['slftp-sites-sections-sync'] })}
              >
                Refresh
              </Button>
              <Button 
                leftSection={<IconCloudUpload size={16} />} 
                disabled={selectedSites.size === 0}
                onClick={handleSyncSelected}
              >
                Sync Selected ({selectedSites.size})
              </Button>
            </Group>
          </Group>

          <Table striped highlightOnHover withTableBorder>
            <Table.Thead>
              <Table.Tr>
                <Table.Th style={{ width: 40 }}>
                  <Checkbox 
                    checked={selectedSites.size === sitesNeedingSync.length && sitesNeedingSync.length > 0}
                    indeterminate={selectedSites.size > 0 && selectedSites.size < sitesNeedingSync.length}
                    onChange={handleSelectAll}
                    disabled={sitesNeedingSync.length === 0}
                  />
                </Table.Th>
                <Table.Th style={{ width: 200 }}>Site Name</Table.Th>
                <Table.Th>Section Paths (slftp vs cbftp)</Table.Th>
                <Table.Th style={{ width: 150 }}>Sync Status</Table.Th>
              </Table.Tr>
            </Table.Thead>
            <Table.Tbody>
              {slftpSites?.map((comp) => (
                <Table.Tr key={comp.siteName}>
                  <Table.Td>
                    <Checkbox 
                      checked={selectedSites.has(comp.siteName)}
                      onChange={() => handleToggleSite(comp.siteName)}
                      disabled={!comp.needsSync || !comp.existsInCbftp}
                    />
                  </Table.Td>
                  <Table.Td>
                    <Text fw={600}>{comp.siteName}</Text>
                    {!comp.existsInCbftp && <Badge color="red" size="xs" variant="light">Not in cbftp</Badge>}
                  </Table.Td>
                  <Table.Td>
                    <Stack gap={6}>
                      {comp.sections.map((sec) => (
                        <Group key={sec.name} gap="sm" wrap="nowrap">
                          <Badge 
                            size="sm" variant="filled" 
                            color={sec.status === 'MATCH' ? 'green' : sec.status === 'MISMATCH' ? 'orange' : 'blue'}
                            style={{ minWidth: 90 }}
                          >
                            {sec.name}
                          </Badge>
                          <Text size="sm" style={{ fontFamily: 'monospace' }}>{sec.slftpPath}</Text>
                          
                          {sec.status === 'MISMATCH' && (
                            <>
                              <IconArrowRight size={14} style={{ flexShrink: 0 }} />
                              <Text size="sm" c="orange" fw={500} style={{ fontFamily: 'monospace' }}>{sec.cbftpPath}</Text>
                              <Tooltip label="Path mismatch!"><IconEdit size={16} color="orange" /></Tooltip>
                            </>
                          )}
                          
                          {sec.status === 'MISSING_IN_CBFTP' && (
                            <Tooltip label="Missing in cbftp!"><IconPlus size={16} color="blue" /></Tooltip>
                          )}
                        </Group>
                      ))}
                      {comp.sections.length === 0 && <Text size="xs" c="dimmed" fs="italic">No sections configured in slftp.</Text>}
                    </Stack>
                  </Table.Td>
                  <Table.Td>
                    {!comp.existsInCbftp ? (
                      <Badge color="gray" variant="light" fullWidth>N/A</Badge>
                    ) : comp.needsSync ? (
                      <Badge color="yellow" leftSection={<IconX size={12} />} variant="light" fullWidth>Out of Sync</Badge>
                    ) : (
                      <Badge color="green" leftSection={<IconCheck size={12} />} variant="light" fullWidth>In Sync</Badge>
                    )}
                  </Table.Td>
                </Table.Tr>
              ))}
              {slftpSites?.length === 0 && (
                <Table.Tr>
                  <Table.Td colSpan={4} style={{ textAlign: 'center', padding: '2rem' }}>
                    <Text c="dimmed">No sites found.</Text>
                  </Table.Td>
                </Table.Tr>
              )}
            </Table.Tbody>
          </Table>
        </Stack>
      </Card>

      <Modal 
        opened={confirmModalOpened} 
        onClose={() => !isSyncing && handleCloseModal()} 
        title={isFinished ? "Sync Report" : "Confirm Synchronization"}
        size="lg"
      >
        <Stack>
          {!isSyncing && !isFinished && (
            <>
              <Alert icon={<IconAlertTriangle size="1rem" />} color="yellow" variant="light">
                You are about to sync sections for <b>{selectedSites.size} site(s)</b>. 
                This will update paths and add missing sections in cbftp.
              </Alert>
              <Group justify="flex-end" mt="md">
                <Button variant="default" onClick={handleCloseModal}>Cancel</Button>
                <Button color="blue" onClick={() => syncMutation.mutate()}>Start Sync</Button>
              </Group>
            </>
          )}

          {isSyncing && (
            <Stack py="lg">
              <Text ta="center" fw={500}>Syncing {selectedSites.size} sites...</Text>
              <Progress value={syncProgress} animated size="xl" radius="xl" />
            </Stack>
          )}

          {isFinished && (
            <Stack gap="md">
              <SimpleGrid cols={3}>
                <Paper withBorder p="xs" radius="md" ta="center">
                  <Text size="xs" c="dimmed" tt="uppercase">Processed</Text>
                  <Text fw={700} size="lg">{syncResults.length}</Text>
                </Paper>
                <Paper withBorder p="xs" radius="md" ta="center">
                  <Text size="xs" c="dimmed" tt="uppercase">Success</Text>
                  <Text fw={700} size="lg" c="green">{syncResults.filter(r => r.status === 'success').length}</Text>
                </Paper>
                <Paper withBorder p="xs" radius="md" ta="center">
                  <Text size="xs" c="dimmed" tt="uppercase">Failed</Text>
                  <Text fw={700} size="lg" c="red">{syncResults.filter(r => r.status === 'error').length}</Text>
                </Paper>
              </SimpleGrid>

              <Paper withBorder p="xs" radius="sm">
                <ScrollArea h={250}>
                  <Stack gap="xs">
                    {syncResults.map((res, idx) => (
                      <Group key={idx} justify="space-between" wrap="nowrap" style={{ borderBottom: '1px solid var(--mantine-color-default-border)', paddingBottom: 4 }}>
                        <Group gap="xs">
                          {res.status === 'success' ? <IconCheck size={16} color="green" /> : <IconX size={16} color="red" />}
                          <Text size="sm" fw={500}>{res.site}</Text>
                        </Group>
                        <Text size="xs" c="dimmed">{res.message}</Text>
                      </Group>
                    ))}
                  </Stack>
                </ScrollArea>
              </Paper>
              <Button fullWidth onClick={handleCloseModal}>Close</Button>
            </Stack>
          )}
        </Stack>
      </Modal>
    </Stack>
  );
}
