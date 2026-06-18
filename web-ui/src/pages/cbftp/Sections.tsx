import { useState, useMemo } from 'react';
import { 
  Alert, 
  Badge, 
  Button, 
  Checkbox, 
  Collapse,
  Group, 
  Loader, 
  Modal,
  Paper, 
  SimpleGrid,
  Stack, 
  Table, 
  Text, 
  Title,
  Center,
  Tooltip,
  Switch,
  ThemeIcon,
  ScrollArea,
  SegmentedControl
} from '@mantine/core';
import { useDisclosure } from '@mantine/hooks';

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { 
  IconRefresh, 
  IconArrowRight, 
  IconCheck, 
  IconX, 
  IconAlertCircle,
  IconFolders,
  IconChevronDown,
  IconEdit,
  IconPlus,
  IconArrowLeft
} from '@tabler/icons-react';
import { notifications } from '@mantine/notifications';
import { 
  getSites as getCbftpSites, 
  getSite as getCbftpSite, 
  createSiteSection,
  updateSiteSection,
  deleteSiteSection
} from '../../api/cbftpClient';
import type { CbftpSite } from '../../api/cbftpClient';
import { apiClient } from '../../api/client';

interface SlftpSection {
  section: string;
  dir: string;
}

interface SectionSyncStatus {
  name: string;
  slftpPath: string;
  cbftpPath: string | null;
  status: 'MATCH' | 'MISMATCH' | 'MISSING_IN_CBFTP' | 'EXTRA_IN_CBFTP';
}

interface SiteComparison {
  siteName: string;
  sections: SectionSyncStatus[];
  needsSync: boolean;
  existsInCbftp: boolean;
}

// Direction: 'slftp' = use slftp path, 'cbftp' = use cbftp path
type SyncDirection = 'slftp' | 'cbftp';

interface SyncResult {
  site: string;
  success: boolean;
  error?: string;
  changes: number;
}

interface SiteRowProps {
  comp: SiteComparison;
  isSelected: boolean;
  isExpanded: boolean;
  onToggle: () => void;
  onToggleSelect: () => void;
  onSync: (directions: Map<string, SyncDirection>) => void;
  isSyncing: boolean;
  showOnlyDifferences: boolean;
}

function SiteRow({ comp, isSelected, isExpanded, onToggle, onToggleSelect, onSync, isSyncing, showOnlyDifferences }: SiteRowProps) {
  const mismatchCount = comp.sections.filter(s => s.status === 'MISMATCH').length;
  const missingCount = comp.sections.filter(s => s.status === 'MISSING_IN_CBFTP').length;
  const extraCount = comp.sections.filter(s => s.status === 'EXTRA_IN_CBFTP').length;
  const matchCount = comp.sections.filter(s => s.status === 'MATCH').length;
  
  // State for sync directions per section (only for mismatches)
  const [syncDirections, setSyncDirections] = useState<Map<string, SyncDirection>>(new Map());
  
  // Filter sections when showOnlyDifferences is enabled
  const displaySections = showOnlyDifferences 
    ? comp.sections.filter(s => s.status !== 'MATCH')
    : comp.sections;

  const handleDirectionChange = (sectionName: string, direction: SyncDirection) => {
    const newDirections = new Map(syncDirections);
    newDirections.set(sectionName, direction);
    setSyncDirections(newDirections);
  };

  // Set all mismatches to same direction
  const setAllDirections = (direction: SyncDirection) => {
    const newDirections = new Map(syncDirections);
    comp.sections
      .filter(s => s.status === 'MISMATCH')
      .forEach(s => newDirections.set(s.name, direction));
    setSyncDirections(newDirections);
  };

  return (
    <>
      <Table.Tr 
        style={{ cursor: 'pointer' }} 
        onClick={onToggle}
        bg={isExpanded ? 'var(--nav-hover-bg)' : undefined}
      >
        <Table.Td onClick={(e) => e.stopPropagation()}>
          <Checkbox
            checked={isSelected}
            onChange={onToggleSelect}
            disabled={!comp.needsSync || !comp.existsInCbftp}
            onClick={(e) => e.stopPropagation()}
          />
        </Table.Td>
        <Table.Td>
          <Group gap="xs">
            <ThemeIcon 
              size="sm" 
              variant="light" 
              color={isExpanded ? 'blue' : 'gray'}
              style={{ transition: 'transform 0.2s', transform: isExpanded ? 'rotate(180deg)' : 'rotate(0deg)' }}
            >
              <IconChevronDown size={14} />
            </ThemeIcon>
            <Text fw={600}>{comp.siteName}</Text>
            {!comp.existsInCbftp && <Badge color="red" size="xs" variant="light">Not in cbftp</Badge>}
          </Group>
        </Table.Td>
        <Table.Td>
          <Group gap={4}>
            {comp.sections.length > 0 ? (
              <>
                <Badge size="sm" color="blue" variant="light">
                  {comp.sections.length} total
                </Badge>
                {mismatchCount > 0 && (
                  <Badge size="sm" color="orange" variant="light" leftSection={<IconEdit size={10} />}>
                    {mismatchCount} changed
                  </Badge>
                )}
                {missingCount > 0 && (
                  <Badge size="sm" color="blue" variant="light" leftSection={<IconPlus size={10} />}>
                    {missingCount} new
                  </Badge>
                )}
                {extraCount > 0 && (
                  <Badge size="sm" color="red" variant="light" leftSection={<IconX size={10} />}>
                    {extraCount} extra
                  </Badge>
                )}
                {matchCount > 0 && mismatchCount === 0 && missingCount === 0 && extraCount === 0 && (
                  <Badge size="sm" color="green" variant="light" leftSection={<IconCheck size={10} />}>
                    All match
                  </Badge>
                )}
              </>
            ) : (
              <Text size="sm" c="dimmed">No sections</Text>
            )}
          </Group>
        </Table.Td>
        <Table.Td>
          {!comp.existsInCbftp ? (
            <Badge color="gray" variant="light">N/A</Badge>
          ) : comp.needsSync ? (
            <Badge color="yellow" leftSection={<IconX size={12} />}>Out of Sync</Badge>
          ) : comp.sections.length === 0 ? (
            <Badge color="gray" variant="light">No Sections</Badge>
          ) : (
            <Badge color="green" leftSection={<IconCheck size={12} />}>In Sync</Badge>
          )}
        </Table.Td>
        <Table.Td onClick={(e) => e.stopPropagation()}>
          {comp.existsInCbftp && comp.needsSync && (
            <Tooltip label="Sync this site">
              <Button
                size="xs"
                variant="light"
                leftSection={<IconArrowRight size={14} />}
                onClick={(e) => {
                  e.stopPropagation();
                  onSync(syncDirections);
                }}
                loading={isSyncing}
              >
                Sync
              </Button>
            </Tooltip>
          )}
        </Table.Td>
      </Table.Tr>
      
      {/* Expanded Detail Row */}
      <Table.Tr style={{ display: isExpanded ? 'table-row' : 'none' }}>
        <Table.Td colSpan={5} style={{ padding: 0, border: 'none' }}>
          <Collapse in={isExpanded}>
            <Paper p="md" withBorder radius={0}>
              {/* Bulk direction controls for mismatches */}
              {mismatchCount > 0 && (
                <Group mb="md" p="xs" bg="var(--mantine-color-gray-light)" style={{ borderRadius: 4 }}>
                  <Text size="sm" fw={500}>Set all mismatches to:</Text>
                  <Group gap="xs">
                    <Button size="xs" variant="light" leftSection={<IconArrowRight size={14} />} onClick={() => setAllDirections('slftp')}>
                      slftp
                    </Button>
                    <Button size="xs" variant="light" leftSection={<IconArrowLeft size={14} />} onClick={() => setAllDirections('cbftp')}>
                      cbftp
                    </Button>
                  </Group>
                  <Text size="xs" c="dimmed">
                    (Default: slftp → cbftp)
                  </Text>
                </Group>
              )}
              
              <ScrollArea h={Math.min(displaySections.length * 55 + 20, 400)}>
                <Stack gap="xs">
                  {displaySections.map((sec) => {
                    const direction = syncDirections.get(sec.name) || 'slftp';
                    return (
                    <Group key={sec.name} gap="md" wrap="nowrap" p="xs" style={{ 
                      borderRadius: '4px',
                      border: '1px solid var(--mantine-color-default-border)',
                      background: 'var(--mantine-color-body)'
                    }}>
                      <Badge 
                        size="md" 
                        variant="filled" 
                        color={sec.status === 'MATCH' ? 'green' : sec.status === 'MISMATCH' ? 'orange' : sec.status === 'MISSING_IN_CBFTP' ? 'blue' : 'red'}
                        style={{ minWidth: 100 }}
                      >
                        {sec.name}
                      </Badge>
                      
                      <Group gap="xs" style={{ flex: 1 }} align="center">
                        {sec.status === 'EXTRA_IN_CBFTP' ? (
                          <>
                            <Text size="sm" c="dimmed" fs="italic" style={{ fontFamily: 'monospace', minWidth: 150 }}>(not in slftp)</Text>
                            <IconArrowRight size={14} style={{ flexShrink: 0 }} />
                            <Text size="sm" c="red" fw={500} style={{ fontFamily: 'monospace' }}>{sec.cbftpPath}</Text>
                            <Badge size="sm" color="red" variant="light">Will be removed</Badge>
                          </>
                        ) : (
                          <>
                            <Text size="sm" fw={500} style={{ fontFamily: 'monospace', minWidth: 150 }}>
                              {sec.slftpPath}
                            </Text>
                            
                            {sec.status === 'MISMATCH' && sec.cbftpPath && (
                              <>
                                {/* Direction selector */}
                                <SegmentedControl
                                  size="xs"
                                  value={direction}
                                  onChange={(val) => handleDirectionChange(sec.name, val as SyncDirection)}
                                  data={[
                                    { value: 'slftp', label: <Tooltip label="Use slftp path"><IconArrowRight size={14} /></Tooltip> },
                                    { value: 'cbftp', label: <Tooltip label="Use cbftp path"><IconArrowLeft size={14} /></Tooltip> },
                                  ]}
                                />
                                <Text size="sm" c={direction === 'cbftp' ? 'blue' : 'orange'} fw={500} style={{ fontFamily: 'monospace' }}>
                                  {sec.cbftpPath}
                                </Text>
                              </>
                            )}
                            
                            {sec.status === 'MISSING_IN_CBFTP' && (
                              <>
                                <IconArrowRight size={14} color="var(--primary-light)" />
                                <Badge size="sm" color="blue" variant="light">Will be added</Badge>
                              </>
                            )}
                            
                            {sec.status === 'MATCH' && (
                              <IconCheck size={16} color="var(--mantine-color-green-6)" />
                            )}
                          </>
                        )}
                      </Group>
                    </Group>
                  );})}
                  {displaySections.length === 0 && (
                    <Text c="dimmed" ta="center" py="md">
                      {showOnlyDifferences 
                        ? 'No differences - all sections are in sync!'
                        : 'No sections configured in slftp for this site.'}
                    </Text>
                  )}
                </Stack>
              </ScrollArea>
            </Paper>
          </Collapse>
        </Table.Td>
      </Table.Tr>
    </>
  );
}

export function Sections() {
  const queryClient = useQueryClient();
  const [selectedSites, setSelectedSites] = useState<Set<string>>(new Set());
  const [expandedSites, setExpandedSites] = useState<Set<string>>(new Set());
  const [showOnlyDifferences, setShowOnlyDifferences] = useState(false);
  
  // Report modal state
  const [reportOpened, { open: openReport, close: closeReport }] = useDisclosure(false);
  const [lastSyncResults, setLastSyncResults] = useState<SyncResult[]>([]);
  
  // Global sync directions per site per section
  const [allSyncDirections, setAllSyncDirections] = useState<Map<string, Map<string, SyncDirection>>>(new Map());

  // Fetch slftp sites with sections
  const { data: slftpSites, isLoading: loadingSlftp, error: slftpError } = useQuery({
    queryKey: ['slftp-sites-sections-full'],
    queryFn: async (): Promise<SiteComparison[]> => {
      // 1. Get sites from slftp
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: '' });
      let rawSitesData = res.data.result?.[0] || res.data;
      let sitesList = rawSitesData.Sites || [];
      
      if (typeof sitesList === 'string') {
        try { sitesList = JSON.parse(sitesList); } catch { sitesList = []; }
      }

      // 2. Get cbftp site list for existence check
      const cbftpNames = await getCbftpSites().catch(() => [] as string[]);
      const cbftpNamesSet = new Set(cbftpNames);

      // 3. Get detailed cbftp sites with sections
      let cbftpSitesMap = new Map<string, CbftpSite>();
      try {
        const cbftpSites = await Promise.all(
          cbftpNames.map(name => 
            getCbftpSite(name).catch(() => null)
          )
        );
        cbftpSites.forEach(site => {
          if (site) cbftpSitesMap.set(site.name, site);
        });
      } catch { /* ignore */ }

      const comparisons: SiteComparison[] = [];
      for (const site of sitesList) {
        const name = site.name || site.Name;
        if (!name || name.toLowerCase() === 'slftp') continue;
        
        try {
          // Fetch slftp sections
          const sectionsRes = await apiClient.post('/ApiSitesService/GetSiteSections', { SiteName: name });
          let slSections = sectionsRes.data.result?.[0] || sectionsRes.data;
          if (typeof slSections === 'string') slSections = JSON.parse(slSections);
          // Debug logging for ATL

          // Filter out sections without a valid path (dir-SECTION not defined in slftp)
          const slftpSections: SlftpSection[] = (Array.isArray(slSections) ? slSections : [])
            .filter((ss: SlftpSection) => ss.dir && ss.dir.trim().length > 0);


          // Get cbftp sections
          const cbftpSite = cbftpSitesMap.get(name);
          const cbftpSections = cbftpSite?.sections || [];
          const existsInCbftp = cbftpNamesSet.has(name);

          const cbftpSectionMap = new Map<string, string>();
          cbftpSections.forEach(s => cbftpSectionMap.set(s.name, s.path));

          const slftpSectionNames = new Set(slftpSections.map(ss => ss.section));

          const sectionStatuses: SectionSyncStatus[] = slftpSections.map(ss => {
            const cbftpPath = cbftpSectionMap.get(ss.section) ?? null;
            let status: SectionSyncStatus['status'] = 'MATCH';
            if (cbftpPath === null) {
              status = 'MISSING_IN_CBFTP';
            } else {
              // Normalize paths for comparison (remove trailing slashes)
              const normalizedSlftp = ss.dir.replace(/\/$/, '');
              const normalizedCbftp = cbftpPath.replace(/\/$/, '');
              if (normalizedSlftp !== normalizedCbftp) status = 'MISMATCH';
            }
            return { name: ss.section, slftpPath: ss.dir, cbftpPath, status };
          });

          // Add extra sections that exist in cbftp but not in slftp
          cbftpSections.forEach(cs => {
            if (!slftpSectionNames.has(cs.name)) {
              sectionStatuses.push({
                name: cs.name,
                slftpPath: '',
                cbftpPath: cs.path,
                status: 'EXTRA_IN_CBFTP'
              });
            }
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
    staleTime: 30000,
  });

  const filteredSites = useMemo(() => {
    if (!showOnlyDifferences) return slftpSites || [];
    return (slftpSites || []).filter(s => s.needsSync);
  }, [slftpSites, showOnlyDifferences]);

  const sitesNeedingSync = useMemo(() =>
    (slftpSites || []).filter(c => c.needsSync && c.existsInCbftp),
    [slftpSites]
  );



  // Sync mutation
  const syncMutation = useMutation({
    mutationFn: async ({ siteNames, directions }: { siteNames: string[], directions?: Map<string, Map<string, SyncDirection>> }) => {
      const results: { site: string; success: boolean; error?: string; changes: number }[] = [];

      for (const siteName of siteNames) {
        const comparison = slftpSites?.find(c => c.siteName === siteName);
        if (!comparison || !comparison.existsInCbftp) {
          results.push({ site: siteName, success: false, error: 'Site not found in cbftp', changes: 0 });
          continue;
        }

        const siteDirections = directions?.get(siteName) || new Map<string, SyncDirection>();
        let successCount = 0;
        let errorMsg = '';

        // Process each section individually
        for (const s of comparison.sections) {
          if (s.status === 'MATCH') continue; // Skip already matching sections

          try {
            if (s.status === 'EXTRA_IN_CBFTP') {
              // Delete extra section from site in cbftp
              await deleteSiteSection(siteName, s.name);
              successCount++;
              continue;
            }

            let path: string;
            let syncToSlftp = false;
            
            if (s.status === 'MISMATCH' && s.cbftpPath) {
              const dir = siteDirections.get(s.name) || 'slftp';
              if (dir === 'cbftp') {
                path = s.cbftpPath;
                syncToSlftp = true;
              } else {
                path = s.slftpPath;
              }
            } else {
              path = s.slftpPath;
            }
            
            // Normalize: remove trailing slash (except for root /)
            if (path && path.length > 1 && path.endsWith('/')) {
              path = path.slice(0, -1);
            }
            
            if (!path || path.trim().length === 0) continue;

            if (syncToSlftp) {
              // Update section in slftp
              await apiClient.post('/ApiSitesService/SetSiteSection', {
                SiteName: siteName,
                Section: s.name,
                Dir: path
              });
            } else if (s.status === 'MISSING_IN_CBFTP') {
              // Create new section in cbftp
              await createSiteSection(siteName, { name: s.name, path });
            } else if (s.status === 'MISMATCH') {
              // Update existing section in cbftp
              await updateSiteSection(siteName, s.name, { path });
            }
            successCount++;
          } catch (error: any) {
            errorMsg = `${s.name}: ${error.message || String(error)}`;
            console.error(`Failed to sync section ${s.name} for site ${siteName}:`, error);
          }
        }

        if (successCount > 0 && !errorMsg) {
          results.push({ site: siteName, success: true, changes: successCount });
        } else if (errorMsg) {
          results.push({ site: siteName, success: false, error: errorMsg, changes: successCount });
        } else {
          results.push({ site: siteName, success: true, changes: 0 });
        }
      }

      return results;
    },
    onSuccess: (results) => {
      const successCount = results.filter(r => r.success).length;
      const failCount = results.filter(r => !r.success).length;
      const totalChanges = results.reduce((acc, r) => acc + r.changes, 0);

      setLastSyncResults(results);

      if (successCount > 0) {
        notifications.show({
          title: 'Sync Complete',
          message: `Synced ${successCount} site(s) with ${totalChanges} section changes${failCount > 0 ? `, ${failCount} failed` : ''}`,
          color: failCount > 0 ? 'yellow' : 'green',
        });
      } else if (failCount > 0) {
        notifications.show({
          title: 'Sync Failed',
          message: `Failed to sync ${failCount} site(s)`,
          color: 'red',
        });
      }

      // Open report modal if there are failures
      if (failCount > 0) {
        openReport();
      }

      queryClient.invalidateQueries({ queryKey: ['slftp-sites-sections-full'] });
      queryClient.invalidateQueries({ queryKey: ['dashboardCbftpSyncSummary'] });
      setSelectedSites(new Set());
    },
    onError: (error: Error) => {
      notifications.show({
        title: 'Sync Error',
        message: error.message,
        color: 'red',
      });
    },
  });

  const handleSelectAll = () => {
    const selectable = sitesNeedingSync.map(c => c.siteName);
    
    if (selectedSites.size === selectable.length) {
      setSelectedSites(new Set());
    } else {
      setSelectedSites(new Set(selectable));
    }
  };

  const handleToggleSite = (siteName: string) => {
    const newSelected = new Set(selectedSites);
    if (newSelected.has(siteName)) {
      newSelected.delete(siteName);
    } else {
      newSelected.add(siteName);
    }
    setSelectedSites(newSelected);
  };

  const handleToggleExpand = (siteName: string) => {
    const newExpanded = new Set(expandedSites);
    if (newExpanded.has(siteName)) {
      newExpanded.delete(siteName);
    } else {
      newExpanded.add(siteName);
    }
    setExpandedSites(newExpanded);
  };

  const handleSyncSelected = () => {
    if (selectedSites.size > 0) {
      syncMutation.mutate({ siteNames: [...selectedSites], directions: allSyncDirections });
    }
  };

  const handleSyncSingle = (siteName: string, directions: Map<string, SyncDirection>) => {
    const dirMap = new Map(allSyncDirections);
    dirMap.set(siteName, directions);
    setAllSyncDirections(dirMap);
    syncMutation.mutate({ siteNames: [siteName], directions: dirMap });
  };

  if (loadingSlftp) {
    return (
      <Center h={200}>
        <Loader />
      </Center>
    );
  }

  if (slftpError) {
    return (
      <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
        Failed to load slftp sites: {String(slftpError)}
      </Alert>
    );
  }

  return (
    <Stack gap="md">
      <Paper p="md" withBorder>
        <Group justify="space-between" mb="md">
          <Group>
            <IconFolders size="1.5rem" style={{ opacity: 0.7 }} />
            <div>
              <Title order={4}>Section Sync</Title>
              <Text size="sm" c="dimmed">
                Sync section paths between slftp and cbftp. Click a site to choose sync direction.
              </Text>
            </div>
          </Group>
          <Group>
            <Switch
              label="Only show differences"
              checked={showOnlyDifferences}
              onChange={(e) => {
                setShowOnlyDifferences(e.currentTarget.checked);
                setSelectedSites(new Set());
              }}
            />
            <Button
              variant="light"
              leftSection={<IconRefresh size={16} />}
              onClick={() => queryClient.invalidateQueries({ queryKey: ['slftp-sites-sections-full'] })}
            >
              Refresh
            </Button>
            <Button
              leftSection={<IconArrowRight size={16} />}
              disabled={selectedSites.size === 0 || syncMutation.isPending}
              loading={syncMutation.isPending}
              onClick={handleSyncSelected}
            >
              Sync Selected ({selectedSites.size})
            </Button>
          </Group>
        </Group>

        <Group gap="md" mb="md">
          <Badge color="blue" variant="light" size="lg">
            Total Sites: {slftpSites?.length || 0}
          </Badge>
          <Badge color={sitesNeedingSync.length > 0 ? 'yellow' : 'green'} variant="light" size="lg">
            Need Sync: {sitesNeedingSync.length}
          </Badge>
          {showOnlyDifferences && (
            <Badge color="grape" variant="light" size="lg">
              Showing: {filteredSites.length}
            </Badge>
          )}
        </Group>

        {filteredSites.length > 0 ? (
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
                <Table.Th>Site</Table.Th>
                <Table.Th>Summary</Table.Th>
                <Table.Th style={{ width: 130 }}>Status</Table.Th>
                <Table.Th style={{ width: 100 }}>Action</Table.Th>
              </Table.Tr>
            </Table.Thead>
            <Table.Tbody>
              {filteredSites.map((comp) => (
                <SiteRow
                  key={comp.siteName}
                  comp={comp}
                  isSelected={selectedSites.has(comp.siteName)}
                  isExpanded={expandedSites.has(comp.siteName)}
                  onToggle={() => handleToggleExpand(comp.siteName)}
                  onToggleSelect={() => handleToggleSite(comp.siteName)}
                  onSync={(directions) => handleSyncSingle(comp.siteName, directions)}
                  isSyncing={syncMutation.isPending}
                  showOnlyDifferences={showOnlyDifferences}
                />
              ))}
            </Table.Tbody>
          </Table>
        ) : (
          <Paper p="xl" withBorder>
            <Text ta="center" c="dimmed">
              {showOnlyDifferences 
                ? 'No sites with differences found. All sections are in sync!' 
                : 'No sites found.'}
            </Text>
          </Paper>
        )}
      </Paper>

      {/* Sync Report Modal */}
      <Modal
        opened={reportOpened}
        onClose={closeReport}
        title="Sync Report"
        size="lg"
      >
        <Stack gap="md">
          <SimpleGrid cols={3}>
            <Paper withBorder p="xs" radius="md" ta="center">
              <Text size="xs" c="dimmed" tt="uppercase">Processed</Text>
              <Text fw={700} size="xl">{lastSyncResults.length}</Text>
            </Paper>
            <Paper withBorder p="xs" radius="md" ta="center">
              <Text size="xs" c="dimmed" tt="uppercase">Success</Text>
              <Text fw={700} size="xl" c="green">
                {lastSyncResults.filter(r => r.success).length}
              </Text>
            </Paper>
            <Paper withBorder p="xs" radius="md" ta="center">
              <Text size="xs" c="dimmed" tt="uppercase">Failed</Text>
              <Text fw={700} size="xl" c="red">
                {lastSyncResults.filter(r => !r.success).length}
              </Text>
            </Paper>
          </SimpleGrid>

          {lastSyncResults.some(r => !r.success) && (
            <>
              <Text fw={600} c="red">Failed Sites:</Text>
              <Paper withBorder p="xs" radius="md">
                <ScrollArea h={250}>
                  <Stack gap="xs">
                    {lastSyncResults
                      .filter(r => !r.success)
                      .map((res, idx) => (
                        <Group key={idx} justify="space-between" wrap="nowrap" p="xs" style={{ borderBottom: '1px solid var(--mantine-color-default-border)' }}>
                          <Group gap="xs">
                            <IconX size={16} color="var(--mantine-color-red-6)" />
                            <Text size="sm" fw={600}>{res.site}</Text>
                          </Group>
                          <Text size="xs" c="red" style={{ maxWidth: '60%', textAlign: 'right' }}>
                            {res.error || 'Unknown error'}
                          </Text>
                        </Group>
                      ))}
                  </Stack>
                </ScrollArea>
              </Paper>
            </>
          )}

          {lastSyncResults.some(r => r.success) && (
            <>
              <Text fw={600} c="green">Successful Sites:</Text>
              <Paper withBorder p="xs" radius="md">
                <ScrollArea h={150}>
                  <Stack gap="xs">
                    {lastSyncResults
                      .filter(r => r.success)
                      .map((res, idx) => (
                        <Group key={idx} justify="space-between" wrap="nowrap" p="xs" style={{ borderBottom: '1px solid var(--mantine-color-default-border)' }}>
                          <Group gap="xs">
                            <IconCheck size={16} color="var(--mantine-color-green-6)" />
                            <Text size="sm" fw={600}>{res.site}</Text>
                          </Group>
                          <Badge size="sm" color="blue" variant="light">
                            {res.changes} changes
                          </Badge>
                        </Group>
                      ))}
                  </Stack>
                </ScrollArea>
              </Paper>
            </>
          )}

          <Group justify="flex-end" mt="md">
            <Button onClick={closeReport}>Close</Button>
          </Group>
        </Stack>
      </Modal>
    </Stack>
  );
}
