import { Card, Title, Table, Alert, Loader, Center, TextInput, Button, Stack, Group, Text, ScrollArea, Badge, Switch, Tooltip } from '@mantine/core';
import { IconChevronRight, IconSearch, IconDeviceFloppy, IconPin } from '@tabler/icons-react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useState, useMemo, useRef, useEffect } from 'react';
import { apiClient } from '../api/client';
import { notifications } from '@mantine/notifications';

interface SectionData {
  section: string;
  dir: string;
}

export function Sections() {
  const queryClient = useQueryClient();
  const [selectedSite, setSelectedSite] = useState<string | null>(null);
  const [sectionDirs, setSectionDirs] = useState<Record<string, string>>({});
  const [originalDirs, setOriginalDirs] = useState<Record<string, string>>({});
  const [searchQuery, setSearchQuery] = useState('');
  const [showOnlySet, setShowOnlySet] = useState(true);
  const [preserveSection, setPreserveSection] = useState<string | null>(null);
  const [markedSection, setMarkedSection] = useState<string | null>(null);
  const viewportRef = useRef<HTMLDivElement>(null);

  const { data: sitesData, isLoading: sitesLoading } = useQuery({
    queryKey: ['sites'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: '' });
      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }
      const rawSites = responseData.Sites;
      let parsedSites: any[] = [];
      try {
        if (typeof rawSites === 'string') {
          parsedSites = JSON.parse(rawSites);
        } else if (Array.isArray(rawSites)) {
          parsedSites = rawSites;
        }
      } catch (e) {
        console.error('Failed to parse sites JSON', e);
      }
      return parsedSites.filter(site => site.name.toLowerCase() !== 'slftp');
    },
    refetchOnWindowFocus: false,
  });

  const { data: sectionsData, isLoading: sectionsLoading } = useQuery({
    queryKey: ['sections', selectedSite],
    queryFn: async () => {
      if (!selectedSite) return [];
      const res = await apiClient.post('/ApiSitesService/GetSiteSections', { SiteName: selectedSite });

      let sections: SectionData[] = [];
      try {
        if (typeof res.data === 'string') {
          sections = JSON.parse(res.data);
        } else if (Array.isArray(res.data)) {
          sections = res.data;
        } else if (res.data.result) {
          const resultData = Array.isArray(res.data.result) ? res.data.result[0] : res.data.result;
          if (typeof resultData === 'string') {
            sections = JSON.parse(resultData);
          } else if (Array.isArray(resultData)) {
            sections = resultData;
          }
        }
      } catch (e) {
        console.error('Failed to parse sections:', e);
        return [];
      }

      const dirs: Record<string, string> = {};
      sections.forEach(s => {
        dirs[s.section] = s.dir || '';
      });
      setSectionDirs(dirs);
      setOriginalDirs({ ...dirs });

      return sections;
    },
    enabled: !!selectedSite,
    refetchOnWindowFocus: false,
    refetchOnReconnect: false,
  });

  const saveAllMutation = useMutation({
    mutationFn: async () => {
      const changes = Object.keys(sectionDirs).filter(
        section => sectionDirs[section] !== originalDirs[section]
      );

      for (const section of changes) {
        await apiClient.post('/ApiSitesService/SetSiteSection', {
          SiteName: selectedSite,
          Section: section,
          Dir: sectionDirs[section],
        });
      }

      return changes.length;
    },
    onSuccess: (count) => {
      setOriginalDirs({ ...sectionDirs });
      notifications.show({
        title: 'Saved',
        message: `${count} section(s) updated`,
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['sections', selectedSite] });
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.message,
        color: 'red',
      });
    },
  });

  const handleSaveAll = () => {
    saveAllMutation.mutate();
  };

  const filteredSections = useMemo(() => {
    if (!sectionsData) return [];
    
    let data = sectionsData;

    if (showOnlySet) {
      data = data.filter(s => {
        const dir = sectionDirs[s.section];
        return dir && dir.trim().length > 0;
      });
    }

    if (!searchQuery) return data;

    const query = searchQuery.toLowerCase();
    return data.filter(s => s.section.toLowerCase().includes(query));
  }, [sectionsData, searchQuery, showOnlySet, sectionDirs]);

  const hasChanges = useMemo(() => {
    return Object.keys(sectionDirs).some(
      section => sectionDirs[section] !== originalDirs[section]
    );
  }, [sectionDirs, originalDirs]);

  const changesCount = useMemo(() => {
    return Object.keys(sectionDirs).filter(
      section => sectionDirs[section] !== originalDirs[section]
    ).length;
  }, [sectionDirs, originalDirs]);

  const handleToggleShowOnlySet = (event: React.ChangeEvent<HTMLInputElement>) => {
    const isChecked = event.currentTarget.checked;
    
    if (markedSection) {
       setPreserveSection(markedSection);
    } else if (viewportRef.current) {
      const viewport = viewportRef.current;
      const scrollTop = viewport.scrollTop;
      
      let candidate = null;
      let minDiff = Infinity;
      
      // Find the section closest to the top of the viewport
      for (const s of filteredSections) {
        const el = document.getElementById(`section-row-${s.section}`);
        if (el) {
          const diff = Math.abs(el.offsetTop - scrollTop);
          if (diff < minDiff) {
            minDiff = diff;
            candidate = s.section;
          }
        }
      }

      if (candidate) {
        setPreserveSection(candidate);
      }
    }

    setShowOnlySet(isChecked);
  };

  useEffect(() => {
    if (preserveSection) {
      // Small delay to ensure the DOM has updated with the new filter state
      const timer = setTimeout(() => {
        const el = document.getElementById(`section-row-${preserveSection}`);
        if (el) {
          el.scrollIntoView({ block: 'center' });
        }
        setPreserveSection(null);
      }, 100);
      return () => clearTimeout(timer);
    }
  }, [filteredSections, preserveSection]);

  if (sitesLoading) return <Center h={400}><Loader size="xl" /></Center>;

  if (!selectedSite) {
    return (
      <Card shadow="sm" padding="lg" radius="md" withBorder>
        <Title order={3} mb="md">Sections Manager</Title>
        <Text size="sm" c="dimmed" mb="md">Select a site to manage sections</Text>

        <Table highlightOnHover>
          <Table.Thead>
            <Table.Tr>
              <Table.Th>Site</Table.Th>
              <Table.Th>Actions</Table.Th>
            </Table.Tr>
          </Table.Thead>
          <Table.Tbody>
            {sitesData?.map((site: any) => (
              <Table.Tr key={site.name} style={{ cursor: 'pointer' }} onClick={() => setSelectedSite(site.name)}>
                <Table.Td fw={600}>{site.name}</Table.Td>
                <Table.Td>
                  <Button variant="light" size="xs" rightSection={<IconChevronRight size="1rem" />}>
                    Manage Sections
                  </Button>
                </Table.Td>
              </Table.Tr>
            ))}
          </Table.Tbody>
        </Table>
      </Card>
    );
  }

  return (
    <Card shadow="sm" padding="lg" radius="md" withBorder>
      <Stack gap="md">
        <Group justify="space-between">
          <Title order={3}>Sections for {selectedSite}</Title>
          <Button variant="outline" onClick={() => { setSelectedSite(null); setSearchQuery(''); }}>
            Back to Sites
          </Button>
        </Group>

        {sectionsLoading ? (
          <Center h={300}><Loader size="lg" /></Center>
        ) : (
          <>
            <Group>
              <TextInput
                placeholder="Search sections..."
                leftSection={<IconSearch size="1rem" />}
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.currentTarget.value)}
                style={{ flex: 1, maxWidth: 400 }}
              />
              <Switch
                label="Show configured only"
                checked={showOnlySet}
                onChange={handleToggleShowOnlySet}
              />
            </Group>

            <Text size="xs" c="dimmed">
              Click on a row to mark it. The view will try to keep the marked row visible when toggling filters.
            </Text>

            {filteredSections.length === 0 && searchQuery && (
              <Alert color="yellow" title="No results">
                No sections found matching "{searchQuery}"
              </Alert>
            )}

            {filteredSections.length === 0 && !searchQuery && showOnlySet && (
              <Alert color="blue" title="No configured sections">
                No sections have a directory path configured. Uncheck "Show configured only" to see all available sections.
              </Alert>
            )}

            {filteredSections.length === 0 && !searchQuery && !showOnlySet && (
              <Alert color="yellow" title="No sections">
                No sections found in slftp.precatcher configuration.
              </Alert>
            )}

            {filteredSections.length > 0 && (
              <>
                <Table withTableBorder withColumnBorders style={{ tableLayout: 'fixed', borderBottom: 'none' }}>
                  <Table.Thead>
                    <Table.Tr>
                      <Table.Th style={{ width: 200 }}>Section</Table.Th>
                      <Table.Th>Directory Path</Table.Th>
                    </Table.Tr>
                  </Table.Thead>
                </Table>
                <ScrollArea h={600} viewportRef={viewportRef} type="always">
                  <Table striped highlightOnHover withTableBorder withColumnBorders style={{ tableLayout: 'fixed', borderTop: 'none' }}>
                    <Table.Tbody>
                      {filteredSections.map((sectionData: SectionData) => {
                        const isModified = sectionDirs[sectionData.section] !== originalDirs[sectionData.section];
                        const isMarked = markedSection === sectionData.section;
                        return (
                          <Table.Tr 
                            key={sectionData.section} 
                            id={`section-row-${sectionData.section}`}
                            onClick={() => setMarkedSection(isMarked ? null : sectionData.section)}
                            style={{ 
                              cursor: 'pointer', 
                              backgroundColor: isMarked ? 'var(--mantine-color-blue-light)' : undefined,
                              scrollMarginTop: '45px'
                            }}
                          >
                            <Table.Td style={{ width: 200 }}>
                              <Group gap="xs" justify="space-between">
                                <Group gap="xs">
                                  <Text fw={600}>{sectionData.section}</Text>
                                  {isModified && <Badge size="xs" color="yellow">modified</Badge>}
                                </Group>
                                {isMarked && (
                                  <Tooltip label="Marked for scroll preservation">
                                    <IconPin size="1rem" color="var(--mantine-color-blue-6)" />
                                  </Tooltip>
                                )}
                              </Group>
                            </Table.Td>
                            <Table.Td onClick={(e) => e.stopPropagation()}>
                              <TextInput
                                value={sectionDirs[sectionData.section] || ''}
                                onChange={(e) => setSectionDirs({ ...sectionDirs, [sectionData.section]: e.currentTarget.value })}
                                placeholder="/path/to/section/"
                                size="xs"
                              />
                            </Table.Td>
                          </Table.Tr>
                        );
                      })}
                    </Table.Tbody>
                  </Table>
                </ScrollArea>
              </>
            )}

            <Group justify="space-between" align="center">
              <Text size="sm" c="dimmed">
                Showing {filteredSections.length} of {sectionsData?.length || 0} sections
                {hasChanges && (
                  <Text span c="yellow" fw={600} ml="md">
                    • {changesCount} unsaved change(s)
                  </Text>
                )}
              </Text>
              {hasChanges && (
                <Button
                  leftSection={<IconDeviceFloppy size="1rem" />}
                  onClick={handleSaveAll}
                  loading={saveAllMutation.isPending}
                  color="green"
                  size="md"
                >
                  Save All Changes
                </Button>
              )}
            </Group>
          </>
        )}
      </Stack>
    </Card>
  );
}
