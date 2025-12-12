import { Card, Title, Table, Alert, Loader, Center, TextInput, Button, Stack, Group, Text, ScrollArea, Badge } from '@mantine/core';
import { IconChevronRight, IconSearch, IconDeviceFloppy } from '@tabler/icons-react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useState, useMemo } from 'react';
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
    if (!searchQuery) return sectionsData;

    const query = searchQuery.toLowerCase();
    return sectionsData.filter(s => s.section.toLowerCase().includes(query));
  }, [sectionsData, searchQuery]);

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
            <TextInput
              placeholder="Search sections..."
              leftSection={<IconSearch size="1rem" />}
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.currentTarget.value)}
              style={{ flex: 1, maxWidth: 400 }}
            />

            {filteredSections.length === 0 && searchQuery && (
              <Alert color="yellow" title="No results">
                No sections found matching "{searchQuery}"
              </Alert>
            )}

            {filteredSections.length === 0 && !searchQuery && (
              <Alert color="yellow" title="No sections">
                No sections found in slftp.precatcher configuration.
              </Alert>
            )}

            {filteredSections.length > 0 && (
              <ScrollArea h={600}>
                <Table striped highlightOnHover withTableBorder withColumnBorders>
                  <Table.Thead style={{ position: 'sticky', top: 0, zIndex: 1, background: 'var(--mantine-color-body)' }}>
                    <Table.Tr>
                      <Table.Th style={{ width: 200 }}>Section</Table.Th>
                      <Table.Th>Directory Path</Table.Th>
                    </Table.Tr>
                  </Table.Thead>
                  <Table.Tbody>
                    {filteredSections.map((sectionData: SectionData) => {
                      const isModified = sectionDirs[sectionData.section] !== originalDirs[sectionData.section];
                      return (
                        <Table.Tr key={sectionData.section}>
                          <Table.Td>
                            <Group gap="xs">
                              <Text fw={600}>{sectionData.section}</Text>
                              {isModified && <Badge size="xs" color="yellow">modified</Badge>}
                            </Group>
                          </Table.Td>
                          <Table.Td>
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
