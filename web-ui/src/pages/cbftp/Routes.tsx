import { 
  Card, 
  Title, 
  Stack, 
  Group, 
  Text, 
  Button, 
  Loader, 
  Center, 
  TextInput, 
  CloseButton, 
  Table, 
  Badge, 
  SegmentedControl, 
  Tooltip, 
  ScrollArea, 
  Select,
  Grid,
  ActionIcon
} from '@mantine/core';
import { 
  IconRoute, 
  IconRefresh, 
  IconSearch, 
  IconArrowUpRight, 
  IconArrowDownLeft, 
  IconGridDots, 
  IconX,
  IconUsers
} from '@tabler/icons-react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useMemo, useState } from 'react';
import { getSites, getSite, updateSite } from '../../api/cbftpClient';
import type { CbftpSite } from '../../api/cbftpClient';
import { notifications } from '@mantine/notifications';

export function Routes() {
  const queryClient = useQueryClient();
  const [paletteSearch, setPaletteSearch] = useState('');
  const [matrixSearch, setMatrixSearch] = useState('');
  const [direction, setDirection] = useState<'outgoing' | 'incoming' | 'affil'>('outgoing');
  const [dragOverRow, setDragOverRow] = useState<string | null>(null);

  // Fetch site names
  const { data: siteNames, isLoading: namesLoading, refetch: refetchNames } = useQuery({
    queryKey: ['cbftp-site-names-routes'],
    queryFn: () => getSites(),
  });

  // Fetch detailed site data
  const { data: sites, isLoading: detailsLoading, refetch: refetchDetails } = useQuery({
    queryKey: ['cbftp-sites-details-routes', siteNames],
    queryFn: async () => {
      if (!siteNames) return [];
      const details = await Promise.all(
        siteNames.map(name => getSite(name))
      );
      return details;
    },
    enabled: !!siteNames,
  });

  const updateSiteMutation = useMutation({
    mutationFn: async ({ name, updates }: { name: string; updates: Partial<CbftpSite> }) => {
      await updateSite(name, updates);
    },
    onSuccess: (_, variables) => {
      notifications.show({
        title: 'Route Configuration Saved',
        message: `Routing configuration for ${variables.name} has been synchronized with cbftp.`,
        color: 'green',
        icon: <IconRoute size="1.1rem" />,
      });
      queryClient.invalidateQueries({ queryKey: ['cbftp-sites-details-routes'] });
    },
    onError: (error: any, variables) => {
      notifications.show({
        title: 'Synchronization Failed',
        message: `Failed to update ${variables.name}: ${error.message || 'Unknown error'}`,
        color: 'red',
      });
    }
  });

  const allSiteNames = useMemo(() => {
    if (!siteNames) return [];
    // Filter out slftp management site
    return siteNames.filter(name => name.toLowerCase() !== 'slftp');
  }, [siteNames]);

  const filteredPaletteSites = useMemo(() => {
    return allSiteNames.filter(name => 
      name.toLowerCase().includes(paletteSearch.toLowerCase())
    );
  }, [allSiteNames, paletteSearch]);

  const filteredMatrixSites = useMemo(() => {
    if (!sites) return [];
    return sites
      .filter(s => s.name.toLowerCase() !== 'slftp')
      .filter(s => s.name.toLowerCase().includes(matrixSearch.toLowerCase()));
  }, [sites, matrixSearch]);

  const handleRefresh = () => {
    refetchNames();
    refetchDetails();
  };

  const handleDrop = (e: React.DragEvent, targetSiteName: string) => {
    e.preventDefault();
    setDragOverRow(null);
    const sourceSiteName = e.dataTransfer.getData('text/plain');

    if (!sourceSiteName) return;

    if (sourceSiteName === targetSiteName) {
      notifications.show({
        title: 'Routing Error',
        message: 'A site cannot route to itself.',
        color: 'orange',
      });
      return;
    }

    const site = sites?.find(s => s.name === targetSiteName);
    if (!site) return;

    if (direction === 'outgoing') {
      const excepts = site.except_target_sites || [];
      if (excepts.includes(sourceSiteName)) {
        notifications.show({
          title: 'Already Configured',
          message: `${sourceSiteName} is already in the outgoing exceptions list for ${targetSiteName}.`,
          color: 'blue',
        });
        return;
      }
      updateSiteMutation.mutate({
        name: targetSiteName,
        updates: {
          except_target_sites: [...excepts, sourceSiteName]
        }
      });
    } else if (direction === 'incoming') {
      const excepts = site.except_source_sites || [];
      if (excepts.includes(sourceSiteName)) {
        notifications.show({
          title: 'Already Configured',
          message: `${sourceSiteName} is already in the incoming exceptions list for ${targetSiteName}.`,
          color: 'blue',
        });
        return;
      }
      updateSiteMutation.mutate({
        name: targetSiteName,
        updates: {
          except_source_sites: [...excepts, sourceSiteName]
        }
      });
    } else {
      const excepts = site.except_target_affil_sites || [];
      if (excepts.includes(sourceSiteName)) {
        notifications.show({
          title: 'Already Configured',
          message: `${sourceSiteName} is already in the affiliated exceptions list for ${targetSiteName}.`,
          color: 'blue',
        });
        return;
      }
      updateSiteMutation.mutate({
        name: targetSiteName,
        updates: {
          except_target_affil_sites: [...excepts, sourceSiteName]
        }
      });
    }
  };

  const handleAddSiteInline = (targetSiteName: string, sourceSiteName: string) => {
    if (sourceSiteName === targetSiteName) return;
    
    const site = sites?.find(s => s.name === targetSiteName);
    if (!site) return;

    if (direction === 'outgoing') {
      const excepts = site.except_target_sites || [];
      if (excepts.includes(sourceSiteName)) return;
      updateSiteMutation.mutate({
        name: targetSiteName,
        updates: {
          except_target_sites: [...excepts, sourceSiteName]
        }
      });
    } else if (direction === 'incoming') {
      const excepts = site.except_source_sites || [];
      if (excepts.includes(sourceSiteName)) return;
      updateSiteMutation.mutate({
        name: targetSiteName,
        updates: {
          except_source_sites: [...excepts, sourceSiteName]
        }
      });
    } else {
      const excepts = site.except_target_affil_sites || [];
      if (excepts.includes(sourceSiteName)) return;
      updateSiteMutation.mutate({
        name: targetSiteName,
        updates: {
          except_target_affil_sites: [...excepts, sourceSiteName]
        }
      });
    }
  };

  const handleRemoveException = (siteName: string, exceptionToRemove: string) => {
    const site = sites?.find(s => s.name === siteName);
    if (!site) return;

    if (direction === 'outgoing') {
      const excepts = site.except_target_sites || [];
      updateSiteMutation.mutate({
        name: siteName,
        updates: {
          except_target_sites: excepts.filter(s => s !== exceptionToRemove)
        }
      });
    } else if (direction === 'incoming') {
      const excepts = site.except_source_sites || [];
      updateSiteMutation.mutate({
        name: siteName,
        updates: {
          except_source_sites: excepts.filter(s => s !== exceptionToRemove)
        }
      });
    } else {
      const excepts = site.except_target_affil_sites || [];
      updateSiteMutation.mutate({
        name: siteName,
        updates: {
          except_target_affil_sites: excepts.filter(s => s !== exceptionToRemove)
        }
      });
    }
  };

  const handlePolicyChange = (siteName: string, newPolicy: 'ALLOW' | 'BLOCK') => {
    let updates = {};
    if (direction === 'outgoing') {
      updates = { transfer_target_policy: newPolicy };
    } else if (direction === 'incoming') {
      updates = { transfer_source_policy: newPolicy };
    } else {
      updates = { transfer_target_affil_policy: newPolicy };
    }
    updateSiteMutation.mutate({
      name: siteName,
      updates
    });
  };

  const handleClearAll = (siteName: string) => {
    let updates = {};
    if (direction === 'outgoing') {
      updates = { except_target_sites: [] };
    } else if (direction === 'incoming') {
      updates = { except_source_sites: [] };
    } else {
      updates = { except_target_affil_sites: [] };
    }
    updateSiteMutation.mutate({
      name: siteName,
      updates
    });
  };

  if (namesLoading || detailsLoading) {
    return (
      <Center h={300}>
        <Stack align="center" gap="xs">
          <Loader size="lg" />
          <Text size="sm" c="dimmed">Loading routing configuration from cbftp...</Text>
        </Stack>
      </Center>
    );
  }

  return (
    <Stack gap="md">
      <Group justify="space-between">
        <Group gap="sm">
          <IconRoute size="2rem" color="var(--mantine-color-blue-6)" />
          <div>
            <Title order={3}>cbftp Routing Matrix</Title>
            <Text size="xs" c="dimmed">
              Configure allowed transfer source/target policies and exceptions directly in cbftp's settings.
            </Text>
          </div>
        </Group>
        <Button
          variant="light"
          onClick={handleRefresh}
          leftSection={<IconRefresh size="1rem" />}
        >
          Refresh Data
        </Button>
      </Group>

      <Grid gutter="md">
        {/* Left Column: Draggable Sites Palette */}
        <Grid.Col span={{ base: 12, md: 3 }}>
          <Card shadow="sm" padding="md" radius="md" withBorder style={{ height: '100%' }}>
            <Stack gap="md">
              <Group gap="xs">
                <IconGridDots size="1.2rem" color="var(--mantine-color-blue-6)" />
                <Text fw={600} size="md">Sites Palette</Text>
              </Group>
              
              <Text size="xs" c="dimmed">
                Drag a site badge from below and drop it into a row exception list on the right.
              </Text>

              <TextInput
                placeholder="Filter palette..."
                value={paletteSearch}
                onChange={(e) => setPaletteSearch(e.currentTarget.value)}
                leftSection={<IconSearch size="0.9rem" />}
                rightSection={paletteSearch ? <CloseButton size="xs" onClick={() => setPaletteSearch('')} /> : null}
                size="xs"
              />

              <ScrollArea h={{ base: 200, md: 500 }} offsetScrollbars>
                <Stack gap="xs" pr="xs">
                  {filteredPaletteSites.map((name) => (
                    <Badge
                      key={name}
                      size="lg"
                      variant="light"
                      color="blue"
                      draggable
                      onDragStart={(e) => e.dataTransfer.setData('text/plain', name)}
                      style={{ 
                        cursor: 'grab', 
                        width: '100%', 
                        height: '32px',
                        justifyContent: 'center',
                        textTransform: 'none',
                        letterSpacing: 'normal',
                        userSelect: 'none'
                      }}
                    >
                      {name}
                    </Badge>
                  ))}
                  {filteredPaletteSites.length === 0 && (
                    <Center p="md">
                      <Text size="xs" c="dimmed">No sites found</Text>
                    </Center>
                  )}
                </Stack>
              </ScrollArea>
            </Stack>
          </Card>
        </Grid.Col>

        {/* Right Column: Route Matrix */}
        <Grid.Col span={{ base: 12, md: 9 }}>
          <Card shadow="sm" padding="lg" radius="md" withBorder>
            <Stack gap="md">
              {/* Header and filters */}
              <Group justify="space-between" align="center" wrap="wrap" gap="md">
                <SegmentedControl
                  value={direction}
                  onChange={(val) => setDirection(val as 'outgoing' | 'incoming' | 'affil')}
                  data={[
                    { 
                      label: (
                        <Center style={{ gap: 6 }}>
                          <IconArrowUpRight size="0.9rem" />
                          <span>Outgoing Uploads</span>
                        </Center>
                      ), 
                      value: 'outgoing' 
                    },
                    { 
                      label: (
                        <Center style={{ gap: 6 }}>
                          <IconArrowDownLeft size="0.9rem" />
                          <span>Incoming Downloads</span>
                        </Center>
                      ), 
                      value: 'incoming' 
                    },
                    { 
                      label: (
                        <Center style={{ gap: 6 }}>
                          <IconUsers size="0.9rem" color="var(--mantine-color-purple-6)" />
                          <span>Affiliated Targets</span>
                        </Center>
                      ), 
                      value: 'affil' 
                    },
                  ]}
                  size="sm"
                />

                <TextInput
                  placeholder="Filter matrix rows..."
                  value={matrixSearch}
                  onChange={(e) => setMatrixSearch(e.currentTarget.value)}
                  leftSection={<IconSearch size="0.9rem" />}
                  rightSection={matrixSearch ? <CloseButton size="xs" onClick={() => setMatrixSearch('')} /> : null}
                  size="xs"
                  style={{ minWidth: 240 }}
                />
              </Group>

              <Text size="xs" c="dimmed">
                🎯 {direction === 'outgoing' 
                  ? 'Manage upload destinations. Set general policy, then drop allowed/blocked sites into the exceptions list.' 
                  : direction === 'incoming'
                    ? 'Manage download sources. Set general policy, then drop allowed/blocked sites into the exceptions list.'
                    : 'Manage affiliated upload destinations. Set general policy, then drop allowed/blocked sites into the exceptions list.'}
              </Text>

              {/* Matrix Table */}
              <Table.ScrollContainer minWidth={600}>
                <Table striped highlightOnHover withTableBorder withColumnBorders verticalSpacing="sm">
                  <Table.Thead>
                    <Table.Tr>
                      <Table.Th style={{ width: 140 }}>
                        <Text fw={700} size="sm">Site Name</Text>
                      </Table.Th>
                      <Table.Th style={{ width: 220 }}>
                        <Text fw={700} size="sm">General Policy</Text>
                      </Table.Th>
                      <Table.Th>
                        <Text fw={700} size="sm">
                          Exceptions (Drop Sites Here)
                        </Text>
                      </Table.Th>
                    </Table.Tr>
                  </Table.Thead>
                  
                  <Table.Tbody>
                    {filteredMatrixSites.map((site) => {
                      const isUpdating = updateSiteMutation.isPending && updateSiteMutation.variables?.name === site.name;
                      const policy = direction === 'outgoing'
                        ? (site.transfer_target_policy || 'BLOCK')
                        : direction === 'incoming'
                          ? (site.transfer_source_policy || 'BLOCK')
                          : (site.transfer_target_affil_policy || 'BLOCK');
                      
                      const exceptions = direction === 'outgoing'
                        ? (site.except_target_sites || [])
                        : direction === 'incoming'
                          ? (site.except_source_sites || [])
                          : (site.except_target_affil_sites || []);

                      // Options for inline adding: all sites except this site and already excepted ones
                      const inlineOptions = allSiteNames
                        .filter(name => name !== site.name && !exceptions.includes(name))
                        .map(name => ({ value: name, label: name }));

                      return (
                        <Table.Tr key={site.name} style={{ opacity: isUpdating ? 0.6 : 1, transition: 'opacity 0.2s' }}>
                          {/* Row Header: Site Name */}
                          <Table.Td style={{ fontWeight: 600 }}>
                            <Group gap="xs" wrap="nowrap">
                              {isUpdating ? (
                                <Loader size="xs" />
                              ) : (
                                <IconGridDots 
                                  size="0.85rem" 
                                  color="var(--mantine-color-dimmed)" 
                                  style={{ cursor: 'default' }} 
                                />
                              )}
                              <Text size="sm" fw={600}>{site.name}</Text>
                            </Group>
                          </Table.Td>

                          {/* Row Policy Toggle */}
                          <Table.Td>
                            <SegmentedControl
                              value={policy}
                              onChange={(val) => handlePolicyChange(site.name, val as 'ALLOW' | 'BLOCK')}
                              data={[
                                { label: 'Allow All', value: 'ALLOW' },
                                { label: 'Block All', value: 'BLOCK' }
                              ]}
                              size="xs"
                              color={policy === 'ALLOW' ? 'blue' : 'red'}
                              disabled={isUpdating}
                              fullWidth
                            />
                          </Table.Td>

                          {/* Drop Zone & Badges */}
                          <Table.Td>
                            <div
                              onDragOver={(e) => {
                                e.preventDefault();
                                if (dragOverRow !== site.name) setDragOverRow(site.name);
                              }}
                              onDragLeave={() => setDragOverRow(null)}
                              onDrop={(e) => handleDrop(e, site.name)}
                              style={{
                                border: dragOverRow === site.name 
                                  ? '2px dashed var(--mantine-color-blue-6)' 
                                  : '1px dashed var(--mantine-color-default-border)',
                                backgroundColor: dragOverRow === site.name 
                                  ? 'var(--mantine-color-blue-light)' 
                                  : 'transparent',
                                borderRadius: 'var(--mantine-radius-md)',
                                padding: '8px 12px',
                                minHeight: '52px',
                                display: 'flex',
                                alignItems: 'center',
                                flexWrap: 'wrap',
                                gap: '8px',
                                transition: 'all 0.15s ease'
                              }}
                            >
                              {exceptions.map((exName) => (
                                <Badge
                                  key={exName}
                                  color={policy === 'BLOCK' ? 'teal' : 'red'}
                                  variant="light"
                                  size="md"
                                  rightSection={
                                    <ActionIcon 
                                      size="xs" 
                                      color={policy === 'BLOCK' ? 'teal' : 'red'}
                                      variant="subtle" 
                                      onClick={() => handleRemoveException(site.name, exName)}
                                      disabled={isUpdating}
                                      radius="xl"
                                    >
                                      <IconX size="0.65rem" />
                                    </ActionIcon>
                                  }
                                  style={{ textTransform: 'none' }}
                                >
                                  {exName}
                                </Badge>
                              ))}

                              {/* Placeholder instruction */}
                              {exceptions.length === 0 && (
                                <Text size="xs" c="dimmed" style={{ flexGrow: 1 }}>
                                  {policy === 'BLOCK' 
                                    ? `Block All except: drop allowed ${direction === 'outgoing' ? 'targets' : direction === 'incoming' ? 'sources' : 'affil targets'} here`
                                    : `Allow All except: drop blocked ${direction === 'outgoing' ? 'targets' : direction === 'incoming' ? 'sources' : 'affil targets'} here`}
                                </Text>
                              )}

                              {/* Action buttons (inline selector and clear) */}
                              <Group gap="xs" style={{ marginLeft: 'auto' }}>
                                {inlineOptions.length > 0 && (
                                  <Tooltip label="Add exception site" position="top">
                                    <Select
                                      placeholder="Add..."
                                      data={inlineOptions}
                                      size="xs"
                                      searchable
                                      style={{ width: 100 }}
                                      onChange={(val) => {
                                        if (val) handleAddSiteInline(site.name, val);
                                      }}
                                      value={null}
                                      disabled={isUpdating}
                                      styles={{
                                        input: {
                                          height: '24px',
                                          minHeight: '24px',
                                          lineHeight: '24px',
                                          fontSize: '11px',
                                          paddingLeft: '6px',
                                          paddingRight: '6px'
                                        }
                                      }}
                                    />
                                  </Tooltip>
                                )}

                                {exceptions.length > 0 && (
                                  <Tooltip label="Clear exceptions" position="top">
                                    <ActionIcon 
                                      size="sm" 
                                      color="gray" 
                                      variant="subtle"
                                      onClick={() => handleClearAll(site.name)}
                                      disabled={isUpdating}
                                    >
                                      <IconX size="0.8rem" />
                                    </ActionIcon>
                                  </Tooltip>
                                )}
                              </Group>
                            </div>
                          </Table.Td>
                        </Table.Tr>
                      );
                    })}

                    {filteredMatrixSites.length === 0 && (
                      <Table.Tr>
                        <Table.Td colSpan={3}>
                          <Center p="xl">
                            <Text size="sm" c="dimmed">No sites match the matrix filter.</Text>
                          </Center>
                        </Table.Td>
                      </Table.Tr>
                    )}
                  </Table.Tbody>
                </Table>
              </Table.ScrollContainer>
            </Stack>
          </Card>
        </Grid.Col>
      </Grid>
    </Stack>
  );
}
