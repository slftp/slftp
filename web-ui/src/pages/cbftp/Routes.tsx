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
  ActionIcon,
  Checkbox,
  Divider,
  MultiSelect
} from '@mantine/core';
import { 
  IconRoute, 
  IconRefresh, 
  IconSearch, 
  IconArrowUpRight, 
  IconArrowDownLeft, 
  IconGridDots, 
  IconX,
  IconUsers,
  IconTrash,
  IconBolt
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
  const [selectedSites, setSelectedSites] = useState<string[]>([]);
  const [viewMode, setViewMode] = useState<'matrix' | 'bulk'>('matrix');

  // Dedicated Bulk Tab States
  const [selectedSources, setSelectedSources] = useState<string[]>([]);
  const [selectedDests, setSelectedDests] = useState<string[]>([]);
  const [bulkAction, setBulkAction] = useState<'ALLOW' | 'BLOCK' | 'RESET'>('ALLOW');
  const [addBackRoutes, setAddBackRoutes] = useState(false);
  const [bulkAffilTarget, setBulkAffilTarget] = useState(false);

  const handleDirectionChange = (val: 'outgoing' | 'incoming' | 'affil') => {
    setDirection(val);
    setSelectedSites([]);
  };

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

  const bulkUpdateMutation = useMutation({
    mutationFn: async ({ names, updatesList }: { names: string[]; updatesList: Partial<CbftpSite>[] }) => {
      await Promise.all(
        names.map((name, idx) => updateSite(name, updatesList[idx]))
      );
    },
    onSuccess: (_, variables) => {
      notifications.show({
        title: 'Bulk Route Configuration Saved',
        message: `Routing configuration for ${variables.names.length} sites has been synchronized with cbftp.`,
        color: 'green',
        icon: <IconRoute size="1.1rem" />,
      });
      queryClient.invalidateQueries({ queryKey: ['cbftp-sites-details-routes'] });
      setSelectedSites([]);
    },
    onError: (error: any) => {
      notifications.show({
        title: 'Bulk Synchronization Failed',
        message: `Failed to update sites: ${error.message || 'Unknown error'}`,
        color: 'red',
      });
    }
  });

  const handleBulkPolicyChange = (newPolicy: 'ALLOW' | 'BLOCK') => {
    const names = [...selectedSites];
    const updatesList = names.map(() => {
      if (direction === 'outgoing') {
        return { transfer_target_policy: newPolicy };
      } else if (direction === 'incoming') {
        return { transfer_source_policy: newPolicy };
      } else {
        return { transfer_target_affil_policy: newPolicy };
      }
    });

    bulkUpdateMutation.mutate({ names, updatesList });
  };

  const handleBulkAddException = (sourceSiteName: string) => {
    const names = [...selectedSites];
    const updatesList = names.map(name => {
      const site = sites?.find(s => s.name === name);
      if (!site) return {};

      if (direction === 'outgoing') {
        const excepts = site.except_target_sites || [];
        if (excepts.includes(sourceSiteName)) return {};
        return { except_target_sites: [...excepts, sourceSiteName] };
      } else if (direction === 'incoming') {
        const excepts = site.except_source_sites || [];
        if (excepts.includes(sourceSiteName)) return {};
        return { except_source_sites: [...excepts, sourceSiteName] };
      } else {
        const excepts = site.except_target_affil_sites || [];
        if (excepts.includes(sourceSiteName)) return {};
        return { except_target_affil_sites: [...excepts, sourceSiteName] };
      }
    });

    const namesToUpdate: string[] = [];
    const finalUpdates: Partial<CbftpSite>[] = [];
    names.forEach((name, idx) => {
      const update = updatesList[idx];
      if (update && Object.keys(update).length > 0) {
        namesToUpdate.push(name);
        finalUpdates.push(update);
      }
    });

    if (namesToUpdate.length > 0) {
      bulkUpdateMutation.mutate({ names: namesToUpdate, updatesList: finalUpdates });
    } else {
      notifications.show({
        title: 'No Changes Needed',
        message: 'All selected sites already have this exception configured.',
        color: 'blue'
      });
    }
  };

  const handleBulkClearExceptions = () => {
    const names = [...selectedSites];
    const updatesList = names.map(() => {
      if (direction === 'outgoing') {
        return { except_target_sites: [] };
      } else if (direction === 'incoming') {
        return { except_source_sites: [] };
      } else {
        return { except_target_affil_sites: [] };
      }
    });

    bulkUpdateMutation.mutate({ names, updatesList });
  };

  const handleBulkSetRoutes = async () => {
    if (selectedSources.length === 0 || selectedDests.length === 0) {
      notifications.show({
        title: 'Validation Error',
        message: 'Please select at least one source site and one destination site.',
        color: 'red'
      });
      return;
    }

    if (!sites) return;

    const accumulatedUpdates: Record<string, {
      except_target_sites: string[];
      except_target_affil_sites: string[];
      except_source_sites: string[];
      transfer_target_policy?: 'ALLOW' | 'BLOCK';
      transfer_target_affil_policy?: 'ALLOW' | 'BLOCK';
      transfer_source_policy?: 'ALLOW' | 'BLOCK';
    }> = {};

    const getEntry = (name: string) => {
      if (accumulatedUpdates[name]) return accumulatedUpdates[name];
      const site = sites.find(s => s.name === name);
      if (!site) return null;

      accumulatedUpdates[name] = {
        except_target_sites: [...(site.except_target_sites || [])],
        except_target_affil_sites: [...(site.except_target_affil_sites || [])],
        except_source_sites: [...(site.except_source_sites || [])],
        transfer_target_policy: site.transfer_target_policy,
        transfer_target_affil_policy: site.transfer_target_affil_policy,
        transfer_source_policy: site.transfer_source_policy
      };
      return accumulatedUpdates[name];
    };

    for (const src of selectedSources) {
      for (const dst of selectedDests) {
        if (src === dst) continue;

        const srcEntry = getEntry(src);
        const dstEntry = getEntry(dst);

        if (srcEntry && dstEntry) {
          if (bulkAction === 'RESET') {
            if (bulkAffilTarget) {
              srcEntry.except_target_affil_sites = srcEntry.except_target_affil_sites.filter(x => x !== dst);
            } else {
              srcEntry.except_target_sites = srcEntry.except_target_sites.filter(x => x !== dst);
            }
            dstEntry.except_source_sites = dstEntry.except_source_sites.filter(x => x !== src);
          } else {
            const policyVal = bulkAction === 'ALLOW' ? 'BLOCK' : 'ALLOW';
            if (bulkAffilTarget) {
              if (!srcEntry.except_target_affil_sites.includes(dst)) {
                srcEntry.except_target_affil_sites.push(dst);
              }
              srcEntry.transfer_target_affil_policy = policyVal;
            } else {
              if (!srcEntry.except_target_sites.includes(dst)) {
                srcEntry.except_target_sites.push(dst);
              }
              srcEntry.transfer_target_policy = policyVal;
            }

            if (!dstEntry.except_source_sites.includes(src)) {
              dstEntry.except_source_sites.push(src);
            }
            dstEntry.transfer_source_policy = policyVal;
          }
        }

        if (addBackRoutes) {
          const backSrcEntry = getEntry(dst);
          const backDstEntry = getEntry(src);

          if (backSrcEntry && backDstEntry) {
            if (bulkAction === 'RESET') {
              if (bulkAffilTarget) {
                backSrcEntry.except_target_affil_sites = backSrcEntry.except_target_affil_sites.filter(x => x !== src);
              } else {
                backSrcEntry.except_target_sites = backSrcEntry.except_target_sites.filter(x => x !== src);
              }
              backDstEntry.except_source_sites = backDstEntry.except_source_sites.filter(x => x !== dst);
            } else {
              const policyVal = bulkAction === 'ALLOW' ? 'BLOCK' : 'ALLOW';
              if (bulkAffilTarget) {
                if (!backSrcEntry.except_target_affil_sites.includes(src)) {
                  backSrcEntry.except_target_affil_sites.push(src);
                }
                backSrcEntry.transfer_target_affil_policy = policyVal;
              } else {
                if (!backSrcEntry.except_target_sites.includes(src)) {
                  backSrcEntry.except_target_sites.push(src);
                }
                backSrcEntry.transfer_target_policy = policyVal;
              }

              if (!backDstEntry.except_source_sites.includes(dst)) {
                backDstEntry.except_source_sites.push(dst);
              }
              backDstEntry.transfer_source_policy = policyVal;
            }
          }
        }
      }
    }

    const namesToUpdate: string[] = [];
    const updatesList: Partial<CbftpSite>[] = [];

    for (const [name, mod] of Object.entries(accumulatedUpdates)) {
      const orig = sites.find(s => s.name === name);
      if (!orig) continue;

      const patch: Partial<CbftpSite> = {};
      let isChanged = false;

      const compareArray = (a1: string[], a2: string[]) => {
        if (a1.length !== a2.length) return false;
        return a1.every(x => a2.includes(x));
      };

      if (!compareArray(mod.except_target_sites, orig.except_target_sites || [])) {
        patch.except_target_sites = mod.except_target_sites;
        isChanged = true;
      }
      if (!compareArray(mod.except_target_affil_sites, orig.except_target_affil_sites || [])) {
        patch.except_target_affil_sites = mod.except_target_affil_sites;
        isChanged = true;
      }
      if (!compareArray(mod.except_source_sites, orig.except_source_sites || [])) {
        patch.except_source_sites = mod.except_source_sites;
        isChanged = true;
      }
      if (mod.transfer_target_policy !== orig.transfer_target_policy) {
        patch.transfer_target_policy = mod.transfer_target_policy;
        isChanged = true;
      }
      if (mod.transfer_target_affil_policy !== orig.transfer_target_affil_policy) {
        patch.transfer_target_affil_policy = mod.transfer_target_affil_policy;
        isChanged = true;
      }
      if (mod.transfer_source_policy !== orig.transfer_source_policy) {
        patch.transfer_source_policy = mod.transfer_source_policy;
        isChanged = true;
      }

      if (isChanged) {
        namesToUpdate.push(name);
        updatesList.push(patch);
      }
    }

    if (namesToUpdate.length > 0) {
      bulkUpdateMutation.mutate({ names: namesToUpdate, updatesList });
      setSelectedSources([]);
      setSelectedDests([]);
    } else {
      notifications.show({
        title: 'No Changes Needed',
        message: 'The selected routes already match the desired configuration.',
        color: 'blue'
      });
    }
  };

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
      <Group justify="space-between" align="center" wrap="wrap" gap="md">
        <Group gap="sm">
          <IconRoute size="2rem" color="var(--mantine-color-blue-6)" />
          <div>
            <Title order={3}>cbftp Routing Matrix</Title>
            <Text size="xs" c="dimmed">
              Configure allowed transfer source/target policies and exceptions directly in cbftp's settings.
            </Text>
          </div>
        </Group>
        <Group gap="md">
          <SegmentedControl
            value={viewMode}
            onChange={(val) => setViewMode(val as 'matrix' | 'bulk')}
            data={[
              { label: 'Matrix View', value: 'matrix' },
              { label: 'Bulk Operations', value: 'bulk' },
            ]}
          />
          <Button
            variant="light"
            onClick={handleRefresh}
            leftSection={<IconRefresh size="1rem" />}
          >
            Refresh Data
          </Button>
        </Group>
      </Group>

      {viewMode === 'matrix' ? (
        <Grid gutter="md">
        {/* Left Column: Draggable Sites Palette */}
        <Grid.Col span={{ base: 12, md: 3 }} style={{ position: 'relative' }}>
          <Card 
            shadow="sm" 
            padding="md" 
            radius="md" 
            withBorder 
            style={{ 
              position: 'sticky', 
              top: '80px', 
              zIndex: 10 
            }}
          >
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
                  onChange={(val) => handleDirectionChange(val as 'outgoing' | 'incoming' | 'affil')}
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

              {/* Bulk Actions Panel */}
              {selectedSites.length > 0 && (
                <Card withBorder padding="md" radius="md" style={{ backgroundColor: 'var(--mantine-color-blue-light)' }}>
                  <Stack gap="xs">
                    <Group justify="space-between">
                      <Group gap="xs">
                        <IconRoute size="1.2rem" color="var(--mantine-color-blue-6)" />
                        <Text fw={600} size="sm">Bulk Edit Options ({selectedSites.length} sites selected)</Text>
                      </Group>
                      <Button variant="subtle" size="xs" color="gray" onClick={() => setSelectedSites([])}>
                        Deselect All
                      </Button>
                    </Group>
                    <Divider my="xs" style={{ borderColor: 'var(--mantine-color-blue-3)' }} />
                    <Group gap="md" wrap="wrap">
                      <Group gap="xs">
                        <Text size="xs" fw={500}>Set General Policy:</Text>
                        <Button 
                          size="xs" 
                          variant="filled" 
                          color="blue"
                          onClick={() => handleBulkPolicyChange('ALLOW')}
                          disabled={bulkUpdateMutation.isPending}
                        >
                          Allow All
                        </Button>
                        <Button 
                          size="xs" 
                          variant="filled" 
                          color="red"
                          onClick={() => handleBulkPolicyChange('BLOCK')}
                          disabled={bulkUpdateMutation.isPending}
                        >
                          Block All
                        </Button>
                      </Group>
                      
                      <Divider orientation="vertical" style={{ height: '20px', borderColor: 'var(--mantine-color-blue-3)' }} />

                      <Group gap="xs">
                        <Text size="xs" fw={500}>Add Exception Site:</Text>
                        <Select
                          placeholder="Select site..."
                          data={allSiteNames.filter(name => !selectedSites.includes(name))}
                          size="xs"
                          searchable
                          style={{ width: 150 }}
                          onChange={(val) => {
                            if (val) handleBulkAddException(val);
                          }}
                          value={null}
                          disabled={bulkUpdateMutation.isPending}
                        />
                      </Group>

                      <Divider orientation="vertical" style={{ height: '20px', borderColor: 'var(--mantine-color-blue-3)' }} />

                      <Button
                        size="xs"
                        variant="outline"
                        color="red"
                        leftSection={<IconTrash size="0.8rem" />}
                        onClick={handleBulkClearExceptions}
                        disabled={bulkUpdateMutation.isPending}
                      >
                        Clear All Exceptions
                      </Button>
                    </Group>
                  </Stack>
                </Card>
              )}

              {/* Matrix Table */}
              <Table.ScrollContainer minWidth={600}>
                <Table striped highlightOnHover withTableBorder withColumnBorders verticalSpacing="sm">
                  <Table.Thead>
                    <Table.Tr>
                      <Table.Th style={{ width: 40 }}>
                        <Checkbox
                          checked={
                            filteredMatrixSites.length > 0 && 
                            filteredMatrixSites.every(site => selectedSites.includes(site.name))
                          }
                          indeterminate={
                            filteredMatrixSites.some(site => selectedSites.includes(site.name)) &&
                            !filteredMatrixSites.every(site => selectedSites.includes(site.name))
                          }
                          onChange={(event) => {
                            if (event.currentTarget.checked) {
                              const visibleNames = filteredMatrixSites.map(s => s.name);
                              setSelectedSites(prev => {
                                const newSelection = new Set([...prev, ...visibleNames]);
                                return Array.from(newSelection);
                              });
                            } else {
                              const visibleNames = filteredMatrixSites.map(s => s.name);
                              setSelectedSites(prev => prev.filter(name => !visibleNames.includes(name)));
                            }
                          }}
                        />
                      </Table.Th>
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
                      const isUpdating = (updateSiteMutation.isPending && updateSiteMutation.variables?.name === site.name) ||
                                         (bulkUpdateMutation.isPending && selectedSites.includes(site.name));
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
                          {/* Checkbox Column */}
                          <Table.Td>
                            <Checkbox
                              checked={selectedSites.includes(site.name)}
                              onChange={(event) => {
                                if (event.currentTarget.checked) {
                                  setSelectedSites(prev => [...prev, site.name]);
                                } else {
                                  setSelectedSites(prev => prev.filter(name => name !== site.name));
                                }
                              }}
                              disabled={isUpdating}
                            />
                          </Table.Td>

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
                        <Table.Td colSpan={4}>
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
      ) : (
        <Stack gap="md">
          <Card shadow="sm" padding="lg" radius="md" withBorder>
            <Stack gap="md">
              <Group gap="xs">
                <IconBolt size="1.5rem" color="var(--mantine-color-blue-6)" />
                <Text fw={600} size="lg">Bulk Route Operations</Text>
              </Group>
              <Text size="sm" c="dimmed">
                Configure policies and exception routes for multiple sources and destinations simultaneously.
              </Text>

              <Grid gutter="md">
                <Grid.Col span={{ base: 12, md: 6 }}>
                  <Stack gap="xs">
                    <Text fw={500} size="sm">Source Sites ({selectedSources.length} selected)</Text>
                    <MultiSelect
                      data={allSiteNames}
                      value={selectedSources}
                      onChange={setSelectedSources}
                      placeholder="Select source sites..."
                      searchable
                      clearable
                      maxDropdownHeight={250}
                    />
                  </Stack>
                </Grid.Col>

                <Grid.Col span={{ base: 12, md: 6 }}>
                  <Stack gap="xs">
                    <Text fw={500} size="sm">Destination Sites ({selectedDests.length} selected)</Text>
                    <MultiSelect
                      data={allSiteNames}
                      value={selectedDests}
                      onChange={setSelectedDests}
                      placeholder="Select destination sites..."
                      searchable
                      clearable
                      maxDropdownHeight={250}
                    />
                  </Stack>
                </Grid.Col>
              </Grid>

              <Divider my="sm" />

              <Group align="flex-end" gap="xl" wrap="wrap">
                <Stack gap="xs">
                  <Text fw={500} size="sm">Action / Policy</Text>
                  <SegmentedControl
                    value={bulkAction}
                    onChange={(val) => setBulkAction(val as 'ALLOW' | 'BLOCK' | 'RESET')}
                    data={[
                      { label: 'Allow Transfers', value: 'ALLOW' },
                      { label: 'Block Transfers', value: 'BLOCK' },
                      { label: 'Reset Exceptions', value: 'RESET' }
                    ]}
                    color={bulkAction === 'ALLOW' ? 'blue' : bulkAction === 'BLOCK' ? 'red' : 'gray'}
                  />
                </Stack>

                <Checkbox
                  label="Add back routes (bidirectional)"
                  checked={addBackRoutes}
                  onChange={(e) => setAddBackRoutes(e.currentTarget.checked)}
                  description="Apply this policy configuration in both directions"
                />

                <Checkbox
                  label="Affiliate Targets only"
                  checked={bulkAffilTarget}
                  onChange={(e) => setBulkAffilTarget(e.currentTarget.checked)}
                  description="Apply specifically to target affiliate policies (transfer_target_affil_policy)"
                />
              </Group>

              <Group mt="md">
                <Button
                  onClick={handleBulkSetRoutes}
                  disabled={selectedSources.length === 0 || selectedDests.length === 0}
                  loading={bulkUpdateMutation.isPending}
                  size="md"
                  leftSection={<IconBolt size="1.2rem" />}
                >
                  Apply to {selectedSources.length * selectedDests.length * (addBackRoutes ? 2 : 1)} Routes
                </Button>
                <Button
                  variant="light"
                  color="gray"
                  onClick={() => {
                    setSelectedSources([]);
                    setSelectedDests([]);
                    setBulkAction('ALLOW');
                    setAddBackRoutes(false);
                    setBulkAffilTarget(false);
                  }}
                >
                  Clear Selection
                </Button>
              </Group>

              {selectedSources.length > 0 && selectedDests.length > 0 && (
                <Card withBorder style={{ backgroundColor: 'var(--mantine-color-blue-light)' }} p="sm" radius="md">
                  <Text size="sm" fw={500}>Preview of changes:</Text>
                  <Text size="xs" c="dimmed">
                    Will apply policy <b>{bulkAction}</b> for: {selectedSources.join(', ')} → {selectedDests.join(', ')}
                    {addBackRoutes && ' (and reverse directions)'}
                    {bulkAffilTarget && ' (Target Affiliate Policy only)'}
                  </Text>
                </Card>
              )}
            </Stack>
          </Card>
        </Stack>
      )}
    </Stack>
  );
}
