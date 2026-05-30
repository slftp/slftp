import { Card, Title, Stack, Group, Text, Button, Loader, Center, MultiSelect, TextInput, CloseButton, SimpleGrid, Select, Badge } from '@mantine/core';
import { IconRoute, IconRefresh, IconSearch, IconArrowUpRight, IconArrowDownLeft } from '@tabler/icons-react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useMemo, useState } from 'react';
import { getSites, getSite, updateSite } from '../../api/cbftpClient';
import type { CbftpSite } from '../../api/cbftpClient';
import { notifications } from '@mantine/notifications';

export function Routes() {
  const queryClient = useQueryClient();
  const [search, setSearch] = useState('');
  const [savingSite, setSavingSite] = useState<string | null>(null);

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

  // Local state for modified sites to avoid instant query refetch overwrite
  const [localSettings, setLocalSettings] = useState<Record<string, {
    transfer_target_policy: 'ALLOW' | 'BLOCK';
    except_target_sites: string[];
    transfer_source_policy: 'ALLOW' | 'BLOCK';
    except_source_sites: string[];
  }>>({});

  const updateSiteMutation = useMutation({
    mutationFn: async ({ name, updates }: { name: string; updates: Partial<CbftpSite> }) => {
      setSavingSite(name);
      await updateSite(name, updates);
    },
    onSuccess: (_, variables) => {
      notifications.show({
        title: 'Success',
        message: `Routing configuration for ${variables.name} updated successfully.`,
        color: 'green',
      });
      // Invalidate queries to get fresh data
      queryClient.invalidateQueries({ queryKey: ['cbftp-sites-details-routes'] });
      setSavingSite(null);
    },
    onError: (error: any, variables) => {
      notifications.show({
        title: 'Error',
        message: `Failed to update ${variables.name}: ${error.message || 'Unknown error'}`,
        color: 'red',
      });
      setSavingSite(null);
    }
  });

  const allSiteOptions = useMemo(() => {
    if (!siteNames) return [];
    return siteNames.map(name => ({ value: name, label: name }));
  }, [siteNames]);

  const filteredSites = useMemo(() => {
    if (!sites) return [];
    return sites.filter(s => s.name.toLowerCase().includes(search.toLowerCase()));
  }, [sites, search]);

  const handleSave = (name: string) => {
    const local = localSettings[name];
    if (!local) return;
    updateSiteMutation.mutate({
      name,
      updates: {
        transfer_target_policy: local.transfer_target_policy,
        except_target_sites: local.except_target_sites,
        transfer_source_policy: local.transfer_source_policy,
        except_source_sites: local.except_source_sites,
      }
    });
  };

  const handleFieldChange = (name: string, field: string, value: any) => {
    const currentSite = sites?.find(s => s.name === name);
    if (!currentSite) return;

    setLocalSettings(prev => {
      const existing = prev[name] || {
        transfer_target_policy: currentSite.transfer_target_policy || 'BLOCK',
        except_target_sites: currentSite.except_target_sites || [],
        transfer_source_policy: currentSite.transfer_source_policy || 'BLOCK',
        except_source_sites: currentSite.except_source_sites || [],
      };

      return {
        ...prev,
        [name]: {
          ...existing,
          [field]: value
        }
      };
    });
  };

  const handleRefresh = () => {
    setLocalSettings({});
    refetchNames();
    refetchDetails();
  };

  if (namesLoading || detailsLoading) {
    return (
      <Center h={200}>
        <Loader size="lg" />
      </Center>
    );
  }

  return (
    <Stack gap="md">
      <Group justify="space-between">
        <Group>
          <IconRoute size="1.8rem" color="var(--mantine-color-brand-6)" />
          <div>
            <Title order={3}>cbftp Site-to-Site Routes</Title>
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

      <TextInput
        placeholder="Filter sites..."
        value={search}
        onChange={(e) => setSearch(e.currentTarget.value)}
        leftSection={<IconSearch size="1.1rem" />}
        rightSection={search ? <CloseButton onClick={() => setSearch('')} /> : null}
        style={{ maxWidth: 400 }}
      />

      <SimpleGrid cols={{ base: 1, md: 2, lg: 3 }} spacing="lg">
        {filteredSites.map((site) => {
          const name = site.name;
          const current = localSettings[name] || {
            transfer_target_policy: site.transfer_target_policy || 'BLOCK',
            except_target_sites: site.except_target_sites || [],
            transfer_source_policy: site.transfer_source_policy || 'BLOCK',
            except_source_sites: site.except_source_sites || [],
          };

          const isModified = 
            current.transfer_target_policy !== (site.transfer_target_policy || 'BLOCK') ||
            JSON.stringify(current.except_target_sites.slice().sort()) !== JSON.stringify((site.except_target_sites || []).slice().sort()) ||
            current.transfer_source_policy !== (site.transfer_source_policy || 'BLOCK') ||
            JSON.stringify(current.except_source_sites.slice().sort()) !== JSON.stringify((site.except_source_sites || []).slice().sort());

          // Options for targets/sources exclude the current site
          const exceptOptions = allSiteOptions.filter(o => o.value !== name);

          return (
            <Card key={name} shadow="sm" padding="lg" radius="md" withBorder>
              <Stack gap="md" style={{ height: '100%', justifyContent: 'space-between' }}>
                <Stack gap="sm">
                  <Group justify="space-between">
                    <Text fw={700} size="lg">{name}</Text>
                    {isModified && (
                      <Badge color="orange" variant="light">Modified</Badge>
                    )}
                  </Group>

                  {/* Outgoing Routing (Target Policy) */}
                  <Card withBorder p="sm" radius="sm">
                    <Stack gap="xs">
                      <Group gap="xs">
                        <IconArrowUpRight size="1rem" color="var(--mantine-color-blue-6)" />
                        <Text fw={600} size="sm">Outgoing Uploads (Target Policy)</Text>
                      </Group>
                      
                      <Select
                        label="General Target Policy"
                        size="xs"
                        data={[
                          { value: 'BLOCK', label: 'Allow All (Except Blocked List)' },
                          { value: 'ALLOW', label: 'Block All (Except Allowed List)' }
                        ]}
                        value={current.transfer_target_policy}
                        onChange={(val) => handleFieldChange(name, 'transfer_target_policy', val)}
                      />

                      <MultiSelect
                        label={current.transfer_target_policy === 'BLOCK' ? 'Blocked Target Sites' : 'Allowed Target Sites'}
                        placeholder="Select sites..."
                        size="xs"
                        data={exceptOptions}
                        value={current.except_target_sites}
                        onChange={(val) => handleFieldChange(name, 'except_target_sites', val)}
                        searchable
                        clearable
                      />
                    </Stack>
                  </Card>

                  {/* Incoming Routing (Source Policy) */}
                  <Card withBorder p="sm" radius="sm">
                    <Stack gap="xs">
                      <Group gap="xs">
                        <IconArrowDownLeft size="1rem" color="var(--mantine-color-teal-6)" />
                        <Text fw={600} size="sm">Incoming Downloads (Source Policy)</Text>
                      </Group>
                      
                      <Select
                        label="General Source Policy"
                        size="xs"
                        data={[
                          { value: 'BLOCK', label: 'Allow All (Except Blocked List)' },
                          { value: 'ALLOW', label: 'Block All (Except Allowed List)' }
                        ]}
                        value={current.transfer_source_policy}
                        onChange={(val) => handleFieldChange(name, 'transfer_source_policy', val)}
                      />

                      <MultiSelect
                        label={current.transfer_source_policy === 'BLOCK' ? 'Blocked Source Sites' : 'Allowed Source Sites'}
                        placeholder="Select sites..."
                        size="xs"
                        data={exceptOptions}
                        value={current.except_source_sites}
                        onChange={(val) => handleFieldChange(name, 'except_source_sites', val)}
                        searchable
                        clearable
                      />
                    </Stack>
                  </Card>
                </Stack>

                <Button
                  fullWidth
                  variant={isModified ? 'filled' : 'light'}
                  color={isModified ? 'brand' : 'gray'}
                  onClick={() => handleSave(name)}
                  loading={savingSite === name}
                  disabled={!isModified}
                >
                  {isModified ? 'Apply & Save Config' : 'No Changes'}
                </Button>
              </Stack>
            </Card>
          );
        })}
      </SimpleGrid>
    </Stack>
  );
}
