import { useState, useMemo } from 'react';
import { Alert, Badge, Button, Checkbox, Group, Loader, Paper, Stack, Table, Text, Title, Tooltip, Center } from '@mantine/core';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { IconRefresh, IconArrowRight, IconCheck, IconX, IconAlertCircle } from '@tabler/icons-react';
import { notifications } from '@mantine/notifications';
import { getSites as getCbftpSites, getSite as getCbftpSite, updateSite as updateCbftpSite } from '../../api/cbftpClient';
import type { CbftpSite } from '../../api/cbftpClient';
import { apiClient } from '../../api/client';

interface SlftpSite {
  name: string;
  affils: string[];
}

interface SyncComparison {
  siteName: string;
  slftpAffils: string[];
  cbftpAffils: string[];
  needsSync: boolean;
  existsInCbftp: boolean;
}

export function AffilSync() {
  const queryClient = useQueryClient();
  const [selectedSites, setSelectedSites] = useState<Set<string>>(new Set());

  // Fetch slftp sites
  const { data: slftpSites, isLoading: loadingSlftp, error: slftpError } = useQuery({
    queryKey: ['slftp-sites-affils'],
    queryFn: async (): Promise<SlftpSite[]> => {
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: '' });
      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }
      const rawSites = responseData.Sites;
      let parsedSites: any[] = [];
      if (typeof rawSites === 'string') {
        parsedSites = JSON.parse(rawSites);
      } else if (Array.isArray(rawSites)) {
        parsedSites = rawSites;
      }

      // For each site, get detailed info with affils
      const sitesWithAffils: SlftpSite[] = [];
      for (const site of parsedSites) {
        if (site.name === 'SLFTP') continue; // Skip admin site
        try {
          const infoRes = await apiClient.post('/ApiSitesService/GetSiteInfo', { SiteName: site.name });
          const info = infoRes.data.result?.[0] || infoRes.data;
          const affilsStr = info.Affils || '';
          const affils = affilsStr.split(/\s+/).filter((a: string) => a.trim() !== '');
          sitesWithAffils.push({ name: site.name, affils });
        } catch {
          sitesWithAffils.push({ name: site.name, affils: [] });
        }
      }
      return sitesWithAffils;
    },
    staleTime: 30000,
  });

  // Fetch cbftp sites
  const { data: cbftpSiteNames, isLoading: loadingCbftpNames } = useQuery({
    queryKey: ['cbftp-sites-sync'],
    queryFn: () => getCbftpSites(),
    staleTime: 30000,
  });

  const { data: cbftpSites, isLoading: loadingCbftpDetails } = useQuery<CbftpSite[]>({
    queryKey: ['cbftp-sites-details-sync', cbftpSiteNames],
    queryFn: async () => {
      if (!cbftpSiteNames || cbftpSiteNames.length === 0) return [];
      return Promise.all(cbftpSiteNames.map((name) => getCbftpSite(name)));
    },
    enabled: !!cbftpSiteNames?.length,
    staleTime: 30000,
  });

  // Build comparison data
  const comparisons = useMemo((): SyncComparison[] => {
    if (!slftpSites) return [];

    const cbftpMap = new Map<string, CbftpSite>();
    cbftpSites?.forEach(site => cbftpMap.set(site.name, site));

    return slftpSites.map(slftpSite => {
      const cbftpSite = cbftpMap.get(slftpSite.name);
      const cbftpAffils = cbftpSite?.affils || [];
      const slftpAffils = slftpSite.affils;

      // Compare arrays (order independent)
      const slftpSet = new Set(slftpAffils);
      const cbftpSet = new Set(cbftpAffils);
      const needsSync = slftpSet.size !== cbftpSet.size ||
        [...slftpSet].some(a => !cbftpSet.has(a));

      return {
        siteName: slftpSite.name,
        slftpAffils,
        cbftpAffils,
        needsSync,
        existsInCbftp: !!cbftpSite,
      };
    }).sort((a, b) => {
      // Sites needing sync first, then alphabetically
      if (a.needsSync && !b.needsSync) return -1;
      if (!a.needsSync && b.needsSync) return 1;
      return a.siteName.localeCompare(b.siteName);
    });
  }, [slftpSites, cbftpSites]);

  const sitesNeedingSync = useMemo(() =>
    comparisons.filter(c => c.needsSync && c.existsInCbftp),
    [comparisons]
  );

  // Sync mutation
  const syncMutation = useMutation({
    mutationFn: async (siteNames: string[]) => {
      const results: { site: string; success: boolean; error?: string }[] = [];

      for (const siteName of siteNames) {
        const comparison = comparisons.find(c => c.siteName === siteName);
        if (!comparison || !comparison.existsInCbftp) continue;

        try {
          await updateCbftpSite(siteName, { affils: comparison.slftpAffils });
          results.push({ site: siteName, success: true });
        } catch (error) {
          results.push({ site: siteName, success: false, error: String(error) });
        }
      }

      return results;
    },
    onSuccess: (results) => {
      const successCount = results.filter(r => r.success).length;
      const failCount = results.filter(r => !r.success).length;

      if (successCount > 0) {
        notifications.show({
          title: 'Sync Complete',
          message: `Successfully synced ${successCount} site(s)${failCount > 0 ? `, ${failCount} failed` : ''}`,
          color: failCount > 0 ? 'yellow' : 'green',
        });
      } else if (failCount > 0) {
        notifications.show({
          title: 'Sync Failed',
          message: `Failed to sync ${failCount} site(s)`,
          color: 'red',
        });
      }

      queryClient.invalidateQueries({ queryKey: ['cbftp-sites-details-sync'] });
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
    if (selectedSites.size === sitesNeedingSync.length) {
      setSelectedSites(new Set());
    } else {
      setSelectedSites(new Set(sitesNeedingSync.map(c => c.siteName)));
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

  const handleSyncSelected = () => {
    if (selectedSites.size > 0) {
      syncMutation.mutate([...selectedSites]);
    }
  };

  const handleSyncSingle = (siteName: string) => {
    syncMutation.mutate([siteName]);
  };

  const isLoading = loadingSlftp || loadingCbftpNames || loadingCbftpDetails;

  if (isLoading) {
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
          <div>
            <Title order={4}>Affil Sync</Title>
            <Text size="sm" c="dimmed">
              Sync affils from slftp sites to matching cbftp sites
            </Text>
          </div>
          <Group>
            <Button
              variant="light"
              leftSection={<IconRefresh size={16} />}
              onClick={() => {
                queryClient.invalidateQueries({ queryKey: ['slftp-sites-affils'] });
                queryClient.invalidateQueries({ queryKey: ['cbftp-sites-sync'] });
                queryClient.invalidateQueries({ queryKey: ['cbftp-sites-details-sync'] });
              }}
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
            slftp Sites: {slftpSites?.length || 0}
          </Badge>
          <Badge color="cyan" variant="light" size="lg">
            cbftp Sites: {cbftpSiteNames?.length || 0}
          </Badge>
          <Badge color={sitesNeedingSync.length > 0 ? 'yellow' : 'green'} variant="light" size="lg">
            Need Sync: {sitesNeedingSync.length}
          </Badge>
        </Group>

        {comparisons.length > 0 ? (
          <Table striped highlightOnHover>
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
                <Table.Th>slftp Affils</Table.Th>
                <Table.Th>cbftp Affils</Table.Th>
                <Table.Th>Status</Table.Th>
                <Table.Th style={{ width: 100 }}>Action</Table.Th>
              </Table.Tr>
            </Table.Thead>
            <Table.Tbody>
              {comparisons.map((comp) => (
                <Table.Tr key={comp.siteName}>
                  <Table.Td>
                    <Checkbox
                      checked={selectedSites.has(comp.siteName)}
                      onChange={() => handleToggleSite(comp.siteName)}
                      disabled={!comp.needsSync || !comp.existsInCbftp}
                    />
                  </Table.Td>
                  <Table.Td>
                    <Text fw={500}>{comp.siteName}</Text>
                  </Table.Td>
                  <Table.Td>
                    <Group gap={4}>
                      {comp.slftpAffils.length > 0 ? (
                        comp.slftpAffils.map((affil) => (
                          <Badge key={affil} size="sm" variant="light" color="blue">
                            {affil}
                          </Badge>
                        ))
                      ) : (
                        <Text size="sm" c="dimmed">-</Text>
                      )}
                    </Group>
                  </Table.Td>
                  <Table.Td>
                    {comp.existsInCbftp ? (
                      <Group gap={4}>
                        {comp.cbftpAffils.length > 0 ? (
                          comp.cbftpAffils.map((affil) => (
                            <Badge key={affil} size="sm" variant="light" color="cyan">
                              {affil}
                            </Badge>
                          ))
                        ) : (
                          <Text size="sm" c="dimmed">-</Text>
                        )}
                      </Group>
                    ) : (
                      <Text size="sm" c="dimmed" fs="italic">Not in cbftp</Text>
                    )}
                  </Table.Td>
                  <Table.Td>
                    {!comp.existsInCbftp ? (
                      <Badge color="gray" variant="outline">N/A</Badge>
                    ) : comp.needsSync ? (
                      <Badge color="yellow" leftSection={<IconX size={12} />}>Out of Sync</Badge>
                    ) : (
                      <Badge color="green" leftSection={<IconCheck size={12} />}>In Sync</Badge>
                    )}
                  </Table.Td>
                  <Table.Td>
                    {comp.existsInCbftp && comp.needsSync && (
                      <Tooltip label="Sync this site">
                        <Button
                          size="xs"
                          variant="light"
                          leftSection={<IconArrowRight size={14} />}
                          onClick={() => handleSyncSingle(comp.siteName)}
                          loading={syncMutation.isPending}
                        >
                          Sync
                        </Button>
                      </Tooltip>
                    )}
                  </Table.Td>
                </Table.Tr>
              ))}
            </Table.Tbody>
          </Table>
        ) : (
          <Paper p="xl" withBorder>
            <Text ta="center" c="dimmed">No sites found</Text>
          </Paper>
        )}
      </Paper>
    </Stack>
  );
}
