import { Table, Badge, Title, Card, Alert, Loader, Center, Group, ActionIcon, Tooltip, Text, TextInput, Modal, NumberInput, Button, Stack, Select, Switch, Tabs, Divider, ScrollArea } from '@mantine/core';
import { IconSearch, IconRefresh, IconBolt, IconSettings, IconTrash, IconRepeat, IconToolsKitchen3, IconRobot, IconHeartbeat, IconShieldOff, IconRoute } from '@tabler/icons-react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useState } from 'react';
import { apiClient } from '../api/client';
import type { Site, RouteEntry } from '../api/client';
import { notifications } from '@mantine/notifications';

export function SitesList() {
  const queryClient = useQueryClient();
  const [search, setSearch] = useState('');
  const [selected, setSelected] = useState<Site | null>(null);
  const [slotsValue, setSlotsValue] = useState<number | ''>('');
  const [maxDnValue, setMaxDnValue] = useState<number | ''>('');
  const [maxUpValue, setMaxUpValue] = useState<number | ''>('');
  const [maxPreDnValue, setMaxPreDnValue] = useState<number | ''>('');
  const [statusValue, setStatusValue] = useState<'UP' | 'DOWN' | ''>('');
  const [permDown, setPermDown] = useState(false);
  const [autoLogin, setAutoLogin] = useState(false);
  const [autoRulesInterval, setAutoRulesInterval] = useState<number | ''>('');

  // Fetch Sites
  const { data, isLoading, error } = useQuery({
    queryKey: ['sites'],
    queryFn: async () => {
      // GetSites takes a "Filter" argument.
      // API call: POST /ApiSitesService/GetSites with body { Filter: "" }
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: '' });
      
      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }

      // The 'Sites' field is a JSON string, we need to parse it
      const rawSites = responseData.Sites;
      let parsedSites: Site[] = [];
      try {
        if (typeof rawSites === 'string') {
            parsedSites = JSON.parse(rawSites);
        } else if (Array.isArray(rawSites)) {
            parsedSites = rawSites;
        }
      } catch (e) {
        console.error("Failed to parse sites JSON", e);
      }
      
      return parsedSites;
    },
    refetchInterval: 10000,
  });

  const formatSlots = (site: Site) => {
    const dn = site.max_dn ?? site.slots ?? 0;
    const up = site.max_up ?? 0;
    return `${dn}/${up || '-'}`;
  };

  const formatActive = (site: Site) => {
    const dn = site.num_dn ?? 0;
    const up = site.num_up ?? 0;
    if (dn === 0 && up === 0) return '0';
    return `${dn}/${up}`;
  };

  const { data: routes, isFetching: routesLoading, refetch: refetchRoutes } = useQuery({
    queryKey: ['routes', selected?.name],
    enabled: !!selected,
    queryFn: async (): Promise<RouteEntry[]> => {
      if (!selected) return [];
      const res = await apiClient.post('/ApiSitesService/GetSiteRoutes', { SiteName: selected.name });
      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }
      const rawRoutes = responseData.Routes;
      if (!rawRoutes) return [];
      try {
        if (typeof rawRoutes === 'string') {
          return JSON.parse(rawRoutes);
        }
        if (Array.isArray(rawRoutes)) {
          return rawRoutes;
        }
      } catch (e) {
        console.error('Failed to parse routes JSON', e);
      }
      return [];
    },
  });

  // Mutation for Test Site
  const testSiteMutation = useMutation({
    mutationFn: async (siteName: string) => {
      await apiClient.post('/ApiSitesService/TestSite', { SiteName: siteName });
    },
    onSuccess: (_, siteName) => {
      notifications.show({
        title: 'Test initiated',
        message: `Connection test started for ${siteName} (refreshing soon…)`,
        color: 'blue',
      });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
      // fire a couple of delayed refreshes to surface status changes
      setTimeout(() => queryClient.invalidateQueries({ queryKey: ['sites'] }), 3000);
      setTimeout(() => queryClient.invalidateQueries({ queryKey: ['sites'] }), 8000);
    },
    onError: (err) => {
      notifications.show({
        title: 'Error',
        message: err.message,
        color: 'red',
      });
    },
    onSettled: () => {
      queryClient.invalidateQueries({ queryKey: ['sites'] });
    }
  });

  const saveSettingsMutation = useMutation({
    mutationFn: async (payload: { site: Site; slots: number; maxDn: number; maxUp: number; maxPreDn: number; permDown: boolean; autoLogin: boolean; autoRulesInterval: number }) => {
      // Apply slots first, then max up/dn
      await apiClient.post('/ApiSitesService/SetSiteSlots', { SiteName: payload.site.name, Slots: payload.slots });
      await apiClient.post('/ApiSitesService/SetSiteMaxUpDn', { SiteName: payload.site.name, MaxUp: payload.maxUp, MaxDn: payload.maxDn });
      await apiClient.post('/ApiSitesService/SetSiteMaxPreDn', { SiteName: payload.site.name, MaxPreDn: payload.maxPreDn });
      await apiClient.post('/ApiSitesService/SetSitePermDown', { SiteName: payload.site.name, PermDown: payload.permDown });
      await apiClient.post('/ApiSitesService/SetSiteAutoLogin', { SiteName: payload.site.name, Enabled: payload.autoLogin });
      await apiClient.post('/ApiSitesService/SetSiteAutoRules', { SiteName: payload.site.name, IntervalSeconds: payload.autoRulesInterval });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Gespeichert',
        message: 'Site-Einstellungen wurden aktualisiert.',
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
      setSelected(null);
    },
    onError: (err) => {
      notifications.show({
        title: 'Fehler',
        message: err.message,
        color: 'red',
      });
    }
  });

  const statusMutation = useMutation({
    mutationFn: async (payload: { site: Site; status: 'UP' | 'DOWN' }) => {
      await apiClient.post('/ApiSitesService/SetSiteStatus', { SiteName: payload.site.name, Status: payload.status });
    },
    onSuccess: () => {
      notifications.show({ title: 'Status geändert', message: 'Site-Status aktualisiert.', color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
    },
    onError: (err) => {
      notifications.show({ title: 'Fehler', message: err.message, color: 'red' });
    }
  });

  const ghostMutation = useMutation({
    mutationFn: async (siteName: string) => {
      await apiClient.post('/ApiSitesService/GhostSite', { SiteName: siteName });
    },
    onSuccess: (_, siteName) => {
      notifications.show({ title: 'Ghost-Kill', message: `Ghost-Sessions auf ${siteName} beendet.`, color: 'blue' });
    },
    onError: (err) => notifications.show({ title: 'Fehler', message: err.message, color: 'red' })
  });

  const clearQueueMutation = useMutation({
    mutationFn: async (siteName: string) => {
      await apiClient.post('/ApiQueueService/EmptyQueue', { SiteName: siteName });
    },
    onSuccess: (_, siteName) => {
      notifications.show({ title: 'Queue geleert', message: `Queue für ${siteName} geleert.`, color: 'blue' });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
    },
    onError: (err) => notifications.show({ title: 'Fehler', message: err.message, color: 'red' })
  });

  const recalcMutation = useMutation({
    mutationFn: async (siteName: string) => {
      await apiClient.post('/ApiSitesService/RecalcFreeSlots', { SiteName: siteName });
    },
    onSuccess: (_, siteName) => {
      notifications.show({ title: 'Freeslots aktualisiert', message: `Freeslots für ${siteName} neu berechnet.`, color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
    },
    onError: (err) => notifications.show({ title: 'Fehler', message: err.message, color: 'red' })
  });

  const rebuildMutation = useMutation({
    mutationFn: async (siteName: string) => {
      await apiClient.post('/ApiSitesService/RebuildSlots', { SiteName: siteName });
    },
    onSuccess: (_, siteName) => {
      notifications.show({ title: 'Slots neu aufgebaut', message: `Slots für ${siteName} wurden resettet.`, color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
    },
    onError: (err) => notifications.show({ title: 'Fehler', message: err.message, color: 'red' })
  });

  const runAutorulesMutation = useMutation({
    mutationFn: async (siteName: string) => {
      await apiClient.post('/ApiSitesService/RunSiteAutoRules', { SiteName: siteName });
    },
    onSuccess: (_, siteName) => notifications.show({ title: 'Autorules gestartet', message: `Autorules läuft für ${siteName}.`, color: 'blue' }),
    onError: (err) => notifications.show({ title: 'Fehler', message: err.message, color: 'red' })
  });

  const openEditor = (site: Site) => {
    setSelected(site);
    setSlotsValue(site.slots ?? 0);
    setMaxDnValue(site.max_dn ?? site.slots ?? 0);
    setMaxUpValue(site.max_up ?? 0);
    setMaxPreDnValue(site.max_pre_dn ?? site.max_dn ?? site.slots ?? 0);
    setStatusValue(site.status === 'DOWN' || site.status === 'DOWN_BY_USER' ? 'DOWN' : 'UP');
    setPermDown(Boolean(site.permdown));
    setAutoLogin(Boolean(site.autologin));
    setAutoRulesInterval(site.autorules_interval ?? 0);
  };

  const handleSave = () => {
    if (!selected) return;
    if (slotsValue === '' || maxDnValue === '' || maxUpValue === '' || maxPreDnValue === '' || autoRulesInterval === '') return;
    saveSettingsMutation.mutate({ site: selected, slots: slotsValue, maxDn: maxDnValue, maxUp: maxUpValue, maxPreDn: maxPreDnValue, permDown, autoLogin, autoRulesInterval });
  };

  // Helper for Status Badge
  const getStatusBadge = (status: string) => {
    switch (status) {
      case 'UP': return <Badge color="green">UP</Badge>;
      case 'DOWN': return <Badge color="red">DOWN</Badge>;
      case 'DOWN_BY_USER': return <Badge color="gray">DISABLED</Badge>;
      default: return <Badge color="yellow">{status}</Badge>;
    }
  };

  if (isLoading) return <Center h={400}><Loader size="xl" /></Center>;
  if (error) return <Alert color="red" title="Error">Could not load sites</Alert>;

  // Filter client-side for search
  const filteredSites = data?.filter(site => 
    site.name.toLowerCase().includes(search.toLowerCase())
  ) || [];

  const rows = filteredSites.map((site) => (
    <Table.Tr key={site.name}>
      <Table.Td fw={500}>
        <Group gap="xs">
          <ActionIcon variant="subtle" color="gray" onClick={() => openEditor(site)} aria-label="Edit site">
            <IconSettings size="1rem" />
          </ActionIcon>
          <Text fw={600} onClick={() => openEditor(site)} style={{ cursor: 'pointer' }}>
            {site.name}
          </Text>
        </Group>
      </Table.Td>
      <Table.Td>{getStatusBadge(site.status)}</Table.Td>
      <Table.Td>{formatSlots(site)}</Table.Td>
      <Table.Td>{site.freeslots}</Table.Td>
      <Table.Td>{formatActive(site)}</Table.Td>
      <Table.Td>
        <Group gap="xs">
          <Tooltip label="Test Connection">
            <ActionIcon variant="light" color="blue" onClick={() => testSiteMutation.mutate(site.name)}>
              <IconBolt size="1rem" />
            </ActionIcon>
          </Tooltip>
          <Tooltip label="Ghost connections killen">
            <ActionIcon variant="light" color="orange" onClick={() => ghostMutation.mutate(site.name)}>
              <IconTrash size="1rem" />
            </ActionIcon>
          </Tooltip>
        </Group>
      </Table.Td>
    </Table.Tr>
  ));

  return (
    <Card shadow="sm" padding="lg" radius="md" withBorder>
      <Group justify="space-between" mb="md">
        <Title order={3}>Sites Manager</Title>
        <Group>
           <TextInput 
             placeholder="Search site..." 
             leftSection={<IconSearch size="0.9rem"/>}
             value={search}
             onChange={(e) => setSearch(e.currentTarget.value)}
           />
           <ActionIcon variant="outline" onClick={() => queryClient.invalidateQueries({ queryKey: ['sites']})}>
             <IconRefresh size="1.1rem" />
           </ActionIcon>
        </Group>
      </Group>

      <Table highlightOnHover>
        <Table.Thead>
          <Table.Tr>
            <Table.Th>Name</Table.Th>
            <Table.Th>Status</Table.Th>
            <Table.Th>Max DN/UP</Table.Th>
            <Table.Th>Free Slots</Table.Th>
            <Table.Th>Active DN/UP</Table.Th>
            <Table.Th>Actions</Table.Th>
          </Table.Tr>
        </Table.Thead>
      <Table.Tbody>{rows}</Table.Tbody>
    </Table>
    
    {filteredSites.length === 0 && (
      <Text c="dimmed" ta="center" py="xl">No sites found</Text>
    )}

    <Modal
      opened={!!selected}
      onClose={() => setSelected(null)}
      title={selected ? `Einstellungen: ${selected.name}` : 'Einstellungen'}
      centered
      size="xl"
    >
      <Tabs defaultValue="basics" keepMounted={false}>
        <Tabs.List grow>
          <Tabs.Tab value="basics" leftSection={<IconToolsKitchen3 size="1rem" />}>Basis</Tabs.Tab>
          <Tabs.Tab value="automation" leftSection={<IconRobot size="1rem" />}>Automation</Tabs.Tab>
          <Tabs.Tab value="health" leftSection={<IconHeartbeat size="1rem" />}>Speed & Health</Tabs.Tab>
          <Tabs.Tab value="maintenance" leftSection={<IconShieldOff size="1rem" />}>Maintenance</Tabs.Tab>
          <Tabs.Tab value="routes" leftSection={<IconRoute size="1rem" />}>Routes</Tabs.Tab>
        </Tabs.List>

        <Tabs.Panel value="basics" pt="md">
          <Stack gap="sm">
            <NumberInput
              label="Slots (gesamt)"
              value={slotsValue}
              min={0}
              onChange={(val) => setSlotsValue(val === '' ? '' : Number(val))}
            />
            <Group grow>
              <NumberInput
                label="Max Downloads"
                value={maxDnValue}
                min={0}
                onChange={(val) => setMaxDnValue(val === '' ? '' : Number(val))}
              />
              <NumberInput
                label="Max Uploads"
                value={maxUpValue}
                min={0}
                onChange={(val) => setMaxUpValue(val === '' ? '' : Number(val))}
              />
            </Group>
            <NumberInput
              label="Max Pre-Queue (max_pre_dn)"
              value={maxPreDnValue}
              min={0}
              onChange={(val) => setMaxPreDnValue(val === '' ? '' : Number(val))}
            />
            <Group grow>
              <Switch label="PermDown" checked={permDown} onChange={(e) => setPermDown(e.currentTarget.checked)} />
              <Switch label="Autologin" checked={autoLogin} onChange={(e) => setAutoLogin(e.currentTarget.checked)} />
            </Group>
            <Select
              label="Status"
              data={[
                { value: 'UP', label: 'UP' },
                { value: 'DOWN', label: 'DOWN (disable)' },
              ]}
              value={statusValue}
              onChange={(val) => setStatusValue(val as 'UP' | 'DOWN' | '')}
            />
          </Stack>
        </Tabs.Panel>

        <Tabs.Panel value="automation" pt="md">
          <Stack gap="sm">
            <NumberInput
              label="Autorules Intervall (Sekunden, 0 = aus)"
              value={autoRulesInterval}
              min={0}
              onChange={(val) => setAutoRulesInterval(val === '' ? '' : Number(val))}
            />
            <Group justify="space-between">
              <Button variant="outline" color="blue" loading={runAutorulesMutation.isPending} onClick={() => selected && runAutorulesMutation.mutate(selected.name)} leftSection={<IconRepeat size="1rem" />}>
                Autorules jetzt
              </Button>
              <Button variant="outline" disabled>
                Autodirlist (coming soon)
              </Button>
              <Button variant="outline" disabled>
                Autoindex (coming soon)
              </Button>
            </Group>
          </Stack>
        </Tabs.Panel>

        <Tabs.Panel value="health" pt="md">
          <Stack gap="sm">
            <Group justify="space-between">
              <Button variant="light" color="blue" leftSection={<IconBolt size="1rem" />} disabled>
                Speedtest local
              </Button>
              <Button variant="light" color="blue" disabled>
                Speedtest in/out
              </Button>
              <Button variant="light" color="gray" disabled>
                Speedtest cleanup
              </Button>
            </Group>
            <Divider label="Live" />
            <Text size="sm" c="dimmed">Aktive DN/UP werden live angezeigt; weitere Health-Actions folgen.</Text>
          </Stack>
        </Tabs.Panel>

        <Tabs.Panel value="maintenance" pt="md">
          <Stack gap="sm">
            <Group justify="space-between">
              <Button variant="light" color="orange" loading={clearQueueMutation.isPending} onClick={() => selected && clearQueueMutation.mutate(selected.name)}>
                Queue leeren
              </Button>
              <Button variant="light" color="red" loading={ghostMutation.isPending} onClick={() => selected && ghostMutation.mutate(selected.name)}>
                Ghost kill
              </Button>
              <Button variant="outline" color="gray" loading={statusMutation.isPending} onClick={() => selected && statusValue && statusMutation.mutate({ site: selected, status: statusValue })}>
                Status speichern
              </Button>
            </Group>
            <Group justify="space-between">
              <Button variant="outline" loading={recalcMutation.isPending} onClick={() => selected && recalcMutation.mutate(selected.name)}>
                Recalc freeslots
              </Button>
              <Button variant="outline" loading={rebuildMutation.isPending} onClick={() => selected && rebuildMutation.mutate(selected.name)}>
                Rebuild slots
              </Button>
            </Group>
          </Stack>
        </Tabs.Panel>

        <Tabs.Panel value="routes" pt="md">
          <Stack gap="sm">
            <Group justify="space-between">
              <Text fw={600}>Route-Liste (speed-from)</Text>
              <Button variant="light" onClick={() => refetchRoutes()} loading={routesLoading}>
                Refresh
              </Button>
            </Group>
            <ScrollArea h={260}>
              <Table striped highlightOnHover>
                <Table.Thead>
                  <Table.Tr>
                    <Table.Th>Dest</Table.Th>
                    <Table.Th>Speed</Table.Th>
                    <Table.Th>Flags</Table.Th>
                  </Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                  {(routes || []).map((r) => (
                    <Table.Tr key={r.dest}>
                      <Table.Td>{r.dest}</Table.Td>
                      <Table.Td>{r.speed}</Table.Td>
                      <Table.Td>
                        <Group gap="xs">
                          {r.locked && <Badge color="red" variant="light">locked</Badge>}
                          {r.affil_only && <Badge color="blue" variant="light">affil</Badge>}
                          {r.no_affil && <Badge color="gray" variant="light">no-affil</Badge>}
                          {!r.locked && !r.affil_only && !r.no_affil && <Text size="sm" c="dimmed">-</Text>}
                        </Group>
                      </Table.Td>
                    </Table.Tr>
                  ))}
                  {(!routes || routes.length === 0) && (
                    <Table.Tr>
                      <Table.Td colSpan={3}><Text c="dimmed">Keine Routes gefunden.</Text></Table.Td>
                    </Table.Tr>
                  )}
                </Table.Tbody>
              </Table>
            </ScrollArea>
          </Stack>
        </Tabs.Panel>
      </Tabs>

      <Divider my="md" />
      <Group justify="flex-end" mt="sm">
        <Button variant="default" onClick={() => setSelected(null)}>Abbrechen</Button>
        <Button loading={saveSettingsMutation.isPending} onClick={handleSave}>
          Speichern
        </Button>
      </Group>
    </Modal>
  </Card>
  );
}
