import { Table, Badge, Title, Card, Alert, Loader, Center, Group, ActionIcon, Tooltip, Text, TextInput, Modal, NumberInput, Button, Stack, Select, Switch, Tabs, Divider, Box } from '@mantine/core';
import { IconSearch, IconRefresh, IconBolt, IconSettings, IconTrash, IconToolsKitchen3, IconShieldOff, IconPlus, IconX } from '@tabler/icons-react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useState } from 'react';
import { apiClient } from '../api/client';
import type { Site, Bnc } from '../api/client';
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
  const [usernameValue, setUsernameValue] = useState('');
  const [passwordValue, setPasswordValue] = useState('');
  const [bncs, setBncs] = useState<Bnc[]>([]);
  const [maxIdle, setMaxIdle] = useState<number | ''>(0);
  const [idleInterval, setIdleInterval] = useState<number | ''>(30);
  const [legacyCwd, setLegacyCwd] = useState(false);

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

  // Mutation for BNC Test (executes !bnctest command)
  const testSiteMutation = useMutation({
    mutationFn: async (siteName: string) => {
      await apiClient.post('/ApiSitesService/ExecuteIrcCommand', { Command: `bnctest ${siteName}` });
    },
    onSuccess: (_, siteName) => {
      notifications.show({
        title: 'BNC Test Started',
        message: `Running !bnctest ${siteName} - Check IRC for detailed results (IP, ping times)`,
        color: 'blue',
        autoClose: 8000,
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
    mutationFn: async (payload: { site: Site; slots: number; maxDn: number; maxUp: number; maxPreDn: number; permDown: boolean; autoLogin: boolean; autoRulesInterval: number; username: string; password: string; bncs: Bnc[]; maxIdle: number; idleInterval: number; legacyCwd: boolean; status: 'UP' | 'DOWN' }) => {
      await apiClient.post('/ApiSitesService/SetSiteSlots', { SiteName: payload.site.name, Slots: payload.slots });
      await apiClient.post('/ApiSitesService/SetSiteMaxUpDn', { SiteName: payload.site.name, MaxUp: payload.maxUp, MaxDn: payload.maxDn });
      await apiClient.post('/ApiSitesService/SetSiteMaxPreDn', { SiteName: payload.site.name, MaxPreDn: payload.maxPreDn });
      await apiClient.post('/ApiSitesService/SetSitePermDown', { SiteName: payload.site.name, PermDown: payload.permDown });
      await apiClient.post('/ApiSitesService/SetSiteAutoLogin', { SiteName: payload.site.name, Enabled: payload.autoLogin });
      await apiClient.post('/ApiSitesService/SetSiteAutoRules', { SiteName: payload.site.name, IntervalSeconds: payload.autoRulesInterval });
      await apiClient.post('/ApiSitesService/SetSiteCredentials', { SiteName: payload.site.name, Username: payload.username, Password: payload.password, BncsJson: JSON.stringify(payload.bncs), MaxIdle: payload.maxIdle, IdleInterval: payload.idleInterval, LegacyCwd: payload.legacyCwd });
      await apiClient.post('/ApiSitesService/SetSiteStatus', { SiteName: payload.site.name, Status: payload.status });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Saved',
        message: 'Site settings updated.',
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
      setSelected(null);
    },
    onError: (err) => {
      notifications.show({
        title: 'Error',
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
      notifications.show({ title: 'Status changed', message: 'Site status updated.', color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
    },
    onError: (err) => {
      notifications.show({ title: 'Error', message: err.message, color: 'red' });
    }
  });

  const ghostMutation = useMutation({
    mutationFn: async (siteName: string) => {
      await apiClient.post('/ApiSitesService/GhostSite', { SiteName: siteName });
    },
    onSuccess: (_, siteName) => {
      notifications.show({ title: 'Ghost kill', message: `Ghost sessions on ${siteName} terminated.`, color: 'blue' });
    },
    onError: (err) => notifications.show({ title: 'Error', message: err.message, color: 'red' })
  });

  const clearQueueMutation = useMutation({
    mutationFn: async (siteName: string) => {
      await apiClient.post('/ApiQueueService/EmptyQueue', { SiteName: siteName });
    },
    onSuccess: (_, siteName) => {
      notifications.show({ title: 'Queue cleared', message: `Queue for ${siteName} cleared.`, color: 'blue' });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
    },
    onError: (err) => notifications.show({ title: 'Error', message: err.message, color: 'red' })
  });

  const recalcMutation = useMutation({
    mutationFn: async (siteName: string) => {
      await apiClient.post('/ApiSitesService/RecalcFreeSlots', { SiteName: siteName });
    },
    onSuccess: (_, siteName) => {
      notifications.show({ title: 'Freeslots recalculated', message: `Freeslots for ${siteName} recalculated.`, color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
    },
    onError: (err) => notifications.show({ title: 'Error', message: err.message, color: 'red' })
  });

  const rebuildMutation = useMutation({
    mutationFn: async (siteName: string) => {
      await apiClient.post('/ApiSitesService/RebuildSlots', { SiteName: siteName });
    },
    onSuccess: (_, siteName) => {
      notifications.show({ title: 'Slots rebuilt', message: `Slots for ${siteName} were reset.`, color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
    },
    onError: (err) => notifications.show({ title: 'Error', message: err.message, color: 'red' })
  });

  const openEditor = async (site: Site) => {
    setSelected(site);
    setSlotsValue(site.slots ?? 0);
    setMaxDnValue(site.max_dn ?? site.slots ?? 0);
    setMaxUpValue(site.max_up ?? 0);
    setMaxPreDnValue(site.max_pre_dn ?? site.max_dn ?? site.slots ?? 0);
    setStatusValue(site.status === 'DOWN' || site.status === 'DOWN_BY_USER' ? 'DOWN' : 'UP');
    setPermDown(Boolean(site.permdown));
    setAutoLogin(Boolean(site.autologin));
    setAutoRulesInterval(site.autorules_interval ?? 0);

    try {
      const res = await apiClient.post('/ApiSitesService/GetSiteInfo', { SiteName: site.name });
      const info = res.data.result?.[0] || res.data;

      setUsernameValue(info.Username || '');
      setPasswordValue('');
      setMaxIdle(info.MaxIdle ?? 0);
      setIdleInterval(info.IdleInterval ?? 30);
      setLegacyCwd(Boolean(info.LegacyCwd));

      if (info.Bncs) {
        const parsedBncs = typeof info.Bncs === 'string' ? JSON.parse(info.Bncs) : info.Bncs;
        setBncs(parsedBncs || []);
      } else {
        setBncs([]);
      }
    } catch (e) {
      console.error('Failed to load site info:', e);
      setBncs([]);
    }
  };

  const handleSave = () => {
    if (!selected || !statusValue) return;
    if (slotsValue === '' || maxDnValue === '' || maxUpValue === '' || maxPreDnValue === '' || autoRulesInterval === '' || maxIdle === '' || idleInterval === '') return;
    saveSettingsMutation.mutate({ site: selected, slots: slotsValue, maxDn: maxDnValue, maxUp: maxUpValue, maxPreDn: maxPreDnValue, permDown, autoLogin, autoRulesInterval, username: usernameValue, password: passwordValue, bncs, maxIdle, idleInterval, legacyCwd, status: statusValue });
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

  // Filter client-side for search and exclude slftp management site
  const filteredSites = data?.filter(site =>
    site.name.toLowerCase() !== 'slftp' &&
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
          <Tooltip label="Run BNC Test (!bnctest)">
            <ActionIcon variant="light" color="blue" onClick={() => testSiteMutation.mutate(site.name)}>
              <IconBolt size="1rem" />
            </ActionIcon>
          </Tooltip>
          <Tooltip label="Kill ghost connections">
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
      title={selected ? `Settings: ${selected.name}` : 'Settings'}
      centered
      size="xl"
    >
      <Tabs defaultValue="basics" keepMounted={false} style={{ minHeight: '380px' }}>
        <Tabs.List grow>
          <Tabs.Tab value="basics" leftSection={<IconToolsKitchen3 size="1rem" />}>Basics</Tabs.Tab>
          <Tabs.Tab value="maintenance" leftSection={<IconShieldOff size="1rem" />}>Maintenance</Tabs.Tab>
        </Tabs.List>

        <Tabs.Panel value="basics" pt="md">
          <Stack gap="sm">
            <Divider label="FTP Credentials" />
            <Group grow>
              <TextInput
                label="Username"
                value={usernameValue}
                onChange={(e) => setUsernameValue(e.currentTarget.value)}
              />
              <TextInput
                label="Password"
                type="password"
                value={passwordValue}
                onChange={(e) => setPasswordValue(e.currentTarget.value)}
                placeholder="Leave empty to keep current"
              />
            </Group>

            <Divider label="BNC List" />
            {bncs.map((bnc, index) => (
              <Group key={index} grow>
                <TextInput
                  label={index === 0 ? 'Host' : undefined}
                  value={bnc.host}
                  onChange={(e) => {
                    const newBncs = [...bncs];
                    newBncs[index].host = e.currentTarget.value;
                    setBncs(newBncs);
                  }}
                  placeholder="ftp.example.com"
                />
                <NumberInput
                  label={index === 0 ? 'Port' : undefined}
                  value={bnc.port}
                  min={1}
                  max={65535}
                  onChange={(val) => {
                    const newBncs = [...bncs];
                    newBncs[index].port = val === '' ? 21 : Number(val);
                    setBncs(newBncs);
                  }}
                />
                <Box style={{ alignSelf: index === 0 ? 'flex-end' : 'center' }}>
                  <ActionIcon color="red" onClick={() => setBncs(bncs.filter((_, i) => i !== index))}>
                    <IconX size="1rem" />
                  </ActionIcon>
                </Box>
              </Group>
            ))}
            <Button
              leftSection={<IconPlus size="1rem" />}
              variant="light"
              onClick={() => setBncs([...bncs, { host: '', port: 21 }])}
            >
              Add BNC
            </Button>

            <Divider label="Slots Configuration" />
            <NumberInput
              label="Slots (total)"
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
              label="max_pre_dn"
              value={maxPreDnValue}
              min={0}
              onChange={(val) => setMaxPreDnValue(val === '' ? '' : Number(val))}
            />
            <Group grow>
              <Switch label="PermDown" checked={permDown} onChange={(e) => setPermDown(e.currentTarget.checked)} />
              <Switch label="Autologin" checked={autoLogin} onChange={(e) => setAutoLogin(e.currentTarget.checked)} />
            </Group>
            <Divider label="Connection Settings" />
            <Group grow>
              <NumberInput
                label="max_idle"
                value={maxIdle}
                min={0}
                onChange={(val) => setMaxIdle(val === '' ? '' : Number(val))}
              />
              <NumberInput
                label="idleinterval"
                value={idleInterval}
                min={0}
                onChange={(val) => setIdleInterval(val === '' ? '' : Number(val))}
              />
            </Group>
            <Switch label="legacycwd (glftpd only!)" checked={legacyCwd} onChange={(e) => setLegacyCwd(e.currentTarget.checked)} />
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

        <Tabs.Panel value="maintenance" pt="md">
          <Stack gap="sm">
            <Group justify="space-between">
              <Button variant="light" color="orange" loading={clearQueueMutation.isPending} onClick={() => selected && clearQueueMutation.mutate(selected.name)}>
                Clear queue
              </Button>
              <Button variant="light" color="red" loading={ghostMutation.isPending} onClick={() => selected && ghostMutation.mutate(selected.name)}>
                Kill ghosts
              </Button>
              <Button variant="outline" color="gray" loading={statusMutation.isPending} onClick={() => selected && statusValue && statusMutation.mutate({ site: selected, status: statusValue })}>
                Save status
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
      </Tabs>

      <Divider my="md" />
      <Group justify="flex-end" mt="sm">
        <Button variant="default" onClick={() => setSelected(null)}>Cancel</Button>
        <Button loading={saveSettingsMutation.isPending} onClick={handleSave}>
          Save
        </Button>
      </Group>
    </Modal>
  </Card>
  );
}
