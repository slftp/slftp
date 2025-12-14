import { Table, Badge, Title, Card, Alert, Loader, Center, Group, ActionIcon, Tooltip, Text, TextInput, Modal, NumberInput, Button, Stack, Switch } from '@mantine/core';
import { IconSearch, IconRefresh, IconBolt, IconTrash, IconPlus, IconX } from '@tabler/icons-react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { apiClient } from '../api/client';
import type { Site } from '../api/client';
import { notifications } from '@mantine/notifications';

export function SitesList() {
  const queryClient = useQueryClient();
  const navigate = useNavigate();
  const [search, setSearch] = useState('');

  // Add Site Modal States
  const [addSiteModalOpened, setAddSiteModalOpened] = useState(false);
  const [newSiteName, setNewSiteName] = useState('');
  const [newSiteHost, setNewSiteHost] = useState('');
  const [newSitePort, setNewSitePort] = useState<number | ''>(21);
  const [newSiteUsername, setNewSiteUsername] = useState('');
  const [newSitePassword, setNewSitePassword] = useState('');
  const [newSiteSsl, setNewSiteSsl] = useState(false);

  // Fetch Sites
  const { data, isLoading, error } = useQuery({
    queryKey: ['sites'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: '' });
      
      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }

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
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
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

  const addSiteMutation = useMutation({
    mutationFn: async (payload: { name: string; host: string; port: number; username: string; password: string; sslEnabled: boolean }) => {
      await apiClient.post('/ApiSitesService/AddSite', {
        Name: payload.name,
        Host: payload.host,
        Port: payload.port,
        Username: payload.username,
        Password: payload.password,
        SslEnabled: payload.sslEnabled
      });
    },
    onSuccess: () => {
      notifications.show({ title: 'Site added', message: 'New site created successfully.', color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
      setAddSiteModalOpened(false);
      setNewSiteName('');
      setNewSiteHost('');
      setNewSitePort(21);
      setNewSiteUsername('');
      setNewSitePassword('');
      setNewSiteSsl(false);
    },
    onError: (err) => notifications.show({ title: 'Error', message: err.message, color: 'red' })
  });

  const deleteSiteMutation = useMutation({
    mutationFn: async (siteName: string) => {
      await apiClient.post('/ApiSitesService/DeleteSite', { SiteName: siteName });
    },
    onSuccess: (_, siteName) => {
      notifications.show({ title: 'Site deleted', message: `Site ${siteName} removed.`, color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
    },
    onError: (err) => notifications.show({ title: 'Error', message: err.message, color: 'red' })
  });

  const handleAddSite = () => {
    if (!newSiteName.trim() || !newSiteHost.trim() || newSitePort === '') return;
    addSiteMutation.mutate({
      name: newSiteName,
      host: newSiteHost,
      port: newSitePort,
      username: newSiteUsername,
      password: newSitePassword,
      sslEnabled: newSiteSsl
    });
  };

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

  const filteredSites = data?.filter(site =>
    site.name.toLowerCase() !== 'slftp' &&
    site.name.toLowerCase().includes(search.toLowerCase())
  ) || [];

	  const rows = filteredSites.map((site) => (
	    <Table.Tr key={site.name}>
	      <Table.Td fw={500}>
        <Text 
            fw={600} 
            onClick={() => navigate(`/sites/${site.name}`)} 
            style={{ cursor: 'pointer' }}
            c="blue"
        >
            {site.name}
        </Text>
	      </Table.Td>
	      <Table.Td>{getStatusBadge(site.status)}</Table.Td>
	      <Table.Td>{site.slots ?? '-'}</Table.Td>
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
          <Tooltip label="Delete site">
            <ActionIcon variant="light" color="red" onClick={() => {
              if (confirm(`Delete site ${site.name}?`)) {
                deleteSiteMutation.mutate(site.name);
              }
            }}>
              <IconX size="1rem" />
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
           <Button leftSection={<IconPlus size="1rem" />} onClick={() => setAddSiteModalOpened(true)}>
             Add Site
           </Button>
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
	          <Table.Th>Slots</Table.Th>
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
      opened={addSiteModalOpened}
      onClose={() => setAddSiteModalOpened(false)}
      title="Add New Site"
      centered
      size="md"
    >
      <Stack gap="sm">
        <TextInput
          label="Site Name"
          placeholder="MYSITE"
          value={newSiteName}
          onChange={(e) => setNewSiteName(e.currentTarget.value)}
          required
        />
        <TextInput
          label="Host"
          placeholder="ftp.example.com"
          value={newSiteHost}
          onChange={(e) => setNewSiteHost(e.currentTarget.value)}
          required
        />
        <NumberInput
          label="Port"
          value={newSitePort}
          min={1}
          max={65535}
          onChange={(val) => setNewSitePort(val === '' ? '' : Number(val))}
          required
        />
        <TextInput
          label="Username"
          placeholder="username"
          value={newSiteUsername}
          onChange={(e) => setNewSiteUsername(e.currentTarget.value)}
        />
        <TextInput
          label="Password"
          type="password"
          placeholder="password"
          value={newSitePassword}
          onChange={(e) => setNewSitePassword(e.currentTarget.value)}
        />
        <Switch
          label="Enable SSL/TLS"
          checked={newSiteSsl}
          onChange={(e) => setNewSiteSsl(e.currentTarget.checked)}
        />
      </Stack>

      <Group justify="flex-end" mt="md">
        <Button variant="default" onClick={() => setAddSiteModalOpened(false)}>Cancel</Button>
        <Button
          loading={addSiteMutation.isPending}
          onClick={handleAddSite}
          disabled={!newSiteName.trim() || !newSiteHost.trim() || newSitePort === ''}
        >
          Add Site
        </Button>
      </Group>
    </Modal>
  </Card>
  );
}
