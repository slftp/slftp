import { Table, Badge, Title, Card, Alert, Loader, Center, Group, ActionIcon, Tooltip, Text, TextInput, Modal, NumberInput, Button, Stack, Switch } from '@mantine/core';
import { IconSearch, IconRefresh, IconBolt, IconTrash, IconPlus, IconX, IconCoins } from '@tabler/icons-react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useEffect, useMemo, useRef, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { apiClient } from '../api/client';
import type { Site } from '../api/client';
import { notifications } from '@mantine/notifications';

type SiteCreditsResponse = {
  SiteName?: string;
  Ok?: boolean;
  Message?: string;
  Credits?: string;
  Ratio?: string;
  StatLine?: string;
};

export function SitesList() {
  const queryClient = useQueryClient();
  const navigate = useNavigate();
  const [search, setSearch] = useState('');
  const [siteCredits, setSiteCredits] = useState<Record<string, SiteCreditsResponse & { fetchedAtMs: number }>>({});
  const [creditsLoadingSite, setCreditsLoadingSite] = useState<string | null>(null);
  const creditsInFlightRef = useRef<Set<string>>(new Set());
  const autoCreditsRunRef = useRef(false);
  const siteCreditsRef = useRef(siteCredits);

  useEffect(() => {
    siteCreditsRef.current = siteCredits;
  }, [siteCredits]);

  // Add Site Modal States
  const [addSiteModalOpened, setAddSiteModalOpened] = useState(false);
  const [newSiteName, setNewSiteName] = useState('');
  const [newSiteHost, setNewSiteHost] = useState('');
  const [newSitePort, setNewSitePort] = useState<number | ''>(21);
  const [newSiteUsername, setNewSiteUsername] = useState('');
  const [newSitePassword, setNewSitePassword] = useState('');
  const [newSiteSsl, setNewSiteSsl] = useState(false);

  // Delete Site Confirmation Modal
  const [deleteSiteModalOpened, setDeleteSiteModalOpened] = useState(false);
  const [siteToDelete, setSiteToDelete] = useState<string | null>(null);

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

  const fetchCredits = async (siteName: string, forceRefresh: boolean, silent: boolean): Promise<SiteCreditsResponse> => {
    const res = await apiClient.post('/ApiSitesService/GetSiteCredits', { SiteName: siteName, ForceRefresh: forceRefresh });
    const data: SiteCreditsResponse = res.data.result?.[0] || res.data;
    setSiteCredits((prev) => ({ ...prev, [siteName]: { ...data, fetchedAtMs: Date.now() } }));
    if (!silent && data?.Ok === false) {
      notifications.show({
        title: 'SITE STAT',
        message: data.Message || `Failed to fetch credits/ratio for ${siteName}`,
        color: 'red',
      });
    }
    return data;
  };

  const fetchCreditsMutation = useMutation({
    mutationFn: async (siteName: string) => fetchCredits(siteName, true, false),
    onMutate: (siteName) => setCreditsLoadingSite(siteName),
    onError: (err: any, siteName) => {
      notifications.show({ title: 'Error', message: err.message || `Failed to fetch credits/ratio for ${siteName}`, color: 'red' });
    },
    onSettled: () => setCreditsLoadingSite(null),
  });

  // Auto-refresh credits/ratio at most once per hour per site (independent of the 30s sites refresh)
  const CREDITS_REFRESH_MS = 60 * 60 * 1000;
  const [autoCreditsTick, setAutoCreditsTick] = useState(0);

  // Tick every minute to pick up stale sites even without navigation/reload
  useEffect(() => {
    const id = window.setInterval(() => setAutoCreditsTick((t) => t + 1), 60 * 1000);
    return () => window.clearInterval(id);
  }, []);

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

  const sitesWithoutSlftp = useMemo(() => (
    data?.filter(site => site.name.toLowerCase() !== 'slftp') || []
  ), [data]);

  const filteredSites = useMemo(() => (
    sitesWithoutSlftp.filter(site => site.name.toLowerCase().includes(search.toLowerCase()))
  ), [sitesWithoutSlftp, search]);

  const creditsSitesKey = useMemo(() => sitesWithoutSlftp.map(s => s.name).join('|'), [sitesWithoutSlftp]);

  // Background fetch: only for sites missing/stale credits, sequentially to avoid bursts
  useEffect(() => {
    if (autoCreditsRunRef.current) return;
    autoCreditsRunRef.current = true;

    let cancelled = false;
    const run = async () => {
      const now = Date.now();
      for (const s of sitesWithoutSlftp) {
        if (cancelled) return;
        const existing = siteCreditsRef.current[s.name];
        if (!existing || (now - existing.fetchedAtMs) > CREDITS_REFRESH_MS) {
          if (creditsInFlightRef.current.has(s.name)) continue;
          creditsInFlightRef.current.add(s.name);
          try {
            await fetchCredits(s.name, false, true);
          } catch {
            // silent in background
          } finally {
            creditsInFlightRef.current.delete(s.name);
          }
        }
      }
    };
    run()
      .catch(() => {})
      .finally(() => {
        autoCreditsRunRef.current = false;
      });

    return () => {
      cancelled = true;
    };
  }, [autoCreditsTick, creditsSitesKey, sitesWithoutSlftp]);

  if (isLoading) return <Center h={400}><Loader size="xl" /></Center>;
  if (error) return <Alert color="red" title="Error">Could not load sites</Alert>;

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
        <Table.Td>{site.ircnick?.trim() || '-'}</Table.Td>
	      <Table.Td>{getStatusBadge(site.status)}</Table.Td>
	      <Table.Td>{site.slots ?? '-'}</Table.Td>
	      <Table.Td>{formatSlots(site)}</Table.Td>
	      <Table.Td>{site.freeslots}</Table.Td>
	      <Table.Td>{formatActive(site)}</Table.Td>
        <Table.Td>
          {siteCredits[site.name]?.Ok ? (
            <Text size="sm">
              {(siteCredits[site.name].Credits || '-')} ({(siteCredits[site.name].Ratio || '-')})
            </Text>
          ) : (
            <Text size="sm" c="dimmed">-</Text>
          )}
        </Table.Td>
        <Table.Td>
          {(site.ranklock ?? 0) === 0
            ? <Badge color="gray" variant="light">Dynamic</Badge>
            : <Badge color="yellow" variant="light">{site.ranklock}</Badge>
          }
        </Table.Td>
	      <Table.Td>
	        <Group gap="xs">
          <Tooltip label="Run BNC Test (!bnctest)">
            <ActionIcon variant="light" color="blue" onClick={() => testSiteMutation.mutate(site.name)}>
              <IconBolt size="1rem" />
            </ActionIcon>
          </Tooltip>
          <Tooltip label="Fetch credits/ratio (SITE STAT)">
            <ActionIcon
              variant="light"
              color="grape"
              loading={creditsLoadingSite === site.name}
              onClick={() => fetchCreditsMutation.mutate(site.name)}
            >
              <IconCoins size="1rem" />
            </ActionIcon>
          </Tooltip>
          <Tooltip label="Kill ghost connections">
            <ActionIcon variant="light" color="orange" onClick={() => ghostMutation.mutate(site.name)}>
              <IconTrash size="1rem" />
            </ActionIcon>
          </Tooltip>
          <Tooltip label="Delete site">
            <ActionIcon variant="light" color="red" onClick={() => {
              setSiteToDelete(site.name);
              setDeleteSiteModalOpened(true);
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
            <Table.Th>IRC Nick</Table.Th>
	          <Table.Th>Status</Table.Th>
	          <Table.Th>Slots</Table.Th>
	          <Table.Th>Max DN/UP</Table.Th>
	          <Table.Th>Free Slots</Table.Th>
	          <Table.Th>Active DN/UP</Table.Th>
            <Table.Th>Credits (Ratio)</Table.Th>
            <Table.Th>Rank</Table.Th>
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

    <Modal
      opened={deleteSiteModalOpened}
      onClose={() => {
        setDeleteSiteModalOpened(false);
        setSiteToDelete(null);
      }}
      title="Confirm Site Deletion"
      centered
    >
      <Stack gap="md">
        <Text>
          Are you sure you want to delete site <Text component="span" fw={700} c="red">{siteToDelete}</Text>?
        </Text>
        <Text size="sm" c="dimmed">
          This will remove the site configuration, speed routes, rules, ranks, and precatcher entries. This action cannot be undone.
        </Text>
        <Group justify="flex-end" mt="md">
          <Button
            variant="default"
            onClick={() => {
              setDeleteSiteModalOpened(false);
              setSiteToDelete(null);
            }}
          >
            Cancel
          </Button>
          <Button
            color="red"
            loading={deleteSiteMutation.isPending}
            onClick={() => {
              if (siteToDelete) {
                deleteSiteMutation.mutate(siteToDelete);
                setDeleteSiteModalOpened(false);
                setSiteToDelete(null);
              }
            }}
          >
            Delete Site
          </Button>
        </Group>
      </Stack>
    </Modal>
  </Card>
  );
}
