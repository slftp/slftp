import {
  ActionIcon,
  Alert,
  Button,
  Center,
  Container,
  Group,
  Loader,
  NumberInput,
  Select,
  Stack,
  Switch,
  Textarea,
  TextInput,
  Title,
  Paper,
  SimpleGrid,
  Divider,
  Badge,
  Text,
  Code
} from '@mantine/core';
import { notifications } from '@mantine/notifications';
import { IconArrowLeft, IconPlus, IconX, IconDeviceFloppy, IconWorld, IconRefresh } from '@tabler/icons-react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useState } from 'react';
import { useNavigate, useParams } from 'react-router-dom';
import { apiClient } from '../api/client';
import type { Bnc } from '../api/client';

type SiteUserResponse = {
  SiteName?: string;
  UserName?: string;
  Ok?: boolean;
  Message?: string;
  Output?: string;
};

export function SiteSettings() {
  const { siteName } = useParams();
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const [bncs, setBncs] = useState<Bnc[]>([]);

  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');
  const [affils, setAffils] = useState('');
  const [ircnick, setIrcNick] = useState('');
  const [slots, setSlots] = useState<number | ''>('');
  const [maxDn, setMaxDn] = useState<number | ''>('');
  const [maxUp, setMaxUp] = useState<number | ''>('');
  const [maxPreDn, setMaxPreDn] = useState<number | ''>('');
  const [status, setStatus] = useState<'UP' | 'DOWN' | ''>('');
  const [permDown, setPermDown] = useState(false);
  const [autoLogin, setAutoLogin] = useState(false);
  const [autoRulesInterval, setAutoRulesInterval] = useState<number | ''>('');
  const [maxIdle, setMaxIdle] = useState<number | ''>(0);
  const [idleInterval, setIdleInterval] = useState<number | ''>(30);
  const [legacyCwd, setLegacyCwd] = useState(false);

  // New fields
  const [autoBncTest, setAutoBncTest] = useState<number | ''>('');
  const [autoDirlist, setAutoDirlist] = useState<number | ''>('');
  const [autoIndex, setAutoIndex] = useState<number | ''>('');
  const [autoNuke, setAutoNuke] = useState<number | ''>('');
  const [country, setCountry] = useState('');
  const [dirlistPriority, setDirlistPriority] = useState<string>('2');
  const [newdirDirlistReadd, setNewdirDirlistReadd] = useState<number | ''>(0);
  const [globalDirlistInterval, setGlobalDirlistInterval] = useState<number>(0);
  const [performanceAdjustedDirlist, setPerformanceAdjustedDirlist] = useState(true);
  const [skipUploaded, setSkipUploaded] = useState<string>('0');
  const [killOnStalled, setKillOnStalled] = useState<number | ''>('');
  const [sslMethod, setSslMethod] = useState('0');
  const [sslFxp, setSslFxp] = useState('0');
  const [maxUpPerRip, setMaxUpPerRip] = useState<number | ''>(0);
  const [siteFullName, setSiteFullName] = useState('');
  const [siteLinkSpeed, setSiteLinkSpeed] = useState('');
  const [siteSize, setSiteSize] = useState('');
  const [siteNotes, setSiteNotes] = useState('');
  const [identResponse, setIdentResponse] = useState('');
  const [siteInfos, setSiteInfos] = useState('');
  const [siteUserInfo, setSiteUserInfo] = useState<SiteUserResponse | null>(null);
  const [siteUserFetchedAt, setSiteUserFetchedAt] = useState<number | null>(null);

  const resolveDnsMutation = useMutation({
    mutationFn: async (host: string) => {
        const res = await apiClient.post('/ApiSitesService/ResolveHostname', { Hostname: host });
        // Depending on mORMot return wrapper
        return typeof res.data === 'string' ? res.data : (res.data.result?.[0] || res.data);
    }
  });

  // Fetch Site Details
  const { data: siteInfo, isLoading, error } = useQuery({
    queryKey: ['site', siteName],
    queryFn: async () => {
      if (!siteName) throw new Error('No site name');
      
      const res = await apiClient.post('/ApiSitesService/GetSiteInfo', { SiteName: siteName });
      const info = res.data.result?.[0] || res.data;
      
      // Parse BNCs
      let parsedBncs: Bnc[] = [];
      if (info.Bncs) {
        try {
          parsedBncs = typeof info.Bncs === 'string' ? JSON.parse(info.Bncs) : info.Bncs;
        } catch {}
      }
      return { ...info, bncs: parsedBncs };
    },
    enabled: !!siteName,
  });

  // Effect to populate state
  useEffect(() => {
    if (siteInfo) {
      setUsername(siteInfo.Username || '');
      setAffils(siteInfo.Affils || '');
      setSlots(siteInfo.Slots ?? 0);
      setBncs(siteInfo.bncs || []);
      setMaxIdle(siteInfo.MaxIdle ?? 0);
      setIdleInterval(siteInfo.IdleInterval ?? 30);
      setLegacyCwd(Boolean(siteInfo.LegacyCwd));
      
      setAutoBncTest(siteInfo.AutoBncTestInterval ?? 0);
      setAutoDirlist(siteInfo.AutoDirlistInterval ?? 0);
      setAutoIndex(siteInfo.AutoIndexInterval ?? 0);
      setAutoNuke(siteInfo.AutoNukeInterval ?? 0);
      setCountry(siteInfo.Country || '');
      
      const perfAdj = Boolean(siteInfo.PerformanceAdjustedDirlist);
      setPerformanceAdjustedDirlist(perfAdj);
      setDirlistPriority(perfAdj ? String(siteInfo.DirlistPriority ?? 2) : 'OFF');
      
      setNewdirDirlistReadd(siteInfo.NewdirDirlistReadd === 0 ? '' : (siteInfo.NewdirDirlistReadd ?? ''));
      setGlobalDirlistInterval(siteInfo.GlobalDirlistInterval ?? 0);
      setSkipUploaded(String(siteInfo.SkipBeingUploadedFiles ?? 0));
      setKillOnStalled(siteInfo.KillConnectionOnStalledTransferSeconds ?? 0);
      setSslMethod(String(siteInfo.SslMethod ?? 0));
      setSslFxp(String(siteInfo.SslFxp ?? 0));
      setMaxUpPerRip(siteInfo.MaxUpPerRip ?? 0);
      setSiteFullName(siteInfo.SiteFullName || '');
      setSiteLinkSpeed(siteInfo.SiteLinkSpeed || '');
      setSiteSize(siteInfo.SiteSize || '');
      setSiteNotes(siteInfo.SiteNotes || '');
      setIdentResponse(siteInfo.Ident || '');
      setSiteInfos(siteInfo.SiteInfos || '');
    }
  }, [siteInfo]);

  // We need the runtime stats/config that GetSites returns (max_dn, max_up etc).
  const { data: siteRuntime } = useQuery({
    queryKey: ['siteRuntime', siteName],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: siteName });
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
      return parsedSites.find((s: any) => s.name === siteName);
    },
    enabled: !!siteName
  });

  useEffect(() => {
    setSiteUserInfo(null);
    setSiteUserFetchedAt(null);
  }, [siteName]);

  useEffect(() => {
    if (siteRuntime) {
      setSlots(siteRuntime.slots ?? 0);
      setMaxDn(siteRuntime.max_dn ?? siteRuntime.slots ?? 0);
      setMaxUp(siteRuntime.max_up ?? 0);
      setMaxPreDn(siteRuntime.max_pre_dn ?? siteRuntime.max_dn ?? siteRuntime.slots ?? 0);
      setStatus(siteRuntime.status === 'DOWN' || siteRuntime.status === 'DOWN_BY_USER' ? 'DOWN' : 'UP');
      setPermDown(Boolean(siteRuntime.permdown));
      setAutoLogin(Boolean(siteRuntime.autologin));
      
      const perfAdj = Boolean(siteRuntime.performance_adjusted_dirlist);
      setPerformanceAdjustedDirlist(perfAdj);
      setDirlistPriority(perfAdj ? String(siteRuntime.dirlist_priority ?? 2) : 'OFF');
      
      setNewdirDirlistReadd(siteRuntime.newdir_dirlist_readd === 0 ? '' : (siteRuntime.newdir_dirlist_readd ?? ''));
      setAutoRulesInterval(siteRuntime.autorules_interval ?? 0);
      setIrcNick(siteRuntime.ircnick ?? '');
    }
  }, [siteRuntime]);

  const saveSettingsMutation = useMutation({
    mutationFn: async () => {
      if (!siteName) return;
      await apiClient.post('/ApiSitesService/SetSiteSlots', { SiteName: siteName, Slots: Number(slots) });
      await apiClient.post('/ApiSitesService/SetSiteMaxUpDn', { SiteName: siteName, MaxUp: Number(maxUp), MaxDn: Number(maxDn) });
      await apiClient.post('/ApiSitesService/SetSiteMaxPreDn', { SiteName: siteName, MaxPreDn: Number(maxPreDn) });
      await apiClient.post('/ApiSitesService/SetSitePermDown', { SiteName: siteName, PermDown: permDown });
      await apiClient.post('/ApiSitesService/SetSiteAutoLogin', { SiteName: siteName, Enabled: autoLogin });
      await apiClient.post('/ApiSitesService/SetSiteAutoRules', { SiteName: siteName, IntervalSeconds: Number(autoRulesInterval) });
      await apiClient.post('/ApiSitesService/SetSiteAffils', { SiteName: siteName, Affils: affils });
      await apiClient.post('/ApiSitesService/SetSiteIrcNick', { SiteName: siteName, IrcNick: ircnick });
      await apiClient.post('/ApiSitesService/SetSiteCredentials', { SiteName: siteName, Username: username, Password: password, BncsJson: JSON.stringify(bncs), MaxIdle: Number(maxIdle), IdleInterval: Number(idleInterval), LegacyCwd: legacyCwd, SslFxp: Number(sslFxp) });
      await apiClient.post('/ApiSitesService/SetSiteStatus', { SiteName: siteName, Status: status });
      await apiClient.post('/ApiSitesService/SetSiteSslMethod', { SiteName: siteName, SslMethod: Number(sslMethod) });
      
      // New Config endpoint
      await apiClient.post('/ApiSitesService/SetSiteConfig', { 
          SiteName: siteName, 
          Config: {
              autobnctest: Number(autoBncTest),
              autodirlist: Number(autoDirlist),
              autoindex: Number(autoIndex),
              autonuke: Number(autoNuke),
              country: country,
              dirlist_priority: dirlistPriority === 'OFF' ? 2 : Number(dirlistPriority),
              newdir_dirlist_readd: Number(newdirDirlistReadd),
              performance_adjusted_dirlist: performanceAdjustedDirlist,
              skip_being_uploaded_files: Number(skipUploaded),
              kill_connection_on_stalled_transfer: Number(killOnStalled),
              maxupperrip: Number(maxUpPerRip),
              site_full_name: siteFullName,
              site_link_speed: siteLinkSpeed,
              site_size: siteSize,
              site_notes: siteNotes,
              ident_response: identResponse,
              site_infos: siteInfos
          }
      });
    },
    onSuccess: () => {
      notifications.show({ title: 'Saved', message: 'Site settings updated.', color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['sites'] });
      queryClient.invalidateQueries({ queryKey: ['site', siteName] });
      queryClient.invalidateQueries({ queryKey: ['siteRuntime', siteName] });
    },
    onError: (err) => notifications.show({ title: 'Error', message: err.message, color: 'red' })
  });

  const fetchSiteUserMutation = useMutation({
    mutationFn: async (requestedUserName: string) => {
      if (!siteName) throw new Error('No site name');
      const res = await apiClient.post('/ApiSitesService/GetSiteUser', {
        SiteName: siteName,
        UserName: requestedUserName
      });
      return res.data.result?.[0] || res.data;
    },
    onSuccess: (data: SiteUserResponse) => {
      setSiteUserInfo(data);
      setSiteUserFetchedAt(Date.now());
      if (data?.Ok === false && data?.Message) {
        notifications.show({ title: 'SITE USER', message: data.Message, color: 'red' });
      }
    },
    onError: (err: any) => {
      const message = err.message || 'Failed to fetch SITE USER';
      setSiteUserInfo({ Ok: false, Message: message, Output: '' });
      setSiteUserFetchedAt(Date.now());
      notifications.show({ title: 'Error', message, color: 'red' });
    }
  });

  useEffect(() => {
    if (!siteName || !siteInfo) return;
    if (siteUserFetchedAt || fetchSiteUserMutation.isPending) return;
    fetchSiteUserMutation.mutate(siteInfo.Username || '');
  }, [siteName, siteInfo, siteUserFetchedAt, fetchSiteUserMutation.isPending]);

  // Actions
  const ghostMutation = useMutation({
    mutationFn: async () => apiClient.post('/ApiSitesService/GhostSite', { SiteName: siteName }),
    onSuccess: () => notifications.show({ title: 'Success', message: 'Ghosts killed', color: 'green' })
  });
  
  const clearQueueMutation = useMutation({
    mutationFn: async () => apiClient.post('/ApiQueueService/EmptyQueue', { SiteName: siteName }),
    onSuccess: () => notifications.show({ title: 'Success', message: 'Queue cleared', color: 'green' })
  });

  const recalcMutation = useMutation({
    mutationFn: async () => apiClient.post('/ApiSitesService/RecalcFreeSlots', { SiteName: siteName }),
    onSuccess: () => notifications.show({ title: 'Success', message: 'Freeslots recalculated', color: 'green' })
  });

  const rebuildMutation = useMutation({
    mutationFn: async () => apiClient.post('/ApiSitesService/RebuildSlots', { SiteName: siteName }),
    onSuccess: () => notifications.show({ title: 'Success', message: 'Slots rebuilt', color: 'green' })
  });

  if (isLoading) return <Center h={400}><Loader size="xl" /></Center>;
  if (error || !siteName) return <Alert color="red">Error loading site</Alert>;

  return (
    <Container size="xl">
      <Group mb="md">
        <Button variant="subtle" leftSection={<IconArrowLeft size="1rem" />} onClick={() => navigate('/sites')}>
          Back to Sites
        </Button>
        <Title order={2}>{siteName}</Title>
      </Group>

      <Stack gap="xl">
        <Paper withBorder p="md" radius="md">
            <Group justify="space-between" mb="md">
                <Title order={4}>General Settings</Title>
                <Badge size="lg" color={status === 'UP' ? 'green' : (status === 'DOWN' ? 'red' : 'gray')}>
                    {status || 'UNKNOWN'}
                </Badge>
            </Group>
            
            <Stack gap="lg">
                <div>
                    <Text size="sm" fw={500} c="dimmed" mb={8}>Credentials & Status</Text>
                    <SimpleGrid cols={{ base: 1, sm: 2, md: 4 }}>
                        <TextInput
                            label="Username"
                            value={username}
                            onChange={(e) => setUsername(e.currentTarget.value)}
                        />
                        <TextInput
                            label="Password"
                            type="password"
                            value={password}
                            onChange={(e) => setPassword(e.currentTarget.value)}
                            placeholder="Leave empty to keep current"
                        />
                        <Select
                            label="Status"
                            data={[
                                { value: 'UP', label: 'UP' },
                                { value: 'DOWN', label: 'DOWN (disable)' },
                            ]}
                            value={status}
                            onChange={(val) => setStatus(val as any)}
                        />
                    </SimpleGrid>
                </div>

                <Divider />

                <div>
                    <Text size="sm" fw={500} c="dimmed" mb={8}>Limits</Text>
                    <SimpleGrid cols={{ base: 1, sm: 2, md: 4 }}>
                        <NumberInput
                            label="Slots (total)"
                            value={slots}
                            min={0}
                            onChange={(val) => setSlots(val === '' ? '' : Number(val))}
                        />
                        <NumberInput
                            label="Max Pre-DN"
                            value={maxPreDn}
                            min={0}
                            onChange={(val) => setMaxPreDn(val === '' ? '' : Number(val))}
                        />
                        <NumberInput
                            label="Max Downloads"
                            value={maxDn}
                            min={0}
                            onChange={(val) => setMaxDn(val === '' ? '' : Number(val))}
                        />
                        <NumberInput
                            label="Max Uploads"
                            value={maxUp}
                            min={0}
                            onChange={(val) => setMaxUp(val === '' ? '' : Number(val))}
                        />
                        <NumberInput
                            label="Max Up per RIP"
                            value={maxUpPerRip}
                            min={0}
                            onChange={(val) => setMaxUpPerRip(val === '' ? '' : Number(val))}
                            placeholder="0 = unlimited"
                        />
                    </SimpleGrid>
                </div>

                <Divider />

                <div>
                    <Text size="sm" fw={500} c="dimmed" mb={8}>Connection & Options</Text>
                    <SimpleGrid cols={{ base: 1, sm: 2, md: 4 }} spacing="lg">
                        <NumberInput
                            label="Max Idle"
                            value={maxIdle}
                            min={0}
                            onChange={(val) => setMaxIdle(val === '' ? '' : Number(val))}
                        />
                        <NumberInput
                            label="Idle Interval"
                            value={idleInterval}
                            min={0}
                            onChange={(val) => setIdleInterval(val === '' ? '' : Number(val))}
                        />
                        <Select 
                            label="Skip Uploaded Files"
                            data={[
                                { value: '0', label: 'Skip 0-byte only' },
                                { value: '1', label: 'Skip being uploaded + 0-byte' },
                                { value: '2', label: 'Don\'t skip' }
                            ]}
                            value={skipUploaded}
                            onChange={(v) => setSkipUploaded(v || '0')}
                        />
                        <NumberInput
                            label="Kill Stalled (sec, 0=off)"
                            value={killOnStalled}
                            min={0}
                            onChange={(val) => setKillOnStalled(val === '' ? '' : Number(val))}
                        />
                        <Select
                            label="Dirlist Priority"
                            data={[
                                { value: 'OFF', label: 'Off (Static)' },
                                { value: '0', label: 'Very Low' },
                                { value: '1', label: 'Low' },
                                { value: '2', label: 'Normal' },
                                { value: '3', label: 'High' },
                                { value: '4', label: 'Very High' }
                            ]}
                            value={dirlistPriority}
                            onChange={(v) => {
                                if (v === 'OFF') {
                                    setPerformanceAdjustedDirlist(false);
                                    setDirlistPriority('OFF');
                                } else {
                                    setPerformanceAdjustedDirlist(true);
                                    setDirlistPriority(v || '2');
                                }
                            }}
                        />
                        <NumberInput
                            label="Dirlist Interval (ms)"
                            placeholder={`Empty = Global (${globalDirlistInterval}ms)`}
                            value={newdirDirlistReadd}
                            min={0}
                            onChange={(val) => setNewdirDirlistReadd(val === '' ? '' : Number(val))}
                        />
                        <Select
                            label="SSLFXP"
                            value={sslFxp}
                            onChange={(val) => setSslFxp(val || '0')}
                            data={[
                                { value: '0', label: 'Off' },
                                { value: '1', label: 'On' },
                                { value: '2', label: 'Unsupported' }
                            ]}
                        />
                        <Select
                            label="SSL Method"
                            value={sslMethod}
                            onChange={(val) => setSslMethod(val || '0')}
                            data={[
                                { value: '0', label: 'Off' },
                                { value: '1', label: 'Implicit SSL' },
                                { value: '2', label: 'AUTH SSL' },
                                { value: '3', label: 'AUTH TLS' }
                            ]}
                        />
                    </SimpleGrid>
                    <Group mt="md">
                        <Switch label="Auto-Login" checked={autoLogin} onChange={(e) => setAutoLogin(e.currentTarget.checked)} />
                        <Switch label="Legacy CWD" checked={legacyCwd} onChange={(e) => setLegacyCwd(e.currentTarget.checked)} />
                        <Switch label="Permanent Down" checked={permDown} color="red" onChange={(e) => setPermDown(e.currentTarget.checked)} />
                    </Group>
                </div>

                <Divider />
                
                <Textarea
                    label="Affils"
                    description="Whitespace separated"
                    value={affils}
                    onChange={(e) => setAffils(e.currentTarget.value)}
                    placeholder="GRP1 GRP2"
                    minRows={3}
                />

                <Divider />

                <div>
                    <Text size="sm" fw={500} c="dimmed" mb={8}>Site Details</Text>
                    <SimpleGrid cols={{ base: 1, sm: 2, md: 3 }}>
                        <TextInput
                            label="IRC Nick"
                            value={ircnick}
                            onChange={(e) => setIrcNick(e.currentTarget.value)}
                            placeholder="YourNick"
                        />
                        <TextInput
                            label="Country"
                            value={country}
                            onChange={(e) => setCountry(e.currentTarget.value)}
                            placeholder=".DE"
                        />
                        <TextInput
                            label="Ident Response"
                            value={identResponse}
                            onChange={(e) => setIdentResponse(e.currentTarget.value)}
                        />
                        <TextInput
                            label="Full Name"
                            value={siteFullName}
                            onChange={(e) => setSiteFullName(e.currentTarget.value)}
                        />
                        <TextInput
                            label="Link Speed"
                            value={siteLinkSpeed}
                            onChange={(e) => setSiteLinkSpeed(e.currentTarget.value)}
                        />
                        <TextInput
                            label="Site Size"
                            value={siteSize}
                            onChange={(e) => setSiteSize(e.currentTarget.value)}
                        />
                    </SimpleGrid>
                    <Textarea
                        label="Notes"
                        value={siteNotes}
                        onChange={(e) => setSiteNotes(e.currentTarget.value)}
                        minRows={3}
                        mt="md"
                    />
                    <Textarea
                        label="Site Infos"
                        value={siteInfos}
                        onChange={(e) => setSiteInfos(e.currentTarget.value)}
                        minRows={3}
                        mt="md"
                    />
                </div>
            </Stack>
        </Paper>

        <Paper withBorder p="md" radius="md">
            <Group justify="space-between" mb="md" align="center">
                <Group>
                    <Title order={4}>SITE USER</Title>
                    {siteUserInfo && (
                        <Badge color={siteUserInfo.Ok ? 'green' : 'red'}>
                            {siteUserInfo.Ok ? 'OK' : 'ERROR'}
                        </Badge>
                    )}
                </Group>
                <Group gap="sm" align="center">
                    <Text size="sm" c="dimmed">
                        User: {siteInfo?.Username || '-'}
                    </Text>
                    <Button
                        leftSection={<IconRefresh size="1rem" />}
                        variant="light"
                        loading={fetchSiteUserMutation.isPending}
                        onClick={() => fetchSiteUserMutation.mutate(siteInfo?.Username || '')}
                    >
                        Refresh
                    </Button>
                </Group>
            </Group>
            {siteUserInfo ? (
                <Stack gap="xs">
                    {siteUserInfo.Message && siteUserInfo.Ok === false && (
                        <Alert color="red" title="SITE USER">
                            {siteUserInfo.Message}
                        </Alert>
                    )}
                    <Code block style={{ whiteSpace: 'pre' }}>
                        {siteUserInfo.Output || 'No output'}
                    </Code>
                    <Group gap="xs">
                        <Text size="xs" c="dimmed">
                            User: {siteUserInfo.UserName || siteInfo?.Username || '-'}
                        </Text>
                        {siteUserFetchedAt && (
                            <Text size="xs" c="dimmed">
                                Last fetched: {new Date(siteUserFetchedAt).toLocaleString()}
                            </Text>
                        )}
                        <Text size="xs" c="dimmed">
                            Auto fetched on open.
                        </Text>
                    </Group>
                </Stack>
            ) : (
                <Text size="sm" c="dimmed">
                    Loading SITE USER output...
                </Text>
            )}
        </Paper>

        <Paper withBorder p="md" radius="md">
            <Title order={4} mb="md">Automation (Intervals in seconds)</Title>            <SimpleGrid cols={{ base: 1, sm: 2, md: 4 }}>
                <NumberInput 
                    label="Auto BNC Test" 
                    value={autoBncTest} 
                    min={0} 
                    onChange={(v) => setAutoBncTest(v === '' ? '' : Number(v))}
                />
                <NumberInput 
                    label="Auto Dirlist" 
                    value={autoDirlist} 
                    min={0} 
                    onChange={(v) => setAutoDirlist(v === '' ? '' : Number(v))}
                />
                <NumberInput 
                    label="Auto Index" 
                    value={autoIndex} 
                    min={0} 
                    onChange={(v) => setAutoIndex(v === '' ? '' : Number(v))}
                />
                <NumberInput 
                    label="Auto Nuke" 
                    value={autoNuke} 
                    min={0} 
                    onChange={(v) => setAutoNuke(v === '' ? '' : Number(v))}
                />
                <NumberInput 
                    label="Auto Rules" 
                    value={autoRulesInterval} 
                    min={0} 
                    onChange={(v) => setAutoRulesInterval(v === '' ? '' : Number(v))}
                />
            </SimpleGrid>
        </Paper>

        <Paper withBorder p="md" radius="md">
            <Title order={4} mb="md">BNC Configuration</Title>
            <Stack>
                {bncs.map((bnc, index) => (
                  <Group key={index} align="flex-end">
                    <TextInput
                      label={index === 0 ? 'Host' : undefined}
                      value={bnc.host}
                      onChange={(e) => {
                        const newBncs = [...bncs];
                        newBncs[index].host = e.currentTarget.value;
                        setBncs(newBncs);
                      }}
                      placeholder="ftp.example.com"
                      style={{ flex: 1 }}
                    />
                    <ActionIcon 
                        variant="light" 
                        color="blue" 
                        title="Resolve DNS to IP"
                        onClick={() => {
                            if (!bnc.host) return;
                            resolveDnsMutation.mutateAsync(bnc.host).then((ip) => {
                                if (ip && ip !== bnc.host) {
                                    const newBncs = [...bncs];
                                    newBncs[index].host = ip;
                                    setBncs(newBncs);
                                    notifications.show({ title: 'Resolved', message: `${bnc.host} -> ${ip}`, color: 'green' });
                                } else if (ip === bnc.host) {
                                    notifications.show({ title: 'Info', message: 'Already resolved or same', color: 'blue' });
                                } else {
                                    notifications.show({ title: 'Error', message: 'Could not resolve', color: 'red' });
                                }
                            });
                        }}
                    >
                        <IconWorld size="1rem" />
                    </ActionIcon>
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
                      w={100}
                    />
                    <ActionIcon color="red" onClick={() => setBncs(bncs.filter((_, i) => i !== index))} mb={4}>
                        <IconX size="1rem" />
                    </ActionIcon>
                  </Group>
                ))}
                <Button
                  leftSection={<IconPlus size="1rem" />}
                  variant="light"
                  onClick={() => setBncs([...bncs, { host: '', port: 21 }])}
                  w="fit-content"
                >
                  Add BNC
                </Button>
            </Stack>
        </Paper>

        <Paper withBorder p="md" radius="md">
            <Title order={4} mb="md">Maintenance</Title>
            <Group>
              <Button variant="outline" color="orange" onClick={() => clearQueueMutation.mutate()}>Clear Queue</Button>
              <Button variant="outline" color="red" onClick={() => ghostMutation.mutate()}>Kill Ghosts</Button>
              <Button variant="outline" onClick={() => recalcMutation.mutate()}>Recalc Freeslots</Button>
              <Button variant="outline" onClick={() => rebuildMutation.mutate()}>Rebuild Slots</Button>
            </Group>
        </Paper>

        <Group justify="flex-end">
            <Button size="lg" leftSection={<IconDeviceFloppy size="1.2rem"/>} loading={saveSettingsMutation.isPending} onClick={() => saveSettingsMutation.mutate()}>
                Save Settings
            </Button>
        </Group>
      </Stack>
    </Container>
  );
}
