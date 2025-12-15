import { Card, Title, Table, Loader, Center, Tabs, Badge, Button, Group, Text, ActionIcon, Tooltip, Stack, TextInput, Modal, Select, Textarea, Switch } from '@mantine/core';
import { IconNetwork, IconHash, IconRefresh, IconEdit, IconCheck, IconX, IconPlus, IconTrash, IconFilter, IconFlask, IconSearch } from '@tabler/icons-react';
import { useQuery, useQueryClient, useMutation } from '@tanstack/react-query';
import { useState } from 'react';
import { apiClient } from '../api/client';
import { notifications } from '@mantine/notifications';

interface IrcNetwork {
  name: string;
  host: string;
  port: number;
  status: string;
  nickname: string;
  connected: boolean;
  channels_count: number;
}

interface IrcChannel {
  channel: string;
  chankey: string;
  chanroles: string;
  blowkey: string;
}

interface PrecatcherRule {
  id: number;
  netname: string;
  channel: string;
  botnicks: string;
  sitename: string;
  event: string;
  words: string;
  section: string;
}

const EVENT_TYPES = [
  { value: 'PRE', label: 'PRE' },
  { value: 'ADDPRE', label: 'ADDPRE' },
  { value: 'COMPLETE', label: 'COMPLETE' },
  { value: 'NEWDIR', label: 'NEWDIR' },
  { value: 'NUKE', label: 'NUKE' },
  { value: 'REQUEST', label: 'REQUEST' },
];

export function IRC() {
  const queryClient = useQueryClient();
  const [selectedNetwork, setSelectedNetwork] = useState<string | null>(null);
  const [activeTab, setActiveTab] = useState<string>('networks');

  // Channel state
  const [editingChannel, setEditingChannel] = useState<IrcChannel | null>(null);
  const [editChankey, setEditChankey] = useState('');
  const [editBlowkey, setEditBlowkey] = useState('');
  const [editChanroles, setEditChanroles] = useState('');
  const [addingChannel, setAddingChannel] = useState(false);
  const [newChannelName, setNewChannelName] = useState('');
  const [newChankey, setNewChankey] = useState('');
  const [newBlowkey, setNewBlowkey] = useState('');
  const [newChanroles, setNewChanroles] = useState('');

  // Rules state
  const [addingRule, setAddingRule] = useState(false);
  const [newNetname, setNewNetname] = useState('');
  const [newChannel, setNewChannel] = useState('');
  const [newBotnicks, setNewBotnicks] = useState('');
  const [newSitename, setNewSitename] = useState('');
  const [newEvent, setNewEvent] = useState('PRE');
  const [newWords, setNewWords] = useState('');
  const [newSection, setNewSection] = useState('');

  // Test state
  const [testNetname, setTestNetname] = useState('');
  const [testChannel, setTestChannel] = useState('');
  const [testNick, setTestNick] = useState('');
  const [testText, setTestText] = useState('');
  const [testOutput, setTestOutput] = useState('');

  // Catchlist filter state
  const [catchlistFilter, setCatchlistFilter] = useState('');

  const { data: networks, isLoading: networksLoading } = useQuery({
    queryKey: ['irc-networks'],
    queryFn: async (): Promise<IrcNetwork[]> => {
      const res = await apiClient.post('/ApiIrcService/GetNetworks', {});

      let networks: IrcNetwork[] = [];
      try {
        if (res.data.result && Array.isArray(res.data.result)) {
          const resultData = res.data.result[0];
          if (Array.isArray(resultData)) {
            networks = resultData;
          }
        } else if (typeof res.data === 'string') {
          networks = JSON.parse(res.data);
        } else if (Array.isArray(res.data)) {
          networks = res.data;
        }
      } catch (e) {
        console.error('Failed to parse IRC networks:', e);
        return [];
      }

      return networks;
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const { data: channels, isLoading: channelsLoading } = useQuery({
    queryKey: ['irc-channels', selectedNetwork],
    queryFn: async (): Promise<IrcChannel[]> => {
      if (!selectedNetwork) return [];
      const res = await apiClient.post('/ApiIrcService/GetChannels', { NetName: selectedNetwork });

      let channels: IrcChannel[] = [];
      try {
        if (res.data.result && Array.isArray(res.data.result)) {
          const resultData = res.data.result[0];
          if (typeof resultData === 'string') {
            channels = JSON.parse(resultData);
          } else if (Array.isArray(resultData)) {
            channels = resultData;
          }
        } else if (typeof res.data === 'string') {
          channels = JSON.parse(res.data);
        } else if (Array.isArray(res.data)) {
          channels = res.data;
        }
      } catch (e) {
        console.error('Failed to parse IRC channels:', e);
        return [];
      }

      return channels;
    },
    enabled: !!selectedNetwork,
    refetchOnWindowFocus: false,
  });

  const { data: rules, isLoading: rulesLoading } = useQuery({
    queryKey: ['precatcher-rules'],
    queryFn: async (): Promise<PrecatcherRule[]> => {
      const res = await apiClient.post('/ApiPrecatcherService/GetPrecatcherRules', {});

      let rules: PrecatcherRule[] = [];
      try {
        if (res.data.result && Array.isArray(res.data.result)) {
          const resultData = res.data.result[0];
          if (Array.isArray(resultData)) {
            rules = resultData;
          }
        } else if (typeof res.data === 'string') {
          rules = JSON.parse(res.data);
        } else if (Array.isArray(res.data)) {
          rules = res.data;
        }
      } catch (e) {
        console.error('Failed to parse precatcher rules:', e);
        return [];
      }

      return rules;
    },
    refetchOnWindowFocus: false,
    enabled: activeTab === 'rules',
  });

  const getStatusBadge = (network: IrcNetwork) => {
    if (network.connected) {
      return <Badge color="green">Connected</Badge>;
    } else if (network.status.includes('onnect')) {
      return <Badge color="yellow">Connecting</Badge>;
    } else {
      return <Badge color="red">Disconnected</Badge>;
    }
  };

  const handleViewChannels = (networkName: string) => {
    setSelectedNetwork(networkName);
    setActiveTab('channels');
  };

  const saveChannelMutation = useMutation({
    mutationFn: async () => {
      if (!editingChannel || !selectedNetwork) return;

      // Save chankey
      await apiClient.post('/ApiIrcService/SetChannelKey', {
        NetName: selectedNetwork,
        Channel: editingChannel.channel,
        ChanKey: editChankey,
      });

      // Save blowkey
      await apiClient.post('/ApiIrcService/SetChannelBlowkey', {
        NetName: selectedNetwork,
        Channel: editingChannel.channel,
        Blowkey: editBlowkey,
      });

      // Save roles
      await apiClient.post('/ApiIrcService/SetChannelRoles', {
        NetName: selectedNetwork,
        Channel: editingChannel.channel,
        Roles: editChanroles,
      });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Saved',
        message: 'Channel settings updated',
        color: 'green',
      });
      setEditingChannel(null);
      queryClient.invalidateQueries({ queryKey: ['irc-channels', selectedNetwork] });
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.message,
        color: 'red',
      });
    },
  });

  const addChannelMutation = useMutation({
    mutationFn: async () => {
      if (!selectedNetwork) return;

      await apiClient.post('/ApiIrcService/AddChannel', {
        NetName: selectedNetwork,
        Channel: newChannelName,
        ChanKey: newChankey,
        Blowkey: newBlowkey,
        Roles: newChanroles,
      });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Added',
        message: 'Channel added successfully',
        color: 'green',
      });
      setAddingChannel(false);
      setNewChannelName('');
      setNewChankey('');
      setNewBlowkey('');
      setNewChanroles('');
      queryClient.invalidateQueries({ queryKey: ['irc-channels', selectedNetwork] });
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.message,
        color: 'red',
      });
    },
  });

  const deleteChannelMutation = useMutation({
    mutationFn: async (channelName: string) => {
      if (!selectedNetwork) return;

      await apiClient.post('/ApiIrcService/DeleteChannel', {
        NetName: selectedNetwork,
        Channel: channelName,
      });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Deleted',
        message: 'Channel deleted successfully',
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['irc-channels', selectedNetwork] });
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.message,
        color: 'red',
      });
    },
  });

  const openEditModal = (channel: IrcChannel) => {
    setEditingChannel(channel);
    setEditChankey(channel.chankey || '');
    setEditBlowkey('');
    setEditChanroles(channel.chanroles || '');
  };

  const addRuleMutation = useMutation({
    mutationFn: async () => {
      await apiClient.post('/ApiPrecatcherService/AddPrecatcherRule', {
        netname: newNetname,
        channel: newChannel,
        botnicks: newBotnicks,
        sitename: newSitename,
        event: newEvent,
        words: newWords,
        section: newSection,
      });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Added',
        message: 'Precatcher rule added successfully',
        color: 'green',
      });
      setAddingRule(false);
      setNewNetname('');
      setNewChannel('');
      setNewBotnicks('');
      setNewSitename('');
      setNewEvent('PRE');
      setNewWords('');
      setNewSection('');
      queryClient.invalidateQueries({ queryKey: ['precatcher-rules'] });
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.message,
        color: 'red',
      });
    },
  });

  const deleteRuleMutation = useMutation({
    mutationFn: async (ruleId: number) => {
      await apiClient.post('/ApiPrecatcherService/DeletePrecatcherRule', {
        RuleId: ruleId,
      });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Deleted',
        message: 'Precatcher rule deleted successfully',
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['precatcher-rules'] });
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.message,
        color: 'red',
      });
    },
  });

  const testPrecatcherMutation = useMutation({
    mutationFn: async () => {
      const announce = JSON.stringify({
        netname: testNetname,
        channel: testChannel,
        nick: testNick,
        text: testText,
      });
      return await apiClient.post('/ApiPrecatcherService/TestPrecatcher', { Announce: announce });
    },
    onSuccess: (res) => {
      const result = res.data.result ? res.data.result[0] : res.data;
      if (result.success) {
        notifications.show({
          title: 'Test Complete',
          message: result.message || 'Precatcher test completed successfully',
          color: 'green',
        });
        setTestOutput(result.output || '');
      } else {
        notifications.show({
          title: 'Test Failed',
          message: result.error || 'Unknown error',
          color: 'red',
        });
        setTestOutput(result.output || '');
      }
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.message,
        color: 'red',
      });
    },
  });

  // Network state
  const [addingNetwork, setAddingNetwork] = useState(false);
  const [newNetworkName, setNewNetworkName] = useState('');
  const [newNetworkHost, setNewNetworkHost] = useState('');
  const [newNetworkPort, setNewNetworkPort] = useState('6697');
  const [newNetworkSsl, setNewNetworkSsl] = useState(true);
  const [newNetworkPassword, setNewNetworkPassword] = useState('');
  const [newNetworkNick, setNewNetworkNick] = useState('');
  const [newNetworkIdent, setNewNetworkIdent] = useState('');
  const [newNetworkUser, setNewNetworkUser] = useState('');

  const addNetworkMutation = useMutation({
    mutationFn: async () => {
      await apiClient.post('/ApiIrcService/AddNetwork', {
        NetName: newNetworkName,
        Host: newNetworkHost,
        Port: parseInt(newNetworkPort, 10),
        Ssl: newNetworkSsl,
        Password: newNetworkPassword,
        Nick: newNetworkNick,
        Ident: newNetworkIdent,
        User: newNetworkUser,
      });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Added',
        message: 'Network added successfully',
        color: 'green',
      });
      setAddingNetwork(false);
      setNewNetworkName('');
      setNewNetworkHost('');
      setNewNetworkPort('6697');
      setNewNetworkSsl(true);
      setNewNetworkPassword('');
      setNewNetworkNick('');
      setNewNetworkIdent('');
      setNewNetworkUser('');
      queryClient.invalidateQueries({ queryKey: ['irc-networks'] });
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.message,
        color: 'red',
      });
    },
  });

  const deleteNetworkMutation = useMutation({
    mutationFn: async (networkName: string) => {
      await apiClient.post('/ApiIrcService/DeleteNetwork', {
        NetName: networkName,
      });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Deleted',
        message: 'Network deleted successfully',
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['irc-networks'] });
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.message,
        color: 'red',
      });
    },
  });

  return (
    <Card shadow="sm" padding="lg" radius="md" withBorder>
      <Tabs value={activeTab} onChange={(value) => setActiveTab(value || 'networks')}>
        <Tabs.List>
          <Tabs.Tab value="networks" leftSection={<IconNetwork size="1rem" />}>
            Networks
          </Tabs.Tab>
          <Tabs.Tab value="channels" leftSection={<IconHash size="1rem" />}>
            Channels
          </Tabs.Tab>
          <Tabs.Tab value="rules" leftSection={<IconFilter size="1rem" />}>
            Catchlist
          </Tabs.Tab>
          <Tabs.Tab value="test" leftSection={<IconFlask size="1rem" />}>
            Test
          </Tabs.Tab>
        </Tabs.List>

        <Tabs.Panel value="networks" pt="md">
          <Stack gap="md">
            <Group justify="space-between">
              <Title order={3}>IRC Networks</Title>
              <Group>
                <Button variant="filled" size="xs" leftSection={<IconPlus size="1rem" />} onClick={() => setAddingNetwork(true)}>
                    Add Network
                </Button>
                <ActionIcon variant="outline" onClick={() => queryClient.invalidateQueries({ queryKey: ['irc-networks'] })}>
                    <IconRefresh size="1.1rem" />
                </ActionIcon>
              </Group>
            </Group>

            {networksLoading ? (
              <Center h={300}><Loader size="lg" /></Center>
            ) : (
              <Table highlightOnHover withTableBorder withColumnBorders>
                <Table.Thead>
                  <Table.Tr>
                    <Table.Th>Network</Table.Th>
                    <Table.Th>Server</Table.Th>
                    <Table.Th>Nickname</Table.Th>
                    <Table.Th>Status</Table.Th>
                    <Table.Th>Channels</Table.Th>
                    <Table.Th>Actions</Table.Th>
                  </Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                  {networks?.map((network) => (
                    <Table.Tr key={network.name}>
                      <Table.Td fw={600}>{network.name}</Table.Td>
                      <Table.Td>{network.host}:{network.port}</Table.Td>
                      <Table.Td>{network.nickname || '-'}</Table.Td>
                      <Table.Td>{getStatusBadge(network)}</Table.Td>
                      <Table.Td>{network.channels_count}</Table.Td>
                      <Table.Td>
                        <Group gap="xs">
                            <Tooltip label="View channels">
                            <ActionIcon
                                variant="light"
                                color="blue"
                                onClick={() => handleViewChannels(network.name)}
                            >
                                <IconHash size="1rem" />
                            </ActionIcon>
                            </Tooltip>
                            <Tooltip label="Delete network">
                            <ActionIcon
                                variant="light"
                                color="red"
                                onClick={() => deleteNetworkMutation.mutate(network.name)}
                            >
                                <IconTrash size="1rem" />
                            </ActionIcon>
                            </Tooltip>
                        </Group>
                      </Table.Td>
                    </Table.Tr>
                  ))}
                </Table.Tbody>
              </Table>
            )}

            {networks && networks.length === 0 && (
              <Text c="dimmed" ta="center" py="xl">No IRC networks configured</Text>
            )}
          </Stack>
        </Tabs.Panel>

        <Tabs.Panel value="channels" pt="md">
          <Stack gap="md">
            <Group justify="space-between">
              <Title order={3}>
                {selectedNetwork ? `Channels for ${selectedNetwork}` : 'IRC Channels'}
              </Title>
              {selectedNetwork && (
                <Group gap="xs">
                  <Button variant="filled" size="xs" leftSection={<IconPlus size="1rem" />} onClick={() => setAddingChannel(true)}>
                    Add Channel
                  </Button>
                  <ActionIcon variant="outline" onClick={() => queryClient.invalidateQueries({ queryKey: ['irc-channels', selectedNetwork] })}>
                    <IconRefresh size="1.1rem" />
                  </ActionIcon>
                  <Button variant="outline" size="xs" onClick={() => setSelectedNetwork(null)}>
                    Select Network
                  </Button>
                </Group>
              )}
            </Group>

            {!selectedNetwork ? (
              <Card withBorder>
                <Stack gap="sm">
                  <Text size="sm" c="dimmed">Select a network to view channels:</Text>
                  <Group gap="xs">
                    {networks?.map((network) => (
                      <Button
                        key={network.name}
                        variant="light"
                        onClick={() => setSelectedNetwork(network.name)}
                      >
                        {network.name}
                      </Button>
                    ))}
                  </Group>
                </Stack>
              </Card>
            ) : channelsLoading ? (
              <Center h={300}><Loader size="lg" /></Center>
            ) : (
              <Table highlightOnHover withTableBorder withColumnBorders>
                <Table.Thead>
                  <Table.Tr>
                    <Table.Th>Channel</Table.Th>
                    <Table.Th>Channel Key</Table.Th>
                    <Table.Th>Blowfish Key</Table.Th>
                    <Table.Th>Roles</Table.Th>
                    <Table.Th>Actions</Table.Th>
                  </Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                  {channels?.map((channel) => (
                    <Table.Tr key={channel.channel}>
                      <Table.Td fw={600}>{channel.channel}</Table.Td>
                      <Table.Td>
                        {channel.chankey ? (
                          <Badge size="sm" color="blue">Set</Badge>
                        ) : (
                          <Text size="sm" c="dimmed">-</Text>
                        )}
                      </Table.Td>
                      <Table.Td>
                        {channel.blowkey ? (
                          <Badge size="sm" color="green">{channel.blowkey}</Badge>
                        ) : (
                          <Text size="sm" c="dimmed">-</Text>
                        )}
                      </Table.Td>
                      <Table.Td>{channel.chanroles || '-'}</Table.Td>
                      <Table.Td>
                        <Group gap="xs">
                          <ActionIcon
                            variant="light"
                            color="blue"
                            onClick={() => openEditModal(channel)}
                          >
                            <IconEdit size="1rem" />
                          </ActionIcon>
                          <ActionIcon
                            variant="light"
                            color="red"
                            onClick={() => deleteChannelMutation.mutate(channel.channel)}
                          >
                            <IconTrash size="1rem" />
                          </ActionIcon>
                        </Group>
                      </Table.Td>
                    </Table.Tr>
                  ))}
                </Table.Tbody>
              </Table>
            )}

            {channels && channels.length === 0 && selectedNetwork && (
              <Text c="dimmed" ta="center" py="xl">No channels configured for {selectedNetwork}</Text>
            )}
          </Stack>
        </Tabs.Panel>

        <Tabs.Panel value="rules" pt="md">
          <Stack gap="md">
            <Group justify="space-between">
              <Title order={3}>Complete Catchlist</Title>
              <Group gap="xs">
                <TextInput 
                  placeholder="Filter... (e.g. site:name chan:#pre)" 
                  leftSection={<IconSearch size="1rem" />}
                  value={catchlistFilter}
                  onChange={(event) => setCatchlistFilter(event.currentTarget.value)}
                  style={{ width: 300 }}
                />
                <Button variant="filled" size="xs" leftSection={<IconPlus size="1rem" />} onClick={() => setAddingRule(true)}>
                  Add Entry
                </Button>
                <ActionIcon variant="outline" onClick={() => queryClient.invalidateQueries({ queryKey: ['precatcher-rules'] })}>
                  <IconRefresh size="1.1rem" />
                </ActionIcon>
              </Group>
            </Group>

            {rulesLoading ? (
              <Center h={300}><Loader size="lg" /></Center>
            ) : (
              <Table highlightOnHover withTableBorder withColumnBorders>
                <Table.Thead>
                  <Table.Tr>
                    <Table.Th>ID</Table.Th>
                    <Table.Th>Site</Table.Th>
                    <Table.Th>Network</Table.Th>
                    <Table.Th>Channel</Table.Th>
                    <Table.Th>Bot Nicks</Table.Th>
                    <Table.Th>Event</Table.Th>
                    <Table.Th>Words</Table.Th>
                    <Table.Th>Section</Table.Th>
                    <Table.Th>Actions</Table.Th>
                  </Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                  {rules
                    ?.filter((rule) => {
                      if (!catchlistFilter) return true;
                      
                      const terms = catchlistFilter.toLowerCase().split(' ');
                      return terms.every(term => {
                        if (term.includes(':')) {
                          const [key, value] = term.split(':');
                          if (!value) return true;
                          
                          switch (key) {
                            case 'id': return rule.id.toString().includes(value);
                            case 'site': return rule.sitename.toLowerCase().includes(value);
                            case 'net': return rule.netname.toLowerCase().includes(value);
                            case 'chan': return rule.channel.toLowerCase().includes(value);
                            case 'nick': return rule.botnicks.toLowerCase().includes(value);
                            case 'event': return rule.event.toLowerCase().includes(value);
                            case 'words': return rule.words.toLowerCase().includes(value);
                            case 'sec': return rule.section.toLowerCase().includes(value);
                            default: return false;
                          }
                        }
                        
                        return (
                          rule.sitename.toLowerCase().includes(term) ||
                          rule.netname.toLowerCase().includes(term) ||
                          rule.channel.toLowerCase().includes(term) ||
                          rule.botnicks.toLowerCase().includes(term) ||
                          rule.event.toLowerCase().includes(term) ||
                          rule.words.toLowerCase().includes(term) ||
                          rule.section.toLowerCase().includes(term)
                        );
                      });
                    })
                    .map((rule) => (
                    <Table.Tr key={rule.id}>
                      <Table.Td>{rule.id}</Table.Td>
                      <Table.Td fw={600}>{rule.sitename}</Table.Td>
                      <Table.Td>{rule.netname}</Table.Td>
                      <Table.Td>{rule.channel}</Table.Td>
                      <Table.Td>{rule.botnicks}</Table.Td>
                      <Table.Td><Badge size="sm">{rule.event}</Badge></Table.Td>
                      <Table.Td style={{ maxWidth: 200, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                        <Tooltip label={rule.words}>
                          <Text size="sm">{rule.words}</Text>
                        </Tooltip>
                      </Table.Td>
                      <Table.Td>{rule.section}</Table.Td>
                      <Table.Td>
                        <ActionIcon
                          variant="light"
                          color="red"
                          onClick={() => deleteRuleMutation.mutate(rule.id)}
                        >
                          <IconTrash size="1rem" />
                        </ActionIcon>
                      </Table.Td>
                    </Table.Tr>
                  ))}
                </Table.Tbody>
              </Table>
            )}

            {rules && rules.length === 0 && (
              <Text c="dimmed" ta="center" py="xl">No catchlist entries configured</Text>
            )}
          </Stack>
        </Tabs.Panel>

        <Tabs.Panel value="test" pt="md">
          <Stack gap="md">
            <Title order={3}>Test Precatcher</Title>
            <Text size="sm" c="dimmed">Test an IRC announce message against your complete catchlist</Text>

            <TextInput
              label="Network Name"
              value={testNetname}
              onChange={(e) => setTestNetname(e.currentTarget.value)}
              placeholder="IRCNET"
              required
            />

            <TextInput
              label="Channel"
              value={testChannel}
              onChange={(e) => setTestChannel(e.currentTarget.value)}
              placeholder="#pre"
              required
            />

            <TextInput
              label="Bot Nick"
              value={testNick}
              onChange={(e) => setTestNick(e.currentTarget.value)}
              placeholder="PreBot"
              required
            />

            <Textarea
              label="Announce Text"
              value={testText}
              onChange={(e) => setTestText(e.currentTarget.value)}
              placeholder="[PRE] Some.Release-GRP in 0DAY"
              required
              minRows={3}
            />

            <Button
              onClick={() => testPrecatcherMutation.mutate()}
              loading={testPrecatcherMutation.isPending}
              leftSection={<IconFlask size="1rem" />}
            >
              Test
            </Button>

            {testOutput && (
              <Textarea
                label="Catchtest Output"
                value={testOutput}
                readOnly
                autosize
                minRows={6}
              />
            )}
          </Stack>
        </Tabs.Panel>

      </Tabs>

      <Modal
        opened={addingChannel}
        onClose={() => setAddingChannel(false)}
        title={`Add Channel to ${selectedNetwork}`}
        centered
      >
        <Stack gap="md">
          <TextInput
            label="Channel Name"
            value={newChannelName}
            onChange={(e) => setNewChannelName(e.currentTarget.value)}
            placeholder="#channel-name"
            required
          />
          <TextInput
            label="Channel Key"
            value={newChankey}
            onChange={(e) => setNewChankey(e.currentTarget.value)}
            placeholder="Leave empty if no key required"
          />
          <TextInput
            label="Blowfish Key"
            value={newBlowkey}
            onChange={(e) => setNewBlowkey(e.currentTarget.value)}
            placeholder="Leave empty for no encryption"
          />
          <TextInput
            label="Roles"
            value={newChanroles}
            onChange={(e) => setNewChanroles(e.currentTarget.value)}
            placeholder="e.g., ADDPRE, NUKE, UNNUKE"
            description="Channel roles/permissions"
          />
          <Group justify="flex-end">
            <Button variant="default" onClick={() => setAddingChannel(false)} leftSection={<IconX size="1rem" />}>
              Cancel
            </Button>
            <Button onClick={() => addChannelMutation.mutate()} loading={addChannelMutation.isPending} leftSection={<IconCheck size="1rem" />}>
              Add
            </Button>
          </Group>
        </Stack>
      </Modal>

      <Modal
        opened={!!editingChannel}
        onClose={() => setEditingChannel(null)}
        title={`Edit Channel: ${editingChannel?.channel} @ ${selectedNetwork}`}
        centered
      >
        <Stack gap="md">
          <TextInput
            label="Channel Key"
            value={editChankey}
            onChange={(e) => setEditChankey(e.currentTarget.value)}
            placeholder="Leave empty if no key required"
          />
          <TextInput
            label="Blowfish Key"
            value={editBlowkey}
            onChange={(e) => setEditBlowkey(e.currentTarget.value)}
            placeholder="Leave empty to keep current blowkey"
            description="Only enter if you want to change the blowfish key"
          />
          <TextInput
            label="Roles"
            value={editChanroles}
            onChange={(e) => setEditChanroles(e.currentTarget.value)}
            placeholder="e.g., ADDPRE, NUKE, UNNUKE"
            description="Channel roles/permissions"
          />
          <Group justify="flex-end">
            <Button variant="default" onClick={() => setEditingChannel(null)} leftSection={<IconX size="1rem" />}>
              Cancel
            </Button>
            <Button onClick={() => saveChannelMutation.mutate()} loading={saveChannelMutation.isPending} leftSection={<IconCheck size="1rem" />}>
              Save
            </Button>
          </Group>
        </Stack>
      </Modal>

      <Modal
        opened={addingRule}
        onClose={() => setAddingRule(false)}
        title="Add Catchlist Entry"
        centered
        size="lg"
      >
        <Stack gap="md">
          <TextInput
            label="Site Name"
            value={newSitename}
            onChange={(e) => setNewSitename(e.currentTarget.value)}
            placeholder="SITEONE"
            required
          />

          <TextInput
            label="Network Name"
            value={newNetname}
            onChange={(e) => setNewNetname(e.currentTarget.value)}
            placeholder="IRCNET"
            required
          />

          <TextInput
            label="Channel"
            value={newChannel}
            onChange={(e) => setNewChannel(e.currentTarget.value)}
            placeholder="#pre"
            required
          />

          <TextInput
            label="Bot Nicks"
            value={newBotnicks}
            onChange={(e) => setNewBotnicks(e.currentTarget.value)}
            placeholder="PreBot|AnotherBot"
            description="Use | to separate multiple nicks"
            required
          />

          <Select
            label="Event Type"
            value={newEvent}
            onChange={(value) => setNewEvent(value || 'PRE')}
            data={EVENT_TYPES}
            required
          />

          <TextInput
            label="Words"
            value={newWords}
            onChange={(e) => setNewWords(e.currentTarget.value)}
            placeholder="*release*"
            description="Keywords to match in the announce"
            required
          />

          <TextInput
            label="Section"
            value={newSection}
            onChange={(e) => setNewSection(e.currentTarget.value)}
            placeholder="0DAY"
            required
          />

          <Group justify="flex-end">
            <Button variant="default" onClick={() => setAddingRule(false)}>
              Cancel
            </Button>
            <Button onClick={() => addRuleMutation.mutate()} loading={addRuleMutation.isPending} leftSection={<IconPlus size="1rem" />}>
              Add
            </Button>
          </Group>
        </Stack>
      </Modal>

      <Modal
        opened={addingNetwork}
        onClose={() => setAddingNetwork(false)}
        title="Add IRC Network"
        centered
      >
        <Stack gap="md">
          <TextInput
            label="Network Name"
            value={newNetworkName}
            onChange={(e) => setNewNetworkName(e.currentTarget.value)}
            placeholder="MyNetwork"
            required
          />
          <TextInput
            label="Host"
            value={newNetworkHost}
            onChange={(e) => setNewNetworkHost(e.currentTarget.value)}
            placeholder="irc.network.org"
            required
          />
          <TextInput
            label="Port"
            value={newNetworkPort}
            onChange={(e) => setNewNetworkPort(e.currentTarget.value)}
            required
          />
          <Switch
            label="Use SSL"
            checked={newNetworkSsl}
            onChange={(event) => setNewNetworkSsl(event.currentTarget.checked)}
          />
          <TextInput
            label="Password"
            value={newNetworkPassword}
            onChange={(e) => setNewNetworkPassword(e.currentTarget.value)}
            placeholder="Optional"
          />
          <TextInput
            label="Nick"
            value={newNetworkNick}
            onChange={(e) => setNewNetworkNick(e.currentTarget.value)}
            placeholder="Optional"
          />
          <TextInput
            label="Ident"
            value={newNetworkIdent}
            onChange={(e) => setNewNetworkIdent(e.currentTarget.value)}
            placeholder="Optional"
          />
          <TextInput
            label="User"
            value={newNetworkUser}
            onChange={(e) => setNewNetworkUser(e.currentTarget.value)}
            placeholder="Optional"
          />
          <Group justify="flex-end">
            <Button variant="default" onClick={() => setAddingNetwork(false)} leftSection={<IconX size="1rem" />}>
              Cancel
            </Button>
            <Button onClick={() => addNetworkMutation.mutate()} loading={addNetworkMutation.isPending} leftSection={<IconCheck size="1rem" />}>
              Add
            </Button>
          </Group>
        </Stack>
      </Modal>
    </Card>
  );
}
