import { Card, Title, Table, Loader, Center, Tabs, Badge, Button, Group, Text, ActionIcon, Tooltip, Stack, TextInput, Modal, Select, Textarea } from '@mantine/core';
import { IconFilter, IconMapPin, IconFlask, IconRefresh, IconPlus, IconTrash, IconEdit } from '@tabler/icons-react';
import { useQuery, useQueryClient, useMutation } from '@tanstack/react-query';
import { useState } from 'react';
import { apiClient } from '../api/client';
import type { Site } from '../api/client';
import { notifications } from '@mantine/notifications';
import { parseIrcAnnounce, parsedAnnounceToWords } from '../utils/ircAnnounceParser';
import type { ParsedAnnounce } from '../utils/ircAnnounceParser';

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

interface SectionMapping {
  id: number;
  origsection: string;
  newsection: string;
  mask: string;
}

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

const EVENT_TYPES = [
  { value: 'PRE', label: 'PRE' },
  { value: 'ADDPRE', label: 'ADDPRE' },
  { value: 'COMPLETE', label: 'COMPLETE' },
  { value: 'NEWDIR', label: 'NEWDIR' },
  { value: 'NUKE', label: 'NUKE' },
  { value: 'REQUEST', label: 'REQUEST' },
];

export function Precatcher() {
  const queryClient = useQueryClient();
  const [activeTab, setActiveTab] = useState<string>('rules');
  const [addingRule, setAddingRule] = useState(false);
  const [editingRule, setEditingRule] = useState<PrecatcherRule | null>(null);

  // Add Rule form state
  const [ircAnnounce, setIrcAnnounce] = useState('');
  const [newNetname, setNewNetname] = useState('');
  const [newChannel, setNewChannel] = useState('');
  const [newBotnicks, setNewBotnicks] = useState('');
  const [newSitename, setNewSitename] = useState('');
  const [newEvent, setNewEvent] = useState('PRE');
  const [newWords, setNewWords] = useState('');
  const [newSection, setNewSection] = useState('');

  // Test form state
  const [testNetname, setTestNetname] = useState('');
  const [testChannel, setTestChannel] = useState('');
  const [testNick, setTestNick] = useState('');
  const [testText, setTestText] = useState('');
  const [testOutput, setTestOutput] = useState('');

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
  });

  const { data: networks } = useQuery({
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
    refetchOnWindowFocus: false,
  });

  const { data: addRuleChannels } = useQuery({
    queryKey: ['irc-channels', 'rule', newNetname],
    queryFn: async (): Promise<IrcChannel[]> => {
      if (!newNetname) return [];
      const res = await apiClient.post('/ApiIrcService/GetChannels', { NetName: newNetname });

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
    enabled: !!newNetname,
    refetchOnWindowFocus: false,
  });

  const { data: sites } = useQuery({
    queryKey: ['sites'],
    queryFn: async (): Promise<Site[]> => {
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
        console.error('Failed to parse sites JSON', e);
      }

      return parsedSites;
    },
    refetchOnWindowFocus: false,
  });

  const { data: mappings, isLoading: mappingsLoading } = useQuery({
    queryKey: ['precatcher-mappings'],
    queryFn: async (): Promise<SectionMapping[]> => {
      const res = await apiClient.post('/ApiPrecatcherService/GetMappings', {});

      let mappings: SectionMapping[] = [];
      try {
        if (res.data.result && Array.isArray(res.data.result)) {
          const resultData = res.data.result[0];
          if (Array.isArray(resultData)) {
            mappings = resultData;
          }
        } else if (typeof res.data === 'string') {
          mappings = JSON.parse(res.data);
        } else if (Array.isArray(res.data)) {
          mappings = res.data;
        }
      } catch (e) {
        console.error('Failed to parse mappings:', e);
        return [];
      }

      return mappings;
    },
    refetchOnWindowFocus: false,
    enabled: activeTab === 'mappings',
  });

  const addRuleMutation = useMutation({
    mutationFn: async () => {
      const payload = {
        RuleData: {
          netname: newNetname,
          channel: newChannel,
          botnicks: newBotnicks,
          sitename: newSitename,
          event: newEvent,
          words: newWords,
          section: newSection,
        },
      };
      console.debug('[precatcher] add rule payload', payload);
      const res = await apiClient.post('/ApiPrecatcherService/AddPrecatcherRule', payload);
      console.debug('[precatcher] add rule response', res.data);
    },
    onSuccess: () => {
      notifications.show({
        title: 'Added',
        message: 'Precatcher rule added successfully',
        color: 'green',
      });
      setAddingRule(false);
      setEditingRule(null);
      setIrcAnnounce('');
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

  const updateRuleMutation = useMutation({
    mutationFn: async () => {
      if (!editingRule) return;
      const payload = {
        RuleId: editingRule.id,
        RuleData: {
          netname: newNetname,
          channel: newChannel,
          botnicks: newBotnicks,
          sitename: newSitename,
          event: newEvent,
          words: newWords,
          section: newSection,
        },
      };
      console.debug('[precatcher] update rule payload', payload);
      const res = await apiClient.post('/ApiPrecatcherService/UpdatePrecatcherRule', payload);
      console.debug('[precatcher] update rule response', res.data);
    },
    onSuccess: () => {
      notifications.show({
        title: 'Updated',
        message: 'Precatcher rule updated successfully',
        color: 'green',
      });
      setAddingRule(false);
      setEditingRule(null);
      setIrcAnnounce('');
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

  const handleEditRule = (rule: PrecatcherRule) => {
    setEditingRule(rule);
    setNewNetname(rule.netname);
    setNewChannel(rule.channel);
    setNewBotnicks(rule.botnicks);
    setNewSitename(rule.sitename);
    setNewEvent(rule.event);
    setNewWords(rule.words);
    setNewSection(rule.section);
    setAddingRule(true);
  };

  const pickWordsFromRules = (parsed: ParsedAnnounce, rulesList: PrecatcherRule[]) => {
    if (!rulesList || rulesList.length === 0) return '';

    const botNick = parsed.botNick.trim().toLowerCase();
    if (!botNick) return '';

    const eventType = parsed.eventType.trim().toUpperCase();
    if (!eventType) return '';

    const matchesBotnick = (value: string) => {
      if (!value) return false;
      const tokens = value
        .split(/[,\s]+/)
        .map((token) => token.trim().toLowerCase())
        .filter(Boolean);
      return tokens.includes(botNick);
    };

    const matchesEvent = (value: string) => value.trim().toUpperCase() === eventType;
    const baseMatches = rulesList.filter((rule) => matchesEvent(rule.event) && matchesBotnick(rule.botnicks) && rule.words);
    if (baseMatches.length === 0) return '';

    const counts = new Map<string, { count: number; firstIndex: number }>();
    baseMatches.forEach((rule, index) => {
      const key = rule.words;
      const entry = counts.get(key);
      if (entry) {
        entry.count += 1;
      } else {
        counts.set(key, { count: 1, firstIndex: index });
      }
    });

    let best = '';
    let bestCount = 0;
    let bestIndex = Number.POSITIVE_INFINITY;
    counts.forEach((entry, key) => {
      if (entry.count > bestCount || (entry.count === bestCount && entry.firstIndex < bestIndex)) {
        best = key;
        bestCount = entry.count;
        bestIndex = entry.firstIndex;
      }
    });

    return best;
  };

  const handleAnnounceChange = (value: string) => {
    setIrcAnnounce(value);

    if (!value.trim()) {
      setNewBotnicks('');
      setNewEvent('PRE');
      setNewWords('');
      return;
    }

    const parsed = parseIrcAnnounce(value);
    if (!parsed) {
      setNewBotnicks('');
      setNewEvent('PRE');
      setNewWords('');
      return;
    }

    setNewBotnicks(parsed.botNick);

    setNewEvent(parsed.eventType);

    const wordsFromRules = pickWordsFromRules(parsed, rules || []);
    setNewWords(wordsFromRules || parsedAnnounceToWords(parsed));

    notifications.show({
      title: 'Auto-filled',
      message: `Parsed announce from bot ${parsed.botNick}`,
      color: 'blue',
      autoClose: 2000,
    });
  };

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

  return (
    <Card shadow="sm" padding="lg" radius="md" withBorder>
      <Tabs value={activeTab} onChange={(value) => setActiveTab(value || 'rules')}>
        <Tabs.List>
          <Tabs.Tab value="rules" leftSection={<IconFilter size="1rem" />}>
            Catchlist
          </Tabs.Tab>
          <Tabs.Tab value="mappings" leftSection={<IconMapPin size="1rem" />}>
            Mappings
          </Tabs.Tab>
          <Tabs.Tab value="test" leftSection={<IconFlask size="1rem" />}>
            Test
          </Tabs.Tab>
        </Tabs.List>

        <Tabs.Panel value="rules" pt="md">
          <Stack gap="md">
            <Group justify="space-between">
              <Title order={3}>Complete Catchlist</Title>
              <Group gap="xs">
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
                  {rules?.map((rule) => (
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
                        <Group gap={4}>
                          <ActionIcon
                            variant="light"
                            color="blue"
                            onClick={() => handleEditRule(rule)}
                          >
                            <IconEdit size="1rem" />
                          </ActionIcon>
                          <ActionIcon
                            variant="light"
                            color="red"
                            onClick={() => deleteRuleMutation.mutate(rule.id)}
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

            {rules && rules.length === 0 && (
              <Text c="dimmed" ta="center" py="xl">No catchlist entries configured</Text>
            )}
          </Stack>
        </Tabs.Panel>

        <Tabs.Panel value="mappings" pt="md">
          <Stack gap="md">
            <Group justify="space-between">
              <Title order={3}>Section Mappings</Title>
              <ActionIcon variant="outline" onClick={() => queryClient.invalidateQueries({ queryKey: ['precatcher-mappings'] })}>
                <IconRefresh size="1.1rem" />
              </ActionIcon>
            </Group>

            {mappingsLoading ? (
              <Center h={300}><Loader size="lg" /></Center>
            ) : (
              <Table highlightOnHover withTableBorder withColumnBorders>
                <Table.Thead>
                  <Table.Tr>
                    <Table.Th>Original Section</Table.Th>
                    <Table.Th>New Section</Table.Th>
                    <Table.Th>Mask</Table.Th>
                  </Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                  {mappings?.map((mapping) => (
                    <Table.Tr key={mapping.id}>
                      <Table.Td fw={600}>{mapping.origsection}</Table.Td>
                      <Table.Td>{mapping.newsection}</Table.Td>
                      <Table.Td><Badge size="sm" variant="light">{mapping.mask}</Badge></Table.Td>
                    </Table.Tr>
                  ))}
                </Table.Tbody>
              </Table>
            )}

            {mappings && mappings.length === 0 && (
              <Text c="dimmed" ta="center" py="xl">No section mappings configured</Text>
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
        opened={addingRule}
        onClose={() => {
          setAddingRule(false);
          setEditingRule(null);
          setIrcAnnounce('');
          setNewNetname('');
          setNewChannel('');
          setNewBotnicks('');
          setNewSitename('');
          setNewEvent('PRE');
          setNewWords('');
          setNewSection('');
        }}
        title={editingRule ? "Edit Catchlist Entry" : "Add Catchlist Entry"}
        centered
        size="lg"
      >
        <Stack gap="md">
          {!editingRule && (
            <Textarea
              label="IRC Announce (Optional)"
              value={ircAnnounce}
              onChange={(e) => handleAnnounceChange(e.currentTarget.value)}
              placeholder="Paste IRC announce here to auto-fill fields"
              minRows={2}
              description="Paste an IRC announce to automatically extract bot nick, event type, and release pattern"
            />
          )}

          {(sites && sites.length > 0) ? (
            <Select
              label="Site Name"
              value={newSitename}
              onChange={(value) => setNewSitename(value || '')}
              data={sites.map((site) => ({ value: site.name, label: site.name }))}
              placeholder="Select a site"
              searchable
              required
            />
          ) : (
            <TextInput
              label="Site Name"
              value={newSitename}
              onChange={(e) => setNewSitename(e.currentTarget.value)}
              placeholder="SITEONE"
              required
            />
          )}

          {(networks && networks.length > 0) ? (
            <Select
              label="Network Name"
              value={newNetname}
              onChange={(value) => {
                setNewNetname(value || '');
                setNewChannel('');
              }}
              data={networks.map((net) => ({ value: net.name, label: net.name }))}
              placeholder="Select a network"
              searchable
              required
            />
          ) : (
            <TextInput
              label="Network Name"
              value={newNetname}
              onChange={(e) => setNewNetname(e.currentTarget.value)}
              placeholder="IRCNET"
              required
            />
          )}

          {(addRuleChannels && addRuleChannels.length > 0) ? (
            <Select
              label="Channel"
              value={newChannel}
              onChange={(value) => setNewChannel(value || '')}
              data={addRuleChannels.map((chan) => ({ value: chan.channel, label: chan.channel }))}
              placeholder="Select a channel"
              searchable
              required
            />
          ) : (
            <TextInput
              label="Channel"
              value={newChannel}
              onChange={(e) => setNewChannel(e.currentTarget.value)}
              placeholder="#pre"
              required
            />
          )}

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
            placeholder="NEW,RACE,join"
            description="Comma-separated keywords (substring match, no wildcards)"
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
            <Button variant="default" onClick={() => {
              setAddingRule(false);
              setEditingRule(null);
            }}>
              Cancel
            </Button>
            <Button 
              onClick={() => editingRule ? updateRuleMutation.mutate() : addRuleMutation.mutate()} 
              loading={editingRule ? updateRuleMutation.isPending : addRuleMutation.isPending} 
              leftSection={editingRule ? <IconEdit size="1rem" /> : <IconPlus size="1rem" />}
            >
              {editingRule ? "Update" : "Add"}
            </Button>
          </Group>
        </Stack>
      </Modal>
    </Card>
  );
}
