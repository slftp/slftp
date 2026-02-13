import { Card, Title, Table, Loader, Center, Tabs, Badge, Button, Group, Text, ActionIcon, Tooltip, Stack, TextInput, Modal, Select, Textarea, Switch, ScrollArea, MultiSelect } from '@mantine/core';
import { IconNetwork, IconHash, IconRefresh, IconEdit, IconCheck, IconX, IconPlus, IconTrash, IconFilter, IconFlask, IconSearch, IconListCheck, IconCopy } from '@tabler/icons-react';
import { useQuery, useQueryClient, useMutation } from '@tanstack/react-query';
import { useState } from 'react';
import { apiClient } from '../api/client';
import type { Site } from '../api/client';
import { notifications } from '@mantine/notifications';
import { parseIrcAnnounce, parsedAnnounceToWords } from '../utils/ircAnnounceParser';
import type { ParsedAnnounce } from '../utils/ircAnnounceParser';

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
  is_added: boolean;
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

interface RecentRelease {
  ReleaseName: string;
  Section: string;
  Added: number;
  PazoId: number;
  Ready: boolean;
  Stopped: boolean;
  QueueNumber: number;
  Sites: string[];
  TotalSites?: number;
  AllowedSites?: number;
  PresentSites?: number;
  ExpectedSites?: number;
  ExpectedSitesList?: string[];
  NotAllowedSites?: number;
}

interface PrecatcherHit {
  id: number;
  atUnix: number;
  netname: string;
  channel: string;
  nick: string;
  sitename: string;
  event: string;
  section: string;
  releaseName: string;
  ruleId: number;
  ruleLine: string;
  text: string;
}

interface ReleaseSiteDetail {
  SiteName: string;
  Complete: boolean;
  FileCount: number;
  TotalFiles: number;
  FilesRacedByMe: number;
  Percent: number;
  Status: string;
}

interface ReleaseDetails {
  ReleaseName: string;
  Section: string;
  Added: string;
  PazoId: number;
  Ready: boolean;
  Stopped: boolean;
  QueueNumber: number;
  SiteDetails: ReleaseSiteDetail[];
  TotalFiles: number;
  ErrorReason: string;
}

const EVENT_TYPES = [
  { value: 'PRE', label: 'PRE' },
  { value: 'ADDPRE', label: 'ADDPRE' },
  { value: 'COMPLETE', label: 'COMPLETE' },
  { value: 'NEWDIR', label: 'NEWDIR' },
  { value: 'NUKE', label: 'NUKE' },
  { value: 'REQUEST', label: 'REQUEST' },
];

const CHANROLE_OPTIONS = [
  { value: 'ADMIN', label: 'ADMIN' },
  { value: 'STATS', label: 'STATS' },
  { value: 'ERROR', label: 'ERROR' },
  { value: 'INFO', label: 'INFO' },
  { value: 'INDEXER', label: 'INDEXER' },
  { value: 'GROUP', label: 'GROUP' },
  { value: 'NUKE', label: 'NUKE' },
  { value: 'IRCEVENT', label: 'IRCEVENT' },
  { value: 'KB', label: 'KB' },
  { value: 'UPDATE', label: 'UPDATE' },
  { value: 'SPEEDSTATS', label: 'SPEEDSTATS' },
  { value: 'RACESTATS', label: 'RACESTATS' },
  { value: 'RANKSTATS', label: 'RANKSTATS' },
  { value: 'PRECATCHSTATS', label: 'PRECATCHSTATS' },
  { value: 'SKIPLOG', label: 'SKIPLOG' },
  { value: 'ROUTEINFOS', label: 'ROUTEINFOS' },
  { value: 'ADDPRE', label: 'ADDPRE' },
  { value: 'ADDTVMAZE', label: 'ADDTVMAZE' },
  { value: 'ADDURL', label: 'ADDURL' },
  { value: 'ADDIMDB', label: 'ADDIMDB' },
  { value: 'ADDPREECHO', label: 'ADDPREECHO' },
  { value: 'ADDGN', label: 'ADDGN' },
  { value: 'ADDTVMAZEECHO', label: 'ADDTVMAZEECHO' },
  { value: 'ADDURLECHO', label: 'ADDURLECHO' },
  { value: 'ADDIMDBECHO', label: 'ADDIMDBECHO' },
  { value: 'ADDGNECHO', label: 'ADDGNECHO' },
];

const parseChanroles = (roles: string) =>
  roles
    .split(/[ ,]+/)
    .map((role) => role.trim())
    .filter(Boolean);

const formatChanroles = (roles: string[]) => roles.join(' ');

const buildChanroleOptions = (roles: string[]) => {
  const byValue = new Map(CHANROLE_OPTIONS.map((opt) => [opt.value, opt]));
  for (const role of roles) {
    if (!byValue.has(role)) {
      byValue.set(role, { value: role, label: role });
    }
  }
  return Array.from(byValue.values());
};

export function IRC() {
  const queryClient = useQueryClient();
  const [selectedNetwork, setSelectedNetwork] = useState<string | null>(null);
  const [activeTab, setActiveTab] = useState<string>('networks');

  const formatUnix = (unix?: number) => {
    if (!unix) return '';
    try {
      return new Date(unix * 1000).toLocaleString();
    } catch {
      return '';
    }
  };

  const [matchesWindow, setMatchesWindow] = useState<string>('24h');
  const [matchesFilter, setMatchesFilter] = useState('');
  const [selectedMatchRelease, setSelectedMatchRelease] = useState<RecentRelease | null>(null);
  const [matchModalOpened, setMatchModalOpened] = useState(false);

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
  const editChanroleValues = parseChanroles(editChanroles);
  const newChanroleValues = parseChanroles(newChanroles);

  // Rules state
  const [addingRule, setAddingRule] = useState(false);
  const [editingRule, setEditingRule] = useState<PrecatcherRule | null>(null);
  const [ircAnnounce, setIrcAnnounce] = useState('');
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
    enabled: activeTab === 'rules' || activeTab === 'matches',
  });

  const getWindowSinceUnix = () => {
    const now = Math.floor(Date.now() / 1000);
    switch (matchesWindow) {
      case '1h': return now - 3600;
      case '6h': return now - 6 * 3600;
      case '24h': return now - 24 * 3600;
      case '7d': return now - 7 * 24 * 3600;
      case 'all': return 0;
      default: return now - 24 * 3600;
    }
  };

  const { data: recentReleases, isLoading: releasesLoading } = useQuery({
    queryKey: ['precatcher-releases', matchesWindow],
    queryFn: async (): Promise<RecentRelease[]> => {
      const res = await apiClient.post('/ApiSystemService/GetRecentReleases', { Limit: 50 });
      try {
        if (res.data?.result && Array.isArray(res.data.result)) {
          const payload = res.data.result[0];
          const releases = payload?.Releases;
          if (Array.isArray(releases)) return releases as RecentRelease[];
          if (typeof releases === 'string') {
            const parsed = JSON.parse(releases);
            return Array.isArray(parsed) ? (parsed as RecentRelease[]) : [];
          }
          return [];
        }
        return [];
      } catch {
        return [];
      }
    },
    refetchOnWindowFocus: false,
    enabled: activeTab === 'matches',
  });

  const { data: precatcherHits, isLoading: hitsLoading } = useQuery({
    queryKey: ['precatcher-hits', matchesWindow],
    queryFn: async (): Promise<PrecatcherHit[]> => {
      const res = await apiClient.post('/ApiPrecatcherService/GetHits', {
        Limit: 1000,
        SinceUnix: getWindowSinceUnix(),
        ReleaseName: '',
        SiteName: '',
      });
      let hits: PrecatcherHit[] = [];
      try {
        if (res.data?.result && Array.isArray(res.data.result)) {
          const resultData = res.data.result[0];
          if (Array.isArray(resultData)) hits = resultData as PrecatcherHit[];
          else if (typeof resultData === 'string') hits = JSON.parse(resultData) as PrecatcherHit[];
        } else if (typeof res.data === 'string') {
          hits = JSON.parse(res.data) as PrecatcherHit[];
        } else if (Array.isArray(res.data)) {
          hits = res.data as PrecatcherHit[];
        }
      } catch {
        return [];
      }
      return hits;
    },
    refetchOnWindowFocus: false,
    enabled: activeTab === 'matches',
  });

  const { data: matchReleaseDetails, isLoading: matchReleaseDetailsLoading } = useQuery({
    queryKey: ['releaseDetails', selectedMatchRelease?.PazoId],
    queryFn: async (): Promise<ReleaseDetails | null> => {
      if (!selectedMatchRelease?.PazoId) return null;
      const res = await apiClient.post('/ApiSystemService/GetReleaseDetails', { PazoId: selectedMatchRelease.PazoId });
      if (res.data && res.data.result && Array.isArray(res.data.result)) {
        const details = res.data.result[0];
        if (details && details.SiteDetails) {
          const siteDetails = (() => {
            if (Array.isArray(details.SiteDetails)) return details.SiteDetails;
            if (typeof details.SiteDetails === 'string') {
              try {
                const parsed = JSON.parse(details.SiteDetails);
                return Array.isArray(parsed) ? parsed : [];
              } catch {
                return [];
              }
            }
            return [];
          })();
          return { ...details, SiteDetails: siteDetails } as ReleaseDetails;
        }
        return details as ReleaseDetails;
      }
      return null;
    },
    enabled: matchModalOpened && !!selectedMatchRelease?.PazoId,
    refetchOnWindowFocus: false,
  });

  const normalize = (s?: string) => (s || '').trim().toLowerCase();
  const isMatchAnnounceEvent = (event?: string) => {
    const e = normalize(event);
    return e === 'pre' || e === 'newdir';
  };

  const isPresentOnSite = (sd?: ReleaseSiteDetail) => {
    if (!sd) return false;
    const st = normalize(sd.Status);
    if (st.includes('present')) return true;
    if (st === 'pre' || st === 'complete') return true;
    if ((sd.FileCount || 0) > 0) return true;
    if ((sd.Percent || 0) > 0) return true;
    if ((sd.FilesRacedByMe || 0) > 0) return true;
    if (sd.Complete) return true;
    return false;
  };

  const matchesByRelease = (() => {
    const map = new Map<string, { hits: PrecatcherHit[]; sites: Set<string>; lastAt: number }>();
    for (const hit of precatcherHits || []) {
      if (!isMatchAnnounceEvent(hit.event)) continue;
      const key = hit.releaseName || '';
      if (!key) continue;
      const existing = map.get(key);
      if (!existing) {
        map.set(key, { hits: [hit], sites: new Set([hit.sitename]), lastAt: hit.atUnix });
      } else {
        existing.hits.push(hit);
        existing.sites.add(hit.sitename);
        if (hit.atUnix > existing.lastAt) existing.lastAt = hit.atUnix;
      }
    }
    return map;
  })();

  const filteredReleases = (recentReleases || []).filter((r) => {
    if (!matchesFilter) return true;
    const term = matchesFilter.toLowerCase();
    const sites = (r.Sites || []).join(' ').toLowerCase();
    return (
      (r.ReleaseName || '').toLowerCase().includes(term) ||
      (r.Section || '').toLowerCase().includes(term) ||
      sites.includes(term)
    );
  });

  const getStatusBadge = (network: IrcNetwork) => {
    if (network.connected) {
      return <Badge variant="light" style={{ background: 'rgba(52, 211, 153, 0.25)', border: '1px solid rgba(52, 211, 153, 0.5)', color: '#34d399' }}>Connected</Badge>;
    } else if (network.status.includes('onnect')) {
      return <Badge variant="light" style={{ background: 'rgba(251, 191, 36, 0.25)', border: '1px solid rgba(251, 191, 36, 0.5)', color: '#fbbf24' }}>Connecting</Badge>;
    } else {
      return <Badge variant="light" style={{ background: 'rgba(248, 113, 113, 0.25)', border: '1px solid rgba(248, 113, 113, 0.5)', color: '#f87171' }}>Disconnected</Badge>;
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
      if (editBlowkey !== '') {
        await apiClient.post('/ApiIrcService/SetChannelBlowkey', {
          NetName: selectedNetwork,
          Channel: editingChannel.channel,
          Blowkey: editBlowkey,
        });
      }

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
      const result = res.data?.result ? res.data.result[0] : res.data;
      if (result !== true) {
        throw new Error('Update failed');
      }
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

  const handleCopyRule = (rule: PrecatcherRule) => {
    setEditingRule(null); // Ensure we are in "add" mode, not "edit"
    setNewNetname(rule.netname);
    setNewChannel(rule.channel);
    setNewBotnicks(rule.botnicks);
    setNewSitename(rule.sitename);
    setNewEvent(rule.event);
    setNewWords(rule.words);
    setNewSection(rule.section);
    setAddingRule(true);
    
    notifications.show({
      title: 'Copied',
      message: 'Rule copied. Modify settings and click Add to save as new entry.',
      color: 'blue',
      autoClose: 3000,
    });
  };

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
  const [editingNetwork, setEditingNetwork] = useState(false);
  const [editNetworkName, setEditNetworkName] = useState('');
  const [editNetworkHost, setEditNetworkHost] = useState('');
  const [editNetworkPort, setEditNetworkPort] = useState('6697');
  const [editNetworkSsl, setEditNetworkSsl] = useState(true);
  const [editNetworkPassword, setEditNetworkPassword] = useState('');
  const [editNetworkNick, setEditNetworkNick] = useState('');
  const [editNetworkIdent, setEditNetworkIdent] = useState('');
  const [editNetworkUser, setEditNetworkUser] = useState('');

  // Delete Network Confirmation Modal
  const [deleteNetworkModalOpened, setDeleteNetworkModalOpened] = useState(false);
  const [networkToDelete, setNetworkToDelete] = useState<string | null>(null);

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

  const updateNetworkMutation = useMutation({
    mutationFn: async () => {
      if (!editNetworkName) return;
      await apiClient.post('/ApiIrcService/SetNetworkConfig', {
        NetName: editNetworkName,
        Host: editNetworkHost,
        Port: parseInt(editNetworkPort, 10),
        Ssl: editNetworkSsl,
        Password: editNetworkPassword,
        Nick: editNetworkNick,
        Ident: editNetworkIdent,
        User: editNetworkUser,
      });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Saved',
        message: 'Network updated successfully',
        color: 'green',
      });
      setEditingNetwork(false);
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

  const openEditNetwork = async (networkName: string) => {
    setEditingNetwork(true);
    setEditNetworkName(networkName);
    try {
      const res = await apiClient.post('/ApiIrcService/GetNetworkConfig', { NetName: networkName });
      const data = res.data.result?.[0] || res.data;
      setEditNetworkHost(data.Host || '');
      setEditNetworkPort(String(data.Port ?? '6697'));
      setEditNetworkSsl(Boolean(data.Ssl));
      setEditNetworkPassword(data.Password || '');
      setEditNetworkNick(data.Nick || '');
      setEditNetworkIdent(data.Ident || '');
      setEditNetworkUser(data.User || '');
    } catch (err: any) {
      notifications.show({
        title: 'Error',
        message: err.message || 'Failed to load network config',
        color: 'red',
      });
    }
  };

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
          <Tabs.Tab value="matches" leftSection={<IconListCheck size="1rem" />}>
            Matches
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
                            <Tooltip label="Edit network">
                            <ActionIcon
                                variant="light"
                                color="blue"
                                onClick={() => openEditNetwork(network.name)}
                            >
                                <IconEdit size="1rem" />
                            </ActionIcon>
                            </Tooltip>
                            <Tooltip label="Delete network">
                            <ActionIcon
                                variant="light"
                                color="red"
                                onClick={() => {
                                  setNetworkToDelete(network.name);
                                  setDeleteNetworkModalOpened(true);
                                }}
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
                          {/* Show + icon for channels that are joined but not added via ircchanadd */}
                          {!channel.is_added ? (
                            <Tooltip label="Add channel (ircchanadd)">
                              <ActionIcon
                                variant="light"
                                color="green"
                                onClick={() => {
                                  setNewChannelName(channel.channel);
                                  setNewChankey('');
                                  setNewBlowkey('');
                                  setNewChanroles('');
                                  setAddingChannel(true);
                                }}
                              >
                                <IconPlus size="1rem" />
                              </ActionIcon>
                            </Tooltip>
                          ) : (
                            <>
                              <Tooltip label="Edit channel">
                                <ActionIcon
                                  variant="light"
                                  color="blue"
                                  onClick={() => openEditModal(channel)}
                                >
                                  <IconEdit size="1rem" />
                                </ActionIcon>
                              </Tooltip>
                              <Tooltip label="Delete channel">
                                <ActionIcon
                                  variant="light"
                                  color="red"
                                  onClick={() => deleteChannelMutation.mutate(channel.channel)}
                                >
                                  <IconTrash size="1rem" />
                                </ActionIcon>
                              </Tooltip>
                            </>
                          )}
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
                        <Group gap={4}>
                          <ActionIcon
                            variant="light"
                            color="blue"
                            onClick={() => handleEditRule(rule)}
                            title="Edit"
                          >
                            <IconEdit size="1rem" />
                          </ActionIcon>
                          <ActionIcon
                            variant="light"
                            color="orange"
                            onClick={() => handleCopyRule(rule)}
                            title="Copy to new entry"
                          >
                            <IconCopy size="1rem" />
                          </ActionIcon>
                          <ActionIcon
                            variant="light"
                            color="red"
                            onClick={() => deleteRuleMutation.mutate(rule.id)}
                            title="Delete"
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

        <Tabs.Panel value="matches" pt="md">
          <Stack gap="md">
            <Group justify="space-between">
              <Title order={3}>Release Matches</Title>
              <Group gap="xs">
                <Select
                  value={matchesWindow}
                  onChange={(v) => setMatchesWindow(v || '24h')}
                  data={[
                    { value: '1h', label: 'Last 1h' },
                    { value: '6h', label: 'Last 6h' },
                    { value: '24h', label: 'Last 24h' },
                    { value: '7d', label: 'Last 7d' },
                    { value: 'all', label: 'All (since start)' },
                  ]}
                  style={{ width: 170 }}
                />
                <TextInput
                  placeholder="Filter releases/sites..."
                  leftSection={<IconSearch size="1rem" />}
                  value={matchesFilter}
                  onChange={(event) => setMatchesFilter(event.currentTarget.value)}
                  style={{ width: 260 }}
                />
                <ActionIcon
                  variant="outline"
                  onClick={() => {
                    queryClient.invalidateQueries({ queryKey: ['precatcher-releases'] });
                    queryClient.invalidateQueries({ queryKey: ['precatcher-hits'] });
                  }}
                >
                  <IconRefresh size="1.1rem" />
                </ActionIcon>
              </Group>
            </Group>

            {(releasesLoading || hitsLoading) ? (
              <Center h={300}><Loader size="lg" /></Center>
            ) : (
              <ScrollArea>
                <Table highlightOnHover withTableBorder withColumnBorders>
                <Table.Thead>
                  <Table.Tr>
                    <Table.Th>Release</Table.Th>
                    <Table.Th>Section</Table.Th>
                    <Table.Th>Added</Table.Th>
                    <Table.Th>Total</Table.Th>
                    <Table.Th>Allowed</Table.Th>
                    <Table.Th>Present</Table.Th>
                    <Table.Th>Expected</Table.Th>
                    <Table.Th>Matched</Table.Th>
                  </Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                  {filteredReleases.map((r) => {
                      const m = matchesByRelease.get(r.ReleaseName);
                      const expectedSitesList = (r.ExpectedSitesList || r.Sites || [])
                        .map((s) => normalize(s))
                        .filter(Boolean);
                      const expectedSitesNormalized = new Set(expectedSitesList);
                      const matchedSites = m
                        ? Array.from(m.sites).filter((s) => expectedSitesNormalized.has(normalize(s))).length
                        : 0;
                      const sitesList = (r.Sites || []).join(', ');
                      const matchedSitesList = m
                        ? Array.from(m.sites).filter((s) => expectedSitesNormalized.has(normalize(s))).join(', ')
                        : '';
                      const totalSites = r.TotalSites ?? (r.Sites || []).length;
                      const allowedSites = r.AllowedSites ?? totalSites;
                      const presentSites = r.PresentSites ?? 0;
                      const expectedSites = r.ExpectedSites ?? expectedSitesNormalized.size ?? allowedSites;
                      const rowFilter = matchesFilter.toLowerCase();
                      if (rowFilter && m) {
                        const anyHitMatches = m.hits.some((h) =>
                          (h.sitename || '').toLowerCase().includes(rowFilter) ||
                          (h.event || '').toLowerCase().includes(rowFilter)
                        );
                        if (!anyHitMatches &&
                          !r.ReleaseName.toLowerCase().includes(rowFilter) &&
                          !r.Section.toLowerCase().includes(rowFilter) &&
                          !(r.Sites || []).join(' ').toLowerCase().includes(rowFilter)
                        ) {
                          return null;
                        }
                      }

                      return (
                        <Table.Tr
                          key={r.PazoId}
                          style={{ cursor: 'pointer' }}
                          onClick={() => {
                            setSelectedMatchRelease(r);
                            setMatchModalOpened(true);
                          }}
                        >
                          <Table.Td fw={600}>{r.ReleaseName}</Table.Td>
                          <Table.Td>{r.Section || '-'}</Table.Td>
                          <Table.Td>{formatUnix(r.Added)}</Table.Td>
                          <Table.Td>
                            <Tooltip label={sitesList || 'No sites'} disabled={!sitesList}>
                              <Badge variant="light">{totalSites}</Badge>
                            </Tooltip>
                          </Table.Td>
                          <Table.Td><Badge variant="light">{allowedSites}</Badge></Table.Td>
                          <Table.Td><Badge variant="light">{presentSites}</Badge></Table.Td>
                          <Table.Td><Badge variant="light">{expectedSites}</Badge></Table.Td>
                          <Table.Td>
                            <Tooltip label={matchedSitesList || ''} disabled={!matchedSitesList}>
                              <Badge color={matchedSites === expectedSites && expectedSites > 0 ? 'green' : (matchedSites > 0 ? 'yellow' : 'gray')}>
                                {matchedSites}/{expectedSites}
                              </Badge>
                            </Tooltip>
                          </Table.Td>
                        </Table.Tr>
                      );
                    })}
                  </Table.Tbody>
                </Table>
              </ScrollArea>
            )}

            <Modal
              opened={matchModalOpened}
              onClose={() => {
                setMatchModalOpened(false);
              }}
              title={`Matches: ${selectedMatchRelease?.ReleaseName || ''}`}
              size="90%"
            >
              {selectedMatchRelease && (
                <ScrollArea h="75vh">
                  <Stack gap="md">
                    <div>
                      <Title order={5}>Site Presence vs. Announce</Title>
                      <Text size="sm" c="dimmed">
                        We expect an announce only if the release is present on a site (based on current release details). This view compares PRE/NEWDIR rules and PRE/NEWDIR hits in the selected window.
                      </Text>
                      {matchReleaseDetailsLoading && <Text size="sm" c="dimmed" mt="xs">Loading release site status...</Text>}
                    </div>

                    {(() => {
                      const releaseSites = (selectedMatchRelease.Sites || [])
                        .map((s) => normalize(s))
                        .filter(Boolean);

                      const hits = matchesByRelease.get(selectedMatchRelease.ReleaseName)?.hits || [];
                      const relevantRules = (rules || []).filter(
                        (r) => releaseSites.includes(normalize(r.sitename)) && isMatchAnnounceEvent(r.event)
                      );

                      const botsBySite = new Map<string, Set<string>>();
                      for (const r of relevantRules) {
                        const siteKey = normalize(r.sitename);
                        const set = botsBySite.get(siteKey) || new Set<string>();
                        for (const b of (r.botnicks || '').split(',').map((x) => normalize(x)).filter(Boolean)) {
                          set.add(b);
                        }
                        botsBySite.set(siteKey, set);
                      }

                      const hitsBySite = new Map<string, PrecatcherHit[]>();
                      for (const h of hits) {
                        if (!isMatchAnnounceEvent(h.event)) continue;
                        const siteKey = normalize(h.sitename);
                        if (!releaseSites.includes(siteKey)) continue;
                        const arr = hitsBySite.get(siteKey) || [];
                        arr.push(h);
                        hitsBySite.set(siteKey, arr);
                      }

                      const detailsBySite = new Map<string, ReleaseSiteDetail>();
                      for (const sd of (matchReleaseDetails?.SiteDetails || [])) {
                        const key = normalize(sd.SiteName);
                        if (key) detailsBySite.set(key, sd);
                      }

                      const rows = releaseSites.map((siteKey) => {
                        const expectedBots = Array.from(botsBySite.get(siteKey) || new Set<string>()).sort();
                        const siteHits = hitsBySite.get(siteKey) || [];
                        const seenBots = Array.from(new Set(siteHits.map((h) => normalize(h.nick)).filter(Boolean))).sort();
                        const missingBots = expectedBots.filter((b) => !seenBots.includes(b));
                        const eventsSeen = Array.from(new Set(siteHits.map((h) => normalize(h.event)).filter(Boolean))).sort();
                        const sd = detailsBySite.get(siteKey);
                        const present = isPresentOnSite(sd);
                        const st = normalize(sd?.Status);
                        const allowedByRules = st !== 'not allowed' && st !== 'not allowed (present)';
                        const expectedAnnounce = present && allowedByRules;
                        return { siteKey, sd, expectedBots, seenBots, missingBots, eventsSeen, hitsCount: siteHits.length, present, expectedAnnounce };
                      });

                      if (rows.length === 0) {
                        return <Text size="sm" c="dimmed">No site data available.</Text>;
                      }

                      const severityRank = (r: any) => {
                        if (r.expectedAnnounce && r.hitsCount === 0) return 0;
                        if (r.expectedAnnounce && r.missingBots.length > 0) return 1;
                        if (r.expectedAnnounce) return 2;
                        return 3;
                      };

                      const sorted = rows
                        .slice()
                        .sort((a, b) => {
                          const ra = severityRank(a);
                          const rb = severityRank(b);
                          if (ra !== rb) return ra - rb;
                          return a.siteKey.localeCompare(b.siteKey);
                        });

                      const missingCount = sorted.filter((r) => r.expectedAnnounce && r.hitsCount === 0).length;
                      const partialCount = sorted.filter((r) => r.expectedAnnounce && r.hitsCount > 0 && r.missingBots.length > 0).length;

                      return (
                        <>
                          <Group gap="xs">
                            <Badge color={missingCount > 0 ? 'red' : 'green'} variant="light">Missing announces: {missingCount}</Badge>
                            <Badge color={partialCount > 0 ? 'yellow' : 'green'} variant="light">Missing bots: {partialCount}</Badge>
                            <Text size="sm" c="dimmed">Sorted by importance (missing/partial first)</Text>
                          </Group>

                          <Table withTableBorder withColumnBorders highlightOnHover>
                            <Table.Thead>
                              <Table.Tr>
                                <Table.Th>Site</Table.Th>
                                <Table.Th>Present</Table.Th>
                                <Table.Th>Site Status</Table.Th>
                                <Table.Th>Files</Table.Th>
                                <Table.Th>Expected Announce</Table.Th>
                                <Table.Th>Announce Status</Table.Th>
                                <Table.Th>Hits</Table.Th>
                                <Table.Th>Seen Bots</Table.Th>
                                <Table.Th>Missing Bots</Table.Th>
                                <Table.Th>Events Seen</Table.Th>
                              </Table.Tr>
                            </Table.Thead>
                            <Table.Tbody>
                              {sorted.map((r) => {
                                const missingAnnounce = r.expectedAnnounce && r.hitsCount === 0;
                                const partial = r.expectedAnnounce && r.hitsCount > 0 && r.missingBots.length > 0;
                                const ok = r.expectedAnnounce && !missingAnnounce && !partial;

                                const rowStyle =
                                  missingAnnounce ? { backgroundColor: 'rgba(255, 0, 0, 0.08)' } :
                                  partial ? { backgroundColor: 'rgba(255, 165, 0, 0.08)' } :
                                  undefined;

                                return (
                                  <Table.Tr key={r.siteKey} style={rowStyle}>
                              <Table.Td fw={600}>{r.siteKey.toUpperCase()}</Table.Td>
                              <Table.Td>
                                {r.present ? <Badge color="green" variant="light">yes</Badge> : <Badge color="gray" variant="light">no</Badge>}
                              </Table.Td>
                              <Table.Td>{r.sd?.Status || '-'}</Table.Td>
                              <Table.Td>
                                {r.sd ? (
                                  <Text size="sm">{r.sd.FileCount}/{r.sd.TotalFiles} ({Math.round(r.sd.Percent)}%)</Text>
                                ) : (
                                  <Text size="sm" c="dimmed">-</Text>
                                )}
                              </Table.Td>
                              <Table.Td>
                                {r.expectedAnnounce ? <Badge color="blue" variant="light">yes</Badge> : <Badge color="gray" variant="light">no</Badge>}
                              </Table.Td>
                              <Table.Td>
                                {missingAnnounce ? (
                                  <Badge color="red">missing</Badge>
                                ) : partial ? (
                                  <Badge color="yellow">partial</Badge>
                                ) : ok ? (
                                  <Badge color="green">ok</Badge>
                                ) : (
                                  <Badge color="gray" variant="light">n/a</Badge>
                                )}
                              </Table.Td>
                              <Table.Td>
                                {r.hitsCount > 0 ? (
                                  <Badge variant="light">{r.hitsCount}</Badge>
                                ) : (
                                  <Badge color={r.expectedAnnounce ? 'red' : 'gray'} variant="light">no hits</Badge>
                                )}
                              </Table.Td>
                              <Table.Td>{r.seenBots.length > 0 ? r.seenBots.join(', ') : <Text size="sm" c="dimmed">-</Text>}</Table.Td>
                              <Table.Td>
                                {r.missingBots.length > 0 ? (
                                  <Tooltip label={r.missingBots.join(', ')}>
                                    <Badge color="yellow" variant="light">{r.missingBots.length} missing</Badge>
                                  </Tooltip>
                                ) : (
                                  <Text size="sm" c="dimmed">-</Text>
                                )}
                              </Table.Td>
                              <Table.Td>{r.eventsSeen.length > 0 ? r.eventsSeen.join(', ') : <Text size="sm" c="dimmed">-</Text>}</Table.Td>
                                  </Table.Tr>
                                );
                              })}
                            </Table.Tbody>
                          </Table>
                        </>
                      );
                    })()}
                  </Stack>
                </ScrollArea>
              )}
            </Modal>
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
            description="Prefix with 'cbc:' for CBC mode (e.g., cbc:yourkey), otherwise ECB is used"
          />
          <MultiSelect
            label="Roles"
            data={buildChanroleOptions(newChanroleValues)}
            value={newChanroleValues}
            onChange={(values) => setNewChanroles(formatChanroles(values))}
            placeholder="Select channel roles"
            description="Channel roles/permissions (saved space-separated)"
            searchable
            clearable
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
            description="Prefix with 'cbc:' for CBC mode (e.g., cbc:yourkey), otherwise ECB is used"
          />
          <MultiSelect
            label="Roles"
            data={buildChanroleOptions(editChanroleValues)}
            value={editChanroleValues}
            onChange={(values) => setEditChanroles(formatChanroles(values))}
            placeholder="Select channel roles"
            description="Channel roles/permissions (saved space-separated)"
            searchable
            clearable
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
                if (value) {
                  queryClient.invalidateQueries({ queryKey: ['irc-channels', 'rule', value] });
                }
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
            placeholder="PreBot,AnotherBot"
            description="Use , to separate multiple nicks"
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

      <Modal
        opened={editingNetwork}
        onClose={() => setEditingNetwork(false)}
        title="Edit IRC Network"
        centered
      >
        <Stack gap="md">
          <TextInput
            label="Network Name"
            value={editNetworkName}
            disabled
          />
          <TextInput
            label="Host"
            value={editNetworkHost}
            onChange={(e) => setEditNetworkHost(e.currentTarget.value)}
            placeholder="irc.network.org"
            required
          />
          <TextInput
            label="Port"
            value={editNetworkPort}
            onChange={(e) => setEditNetworkPort(e.currentTarget.value)}
            required
          />
          <Switch
            label="Use SSL"
            checked={editNetworkSsl}
            onChange={(event) => setEditNetworkSsl(event.currentTarget.checked)}
          />
          <TextInput
            label="Password"
            value={editNetworkPassword}
            onChange={(e) => setEditNetworkPassword(e.currentTarget.value)}
            placeholder="Optional"
          />
          <TextInput
            label="Nick"
            value={editNetworkNick}
            onChange={(e) => setEditNetworkNick(e.currentTarget.value)}
            placeholder="Optional"
          />
          <TextInput
            label="Ident"
            value={editNetworkIdent}
            onChange={(e) => setEditNetworkIdent(e.currentTarget.value)}
            placeholder="Optional"
          />
          <TextInput
            label="User"
            value={editNetworkUser}
            onChange={(e) => setEditNetworkUser(e.currentTarget.value)}
            placeholder="Optional"
          />
          <Group justify="flex-end">
            <Button variant="default" onClick={() => setEditingNetwork(false)} leftSection={<IconX size="1rem" />}>
              Cancel
            </Button>
            <Button onClick={() => updateNetworkMutation.mutate()} loading={updateNetworkMutation.isPending} leftSection={<IconCheck size="1rem" />}>
              Save
            </Button>
          </Group>
        </Stack>
      </Modal>

      <Modal
        opened={deleteNetworkModalOpened}
        onClose={() => {
          setDeleteNetworkModalOpened(false);
          setNetworkToDelete(null);
        }}
        title="Confirm Network Deletion"
        centered
      >
        <Stack gap="md">
          <Text>
            Are you sure you want to delete network <Text component="span" fw={700} c="red">{networkToDelete}</Text>?
          </Text>
          <Text size="sm" c="dimmed">
            This will remove the network configuration and all associated channels. This action cannot be undone.
          </Text>
          <Group justify="flex-end" mt="md">
            <Button
              variant="default"
              onClick={() => {
                setDeleteNetworkModalOpened(false);
                setNetworkToDelete(null);
              }}
            >
              Cancel
            </Button>
            <Button
              color="red"
              loading={deleteNetworkMutation.isPending}
              onClick={() => {
                if (networkToDelete) {
                  deleteNetworkMutation.mutate(networkToDelete);
                  setDeleteNetworkModalOpened(false);
                  setNetworkToDelete(null);
                }
              }}
            >
              Delete Network
            </Button>
          </Group>
        </Stack>
      </Modal>
    </Card>
  );
}
