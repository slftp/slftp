import { ActionIcon, Alert, Autocomplete, Badge, Button, Card, Group, Loader, Stack, Table, Text, TextInput, Title, Tooltip } from '@mantine/core';
import { notifications } from '@mantine/notifications';
import { IconAlertCircle, IconBolt, IconPlayerPlay, IconWand } from '@tabler/icons-react';
import { useMutation, useQuery } from '@tanstack/react-query';
import { useMemo, useState } from 'react';
import { apiClient } from '../api/client';

type RecentRelease = {
  ReleaseName: string;
  Section: string;
  Added: number;
  PazoId: number;
  Ready: boolean;
  Stopped: boolean;
};

type SimulatorSiteResult = {
  Sitename: string;
  Section: string;
  Allowed: boolean;
  Reason: string;
  RuleAction: string;
  IsAffil: boolean;
  HasSection: boolean;
  SiteDown: boolean;
  PretimeOk: boolean;
};

type SimulatorRouteResult = {
  SourceSite: string;
  DestinationSite: string;
  Rank: number;
  RouteWeight: number;
};

type SimulatorResponse = {
  success: boolean;
  error?: string;
  simulation?: {
    Releasename: string;
    Section: string;
    TotalSites: number;
    AllowedSites: number;
    ErrorMessage: string;
    Sites: SimulatorSiteResult[] | string;
    Routes: SimulatorRouteResult[] | string;
  };
};

function parseMaybeJsonArray<T = any>(value: unknown): T[] {
  if (Array.isArray(value)) return value as T[];
  if (typeof value === 'string') {
    try {
      const parsed = JSON.parse(value);
      return Array.isArray(parsed) ? (parsed as T[]) : [];
    } catch {
      return [];
    }
  }
  return [];
}

function buildPreCommand(section: string, releaseName: string): string {
  const sec = section.trim();
  const rls = releaseName.trim();
  if (!sec) return `pre ${rls}`;
  return `pre ${sec} ${rls}`;
}

function buildPretestCommand(siteName: string, section: string, releaseName: string): string {
  const site = siteName.trim();
  const sec = section.trim();
  const rls = releaseName.trim();
  if (!sec) return `pretest ${site} ${rls}`;
  return `pretest ${site} ${sec} ${rls}`;
}

function buildPrelistCommand(siteName: string, section: string): string {
  const site = siteName.trim();
  const sec = section.trim();
  if (!sec || sec.toUpperCase() === 'PRE') return `prelist ${site}`;
  return `prelist ${site} ${sec}`;
}

export function Pre() {
  const [section, setSection] = useState('PRE');
  const [releaseName, setReleaseName] = useState('');
  const [pretestSite, setPretestSite] = useState('');
  const [pretestSection, setPretestSection] = useState('PRE');
  const [pretestRelease, setPretestRelease] = useState('');
  const [prelistSite, setPrelistSite] = useState('*');
  const [prelistSection, setPrelistSection] = useState('PRE');

  const { data: recentReleasesData } = useQuery({
    queryKey: ['recent-releases'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSystemService/GetRecentReleases', { Limit: 50 });
      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }
      const rawReleases = responseData.Releases;
      if (!rawReleases) return [];
      try {
        if (typeof rawReleases === 'string') {
          return JSON.parse(rawReleases) as RecentRelease[];
        }
        if (Array.isArray(rawReleases)) {
          return rawReleases as RecentRelease[];
        }
      } catch {
        return [];
      }
      return [];
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const { data: sitesData } = useQuery({
    queryKey: ['sites'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: '' });
      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }
      const rawSites = responseData.Sites;
      let parsedSites: any[] = [];
      try {
        if (typeof rawSites === 'string') parsedSites = JSON.parse(rawSites);
        else if (Array.isArray(rawSites)) parsedSites = rawSites;
      } catch {
        return [];
      }
      return parsedSites.filter(site => String(site?.name || '').toLowerCase() !== 'slftp');
    },
    refetchOnWindowFocus: false,
  });

  const releaseOptions = useMemo(() => {
    const recentReleases = recentReleasesData || [];
    const names = recentReleases.map(r => r.ReleaseName);
    return [...new Set(names)];
  }, [recentReleasesData]);

  const siteOptions = useMemo(() => {
    const sites = sitesData || [];
    const names = sites.map((s: any) => String(s?.name || '')).filter(Boolean).sort((a: string, b: string) => a.localeCompare(b));
    return names;
  }, [sitesData]);

  const siteOptionsWithWildcard = useMemo(() => ['*', ...siteOptions], [siteOptions]);

  const handleReleaseSelect = (value: string) => {
    const recentReleases = recentReleasesData || [];
    const release = recentReleases.find(r => r.ReleaseName === value);
    if (!release) return;
    setReleaseName(release.ReleaseName);
    setSection(release.Section || 'PRE');
  };

  const detectSectionMutation = useMutation({
    mutationFn: async (rlsName: string) => {
      const res = await apiClient.post('/ApiSimulatorService/DetectSection', { ReleaseName: rlsName });
      if (res.data?.result && Array.isArray(res.data.result)) return res.data.result[0];
      return res.data;
    },
    onSuccess: (data) => {
      if (data?.success && data?.section) {
        setSection(String(data.section));
      }
    },
  });

  const simulateMutation = useMutation({
    mutationFn: async (): Promise<SimulatorResponse> => {
      const res = await apiClient.post('/ApiSimulatorService/Simulate', { Section: section, ReleaseName: releaseName, SimulatePre: true });
      if (res.data?.result && Array.isArray(res.data.result)) return res.data.result[0] as SimulatorResponse;
      return res.data as SimulatorResponse;
    },
  });

  const executePreMutation = useMutation({
    mutationFn: async () => {
      const cmd = buildPreCommand(section, releaseName);
      await apiClient.post('/ApiSitesService/ExecuteIrcCommand', { Command: cmd });
      return cmd;
    },
    onSuccess: (cmd) => {
      notifications.show({
        title: 'PRE started',
        message: `Command sent: ${cmd} (check IRC/logs for details)`,
        color: 'blue',
        autoClose: 8000,
      });
    },
    onError: (err: any) => {
      notifications.show({ title: 'Error', message: err?.message || 'Failed to execute PRE', color: 'red' });
    }
  });

  const executePretestMutation = useMutation({
    mutationFn: async () => {
      const cmd = buildPretestCommand(pretestSite, pretestSection, pretestRelease);
      await apiClient.post('/ApiSitesService/ExecuteIrcCommand', { Command: cmd });
      return cmd;
    },
    onSuccess: (cmd) => {
      notifications.show({
        title: 'PRETEST started',
        message: `Command sent: ${cmd} (check IRC/logs for details)`,
        color: 'blue',
        autoClose: 8000,
      });
    },
    onError: (err: any) => {
      notifications.show({ title: 'Error', message: err?.message || 'Failed to execute PRETEST', color: 'red' });
    }
  });

  const executePrelistMutation = useMutation({
    mutationFn: async () => {
      const cmd = buildPrelistCommand(prelistSite, prelistSection);
      await apiClient.post('/ApiSitesService/ExecuteIrcCommand', { Command: cmd });
      return cmd;
    },
    onSuccess: (cmd) => {
      notifications.show({
        title: 'PRELIST started',
        message: `Command sent: ${cmd} (check IRC/logs for details)`,
        color: 'blue',
        autoClose: 8000,
      });
    },
    onError: (err: any) => {
      notifications.show({ title: 'Error', message: err?.message || 'Failed to execute PRELIST', color: 'red' });
    }
  });

  const sim = simulateMutation.data?.simulation;
  const sites = useMemo(() => parseMaybeJsonArray<SimulatorSiteResult>(sim?.Sites), [sim?.Sites]);

  const canRun = releaseName.trim().length > 0 && section.trim().length > 0;
  const canExecute = releaseName.trim().length > 0;
  const canPretest = pretestSite.trim().length > 0 && pretestRelease.trim().length > 0;
  const canPrelist = prelistSite.trim().length > 0;

  return (
    <Stack>
      <Group justify="space-between" align="center">
        <Group gap="xs" align="center">
          <Title order={2}>PRE</Title>
          <Badge color="yellow" variant="light">Untested</Badge>
        </Group>
        <Badge variant="light">IRC Command</Badge>
      </Group>

      <Card withBorder radius="md" p="md">
        <Stack gap="xs">
          <Text size="sm" c="dimmed">
            Usage: <Text component="span" ff="monospace">pre [section] rlsname</Text>
          </Text>
          <Text size="sm" c="dimmed">
            Description: checks whether the release exists on each site, then sends pre commands. Default section is <Text component="span" ff="monospace">PRE</Text>.
          </Text>
          <Text size="sm" c="dimmed">
            Examples: <Text component="span" ff="monospace">pre MyRip-GRP</Text> · <Text component="span" ff="monospace">pre PRE MyRip-GRP</Text> · <Text component="span" ff="monospace">pre PRE-X264-1080P MyRip-GRP</Text>
          </Text>
        </Stack>
      </Card>

      <Card withBorder radius="md" p="md">
        <Stack gap="md">
          <Autocomplete
            label="Recent Releases (last 50)"
            placeholder="Select a recent release..."
            data={releaseOptions}
            onOptionSubmit={handleReleaseSelect}
            limit={50}
            maxDropdownHeight={300}
          />

          <Group align="end">
            <Group align="end" gap="xs" style={{ flex: 1 }}>
              <TextInput
                label="Section"
                placeholder="PRE"
                value={section}
                onChange={(e) => setSection(e.currentTarget.value)}
                style={{ flex: 1 }}
              />
              <Tooltip label="Detect section from release name" withArrow withinPortal>
                <ActionIcon
                  variant="light"
                  color="blue"
                  size="lg"
                  onClick={() => releaseName.trim() && detectSectionMutation.mutate(releaseName)}
                  loading={detectSectionMutation.isPending}
                  disabled={!releaseName.trim()}
                  style={{ marginBottom: 1 }}
                >
                  <IconWand size="1.125rem" />
                </ActionIcon>
              </Tooltip>
            </Group>
            <TextInput
              label="Release"
              placeholder="ReleaseName..."
              value={releaseName}
              onChange={(e) => setReleaseName(e.currentTarget.value)}
              style={{ flex: 3 }}
            />
            <Button
              variant="light"
              leftSection={<IconPlayerPlay size="1rem" />}
              onClick={() => simulateMutation.mutate()}
              loading={simulateMutation.isPending}
              disabled={!canRun}
            >
              Preview
            </Button>
            <Button
              color="violet"
              leftSection={<IconBolt size="1rem" />}
              onClick={() => executePreMutation.mutate()}
              loading={executePreMutation.isPending}
              disabled={!canExecute}
            >
              Execute PRE
            </Button>
          </Group>
        </Stack>
      </Card>

      <Card withBorder radius="md" p="md">
        <Stack gap="xs">
          <Group justify="space-between" align="center">
            <Title order={3}>PRELIST</Title>
          </Group>
          <Text size="sm" c="dimmed">
            Usage: <Text component="span" ff="monospace">prelist sitename</Text> (or <Text component="span" ff="monospace">*</Text> for all sites)
          </Text>
          <Text size="sm" c="dimmed">
            Description: dirlists predir and sorts them by release.
          </Text>

          <Group align="end">
            <Autocomplete
              label="Site"
              placeholder="e.g. MyHQ or *"
              data={siteOptionsWithWildcard}
              value={prelistSite}
              onChange={setPrelistSite}
              style={{ flex: 2 }}
              limit={50}
              maxDropdownHeight={300}
            />
            <TextInput
              label="Section (optional)"
              placeholder="PRE"
              value={prelistSection}
              onChange={(e) => setPrelistSection(e.currentTarget.value)}
              style={{ flex: 1 }}
            />
            <Button
              leftSection={<IconPlayerPlay size="1rem" />}
              onClick={() => executePrelistMutation.mutate()}
              loading={executePrelistMutation.isPending}
              disabled={!canPrelist}
            >
              Execute PRELIST
            </Button>
          </Group>
        </Stack>
      </Card>

      <Card withBorder radius="md" p="md">
        <Stack gap="xs">
          <Group justify="space-between" align="center">
            <Title order={3}>PRETEST</Title>
          </Group>
          <Text size="sm" c="dimmed">
            Usage: <Text component="span" ff="monospace">pretest sitename [section] rlsname</Text> (default section: <Text component="span" ff="monospace">PRE</Text>)
          </Text>
          <Text size="sm" c="dimmed">
            Description: issues the pre command on a single site (check IRC/logs for output).
          </Text>

          <Group align="end">
            <Autocomplete
              label="Site"
              placeholder="e.g. MyHQ"
              data={siteOptions}
              value={pretestSite}
              onChange={setPretestSite}
              style={{ flex: 2 }}
              limit={50}
              maxDropdownHeight={300}
            />
            <Tooltip label="Leave empty to use default PRE" withArrow withinPortal>
              <TextInput
                label="Section (optional)"
                placeholder="PRE"
                value={pretestSection}
                onChange={(e) => setPretestSection(e.currentTarget.value)}
                style={{ flex: 1 }}
              />
            </Tooltip>
            <TextInput
              label="Release"
              placeholder="MyRip-GRP"
              value={pretestRelease}
              onChange={(e) => setPretestRelease(e.currentTarget.value)}
              style={{ flex: 3 }}
            />
            <Button
              leftSection={<IconBolt size="1rem" />}
              onClick={() => executePretestMutation.mutate()}
              loading={executePretestMutation.isPending}
              disabled={!canPretest}
            >
              Execute PRETEST
            </Button>
          </Group>
        </Stack>
      </Card>

      {simulateMutation.isPending && (
        <Group justify="center" p="md"><Loader size="md" /></Group>
      )}

      {simulateMutation.isError && (
        <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
          {(simulateMutation.error as any)?.message || 'Failed to run preview'}
        </Alert>
      )}

      {simulateMutation.data && simulateMutation.data.success === false && (
        <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
          {simulateMutation.data.error || 'Preview failed'}
        </Alert>
      )}

      {sim && (
        <>
          <Card withBorder radius="md" p="sm">
            <Group justify="space-between">
              <Group gap="xs">
                <Badge color="gray" variant="light">Total sites: {sim.TotalSites}</Badge>
                <Badge color="teal" variant="light">Allowed: {sim.AllowedSites}</Badge>
                <Badge color="violet" variant="light">PRE</Badge>
              </Group>
              <Text size="xs" c="dimmed">{sim.Section} · {sim.Releasename}</Text>
            </Group>
          </Card>

          <Card withBorder radius="md" p="md">
            <Table striped highlightOnHover withTableBorder>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Site</Table.Th>
                  <Table.Th>Allowed</Table.Th>
                  <Table.Th>Rule</Table.Th>
                  <Table.Th>Reason</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {sites
                  .slice()
                  .sort((a, b) => a.Sitename.localeCompare(b.Sitename))
                  .map((s) => (
                    <Table.Tr key={`${s.Sitename}`}>
                      <Table.Td><Text size="sm">{s.Sitename}</Text></Table.Td>
                      <Table.Td>
                        <Badge color={s.Allowed ? 'teal' : 'gray'} variant="light">{s.Allowed ? 'ALLOW' : 'NO'}</Badge>
                      </Table.Td>
                      <Table.Td><Text size="sm" c="dimmed">{s.RuleAction}</Text></Table.Td>
                      <Table.Td><Text size="sm">{s.Reason}</Text></Table.Td>
                    </Table.Tr>
                  ))}
              </Table.Tbody>
            </Table>
          </Card>
        </>
      )}
    </Stack>
  );
}
