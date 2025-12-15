import { Alert, Badge, Button, Card, Group, Loader, ScrollArea, Stack, Table, Text, TextInput, Title, Tooltip } from '@mantine/core';
import { IconAlertCircle, IconRefresh, IconSearch } from '@tabler/icons-react';
import { useQuery } from '@tanstack/react-query';
import { useMemo, useState } from 'react';
import { apiClient } from '../api/client';
import type { Issue, IssuesSummary } from '../api/client';

function parseMaybeJsonArray(value: unknown): any[] {
  if (Array.isArray(value)) return value;
  if (typeof value === 'string') {
    try {
      const parsed = JSON.parse(value);
      return Array.isArray(parsed) ? parsed : [];
    } catch {
      return [];
    }
  }
  return [];
}

function formatWindowSeconds(seconds: number): string {
  if (!Number.isFinite(seconds) || seconds <= 0) return '';
  if (seconds % 86400 === 0) return `${seconds / 86400}d`;
  if (seconds % 3600 === 0) return `${seconds / 3600}h`;
  if (seconds % 60 === 0) return `${seconds / 60}m`;
  return `${seconds}s`;
}

export function Issues() {
  const [filter, setFilter] = useState('');

  const { data: summary, isLoading: summaryLoading } = useQuery({
    queryKey: ['issuesSummary'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiIssuesService/GetSummary', { WindowSeconds: 24 * 3600 });
      if (res.data?.result && Array.isArray(res.data.result)) return res.data.result[0] as IssuesSummary;
      return res.data as IssuesSummary;
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const { data, isLoading, error, refetch, isFetching } = useQuery({
    queryKey: ['issuesList'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiIssuesService/GetIssues', { Limit: 500, SinceUnix: 0, TypesCsv: '' });
      if (res.data?.result && Array.isArray(res.data.result)) {
        const list = res.data.result[0];
        const issues = list?.Issues ? parseMaybeJsonArray(list.Issues) : [];
        return issues as Issue[];
      }
      return [] as Issue[];
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const issues = Array.isArray(data) ? data : [];

  const filtered = useMemo(() => {
    const q = filter.trim().toLowerCase();
    if (!q) return issues;
    return issues.filter((i) => {
      const hay = `${i.IssueType} ${i.Section} ${i.ReleaseName} ${i.SiteName} ${i.Reason} ${i.KbEvent}`.toLowerCase();
      return hay.includes(q);
    });
  }, [issues, filter]);

  const typeColor = (t: string) => {
    const u = (t || '').toUpperCase();
    if (u === 'SKIP') return 'orange';
    if (u === 'DONT_MATCH' || u === 'DONTMATCH') return 'red';
    if (u === 'MISSING_SECTION' || u === 'MISSING_SECTION_DIR') return 'yellow';
    if (u === 'NUKE') return 'grape';
    return 'gray';
  };

  return (
    <Stack>
      <Group justify="space-between" align="center">
        <Title order={2}>Issues</Title>
        <Button leftSection={<IconRefresh size="1rem" />} onClick={() => refetch()} loading={isFetching} variant="light">
          Refresh
        </Button>
      </Group>

      <Group>
        <Card withBorder radius="md" p="sm" style={{ flex: 1 }}>
          {summaryLoading || !summary ? (
            <Group justify="center"><Loader size="sm" /></Group>
          ) : (
            <Group justify="space-between">
              <Group gap="xs">
                <Badge color="gray" variant="light">Total: {summary.Total}</Badge>
                <Badge color="orange" variant="light">Skip: {summary.Skip}</Badge>
                <Badge color="red" variant="light">DontMatch: {summary.DontMatch}</Badge>
                <Badge color="yellow" variant="light">MissingSection: {summary.MissingSection}</Badge>
                <Badge color="grape" variant="light">Nuke: {summary.Nuke}</Badge>
              </Group>
              <Text size="xs" c="dimmed">Window: {formatWindowSeconds(summary.WindowSeconds)}</Text>
            </Group>
          )}
        </Card>

        <TextInput
          placeholder="Search (type/section/release/site/reason/event)..."
          leftSection={<IconSearch size="1rem" />}
          rightSection={
            <Tooltip
              label="Search matches Type, Section, ReleaseName, SiteName, Reason, and KbEvent (case-insensitive substring)."
              withArrow
              withinPortal
            >
              <IconAlertCircle size="1rem" style={{ opacity: 0.6 }} />
            </Tooltip>
          }
          value={filter}
          onChange={(e) => setFilter(e.currentTarget.value)}
          style={{ width: 340 }}
        />
      </Group>

      {error && (
        <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
          {error.message}
        </Alert>
      )}

      <Card withBorder radius="md" p={0}>
        {isLoading && !data ? (
          <Group justify="center" p="md"><Loader size="md" /></Group>
        ) : (
          <ScrollArea h="calc(100vh - 260px)">
            <Table striped highlightOnHover withTableBorder style={{ tableLayout: 'auto' }}>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Time</Table.Th>
                  <Table.Th style={{ width: 120 }}>Type</Table.Th>
                  <Table.Th>Section</Table.Th>
                  <Table.Th>Release</Table.Th>
                  <Table.Th>Site</Table.Th>
                  <Table.Th>Event</Table.Th>
                  <Table.Th>Reason</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {filtered.map((i) => (
                  <Table.Tr key={`${i.Id}`}>
                    <Table.Td>
                      <Text size="xs">
                        {i.TsUnix ? new Date(i.TsUnix * 1000).toLocaleString() : ''}
                      </Text>
                    </Table.Td>
                    <Table.Td>
                      <Badge color={typeColor(i.IssueType)} variant="light" size="sm" style={{ whiteSpace: 'nowrap' }}>
                        {i.IssueType}
                      </Badge>
                    </Table.Td>
                    <Table.Td><Text size="xs">{i.Section}</Text></Table.Td>
                    <Table.Td><Text size="xs">{i.ReleaseName}</Text></Table.Td>
                    <Table.Td><Text size="xs">{i.SiteName}</Text></Table.Td>
                    <Table.Td><Text size="xs" c="dimmed">{i.KbEvent}</Text></Table.Td>
                    <Table.Td><Text size="xs">{i.Reason}</Text></Table.Td>
                  </Table.Tr>
                ))}
                {filtered.length === 0 && (
                  <Table.Tr>
                    <Table.Td colSpan={7}>
                      <Text size="sm" c="dimmed" ta="center" p="md">
                        No issues found.
                      </Text>
                    </Table.Td>
                  </Table.Tr>
                )}
              </Table.Tbody>
            </Table>
          </ScrollArea>
        )}
      </Card>
    </Stack>
  );
}
