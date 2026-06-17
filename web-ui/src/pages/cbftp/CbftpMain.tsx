import { useMemo } from 'react';
import { Alert, Badge, Group, Loader, ScrollArea, Stack, Table, Text, Tooltip } from '@mantine/core';
import { IconAlertCircle } from '@tabler/icons-react';
import { useQuery } from '@tanstack/react-query';
import { getCbftpMain } from '../../api/cbftpClient';
import type { CbftpMainJobEntry, CbftpMainSiteEntry } from '../../api/cbftpClient';

function formatDuration(seconds: number): string {
  if (seconds < 60) {
    return `${seconds}s`;
  }
  const mins = Math.floor(seconds / 60);
  const secs = seconds % 60;
  if (mins < 60) {
    return `${mins}m ${secs}s`;
  }
  const hrs = Math.floor(mins / 60);
  const remMins = mins % 60;
  return `${hrs}h ${remMins}m`;
}

function getPctColor(pct: number): string {
  if (pct >= 90) return 'green';
  if (pct >= 50) return 'yellow';
  return 'red';
}

function getStatusBadge(status: string) {
  switch (status?.toUpperCase()) {
    case 'RUNNING':
      return <Badge size="xs" color="blue">Running</Badge>;
    case 'DONE':
      return <Badge size="xs" color="green">Done</Badge>;
    case 'ABORTED':
      return <Badge size="xs" color="red">Aborted</Badge>;
    case 'PREPARED':
      return <Badge size="xs" color="yellow">Prepared</Badge>;
    default:
      return <Badge size="xs">{status || '-'}</Badge>;
  }
}

function getPriorityBadge(priority: string) {
  const p = priority?.toUpperCase() || '';
  const color = p.includes('VERY_HIGH')
    ? 'red'
    : p.includes('HIGH')
    ? 'orange'
    : p.includes('VERY_LOW')
    ? 'gray'
    : p.includes('LOW')
    ? 'teal'
    : 'blue';
  return <Badge size="xs" color={color}>{priority?.replace(/_/g, ' ') || '-'}</Badge>;
}

function JobsTable({ jobs }: { jobs: CbftpMainJobEntry[] }) {
  const sortedJobs = useMemo(() => {
    return [...jobs].reverse().slice(0, 3);
  }, [jobs]);

  return (
    <Stack gap="xs">
      <Group justify="space-between">
        <Text fw={500} size="sm">Spread Jobs</Text>
        <Text size="xs" c="dimmed">{jobs.length} total</Text>
      </Group>
      <ScrollArea>
        <Table striped highlightOnHover fz="xs" verticalSpacing={2} style={{ tableLayout: 'fixed', width: '100%' }}>
          <Table.Thead>
            <Table.Tr>
              <Table.Th style={{ width: '6%' }}>STARTED</Table.Th>
              <Table.Th style={{ width: '4%' }}>USE</Table.Th>
              <Table.Th style={{ width: '6%' }}>SECTION</Table.Th>
              <Table.Th style={{ width: '35%' }}>NAME</Table.Th>
              <Table.Th style={{ width: '5%' }}>SIZE</Table.Th>
              <Table.Th style={{ width: '3.5%' }}>WORST</Table.Th>
              <Table.Th style={{ width: '3.5%' }}>AVG</Table.Th>
              <Table.Th style={{ width: '3.5%' }}>BEST</Table.Th>
              <Table.Th style={{ width: '5.5%' }}>STATUS</Table.Th>
              <Table.Th style={{ width: '5%' }}>DONE</Table.Th>
              <Table.Th style={{ width: '23%' }}>SITES</Table.Th>
            </Table.Tr>
          </Table.Thead>
          <Table.Tbody>
            {sortedJobs.map((job) => (
              <Table.Tr key={job.name}>
                <Table.Td style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                  <Tooltip label={job.started} openDelay={400}>
                    <Text size="xs">{job.started?.substring(11, 16) || '-'}</Text>
                  </Tooltip>
                </Table.Td>
                <Table.Td>
                  <Text size="xs">{formatDuration(job.use_sec || 0)}</Text>
                </Table.Td>
                <Table.Td>
                  <Badge size="xs" variant="light">{job.section || '-'}</Badge>
                </Table.Td>
                <Table.Td style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                  <Tooltip label={job.name} openDelay={400}>
                    <Text size="xs">{job.name}</Text>
                  </Tooltip>
                </Table.Td>
                <Table.Td>
                  <Text size="xs">{job.size || '-'}</Text>
                </Table.Td>
                <Table.Td>
                  <Text size="xs" fw={500} c={getPctColor(job.worst_pct || 0)}>
                    {job.worst_pct ?? '-'}%
                  </Text>
                </Table.Td>
                <Table.Td>
                  <Text size="xs" fw={500} c={getPctColor(job.avg_pct || 0)}>
                    {job.avg_pct ?? '-'}%
                  </Text>
                </Table.Td>
                <Table.Td>
                  <Text size="xs" fw={500} c={getPctColor(job.best_pct || 0)}>
                    {job.best_pct ?? '-'}%
                  </Text>
                </Table.Td>
                <Table.Td>{getStatusBadge(job.status)}</Table.Td>
                <Table.Td>
                  <Text size="xs">{job.done || '-'}</Text>
                </Table.Td>
                <Table.Td style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                  <Tooltip label={job.sites} openDelay={400}>
                    <Text size="xs" c="dimmed">{job.sites || '-'}</Text>
                  </Tooltip>
                </Table.Td>
              </Table.Tr>
            ))}
            {sortedJobs.length === 0 && (
              <Table.Tr>
                <Table.Td colSpan={11}>
                  <Text c="dimmed" ta="center" size="sm">No active jobs</Text>
                </Table.Td>
              </Table.Tr>
            )}
          </Table.Tbody>
        </Table>
      </ScrollArea>
    </Stack>
  );
}

function StatusIndicator({ active }: { active: boolean }) {
  return (
    <Text
      size="xs"
      fw={700}
      c={active ? 'green' : 'dimmed'}
      style={{ fontFamily: 'monospace' }}
    >
      {active ? '[X]' : '[ ]'}
    </Text>
  );
}

function SitesTable({ sites }: { sites: CbftpMainSiteEntry[] }) {
  return (
    <Stack gap="xs">
      <Group justify="space-between">
        <Text fw={500} size="sm">Sites</Text>
        <Text size="xs" c="dimmed">{sites.length} total</Text>
      </Group>
      <ScrollArea>
        <Table striped highlightOnHover fz="xs" verticalSpacing={2} style={{ tableLayout: 'fixed', width: '100%' }}>
          <Table.Thead>
            <Table.Tr>
              <Table.Th style={{ width: '7%' }}>SITE</Table.Th>
              <Table.Th style={{ width: '7%' }}>LOGINS</Table.Th>
              <Table.Th style={{ width: '7%' }}>UPLOADS</Table.Th>
              <Table.Th style={{ width: '7%' }}>DOWNLOADS</Table.Th>
              <Table.Th style={{ width: '4%' }}>UP</Table.Th>
              <Table.Th style={{ width: '4%' }}>DOWN</Table.Th>
              <Table.Th style={{ width: '6%' }}>DISABLED</Table.Th>
              <Table.Th style={{ width: '9%' }}>UP 24HR</Table.Th>
              <Table.Th style={{ width: '9%' }}>DOWN 24HR</Table.Th>
              <Table.Th style={{ width: '5%' }}>RATIO 24H</Table.Th>
              <Table.Th style={{ width: '9%' }}>ALLUP</Table.Th>
              <Table.Th style={{ width: '9%' }}>ALLDOWN</Table.Th>
              <Table.Th style={{ width: '5%' }}>RATIO ALL</Table.Th>
              <Table.Th style={{ width: '8%' }}>PRIORITY</Table.Th>
            </Table.Tr>
          </Table.Thead>
          <Table.Tbody>
            {sites.map((site) => (
              <Table.Tr key={site.name}>
                <Table.Td>
                  <Text size="xs" fw={500}>{site.name}</Text>
                </Table.Td>
                <Table.Td>
                  <Text size="xs">
                    {site.logins_max > 0 ? `${site.logins_active}/${site.logins_max}` : '-'}
                  </Text>
                </Table.Td>
                <Table.Td>
                  <Text size="xs">
                    {site.uploads_max > 0 ? `${site.uploads_active}/${site.uploads_max}` : '-'}
                  </Text>
                </Table.Td>
                <Table.Td>
                  <Text size="xs">
                    {site.downloads_max > 0 ? `${site.downloads_active}/${site.downloads_max}` : '-'}
                  </Text>
                </Table.Td>
                <Table.Td><StatusIndicator active={site.up} /></Table.Td>
                <Table.Td><StatusIndicator active={site.down} /></Table.Td>
                <Table.Td>
                  <StatusIndicator active={site.disabled} />
                </Table.Td>
                <Table.Td>
                  <Text size="xs">{site.up24hr || '-'}</Text>
                </Table.Td>
                <Table.Td>
                  <Text size="xs">{site.down24hr || '-'}</Text>
                </Table.Td>
                <Table.Td>
                  <Text size="xs" fw={500}>{site.ratio24h || '-'}</Text>
                </Table.Td>
                <Table.Td>
                  <Text size="xs">{site.allup || '-'}</Text>
                </Table.Td>
                <Table.Td>
                  <Text size="xs">{site.alldown || '-'}</Text>
                </Table.Td>
                <Table.Td>
                  <Text size="xs" fw={500}>{site.ratioall || '-'}</Text>
                </Table.Td>
                <Table.Td>{getPriorityBadge(site.priority)}</Table.Td>
              </Table.Tr>
            ))}
            {sites.length === 0 && (
              <Table.Tr>
                <Table.Td colSpan={14}>
                  <Text c="dimmed" ta="center" size="sm">No sites</Text>
                </Table.Td>
              </Table.Tr>
            )}
          </Table.Tbody>
        </Table>
      </ScrollArea>
    </Stack>
  );
}

export function CbftpMain() {
  const { data, isLoading, error } = useQuery({
    queryKey: ['cbftp-main'],
    queryFn: getCbftpMain,
    refetchInterval: 5000,
  });

  if (isLoading) {
    return (
      <Stack align="center" py="xl">
        <Loader />
        <Text size="sm" c="dimmed">Loading cbftp main data...</Text>
      </Stack>
    );
  }

  if (error) {
    return (
      <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
        {error instanceof Error ? error.message : 'Failed to fetch cbftp main data'}
      </Alert>
    );
  }

  return (
    <Stack gap="md">
      <JobsTable jobs={data?.jobs || []} />
      <SitesTable sites={data?.sites || []} />
      {data?.updated && (
        <Text size="xs" c="dimmed" ta="right">
          Last updated: {data.updated}
        </Text>
      )}
    </Stack>
  );
}
