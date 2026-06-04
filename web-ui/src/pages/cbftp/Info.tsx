import { Alert, Card, Grid, Group, Loader, Stack, Text, Title } from '@mantine/core';
import { useQuery } from '@tanstack/react-query';
import { IconAlertCircle, IconServer, IconChartBar } from '@tabler/icons-react';
import { getInfo, getSites } from '../../api/cbftpClient';
import type { CbftpInfo } from '../../api/cbftpClient';

export function Info() {
  const { data, isLoading, error } = useQuery<CbftpInfo>({
    queryKey: ['cbftp-info'],
    queryFn: getInfo,
    refetchInterval: 30000,
  });

  const { data: siteNames } = useQuery<string[]>({
    queryKey: ['cbftp-sites-summary'],
    queryFn: () => getSites(),
    refetchInterval: 30000,
  });

  if (isLoading) {
    return <Loader />;
  }

  if (error) {
    return (
      <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
        {error instanceof Error ? error.message : 'Failed to fetch cbftp info'}
      </Alert>
    );
  }

  const formatUptime = (seconds?: number): string => {
    if (!seconds) return 'N/A';
    const days = Math.floor(seconds / 86400);
    const hours = Math.floor((seconds % 86400) / 3600);
    const minutes = Math.floor((seconds % 3600) / 60);
    return `${days}d ${hours}h ${minutes}m`;
  };

  return (
    <Grid>
      <Grid.Col span={{ base: 12, md: 6 }}>
        <Card shadow="sm" padding="lg" radius="md" withBorder>
          <Group mb="md">
            <IconServer size={24} />
            <Title order={4}>Build Information</Title>
          </Group>
          <Stack gap="sm">
            <Group justify="apart">
              <Text fw={500}>Version:</Text>
              <Text c="dimmed">{data?.version || 'N/A'}</Text>
            </Group>
            <Group justify="apart">
              <Text fw={500}>Build Date:</Text>
              <Text c="dimmed">{data?.build_date || 'N/A'}</Text>
            </Group>
            <Group justify="apart">
              <Text fw={500}>Uptime:</Text>
              <Text c="dimmed">{formatUptime(data?.uptime)}</Text>
            </Group>
          </Stack>
        </Card>
      </Grid.Col>

      <Grid.Col span={{ base: 12, md: 6 }}>
        <Card shadow="sm" padding="lg" radius="md" withBorder>
          <Group mb="md">
            <IconChartBar size={24} />
            <Title order={4}>Statistics</Title>
          </Group>
          <Stack gap="sm">
            <Group justify="apart">
              <Text fw={500}>Total Sites:</Text>
              <Text c="dimmed">{siteNames?.length ?? data?.num_sites ?? 0}</Text>
            </Group>
            <Group justify="apart">
              <Text fw={500}>Spread Jobs:</Text>
              <Text c="dimmed">
                {data?.active_spread_jobs ?? 0} / {data?.num_spread_jobs ?? 0}
              </Text>
            </Group>
            <Group justify="apart">
              <Text fw={500}>Transfer Jobs:</Text>
              <Text c="dimmed">
                {data?.active_transfer_jobs ?? 0} / {data?.num_transfer_jobs ?? 0}
              </Text>
            </Group>
          </Stack>
        </Card>
      </Grid.Col>
    </Grid>
  );
}
