import { Tabs, Title, Container, Text, Alert, Loader, Center } from '@mantine/core';
import { IconServer, IconFolders, IconTrophy, IconArrowsLeftRight, IconTerminal, IconInfoCircle, IconAlertCircle, IconUsers, IconLayoutDashboard, IconRoute } from '@tabler/icons-react';
import { useState, useCallback } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useSearchParams } from 'react-router-dom';
import { isCbftpEnabled } from '../../api/cbftpClient';
import { Sites } from './Sites';
import { Sections } from './Sections';
import { SpreadJobs } from './SpreadJobs';
import { TransferJobs } from './TransferJobs';
import { RawCommands } from './RawCommands';
import { Info } from './Info';
import { AffilSync } from './AffilSync';
import { CbftpMain } from './CbftpMain';
import { Routes } from './Routes';

const VALID_TABS = ['main', 'sites', 'sections', 'spreadjobs', 'transferjobs', 'raw', 'affilsync', 'routes', 'info'];

export function Cbftp() {
  const [searchParams, setSearchParams] = useSearchParams();
  const initialTab = VALID_TABS.includes(searchParams.get('tab') || '')
    ? searchParams.get('tab')
    : 'main';
  const [activeTab, setActiveTab] = useState<string | null>(initialTab);

  const handleTabChange = useCallback((value: string | null) => {
    setActiveTab(value);
    if (value && value !== 'main') {
      setSearchParams({ tab: value });
    } else {
      setSearchParams({});
    }
  }, [setSearchParams]);

  const { data: enabled, isLoading } = useQuery({
    queryKey: ['cbftp-enabled'],
    queryFn: isCbftpEnabled,
    staleTime: 5 * 60 * 1000,
    retry: false,
  });

  if (isLoading) {
    return (
      <Container fluid>
        <Center h={200}>
          <Loader />
        </Center>
      </Container>
    );
  }

  if (!enabled) {
    return (
      <Container fluid>
        <Title order={2} mb="md">cbftp Integration</Title>
        <Alert icon={<IconAlertCircle size="1rem" />} title="cbftp Disabled" color="yellow">
          cbftp integration is disabled. To enable it, set <code>EnableUDP=True</code> in the <code>[UDPConfig]</code> section of your slftp.ini and restart slftp.
        </Alert>
      </Container>
    );
  }

  return (
    <Container fluid>
      <Title order={2} mb="md">cbftp Integration</Title>
      <Text size="sm" c="dimmed" mb="md">
        Note: This integration does not expose all cbftp API features yet. More to come.
      </Text>
      
      <Tabs value={activeTab} onChange={handleTabChange}>
        <Tabs.List>
          <Tabs.Tab value="main" leftSection={<IconLayoutDashboard size="0.8rem" />}>Main</Tabs.Tab>
          <Tabs.Tab value="sites" leftSection={<IconServer size="0.8rem" />}>Sites</Tabs.Tab>
          <Tabs.Tab value="sections" leftSection={<IconFolders size="0.8rem" />}>Sections</Tabs.Tab>
          <Tabs.Tab value="spreadjobs" leftSection={<IconTrophy size="0.8rem" />}>Spread Jobs</Tabs.Tab>
          <Tabs.Tab value="transferjobs" leftSection={<IconArrowsLeftRight size="0.8rem" />}>Transfer Jobs</Tabs.Tab>
          <Tabs.Tab value="raw" leftSection={<IconTerminal size="0.8rem" />}>Raw Commands</Tabs.Tab>
          <Tabs.Tab value="affilsync" leftSection={<IconUsers size="0.8rem" />}>Affil Sync</Tabs.Tab>
          <Tabs.Tab value="routes" leftSection={<IconRoute size="0.8rem" />}>Routes</Tabs.Tab>
          <Tabs.Tab value="info" leftSection={<IconInfoCircle size="0.8rem" />}>Info</Tabs.Tab>
        </Tabs.List>

        <Tabs.Panel value="main" pt="xs">
          <CbftpMain />
        </Tabs.Panel>

        <Tabs.Panel value="sites" pt="xs">
          <Sites />
        </Tabs.Panel>

        <Tabs.Panel value="sections" pt="xs">
          <Sections />
        </Tabs.Panel>

        <Tabs.Panel value="spreadjobs" pt="xs">
          <SpreadJobs />
        </Tabs.Panel>

        <Tabs.Panel value="transferjobs" pt="xs">
          <TransferJobs />
        </Tabs.Panel>

        <Tabs.Panel value="raw" pt="xs">
          <RawCommands />
        </Tabs.Panel>

        <Tabs.Panel value="affilsync" pt="xs">
          <AffilSync />
        </Tabs.Panel>

        <Tabs.Panel value="routes" pt="xs">
          <Routes />
        </Tabs.Panel>

        <Tabs.Panel value="info" pt="xs">
          <Info />
        </Tabs.Panel>
      </Tabs>
    </Container>
  );
}
