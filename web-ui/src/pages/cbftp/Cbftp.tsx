import { Tabs, Title, Container, Text } from '@mantine/core';
import { IconServer, IconFolders, IconTrophy, IconArrowsLeftRight, IconTerminal, IconInfoCircle } from '@tabler/icons-react';
import { useState } from 'react';
import { Sites } from './Sites';
import { Sections } from './Sections';
import { SpreadJobs } from './SpreadJobs';
import { TransferJobs } from './TransferJobs';
import { RawCommands } from './RawCommands';
import { Info } from './Info';

export function Cbftp() {
  const [activeTab, setActiveTab] = useState<string | null>('sites');

  return (
    <Container fluid>
      <Title order={2} mb="md">cbftp Integration</Title>
      <Text size="sm" c="dimmed" mb="md">
        Note: This integration does not expose all cbftp API features yet. More to come.
      </Text>
      
      <Tabs value={activeTab} onChange={setActiveTab}>
        <Tabs.List>
          <Tabs.Tab value="sites" leftSection={<IconServer size="0.8rem" />}>Sites</Tabs.Tab>
          <Tabs.Tab value="sections" leftSection={<IconFolders size="0.8rem" />}>Sections</Tabs.Tab>
          <Tabs.Tab value="spreadjobs" leftSection={<IconTrophy size="0.8rem" />}>Spread Jobs</Tabs.Tab>
          <Tabs.Tab value="transferjobs" leftSection={<IconArrowsLeftRight size="0.8rem" />}>Transfer Jobs</Tabs.Tab>
          <Tabs.Tab value="raw" leftSection={<IconTerminal size="0.8rem" />}>Raw Commands</Tabs.Tab>
          <Tabs.Tab value="info" leftSection={<IconInfoCircle size="0.8rem" />}>Info</Tabs.Tab>
        </Tabs.List>

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

        <Tabs.Panel value="info" pt="xs">
          <Info />
        </Tabs.Panel>
      </Tabs>
    </Container>
  );
}
