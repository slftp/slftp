import { Card, Tabs } from '@mantine/core';
import { IconFolders, IconSettings } from '@tabler/icons-react';
import { useState } from 'react';
import { SectionDirectories } from '../components/SectionDirectories';
import { PrecatcherConfigEditor } from '../components/PrecatcherConfigEditor';

export function Sections() {
  const [activeTab, setActiveTab] = useState<string>('directories');

  return (
    <Card shadow="sm" padding="lg" radius="md" withBorder>
      <Tabs value={activeTab} onChange={(value) => setActiveTab(value || 'directories')}>
        <Tabs.List>
          <Tabs.Tab value="directories" leftSection={<IconFolders size="1rem" />}>
            Directories
          </Tabs.Tab>
          <Tabs.Tab value="config" leftSection={<IconSettings size="1rem" />}>
            Precatcher Config
          </Tabs.Tab>
        </Tabs.List>

        <Tabs.Panel value="directories" pt="md">
          <SectionDirectories />
        </Tabs.Panel>

        <Tabs.Panel value="config" pt="md">
          <PrecatcherConfigEditor />
        </Tabs.Panel>
      </Tabs>
    </Card>
  );
}
