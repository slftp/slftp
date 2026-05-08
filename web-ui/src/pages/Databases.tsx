import { useState } from 'react';
import { Stack, Tabs } from '@mantine/core';
import { IconMovie, IconDeviceTv } from '@tabler/icons-react';
import { Imdb } from './Imdb';
import { TV } from './TV';

export function Databases() {
  const [activeTab, setActiveTab] = useState<string | null>('imdb');

  return (
    <Stack gap="md">
      <Tabs value={activeTab} onChange={setActiveTab}>
        <Tabs.List>
          <Tabs.Tab value="imdb" leftSection={<IconMovie size={16} />}>
            IMDB
          </Tabs.Tab>
          <Tabs.Tab value="tv" leftSection={<IconDeviceTv size={16} />}>
            TV
          </Tabs.Tab>
        </Tabs.List>

        <Tabs.Panel value="imdb" pt="md">
          <Imdb />
        </Tabs.Panel>

        <Tabs.Panel value="tv" pt="md">
          <TV />
        </Tabs.Panel>
      </Tabs>
    </Stack>
  );
}
