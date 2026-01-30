import { Alert, Button, Card, Code, Grid, Group, Loader, ScrollArea, Stack, Text, TextInput, Title } from '@mantine/core';
import { IconAlertCircle, IconRefresh } from '@tabler/icons-react';
import { useQuery } from '@tanstack/react-query';
import { useEffect, useState } from 'react';
import { apiClient } from '../api/client';

function parseApiResult<T>(response: any, fallback: T): T {
  let result = response?.data;
  if (response?.data?.result && Array.isArray(response.data.result)) {
    result = response.data.result[0];
  }
  return (result ?? fallback) as T;
}

export function Help() {
  const [search, setSearch] = useState('');
  const [selected, setSelected] = useState<string | null>(null);

  const { data: docs, isLoading, error, refetch, isFetching } = useQuery({
    queryKey: ['help-docs', search],
    queryFn: async () => {
      const query = search.trim();
      const endpoint = query ? '/ApiHelpService/SearchHelpDocs' : '/ApiHelpService/GetHelpDocs';
      const res = await apiClient.post(endpoint, query ? { Query: query } : {});
      const result = parseApiResult<any>(res, []);
      if (typeof result === 'string') {
        try {
          return JSON.parse(result) as string[];
        } catch {
          return [];
        }
      }
      return Array.isArray(result) ? result.map((name) => String(name)) : [];
    },
    refetchOnWindowFocus: false,
  });

  const { data: content, isLoading: isLoadingContent, error: contentError } = useQuery({
    queryKey: ['help-doc', selected],
    queryFn: async () => {
      const res = await apiClient.post('/ApiHelpService/GetHelpDocContent', { Name: selected });
      const result = parseApiResult<any>(res, '');
      return typeof result === 'string' ? result : String(result ?? '');
    },
    enabled: !!selected,
    refetchOnWindowFocus: false,
  });

  useEffect(() => {
    if (docs && docs.length > 0) {
      if (!selected || !docs.includes(selected)) {
        setSelected(docs[0]);
      }
    } else if (selected) {
      setSelected(null);
    }
  }, [docs, selected]);

  return (
    <Stack>
      <Group justify="space-between">
        <Title order={2}>Help</Title>
        <Button
          leftSection={<IconRefresh size="1rem" />}
          onClick={() => refetch()}
          loading={isFetching}
          variant="light"
        >
          Refresh
        </Button>
      </Group>

      <Group justify="space-between">
        <TextInput
          placeholder="Search docs..."
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
          style={{ width: 320 }}
        />
        <Text size="xs" c="dimmed">{docs?.length || 0} files</Text>
      </Group>

      {error && (
        <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
          {(error as any)?.message || 'Failed to load help documents'}
        </Alert>
      )}

      <Grid gutter="md">
        <Grid.Col span={{ base: 12, md: 2 }}>
          <Card
            withBorder
            radius="md"
            p="xs"
            style={{ height: 'calc(100vh - 220px)', maxWidth: 240, minWidth: 200 }}
          >
            {isLoading ? (
              <Group justify="center" p="md"><Loader size="md" /></Group>
            ) : (
              <ScrollArea h="100%">
                <Stack gap={4}>
                  {(docs || []).map((name) => (
                    <Button
                      key={name}
                      variant={name === selected ? 'light' : 'subtle'}
                      onClick={() => setSelected(name)}
                      fullWidth
                      size="xs"
                      style={{ justifyContent: 'flex-start', whiteSpace: 'nowrap' }}
                    >
                      {name}
                    </Button>
                  ))}
                  {(docs || []).length === 0 && (
                    <Text size="sm" c="dimmed" ta="center" p="md">No documents found.</Text>
                  )}
                </Stack>
              </ScrollArea>
            )}
          </Card>
        </Grid.Col>
        <Grid.Col span={{ base: 12, md: 9 }}>
          <Card withBorder radius="md" p="sm" style={{ height: 'calc(100vh - 220px)' }}>
            {contentError && (
              <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red" mb="sm">
                {(contentError as any)?.message || 'Failed to load document'}
              </Alert>
            )}
            {!selected && (
              <Text c="dimmed" ta="center" mt="xl">Select a document to view.</Text>
            )}
            {selected && (
              <Stack gap="xs" style={{ height: '100%' }}>
                <Title order={5}>{selected}</Title>
                <ScrollArea h="100%">
                  <Code block style={{ fontSize: '0.75rem' }}>
                    {isLoadingContent ? 'Loading...' : (content || 'No content.')}
                  </Code>
                </ScrollArea>
              </Stack>
            )}
          </Card>
        </Grid.Col>
      </Grid>
    </Stack>
  );
}
