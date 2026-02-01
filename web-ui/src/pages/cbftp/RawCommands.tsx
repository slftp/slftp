import { useState, useEffect } from 'react';
import { Badge, Button, Group, Loader, Stack, Table, Textarea, Text, Select, MultiSelect, Switch, NumberInput, Paper, Code, ScrollArea, ActionIcon, Tooltip } from '@mantine/core';
import { useForm } from '@mantine/form';
import { useMutation, useQuery } from '@tanstack/react-query';
import { IconTerminal, IconRefresh, IconHistory } from '@tabler/icons-react';
import { notifications } from '@mantine/notifications';
import {
  sendRawCommand,
  getRawCommandResult,
  getSites,
  getSections,
} from '../../api/cbftpClient';
import type {
  RawCommandRequest,
  RawCommandResponse,
  RawCommandResult,
} from '../../api/cbftpClient';

interface CommandHistory {
  command: string;
  timestamp: number;
}

export function RawCommands() {
  const [results, setResults] = useState<RawCommandResult[]>([]);
  const [asyncRequestId, setAsyncRequestId] = useState<number | null>(null);
  const [commandHistory, setCommandHistory] = useState<CommandHistory[]>(() => {
    const saved = localStorage.getItem('cbftp-command-history');
    return saved ? JSON.parse(saved) : [];
  });

  const { data: siteNames } = useQuery<string[]>({
    queryKey: ['cbftp-sites-for-raw'],
    queryFn: () => getSites(),
  });

  const { data: sectionNames } = useQuery<string[]>({
    queryKey: ['cbftp-sections-for-raw'],
    queryFn: getSections,
  });

  // Poll for async results
  const { data: asyncResults } = useQuery<RawCommandResponse>({
    queryKey: ['cbftp-raw-result', asyncRequestId],
    queryFn: () => getRawCommandResult(asyncRequestId!),
    enabled: !!asyncRequestId,
    refetchInterval: 30000,
  });

  useEffect(() => {
    if (asyncResults?.results) {
      setResults(asyncResults.results);
      // Stop polling once we have results
      setAsyncRequestId(null);
    }
  }, [asyncResults]);

  const executeMutation = useMutation({
    mutationFn: sendRawCommand,
    onSuccess: (response, variables) => {
      if (response.id) {
        // Async command
        setAsyncRequestId(response.id);
        notifications.show({
          title: 'Command Sent',
          message: `Async command started (ID: ${response.id}). Polling for results...`,
          color: 'blue',
        });
      } else if (response.results) {
        // Sync command
        setResults(response.results);
        notifications.show({
          title: 'Success',
          message: 'Command executed successfully',
          color: 'green',
        });
      }

      // Save to history
      const newHistory = [
        { command: variables.command, timestamp: Date.now() },
        ...commandHistory.filter((h) => h.command !== variables.command).slice(0, 9),
      ];
      setCommandHistory(newHistory);
      localStorage.setItem('cbftp-command-history', JSON.stringify(newHistory));
    },
    onError: (error: Error) => {
      notifications.show({
        title: 'Error',
        message: error.message,
        color: 'red',
      });
    },
  });

  const form = useForm({
    initialValues: {
      command: '',
      siteSelection: 'specific' as 'specific' | 'with_sections' | 'all',
      sites: [] as string[],
      sites_with_sections: [] as string[],
      pathType: 'none' as 'none' | 'path' | 'section',
      path: '',
      path_section: '',
      timeout: 10,
      async: false,
    },
    validate: {
      command: (value) => (value.trim() ? null : 'Command is required'),
    },
  });

  const handleSubmit = (values: typeof form.values) => {
    const request: RawCommandRequest = {
      command: values.command,
      timeout: values.timeout,
      async: values.async,
    };

    if (values.siteSelection === 'specific' && values.sites.length > 0) {
      request.sites = values.sites;
    } else if (values.siteSelection === 'with_sections' && values.sites_with_sections.length > 0) {
      request.sites_with_sections = values.sites_with_sections;
    } else if (values.siteSelection === 'all') {
      request.sites_all = true;
    }

    if (values.pathType === 'path' && values.path) {
      request.path = values.path;
    } else if (values.pathType === 'section' && values.path_section) {
      request.path_section = values.path_section;
    }

    executeMutation.mutate(request);
  };

  const loadFromHistory = (command: string) => {
    form.setFieldValue('command', command);
  };

  const clearResults = () => {
    setResults([]);
    setAsyncRequestId(null);
  };

  return (
    <Stack gap="md">
      <form onSubmit={form.onSubmit(handleSubmit)}>
        <Stack gap="md">
          <Textarea
            label="Command"
            placeholder="e.g., SITE WHO"
            required
            rows={3}
            {...form.getInputProps('command')}
          />

          <Select
            label="Site Selection"
            data={[
              { value: 'specific', label: 'Specific Sites' },
              { value: 'with_sections', label: 'Sites with Sections' },
              { value: 'all', label: 'All Sites' },
            ]}
            {...form.getInputProps('siteSelection')}
          />

          {form.values.siteSelection === 'specific' && (
            <MultiSelect
              label="Sites"
              data={(siteNames || []) as any}
              searchable
              {...form.getInputProps('sites')}
            />
          )}

          {form.values.siteSelection === 'with_sections' && (
            <MultiSelect
              label="Sections"
              data={sectionNames || []}
              searchable
              {...form.getInputProps('sites_with_sections')}
            />
          )}

          <Select
            label="Path Options"
            data={[
              { value: 'none', label: 'None' },
              { value: 'path', label: 'Specific Path' },
              { value: 'section', label: 'Section' },
            ]}
            {...form.getInputProps('pathType')}
          />

          {form.values.pathType === 'path' && (
            <Textarea
              label="Path"
              placeholder="/path/to/directory"
              {...form.getInputProps('path')}
            />
          )}

          {form.values.pathType === 'section' && (
            <Select
              label="Section"
              data={sectionNames || []}
              searchable
              {...form.getInputProps('path_section')}
            />
          )}

          <Group grow>
            <NumberInput
              label="Timeout (seconds)"
              min={1}
              max={300}
              {...form.getInputProps('timeout')}
            />

            <Switch
              label="Async Mode"
              description="Run in background and poll for results"
              {...form.getInputProps('async', { type: 'checkbox' })}
              mt="xl"
            />
          </Group>

          <Group justify="space-between">
            <Group gap="xs">
              <Button
                type="submit"
                leftSection={<IconTerminal size={16} />}
                loading={executeMutation.isPending}
              >
                Execute
              </Button>
              {results.length > 0 && (
                <Button variant="default" onClick={clearResults}>
                  Clear Results
                </Button>
              )}
            </Group>

            {asyncRequestId && (
              <Group gap="xs">
                <Loader size="sm" />
                <Text size="sm">Polling for results (ID: {asyncRequestId})...</Text>
              </Group>
            )}
          </Group>
        </Stack>
      </form>

      {/* Command History */}
      {commandHistory.length > 0 && (
        <Paper p="md" withBorder>
          <Group mb="sm">
            <IconHistory size={20} />
            <Text fw={500}>Command History</Text>
          </Group>
          <ScrollArea h={120}>
            <Stack gap="xs">
              {commandHistory.map((item, index) => (
                <Group key={index} justify="space-between">
                  <Code style={{ flex: 1 }}>{item.command}</Code>
                  <Tooltip label="Load">
                    <ActionIcon size="sm" onClick={() => loadFromHistory(item.command)}>
                      <IconRefresh size={14} />
                    </ActionIcon>
                  </Tooltip>
                </Group>
              ))}
            </Stack>
          </ScrollArea>
        </Paper>
      )}

      {/* Results */}
      {results.length > 0 && (
        <Paper p="md" withBorder>
          <Text fw={500} mb="md">
            Results ({results.length} sites)
          </Text>
          <ScrollArea h={400}>
            <Table striped>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Site</Table.Th>
                  <Table.Th>Status</Table.Th>
                  <Table.Th>Output</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {results.map((result, index) => (
                  <Table.Tr key={index}>
                    <Table.Td>{result.site}</Table.Td>
                    <Table.Td>
                      <Badge color={result.success ? 'green' : 'red'}>
                        {result.success ? 'Success' : 'Failed'}
                      </Badge>
                    </Table.Td>
                    <Table.Td>
                      {result.error ? (
                        <Text c="red" size="sm">
                          {result.error}
                        </Text>
                      ) : (
                        <Code block style={{ whiteSpace: 'pre-wrap', maxWidth: 600 }}>
                          {result.output || 'No output'}
                        </Code>
                      )}
                    </Table.Td>
                  </Table.Tr>
                ))}
              </Table.Tbody>
            </Table>
          </ScrollArea>
        </Paper>
      )}
    </Stack>
  );
}
