import { useEffect, useMemo, useState } from 'react';
import {
  ActionIcon,
  Badge,
  Box,
  Button,
  Center,
  Code,
  Collapse,
  CopyButton,
  Divider,
  Group,
  Loader,
  Modal,
  MultiSelect,
  NumberInput,
  Paper,
  ScrollArea,
  SegmentedControl,
  Select,
  SimpleGrid,
  Stack,
  Switch,
  Table,
  Text,
  TextInput,
  Textarea,
  Tooltip,
} from '@mantine/core';
import { useForm } from '@mantine/form';
import { useQuery } from '@tanstack/react-query';
import {
  IconAlertCircle,
  IconChartBar,
  IconCheck,
  IconCheck as IconCheckMini,
  IconChevronDown,
  IconChevronRight,
  IconCopy,
  IconDeviceFloppy,
  IconFileText,
  IconHighlight,
  IconHistory,
  IconPlayerPlay,
  IconRefresh,
  IconTerminal,
  IconTrash,
  IconX,
} from '@tabler/icons-react';
import { notifications } from '@mantine/notifications';
import { useDisclosure } from '@mantine/hooks';
import {
  getRawCommandResult,
  getSections,
  getSites,
  sendRawCommand,
} from '../../api/cbftpClient';
import type {
  RawCommandRequest,
  RawCommandResponse,
  RawCommandResult,
} from '../../api/cbftpClient';

type SiteSelectionMode = 'specific' | 'with_sections' | 'all';
type PathType = 'none' | 'path' | 'section';
type ResultViewMode = 'raw' | 'table';

interface CommandHistoryEntry {
  command: string;
}

interface CommandPreset {
  id: string;
  label: string;
  command: string;
  sites: string[];
  sites_with_sections: string[];
  siteSelection: SiteSelectionMode;
  pathType: PathType;
  path: string;
  path_section: string;
  timeout: number;
  async: boolean;
}

interface MultiCommandResult {
  command: string;
  successes: RawCommandResult[];
  failures: RawCommandResult[];
}

interface ParsedLine {
  original: string;
  isStats: boolean;
  rank?: string;
  user?: string;
  tagline?: string;
  files?: string;
  size?: string;
  speed?: string;
}

const STORAGE_HISTORY_KEY = 'cbftp-command-history';
const STORAGE_PRESETS_KEY = 'cbftp-command-presets';
const STORAGE_HIGHLIGHT_KEY = 'cbftp-raw-highlight';

function getErrorMessage(error: unknown): string {
  if (error instanceof Error) return error.message;
  if (typeof error === 'string') return error;
  return 'Unknown error';
}

function loadHistory(): CommandHistoryEntry[] {
  const saved = localStorage.getItem(STORAGE_HISTORY_KEY);
  if (!saved) return [];

  try {
    const parsed: unknown = JSON.parse(saved);
    if (!Array.isArray(parsed)) return [];
    return parsed
      .map((item) => {
        if (typeof item === 'string') return { command: item };
        if (typeof item === 'object' && item !== null && 'command' in item) {
          const command = (item as { command?: unknown }).command;
          if (typeof command === 'string' && command.trim() !== '') return { command };
        }
        return null;
      })
      .filter((item): item is CommandHistoryEntry => item !== null)
      .slice(0, 10);
  } catch {
    return [];
  }
}

function loadPresets(): CommandPreset[] {
  const saved = localStorage.getItem(STORAGE_PRESETS_KEY);
  if (!saved) return [];

  try {
    const parsed: unknown = JSON.parse(saved);
    if (!Array.isArray(parsed)) return [];

    return parsed
      .map((item) => {
        if (typeof item !== 'object' || item === null) return null;
        const obj = item as Record<string, unknown>;
        if (typeof obj.id !== 'string' || typeof obj.label !== 'string' || typeof obj.command !== 'string') return null;

        const siteSelection: SiteSelectionMode =
          obj.siteSelection === 'with_sections' || obj.siteSelection === 'all' ? obj.siteSelection : 'specific';
        const pathType: PathType =
          obj.pathType === 'path' || obj.pathType === 'section' ? obj.pathType : 'none';

        return {
          id: obj.id,
          label: obj.label,
          command: obj.command,
          sites: Array.isArray(obj.sites) ? obj.sites.filter((v): v is string => typeof v === 'string') : [],
          sites_with_sections: Array.isArray(obj.sites_with_sections)
            ? obj.sites_with_sections.filter((v): v is string => typeof v === 'string')
            : [],
          siteSelection,
          pathType,
          path: typeof obj.path === 'string' ? obj.path : '',
          path_section: typeof obj.path_section === 'string' ? obj.path_section : '',
          timeout: typeof obj.timeout === 'number' && Number.isFinite(obj.timeout) ? obj.timeout : 30,
          async: obj.async === true,
        } as CommandPreset;
      })
      .filter((item): item is CommandPreset => item !== null);
  } catch {
    return [];
  }
}

function normalizeResults(response: RawCommandResponse): { successes: RawCommandResult[]; failures: RawCommandResult[] } {
  const successes = [...(response.successes || [])].sort((a, b) => a.name.localeCompare(b.name));
  const failures = [...(response.failures || [])].sort((a, b) => a.name.localeCompare(b.name));
  return { successes, failures };
}

function parseOutput(text: string): ParsedLine[] {
  if (!text) return [];
  const lines = text.split('\n');
  const statsPattern =
    /^(\s*200-?\s*)(\[\d+\])\s+(\S+)\s+(.+?)\s+(\d[\d,]*)\s+([\d,]+[.,]?\d*\s*\w+)\s+([\d,]+[.,]?\d*\s*\w+\/s)\s*$/;

  return lines.map((line) => {
    const match = line.match(statsPattern);
    if (!match) return { original: line, isStats: false };

    const [, , rank, user, tagline, files, size, speed] = match;
    return {
      original: line,
      isStats: true,
      rank,
      user,
      tagline: tagline.trim(),
      files,
      size,
      speed,
    };
  });
}

function formatSizeToGB(sizeStr: string | undefined): string {
  if (!sizeStr) return '';
  const numericPart = sizeStr.replace(/,/g, '').match(/[\d.]+/);
  if (!numericPart) return sizeStr;

  const value = parseFloat(numericPart[0]);
  const lower = sizeStr.toLowerCase();
  if (lower.includes('m')) return `${Math.round(value / 1024)}GB`;
  if (lower.includes('g')) return `${Math.round(value)}GB`;

  return sizeStr;
}

function HighlightText({ text, highlight }: { text: string; highlight: string }) {
  if (!highlight || !text) return <>{text}</>;
  const terms = highlight
    .split(',')
    .map((t) => t.trim())
    .filter(Boolean);
  if (terms.length === 0) return <>{text}</>;

  const hasMatch = terms.some((term) => text.includes(term));
  if (!hasMatch) return <>{text}</>;

  return (
    <Text span fw={800}>
      {text}
    </Text>
  );
}

function OutputTable({ data, highlight }: { data: ParsedLine[]; highlight: string }) {
  const statsRows = data.filter((line) => line.isStats);
  const highlightTerms = highlight
    .split(',')
    .map((t) => t.trim())
    .filter(Boolean);

  if (statsRows.length === 0) {
    return (
      <Center p="md">
        <Text c="dimmed" size="sm" fs="italic">
          No stats data found. Switch to "Raw Output" to view full log.
        </Text>
      </Center>
    );
  }

  return (
    <Table withTableBorder withColumnBorders layout="fixed" verticalSpacing="xs">
      <Table.Thead>
        <Table.Tr>
          <Table.Th w={45} style={{ textAlign: 'center', padding: '8px 4px' }}>
            #
          </Table.Th>
          <Table.Th style={{ padding: '8px 8px' }}>User</Table.Th>
          <Table.Th w={85} style={{ textAlign: 'right', padding: '8px 8px' }}>
            Size
          </Table.Th>
        </Table.Tr>
      </Table.Thead>
      <Table.Tbody>
        {statsRows.map((row, idx) => {
          const displaySize = formatSizeToGB(row.size);
          const isRowMatch =
            highlightTerms.length > 0 &&
            highlightTerms.some(
              (term) => (row.user || '').includes(term) || (row.rank || '').includes(term) || displaySize.includes(term),
            );

          return (
            <Table.Tr key={idx} bg={isRowMatch ? 'rgba(34, 139, 230, 0.15)' : undefined}>
              <Table.Td style={{ fontFamily: 'monospace', textAlign: 'center', padding: '8px 4px' }}>
                <HighlightText text={row.rank?.replace(/\[|\]/g, '') || ''} highlight={highlight} />
              </Table.Td>
              <Table.Td fw={600} style={{ padding: '8px 8px', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                <HighlightText text={row.user || ''} highlight={highlight} />
              </Table.Td>
              <Table.Td style={{ textAlign: 'right', fontFamily: 'monospace', padding: '8px 8px', fontWeight: 700 }}>
                <HighlightText text={displaySize} highlight={highlight} />
              </Table.Td>
            </Table.Tr>
          );
        })}
      </Table.Tbody>
    </Table>
  );
}

function RawOutput({ text, highlight }: { text: string; highlight: string }) {
  if (!text) return <Text c="dimmed" size="xs" fs="italic">No output</Text>;

  const lines = text.split('\n');
  const highlightTerms = highlight
    .split(',')
    .map((term) => term.trim())
    .filter(Boolean);

  return (
    <Code block style={{ fontSize: '11px', backgroundColor: 'transparent', padding: 0, lineHeight: 1.5 }}>
      {lines.map((line, i) => {
        const isMatch = highlightTerms.length > 0 && highlightTerms.some((term) => line.includes(term));
        return (
          <div
            key={i}
            style={{
              backgroundColor: isMatch ? 'rgba(34, 139, 230, 0.15)' : 'transparent',
              color: isMatch ? 'var(--mantine-color-blue-1)' : 'inherit',
              fontWeight: isMatch ? 600 : 'inherit',
              padding: '0 4px',
              borderLeft: isMatch ? '3px solid var(--mantine-color-blue-5)' : '3px solid transparent',
              whiteSpace: 'pre-wrap',
            }}
          >
            {line}
          </div>
        );
      })}
    </Code>
  );
}

export function RawCommands() {
  const [results, setResults] = useState<MultiCommandResult[]>([]);
  const [isExecuting, setIsExecuting] = useState(false);
  const [asyncRequestId, setAsyncRequestId] = useState<number | null>(null);
  const [asyncCommand, setAsyncCommand] = useState('');
  const [resultViewMode, setResultViewMode] = useState<ResultViewMode>('raw');

  const [highlightText, setHighlightText] = useState(() => localStorage.getItem(STORAGE_HIGHLIGHT_KEY) || '');
  const [commandHistory, setCommandHistory] = useState<CommandHistoryEntry[]>(() => loadHistory());
  const [presets, setPresets] = useState<CommandPreset[]>(() => loadPresets());

  const [savePresetOpened, setSavePresetOpened] = useState(false);
  const [presetName, setPresetName] = useState('');
  const [historyOpened, { toggle: toggleHistory }] = useDisclosure(false);

  useEffect(() => {
    localStorage.setItem(STORAGE_HIGHLIGHT_KEY, highlightText);
  }, [highlightText]);

  const { data: siteNames } = useQuery<string[]>({
    queryKey: ['cbftp-sites-for-raw'],
    queryFn: () => getSites(),
  });

  const { data: sectionNames } = useQuery<string[]>({
    queryKey: ['cbftp-sections-for-raw'],
    queryFn: () => getSections(),
  });

  const { data: asyncResults, error: asyncError } = useQuery<RawCommandResponse>({
    queryKey: ['cbftp-raw-result', asyncRequestId],
    queryFn: () => getRawCommandResult(asyncRequestId!),
    enabled: !!asyncRequestId,
    refetchInterval: (query) => {
      if (!asyncRequestId) return false;
      const data = query.state.data;
      if (data && (Array.isArray(data.successes) || Array.isArray(data.failures))) return false;
      return 3000;
    },
  });

  const asyncDone = !!asyncRequestId && !!asyncResults && (Array.isArray(asyncResults.successes) || Array.isArray(asyncResults.failures));
  const isPollingAsync = !!asyncRequestId && !asyncDone && !asyncError;

  const asyncResultSet = useMemo<MultiCommandResult[]>(() => {
    if (!asyncDone || !asyncResults) return [];
    const normalized = normalizeResults(asyncResults);
    return [
      {
        command: asyncCommand ? `${asyncCommand} (async)` : 'Async Result',
        successes: normalized.successes,
        failures: normalized.failures,
      },
    ];
  }, [asyncDone, asyncResults, asyncCommand]);

  const activeResults = asyncResultSet.length > 0 ? asyncResultSet : results;

  const form = useForm({
    initialValues: {
      command: '',
      siteSelection: 'specific' as SiteSelectionMode,
      sites: [] as string[],
      sites_with_sections: [] as string[],
      pathType: 'none' as PathType,
      path: '',
      path_section: '',
      timeout: 30,
      async: false,
    },
    validate: {
      command: (value) => (value.trim() ? null : 'Command is required'),
    },
  });

  const updateHistory = (commandInput: string) => {
    const entry = commandInput.trim();
    if (!entry) return;

    const newHistory = [{ command: entry }, ...commandHistory.filter((h) => h.command !== entry)].slice(0, 10);
    setCommandHistory(newHistory);
    localStorage.setItem(STORAGE_HISTORY_KEY, JSON.stringify(newHistory));
  };

  const buildBaseRequest = (values: typeof form.values): Omit<RawCommandRequest, 'command'> => {
    const request: Omit<RawCommandRequest, 'command'> = {
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

    return request;
  };

  const handleSubmit = async (values: typeof form.values) => {
    setIsExecuting(true);
    setResultViewMode('raw');
    setResults([]);
    setAsyncRequestId(null);
    setAsyncCommand('');

    const commands = values.command
      .split(',')
      .map((c) => c.trim())
      .filter(Boolean);

    if (commands.length === 0) {
      setIsExecuting(false);
      return;
    }

    updateHistory(values.command);

    const baseRequest = buildBaseRequest(values);

    if (values.async) {
      const asyncCommandInput = commands[0];
      if (commands.length > 1) {
        notifications.show({
          title: 'Warning',
          message: 'Async mode supports one command. Only the first command will run.',
          color: 'orange',
        });
      }

      try {
        const response = await sendRawCommand({ ...baseRequest, command: asyncCommandInput });
        if (response.id) {
          setAsyncRequestId(response.id);
          setAsyncCommand(asyncCommandInput);
          notifications.show({
            title: 'Async Started',
            message: `Command started (ID: ${response.id}). Polling for results...`,
            color: 'blue',
          });
        } else {
          const normalized = normalizeResults(response);
          setResults([
            {
              command: asyncCommandInput,
              successes: normalized.successes,
              failures: normalized.failures,
            },
          ]);
          notifications.show({ title: 'Done', message: 'Command finished immediately.', color: 'green' });
        }
      } catch (error: unknown) {
        const message = getErrorMessage(error);
        setResults([
          {
            command: asyncCommandInput,
            successes: [],
            failures: [{ name: 'System', error: message }],
          },
        ]);
        notifications.show({ title: 'Error', message, color: 'red' });
      } finally {
        setIsExecuting(false);
      }

      return;
    }

    const settledResults = await Promise.all(
      commands.map(async (command): Promise<MultiCommandResult> => {
        try {
          const response = await sendRawCommand({ ...baseRequest, command });
          if (response.id) {
            return {
              command,
              successes: [],
              failures: [{ name: 'System', reason: `Unexpected async response (id=${response.id}) in sync mode` }],
            };
          }

          const normalized = normalizeResults(response);
          return {
            command,
            successes: normalized.successes,
            failures: normalized.failures,
          };
        } catch (error: unknown) {
          return {
            command,
            successes: [],
            failures: [{ name: 'System', error: getErrorMessage(error) }],
          };
        }
      }),
    );

    setResults(settledResults);
    setIsExecuting(false);

    const hasFailures = settledResults.some((item) => item.failures.length > 0);
    notifications.show({
      title: hasFailures ? 'Completed with Errors' : 'Success',
      message: `${settledResults.length} command${settledResults.length === 1 ? '' : 's'} executed`,
      color: hasFailures ? 'orange' : 'green',
    });
  };

  const handleSavePreset = () => {
    const label = presetName.trim();
    if (!label) return;

    const newPreset: CommandPreset = {
      id: `${label}-${Math.random().toString(36).slice(2, 9)}`,
      label,
      command: form.values.command,
      sites: form.values.sites,
      sites_with_sections: form.values.sites_with_sections,
      siteSelection: form.values.siteSelection,
      pathType: form.values.pathType,
      path: form.values.path,
      path_section: form.values.path_section,
      timeout: form.values.timeout,
      async: form.values.async,
    };

    const updatedPresets = [...presets, newPreset];
    setPresets(updatedPresets);
    localStorage.setItem(STORAGE_PRESETS_KEY, JSON.stringify(updatedPresets));
    setSavePresetOpened(false);
    setPresetName('');

    notifications.show({ title: 'Saved', message: 'Preset saved successfully', color: 'green' });
  };

  const loadPreset = (preset: CommandPreset) => {
    form.setValues({
      command: preset.command,
      sites: preset.sites,
      sites_with_sections: preset.sites_with_sections,
      siteSelection: preset.siteSelection,
      pathType: preset.pathType,
      path: preset.path,
      path_section: preset.path_section,
      timeout: preset.timeout,
      async: preset.async,
    });

    notifications.show({ title: 'Loaded', message: `Preset "${preset.label}" applied`, color: 'blue' });
  };

  const deletePreset = (id: string) => {
    const updated = presets.filter((preset) => preset.id !== id);
    setPresets(updated);
    localStorage.setItem(STORAGE_PRESETS_KEY, JSON.stringify(updated));
  };

  const clearResults = () => {
    setResults([]);
    setAsyncRequestId(null);
    setAsyncCommand('');
  };

  const unifiedData = useMemo(() => {
    if (activeResults.length === 0) return null;

    const allSites = new Set<string>();
    activeResults.forEach((entry) => {
      entry.successes.forEach((item) => allSites.add(item.name));
      entry.failures.forEach((item) => allSites.add(item.name));
    });

    const sites = Array.from(allSites).sort();

    return {
      commands: activeResults.map((entry) => entry.command),
      rows: sites.map((site) => ({
        site,
        results: activeResults.map((entry) => {
          const success = entry.successes.find((item) => item.name === site);
          if (success) return { type: 'success' as const, content: success.result || '' };

          const failure = entry.failures.find((item) => item.name === site);
          if (failure) return { type: 'failure' as const, content: failure.reason || failure.error || '' };

          return { type: 'missing' as const, content: '' };
        }),
      })),
    };
  }, [activeResults]);

  return (
    <Stack gap="md">
      <Paper p="sm" withBorder radius="md">
        <Group justify="space-between" mb={presets.length > 0 ? 'xs' : 10}>
          <Text fw={600} size="sm" c="dimmed">
            Command Presets
          </Text>
          <Button
            variant="light"
            size="xs"
            color="blue"
            leftSection={<IconDeviceFloppy size={14} />}
            onClick={() => setSavePresetOpened(true)}
            disabled={!form.values.command.trim()}
          >
            Save Preset
          </Button>
        </Group>

        {presets.length > 0 ? (
          <Group gap="xs">
            {presets.map((preset) => (
              <Group
                key={preset.id}
                gap={0}
                style={{ border: '1px solid var(--mantine-color-blue-9)', borderRadius: 'var(--mantine-radius-sm)', overflow: 'hidden' }}
              >
                <Button variant="light" size="xs" color="blue" onClick={() => loadPreset(preset)} style={{ borderRadius: 0 }}>
                  {preset.label}
                </Button>
                <Tooltip label="Delete">
                  <ActionIcon
                    variant="light"
                    color="red"
                    size="xs"
                    style={{ borderRadius: 0, height: 30, width: 24 }}
                    onClick={() => deletePreset(preset.id)}
                  >
                    <IconTrash size={12} />
                  </ActionIcon>
                </Tooltip>
              </Group>
            ))}
          </Group>
        ) : (
          <Text size="xs" c="dimmed">No presets saved yet.</Text>
        )}
      </Paper>

      <form onSubmit={form.onSubmit(handleSubmit)}>
        <Stack gap="md">
          <Textarea
            label="Command(s)"
            placeholder="e.g., SITE WHO, SITE ALOG"
            description="Separate multiple commands with a comma to run them together."
            required
            rows={2}
            {...form.getInputProps('command')}
            style={{ fontFamily: 'monospace' }}
          />

          <Group grow align="flex-start">
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
                data={siteNames || []}
                searchable
                placeholder="Select sites..."
                {...form.getInputProps('sites')}
              />
            )}

            {form.values.siteSelection === 'with_sections' && (
              <MultiSelect
                label="Sections"
                data={sectionNames || []}
                searchable
                placeholder="Select sections..."
                {...form.getInputProps('sites_with_sections')}
              />
            )}
          </Group>

          <Group align="flex-end">
            <Select
              label="Path Options"
              data={[
                { value: 'none', label: 'None' },
                { value: 'path', label: 'Specific Path' },
                { value: 'section', label: 'Section' },
              ]}
              {...form.getInputProps('pathType')}
              style={{ width: '100%', maxWidth: 220 }}
            />

            {form.values.pathType === 'path' && (
              <TextInput label="Path" placeholder="/path/to/directory" {...form.getInputProps('path')} style={{ flex: 1 }} />
            )}

            {form.values.pathType === 'section' && (
              <Select
                label="Section"
                data={sectionNames || []}
                searchable
                {...form.getInputProps('path_section')}
                style={{ flex: 1 }}
              />
            )}

            <NumberInput label="Timeout" min={1} max={300} {...form.getInputProps('timeout')} style={{ width: 90 }} />
            <Switch label="Async" {...form.getInputProps('async', { type: 'checkbox' })} />
          </Group>

          <Group justify="space-between" mt="xs">
            <Group gap="xs">
              <Button type="submit" leftSection={<IconPlayerPlay size={16} />} loading={isExecuting}>
                Execute
              </Button>
              {(activeResults.length > 0 || asyncRequestId) && (
                <Button variant="default" onClick={clearResults} leftSection={<IconX size={14} />}>
                  Clear Results
                </Button>
              )}
            </Group>

            {isPollingAsync && (
              <Group gap="xs">
                <Loader size="sm" />
                <Text size="sm">Polling (ID: {asyncRequestId})...</Text>
              </Group>
            )}
          </Group>
        </Stack>
      </form>

      {asyncError && (
        <Paper withBorder p="sm" style={{ borderColor: 'var(--mantine-color-red-6)' }}>
          <Group gap="xs">
            <IconAlertCircle size={16} color="var(--mantine-color-red-6)" />
            <Text size="sm" c="red">
              Failed to poll async result: {getErrorMessage(asyncError)}
            </Text>
          </Group>
        </Paper>
      )}

      {unifiedData && (
        <Stack gap="xs">
          <Divider label="Output" labelPosition="center" />

          <Group justify="space-between" align="center" wrap="wrap">
            <Group gap="xs">
              <Badge size="sm" variant="light" color="gray">
                {unifiedData.rows.length} {unifiedData.rows.length === 1 ? 'site' : 'sites'}
              </Badge>
              <Badge size="sm" variant="light" color="blue">
                {unifiedData.commands.length} {unifiedData.commands.length === 1 ? 'command' : 'commands'}
              </Badge>
            </Group>

            <Group>
              <SegmentedControl
                size="xs"
                value={resultViewMode}
                onChange={(val) => setResultViewMode(val === 'table' ? 'table' : 'raw')}
                data={[
                  {
                    label: (
                      <Center style={{ gap: 6 }}>
                        <IconFileText size={14} />
                        <span>Raw Output</span>
                      </Center>
                    ),
                    value: 'raw',
                  },
                  {
                    label: (
                      <Center style={{ gap: 6 }}>
                        <IconChartBar size={14} />
                        <span>Stats</span>
                      </Center>
                    ),
                    value: 'table',
                  },
                ]}
              />

              <TextInput
                placeholder="Highlight term..."
                leftSection={<IconHighlight size={14} />}
                size="xs"
                value={highlightText}
                onChange={(e) => setHighlightText(e.currentTarget.value)}
                style={{ width: 200 }}
                rightSection={
                  highlightText ? (
                    <ActionIcon size="xs" variant="transparent" onClick={() => setHighlightText('')}>
                      <IconX size={12} />
                    </ActionIcon>
                  ) : undefined
                }
              />
            </Group>
          </Group>

          <ScrollArea.Autosize mah="calc(100vh - 300px)" type="auto" offsetScrollbars>
            <Stack gap="md">
              {unifiedData.rows.map((row) => (
                <Paper key={`site-${row.site}`} p="sm" withBorder radius="md">
                  <Group gap="xs" mb="sm">
                    <Badge size="md" variant="light" color="blue" radius="sm" style={{ fontWeight: 600 }}>
                      {row.site}
                    </Badge>
                    {row.results.some((result) => result.type === 'failure') && (
                      <Badge size="xs" color="red" variant="dot">errors</Badge>
                    )}
                    {row.results.every((result) => result.type === 'success') && (
                      <Badge size="xs" color="green" variant="dot">ok</Badge>
                    )}
                  </Group>

                  <SimpleGrid cols={{ base: 1, md: Math.min(unifiedData.commands.length, 2) }} spacing="sm">
                    {row.results.map((result, idx) => (
                      <Box key={`${row.site}-${idx}`}>
                        <Group gap={4} mb={6} justify="space-between">
                          <Group gap={4}>
                            <IconTerminal size={12} style={{ opacity: 0.7 }} />
                            <Text size="xs" fw={600} c="dimmed" style={{ fontFamily: 'monospace' }}>
                              {unifiedData.commands[idx]}
                            </Text>
                            {result.type === 'success' && <IconCheck size={14} color="var(--mantine-color-green-6)" stroke={2.5} />}
                            {result.type === 'failure' && <IconAlertCircle size={14} color="var(--mantine-color-red-6)" stroke={2.5} />}
                          </Group>

                          {result.content && (
                            <CopyButton value={result.content} timeout={2000}>
                              {({ copied, copy }) => (
                                <Tooltip label={copied ? 'Copied' : 'Copy output'} withArrow position="left">
                                  <ActionIcon color={copied ? 'teal' : 'gray'} variant="subtle" onClick={copy} size="xs">
                                    {copied ? <IconCheckMini size={12} /> : <IconCopy size={12} />}
                                  </ActionIcon>
                                </Tooltip>
                              )}
                            </CopyButton>
                          )}
                        </Group>

                        <Paper
                          p={resultViewMode === 'raw' ? 'xs' : 0}
                          withBorder
                          bg={result.type === 'failure' ? 'rgba(255, 0, 0, 0.05)' : undefined}
                          style={{
                            borderColor:
                              result.type === 'failure'
                                ? 'var(--mantine-color-red-6)'
                                : 'var(--mantine-color-default-border)',
                            borderWidth: 1,
                            overflow: 'hidden',
                          }}
                        >
                          {result.type === 'missing' ? (
                            <Text c="dimmed" size="xs" fs="italic" p="xs">
                              No response
                            </Text>
                          ) : resultViewMode === 'table' ? (
                            <OutputTable data={parseOutput(result.content)} highlight={highlightText} />
                          ) : (
                            <ScrollArea.Autosize mah={400} type="auto">
                              <RawOutput text={result.content} highlight={highlightText} />
                            </ScrollArea.Autosize>
                          )}
                        </Paper>
                      </Box>
                    ))}
                  </SimpleGrid>
                </Paper>
              ))}
            </Stack>
          </ScrollArea.Autosize>
        </Stack>
      )}

      <Box mt="xl">
        <Button
          variant="subtle"
          size="xs"
          onClick={toggleHistory}
          leftSection={historyOpened ? <IconChevronDown size={14} /> : <IconChevronRight size={14} />}
          color="gray"
        >
          {historyOpened ? 'Hide History' : 'Show History'}
        </Button>

        <Collapse in={historyOpened}>
          <Paper p="md" withBorder mt="xs">
            <Group mb="sm" justify="space-between">
              <Group gap="xs">
                <IconHistory size={20} />
                <Text fw={500}>Recent History</Text>
              </Group>
              {commandHistory.length > 0 && (
                <Text size="xs" c="dimmed">
                  Last {commandHistory.length} command{commandHistory.length !== 1 ? 's' : ''}
                </Text>
              )}
            </Group>

            <ScrollArea h={120}>
              <Stack gap="xs">
                {commandHistory.length > 0 ? (
                  commandHistory.map((item, index) => (
                    <Group key={index} justify="space-between" wrap="nowrap">
                      <Code style={{ flex: 1, overflow: 'hidden', textOverflow: 'ellipsis' }}>{item.command}</Code>
                      <Tooltip label="Load command">
                        <ActionIcon size="sm" variant="light" onClick={() => form.setFieldValue('command', item.command)}>
                          <IconRefresh size={14} />
                        </ActionIcon>
                      </Tooltip>
                    </Group>
                  ))
                ) : (
                  <Text size="sm" c="dimmed" ta="center" py="md">
                    No history available.
                  </Text>
                )}
              </Stack>
            </ScrollArea>
          </Paper>
        </Collapse>
      </Box>

      <Modal opened={savePresetOpened} onClose={() => setSavePresetOpened(false)} title="Save Command Preset">
        <Stack>
          <TextInput
            label="Preset Name"
            placeholder="e.g. Daily Stats"
            value={presetName}
            onChange={(e) => setPresetName(e.currentTarget.value)}
            data-autofocus
          />
          <Text size="xs" c="dimmed">
            Saves command, site selection, path and options for quick reuse.
          </Text>
          <Group justify="flex-end" mt="md">
            <Button variant="default" onClick={() => setSavePresetOpened(false)}>
              Cancel
            </Button>
            <Button onClick={handleSavePreset} disabled={!presetName.trim()}>
              Save Preset
            </Button>
          </Group>
        </Stack>
      </Modal>
    </Stack>
  );
}
