import { Alert, Badge, Button, Card, Group, Loader, Stack, Table, Text, Textarea, Title, Tooltip, ActionIcon } from '@mantine/core';
import { IconAlertCircle, IconPlayerPlay, IconDeviceFloppy, IconCheck, IconX, IconUpload } from '@tabler/icons-react';
import { useMutation, useQuery } from '@tanstack/react-query';
import { useState, useCallback, useRef } from 'react';
import { batchTestSections, saveSectionTesterData, loadSectionTesterData, type SectionTestItem } from '../api/client';

function parseReleaseText(content: string): SectionTestItem[] {
  const lines = content.split('\n');
  const items: SectionTestItem[] = [];
  
  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith('#')) continue;
    
    // Parse format: "Release.Name-GRP SECTION"
    // Find the last space to split release from section
    const lastSpaceIdx = trimmed.lastIndexOf(' ');
    if (lastSpaceIdx > 0) {
      const releaseName = trimmed.substring(0, lastSpaceIdx).trim();
      const section = trimmed.substring(lastSpaceIdx + 1).trim();
      if (releaseName && section) {
        items.push({ name: releaseName, section: section.toUpperCase() });
      }
    }
  }
  
  return items;
}

export function SectionTester() {
  const [content, setContent] = useState<string>('');
  const [parsedItems, setParsedItems] = useState<SectionTestItem[]>([]);
  const fileInputRef = useRef<HTMLInputElement>(null);

  // Load saved data on mount
  useQuery({
    queryKey: ['section-tester-data'],
    queryFn: async () => {
      const data = await loadSectionTesterData();
      setContent(data);
      setParsedItems(parseReleaseText(data));
      return data;
    },
    refetchOnWindowFocus: false,
  });

  // Save mutation
  const saveMutation = useMutation({
    mutationFn: saveSectionTesterData,
  });

  // Test mutation
  const testMutation = useMutation({
    mutationFn: async () => {
      const items = parseReleaseText(content);
      if (items.length === 0) {
        throw new Error('No valid releases found in text');
      }
      setParsedItems(items);
      return batchTestSections(items);
    },
  });

  const handleContentChange = useCallback((value: string) => {
    setContent(value);
    setParsedItems(parseReleaseText(value));
  }, []);

  const handleSave = useCallback(() => {
    saveMutation.mutate(content);
  }, [content, saveMutation]);

  const handleRunTest = useCallback(() => {
    testMutation.mutate();
  }, [testMutation]);

  const handleFileUpload = useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (!file) return;

    const reader = new FileReader();
    reader.onload = (e) => {
      const text = e.target?.result as string;
      setContent(text);
      setParsedItems(parseReleaseText(text));
    };
    reader.readAsText(file);
    
    // Reset file input
    if (fileInputRef.current) {
      fileInputRef.current.value = '';
    }
  }, []);

  const results = testMutation.data;
  const hasResults = results && results.success && results.results.length > 0;

  return (
    <Stack gap="md">
      {/* Header with stats */}
      <Card withBorder radius="md" p="sm">
        <Group justify="space-between" wrap="wrap">
          <Group gap="xs">
            <Badge color="gray" variant="light">Total: {parsedItems.length}</Badge>
            {hasResults && (
              <>
                <Badge color="teal" variant="light">Matched: {results.stats.matched}</Badge>
                <Badge color="red" variant="light">Failed: {results.stats.failed}</Badge>
              </>
            )}
          </Group>
          <Group gap="xs">
            <input
              type="file"
              accept=".txt"
              style={{ display: 'none' }}
              ref={fileInputRef}
              onChange={handleFileUpload}
            />
            <Tooltip label="Upload Release.txt file">
              <ActionIcon
                variant="light"
                color="blue"
                size="lg"
                onClick={() => fileInputRef.current?.click()}
              >
                <IconUpload size="1.125rem" />
              </ActionIcon>
            </Tooltip>
            <Tooltip label="Save test data">
              <ActionIcon
                variant="light"
                color="green"
                size="lg"
                onClick={handleSave}
                loading={saveMutation.isPending}
              >
                <IconDeviceFloppy size="1.125rem" />
              </ActionIcon>
            </Tooltip>
            <Button
              leftSection={<IconPlayerPlay size="1rem" />}
              onClick={handleRunTest}
              loading={testMutation.isPending}
              disabled={parsedItems.length === 0}
            >
              Run Test
            </Button>
          </Group>
        </Group>
      </Card>

      {/* Content textarea */}
      <Card withBorder radius="md" p="md">
        <Stack gap="xs">
          <Group justify="space-between">
            <Text size="sm" fw={500}>Release.txt Content</Text>
            <Text size="xs" c="dimmed">Format: ReleaseName Section (one per line)</Text>
          </Group>
          <Textarea
            value={content}
            onChange={(e) => handleContentChange(e.currentTarget.value)}
            placeholder="Example:\nRelease.Name-GRP MP3\nAnother.Release-GRP TV-DVDR-DE"
            minRows={8}
            maxRows={15}
            autosize
            styles={{
              input: {
                fontFamily: 'monospace',
                fontSize: '0.9rem',
              },
            }}
          />
        </Stack>
      </Card>

      {/* Loading state */}
      {testMutation.isPending && (
        <Group justify="center" p="md">
          <Loader size="md" />
          <Text size="sm" c="dimmed">Testing sections...</Text>
        </Group>
      )}

      {/* Error state */}
      {testMutation.isError && (
        <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
          {(testMutation.error as any)?.message || 'Failed to run section test'}
        </Alert>
      )}

      {/* Results table */}
      {hasResults && (
        <Card withBorder radius="md" p="md">
          <Stack gap="md">
            <Group justify="space-between">
              <Title order={4}>Test Results</Title>
              <Group gap="xs">
                <Badge color="teal" leftSection={<IconCheck size="0.8rem" />}>
                  {results.stats.matched} matched
                </Badge>
                <Badge color="red" leftSection={<IconX size="0.8rem" />}>
                  {results.stats.failed} failed
                </Badge>
              </Group>
            </Group>

            <Table striped highlightOnHover withTableBorder>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Release</Table.Th>
                  <Table.Th>Expected Section</Table.Th>
                  <Table.Th>Detected Section</Table.Th>
                  <Table.Th style={{ width: 100 }}>Status</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {results.results.map((result, idx) => (
                  <Table.Tr 
                    key={idx}
                    style={{
                      backgroundColor: result.matched 
                        ? 'rgba(0, 150, 0, 0.05)' 
                        : 'rgba(255, 0, 0, 0.05)',
                      borderLeft: `4px solid ${result.matched ? '#099268' : '#fa5252'}`,
                    }}
                  >
                    <Table.Td>
                      <Text size="sm" style={{ fontFamily: 'monospace' }}>
                        {result.releaseName}
                      </Text>
                    </Table.Td>
                    <Table.Td>
                      <Text size="sm" fw={500}>
                        {result.expectedSection}
                      </Text>
                    </Table.Td>
                    <Table.Td>
                      <Text size="sm" c={result.matched ? 'dimmed' : 'red'}>
                        {result.detectedSection || '(none)'}
                      </Text>
                    </Table.Td>
                    <Table.Td>
                      {result.matched ? (
                        <Badge color="teal" size="sm" leftSection={<IconCheck size="0.7rem" />}>
                          OK
                        </Badge>
                      ) : (
                        <Badge color="red" size="sm" leftSection={<IconX size="0.7rem" />}>
                          FAIL
                        </Badge>
                      )}
                    </Table.Td>
                  </Table.Tr>
                ))}
                {results.results.length === 0 && (
                  <Table.Tr>
                    <Table.Td colSpan={4}>
                      <Text size="sm" c="dimmed" ta="center" p="md">
                        No results to display
                      </Text>
                    </Table.Td>
                  </Table.Tr>
                )}
              </Table.Tbody>
            </Table>
          </Stack>
        </Card>
      )}

      {/* Save success notification */}
      {saveMutation.isSuccess && (
        <Alert icon={<IconCheck size="1rem" />} color="green" variant="light">
          Test data saved successfully
        </Alert>
      )}
    </Stack>
  );
}
// SECTION_TESTER_COMPONENT_V1_UNIQUE_ID
