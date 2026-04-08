import { Button, Card, Group, Stack, Switch, Title, Loader, Alert, TextInput, Text, Checkbox } from '@mantine/core';
import { IconRefresh, IconAlertCircle, IconDownload } from '@tabler/icons-react';
import { useQuery } from '@tanstack/react-query';
import { useState, useRef, useMemo } from 'react';
import { Virtuoso } from 'react-virtuoso';
import { apiClient } from '../api/client';

export function Logs() {
  const [autoRefresh, setAutoRefresh] = useState(true);
  const [filter, setFilter] = useState('');
  const [useRegex, setUseRegex] = useState(false);
  const [caseSensitive, setCaseSensitive] = useState(false);
  const virtuosoRef = useRef<any>(null);

  const { data, isLoading, error, refetch, isFetching } = useQuery({
    queryKey: ['logs'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiLogService/GetLogs', { Lines: 2000 });
      let result = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
         // mORMot wrapper
         result = res.data.result[0];
      }
      
      let parsed = [];
      // result should be an array of strings
      if (typeof result === 'string') {
        try {
            parsed = JSON.parse(result);
        } catch {
            parsed = [];
        }
      } else if (Array.isArray(result)) {
        parsed = result;
      }

      // Fix: If backend returns a single blob string in the array
      if (parsed.length === 1 && typeof parsed[0] === 'string' && parsed[0].includes('\n')) {
         return parsed[0].split('\n').map((l: string) => l.trim()).filter((l: string) => l.length > 0);
      }

      return parsed;
    },
    refetchInterval: autoRefresh ? 30000 : false,
    refetchOnWindowFocus: false,
  });

  const rawLogs = Array.isArray(data) ? data : [];

  const filteredLogs = useMemo(() => {
    if (!filter) return rawLogs;

    try {
      if (useRegex) {
        const regex = new RegExp(filter, caseSensitive ? '' : 'i');
        return rawLogs.filter((line: string) => regex.test(line));
      } else {
        const lowerFilter = caseSensitive ? filter : filter.toLowerCase();
        return rawLogs.filter((line: string) => {
            const l = caseSensitive ? line : line.toLowerCase();
            return l.includes(lowerFilter);
        });
      }
    } catch (e) {
      // Invalid regex fallback
      return rawLogs;
    }
  }, [rawLogs, filter, useRegex, caseSensitive]);

  return (
    <Stack h="calc(100vh - 100px)">
      <Group justify="space-between" align="center">
        <Title order={2}>System Logs</Title>
        <Group>
           <Switch
            label="Auto-refresh (30s)"
            checked={autoRefresh}
            onChange={(e) => setAutoRefresh(e.currentTarget.checked)}
           />
           <Button 
             leftSection={<IconRefresh size="1rem" />} 
             onClick={() => refetch()} 
             loading={isFetching}
             variant="light"
           >
             Refresh
           </Button>
           <Button
             leftSection={<IconDownload size="1rem" />}
             variant="default"
             onClick={() => {
                const blob = new Blob([filteredLogs.join('\n')], { type: 'text/plain' });
                const url = window.URL.createObjectURL(blob);
                const a = document.createElement('a');
                a.href = url;
                a.download = `slftp-${new Date().toISOString()}.log`;
                a.click();
             }}
           >
             Download
           </Button>
        </Group>
      </Group>

      <Card p="sm" withBorder radius="md">
        <Group>
           <TextInput 
             placeholder="Filter logs..." 
             value={filter} 
             onChange={(e) => setFilter(e.currentTarget.value)}
             style={{ flex: 1 }}
           />
           <Checkbox 
             label="Regex" 
             checked={useRegex} 
             onChange={(e) => setUseRegex(e.currentTarget.checked)}
           />
           <Checkbox 
             label="Case Sensitive" 
             checked={caseSensitive} 
             onChange={(e) => setCaseSensitive(e.currentTarget.checked)}
           />
        </Group>
      </Card>

      {error && (
        <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
          {error.message}
        </Alert>
      )}

      <Card withBorder radius="md" p={0} style={{ flex: 1, overflow: 'hidden' }}>
        {isLoading && !data ? (
           <Center h="100%">
             <Loader size="xl" />
           </Center>
        ) : (
          <Virtuoso
            ref={virtuosoRef}
            data={filteredLogs}
            initialTopMostItemIndex={filteredLogs.length - 1}
            followOutput="smooth"
            style={{ height: '100%', background: 'var(--bg-card-secondary)' }}
            itemContent={(_index, line) => (
              <div style={{ padding: '0 12px', fontFamily: 'monospace', fontSize: '13px', lineHeight: '1.6', whiteSpace: 'pre-wrap', color: 'var(--text-primary)' }}>
                {line}
              </div>
            )}
          />
        )}
      </Card>
      
      <Group justify="space-between">
        <Text size="xs" c="dimmed">
            {filter ? `Showing ${filteredLogs.length} matching lines (Total: ${rawLogs.length})` : `Total: ${rawLogs.length} lines`}
        </Text>
      </Group>
    </Stack>
  );
}

// Helper component for Center not imported
function Center({ children, h }: { children: React.ReactNode, h?: string | number }) {
  return (
    <div style={{ display: 'flex', justifyContent: 'center', alignItems: 'center', height: h }}>
      {children}
    </div>
  );
}