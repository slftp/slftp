import {
  Alert,
  Badge,
  Button,
  Center,
  Code,
  Divider,
  Grid,
  Group,
  Loader,
  Paper,
  ScrollArea,
  Select,
  Stack,
  Tabs,
  Text,
  Textarea,
  TextInput,
  Title,
  Tooltip,
  ActionIcon,
} from '@mantine/core';
import { useMutation, useQuery } from '@tanstack/react-query';
import { useEffect, useMemo, useRef, useState } from 'react';
import { useSearchParams } from 'react-router-dom';
import { notifications } from '@mantine/notifications';
import { IconCheck, IconCode, IconDeviceFloppy, IconFileText, IconRefresh, IconSearch, IconAlertCircle, IconArrowRight } from '@tabler/icons-react';
import { apiClient } from '../api/client';
import type { Site } from '../api/client';

type RuleError = { line: number; message: string };
type RuleCondition = { name: string; ops: string; description: string };

export function Rules() {
  const [searchParams, setSearchParams] = useSearchParams();
  const [siteName, setSiteName] = useState<string>('');
  const [rtplContent, setRtplContent] = useState('');
  const [rtplMd5, setRtplMd5] = useState('');
  const [rtplPath, setRtplPath] = useState('');
  const [siteRulesSnapshotContent, setSiteRulesSnapshotContent] = useState('');
  const [siteRulesSnapshotPath, setSiteRulesSnapshotPath] = useState('');
  const [errors, setErrors] = useState<RuleError[]>([]);
  const [syntaxOk, setSyntaxOk] = useState<boolean | null>(null);
  const [isCheckingSyntax, setIsCheckingSyntax] = useState(false);
  const [conditionSearch, setConditionSearch] = useState('');
  const rtplTextareaRef = useRef<HTMLTextAreaElement | null>(null);
  const rtplLineNumbersRef = useRef<HTMLPreElement | null>(null);
  const [hasLoaded, setHasLoaded] = useState(false);
  const [activeTab, setActiveTab] = useState<string | null>('editor');

  const rtplLineNumbers = useMemo(() => {
    const count = Math.max(1, rtplContent.split('\n').length);
    return Array.from({ length: count }, (_, idx) => String(idx + 1)).join('\n');
  }, [rtplContent]);

  const { data: sites, isLoading, error } = useQuery({
    queryKey: ['sites'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: '' });
      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }
      const rawSites = responseData.Sites;
      let parsedSites: Site[] = [];
      if (typeof rawSites === 'string') parsedSites = JSON.parse(rawSites);
      if (Array.isArray(rawSites)) parsedSites = rawSites;
      return parsedSites;
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const siteOptions = useMemo(() => {
    const opts = [{ value: '*', label: '* (global rules)' }];
    const list = (sites || []).filter((s) => s.name.toLowerCase() !== 'slftp').map((s) => ({ value: s.name, label: s.name }));
    list.sort((a, b) => a.label.localeCompare(b.label));
    return opts.concat(list);
  }, [sites]);

  const { data: conditions } = useQuery({
    queryKey: ['rule-conditions'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSitesService/GetRuleConditions', {});
      const raw = res.data.result?.[0] || res.data;
      const arr = typeof raw === 'string' ? JSON.parse(raw) : raw;
      return Array.isArray(arr) ? (arr as RuleCondition[]) : [];
    },
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  useEffect(() => {
    if (!siteName && siteOptions.length > 0) setSiteName(siteOptions[0].value);
  }, [siteName, siteOptions]);

  // Read site from URL parameter and auto-load
  useEffect(() => {
    const siteParam = searchParams.get('site');
    if (siteParam && siteOptions.length > 0) {
      const siteExists = siteOptions.some(opt => opt.value === siteParam);
      if (siteExists && siteParam !== siteName) {
        setSiteName(siteParam);
        // Auto-load the rules for this site
        setTimeout(() => loadMutation.mutate(siteParam), 100);
        // Clear the URL parameter after loading
        setSearchParams({});
      }
    }
  }, [searchParams, siteOptions]);

  const insertAtCursorOrReplaceSelection = (insertText: string) => {
    const el = rtplTextareaRef.current;
    if (!el) {
      setRtplContent((prev) => prev + insertText);
      return;
    }

    const start = el.selectionStart ?? rtplContent.length;
    const end = el.selectionEnd ?? start;
    const before = rtplContent.slice(0, start);
    const after = rtplContent.slice(end);
    const next = before + insertText + after;
    setRtplContent(next);

    requestAnimationFrame(() => {
      const newPos = start + insertText.length;
      el.focus();
      el.setSelectionRange(newPos, newPos);
    });
  };

  const focusLine = (lineNumber: number) => {
    // Switch to editor tab if not active
    if (activeTab !== 'editor' && activeTab !== 'split') {
      setActiveTab('editor');
    }
    
    // Allow tab switch to happen
    setTimeout(() => {
      const el = rtplTextareaRef.current;
      if (!el) return;
      if (lineNumber <= 0) return;
      const lines = rtplContent.split('\n');
      const idx = Math.min(lineNumber - 1, lines.length - 1);
      let offset = 0;
      for (let i = 0; i < idx; i++) offset += lines[i].length + 1;
      
      el.focus();
      el.setSelectionRange(offset, offset);
      
      // Try to scroll line into view (approximate)
      const lineHeight = 20; // approximate
      el.scrollTop = (lineNumber - 5) * lineHeight;
    }, 100);
  };

  const syncRtplLineNumberScroll = () => {
    const el = rtplTextareaRef.current;
    const gutter = rtplLineNumbersRef.current;
    if (!el || !gutter) return;
    gutter.scrollTop = el.scrollTop;
  };

  const filteredConditions = useMemo(() => {
    const list = conditions || [];
    if (!conditionSearch.trim()) return list;
    const q = conditionSearch.trim().toLowerCase();
    return list.filter((c) => c.name.toLowerCase().includes(q) || c.description.toLowerCase().includes(q));
  }, [conditions, conditionSearch]);

  const loadMutation = useMutation({
    mutationFn: async (selectedSite: string) => {
      const [rtplRes, snapshotRes] = await Promise.all([
        apiClient.post('/ApiSitesService/GetSiteRtpl', { SiteName: selectedSite }),
        selectedSite === '*' ? Promise.resolve({ data: { Content: '', Path: '', Md5: '', Exists: false } }) : apiClient.post('/ApiSitesService/GetSiteRulesSnapshot', { SiteName: selectedSite }),
      ]);
      const rtplInfo = rtplRes.data.result?.[0] || rtplRes.data;
      const snapInfo = snapshotRes.data.result?.[0] || snapshotRes.data;
      return { rtplInfo, snapInfo };
    },
    onSuccess: ({ rtplInfo, snapInfo }) => {
      setErrors([]);
      setRtplContent(rtplInfo.Content || '');
      setRtplMd5(rtplInfo.Md5 || '');
      setRtplPath(rtplInfo.Path || '');
      setSiteRulesSnapshotContent(snapInfo.Content || '');
      setSiteRulesSnapshotPath(snapInfo.Path || '');
      setHasLoaded(true);
      notifications.show({ title: 'Loaded', message: 'Rules loaded.', color: 'green' });
    },
    onError: (err) => notifications.show({ title: 'Error', message: err.message, color: 'red' }),
  });

  const validateMutation = useMutation({
    mutationFn: async (content: string) => {
      const res = await apiClient.post('/ApiSitesService/ValidateRtpl', { Content: content });
      return res.data.result?.[0] || res.data;
    },
  });

  const saveMutation = useMutation({
    mutationFn: async (reload: boolean) => {
      const res = await apiClient.post('/ApiSitesService/SaveSiteRtpl', { SiteName: siteName, Content: rtplContent, ExpectedMd5: rtplMd5, Reload: reload });
      return res.data.result?.[0] || res.data;
    },
    onSuccess: (data) => {
      if (!data.Ok) {
        let parsed: RuleError[] = [];
        try {
          const raw = data.Errors;
          const arr = typeof raw === 'string' ? JSON.parse(raw) : raw;
          parsed = Array.isArray(arr) ? arr : [];
        } catch {
          parsed = [];
        }
        setErrors(parsed);
        notifications.show({ title: 'Save failed', message: data.Message || 'Could not save rules.', color: 'red' });
        return;
      }
      setErrors([]);
      setSyntaxOk(true);
      setRtplMd5(data.Md5 || '');
      setRtplPath(data.Path || rtplPath);
      notifications.show({ title: 'Saved', message: 'Rules saved.', color: 'green' });
    },
    onError: (err) => notifications.show({ title: 'Error', message: err.message, color: 'red' }),
  });

  const reloadMutation = useMutation({
    mutationFn: async () => {
      await apiClient.post('/ApiSitesService/ReloadRules', {});
    },
    onSuccess: () => notifications.show({ title: 'Reloaded', message: 'Rules reloaded from disk.', color: 'green' }),
    onError: (err) => notifications.show({ title: 'Error', message: err.message, color: 'red' }),
  });

  const runSyntaxCheck = (content: string, showToast: boolean) => {
    setIsCheckingSyntax(true);
    setSyntaxOk(null);
    validateMutation.mutate(content, {
      onSuccess: (data) => {
        let parsed: RuleError[] = [];
        try {
          const raw = data.Errors;
          const arr = typeof raw === 'string' ? JSON.parse(raw) : raw;
          parsed = Array.isArray(arr) ? arr : [];
        } catch {
          parsed = [];
        }
        setErrors(parsed);
        setSyntaxOk(Boolean(data.Ok));
        setIsCheckingSyntax(false);
        if (showToast) {
          if (data.Ok) notifications.show({ title: 'Syntax OK', message: 'No parser errors found.', color: 'green' });
          else notifications.show({ title: 'Syntax errors', message: `${parsed.length} error(s) found.`, color: 'red' });
        }
      },
    });
  };

  useEffect(() => {
    if (!hasLoaded) return;
    const t = setTimeout(() => {
      runSyntaxCheck(rtplContent, false);
    }, 600);
    return () => clearTimeout(t);
  }, [hasLoaded, rtplContent]);

  if (isLoading) return <Center h={400}><Loader size="xl" /></Center>;
  if (error) return <Alert color="red" title="Error">Could not load sites</Alert>;

  const editorPanel = (
    <div
      style={{
        border: '1px solid var(--mantine-color-default-border)',
        borderRadius: 'var(--mantine-radius-md)',
        overflow: 'hidden',
        display: 'flex',
        height: 'calc(100vh - 300px)',
        minHeight: '500px',
        background: 'var(--mantine-color-default)',
        position: 'relative',
      }}
    >
      <pre
        ref={rtplLineNumbersRef}
        style={{
          margin: 0,
          padding: '12px 10px 12px 12px',
          width: 52,
          textAlign: 'right',
          color: 'var(--mantine-color-dimmed)',
          background: 'var(--mantine-color-body)',
          borderRight: '1px solid var(--mantine-color-default-border)',
          fontFamily: 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
          fontSize: 13,
          lineHeight: 1.45,
          overflow: 'hidden',
          userSelect: 'none',
        }}
      >
        {rtplLineNumbers}
      </pre>
      <Textarea
        value={rtplContent}
        onChange={(e) => setRtplContent(e.currentTarget.value)}
        onScroll={syncRtplLineNumberScroll}
        variant="unstyled"
        ref={rtplTextareaRef}
        placeholder="Select a site and click Load to start editing..."
        styles={{
          root: { flex: 1, height: '100%', overflow: 'hidden' },
          wrapper: { height: '100%' },
          input: {
            height: '100%',
            padding: '12px 12px 12px 10px',
            fontFamily:
              'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
            fontSize: 13,
            lineHeight: 1.45,
            whiteSpace: 'pre',
            overflowX: 'auto',
          },
        }}
      />
    </div>
  );

  const snapshotPanel = (
    <Stack gap="xs" h="100%">
       <Group justify="space-between">
         <Text size="sm" fw={500}>Current Site Rules Snapshot</Text>
         <Code>{siteRulesSnapshotPath}</Code>
       </Group>
      <Textarea
        value={siteRulesSnapshotContent}
        readOnly
        variant="filled"
        styles={{
          input: {
            fontFamily: 'monospace',
            height: 'calc(100vh - 340px)',
            minHeight: '460px',
            fontSize: '13px',
            whiteSpace: 'pre',
            overflowX: 'auto',
          }
        }}
      />
    </Stack>
  );

  return (
    <Stack gap="md" h="100%">
      <Paper p="md" shadow="sm" radius="md" withBorder>
        <Group justify="space-between" align="center">
          <Group>
            <Title order={3}>Rules Editor</Title>
            {hasLoaded && (
              <Group gap="xs">
                 <Badge variant="dot" color={syntaxOk === false ? 'red' : syntaxOk === true ? 'green' : 'gray'}>
                    {isCheckingSyntax ? 'Checking...' : syntaxOk === false ? 'Syntax Errors' : syntaxOk === true ? 'Syntax OK' : 'Ready'}
                 </Badge>
              </Group>
            )}
          </Group>
          
          <Group>
             <Select
              placeholder="Select Site"
              value={siteName}
              data={siteOptions}
              onChange={(v) => v && setSiteName(v)}
              searchable
              w={250}
            />
            <Tooltip label="Load rtpl (Loads the actual file content from the server into this editor)">
              <Button variant="default" onClick={() => loadMutation.mutate(siteName)} loading={loadMutation.isPending} disabled={!siteName}>
                Load rtpl
              </Button>
            </Tooltip>
            
            <Divider orientation="vertical" />

            <Tooltip label="Syntax check (Checks the current editor content for syntax errors without saving)">
               <ActionIcon variant="light" color="blue" size="lg" onClick={() => runSyntaxCheck(rtplContent, true)} loading={isCheckingSyntax} disabled={!hasLoaded}>
                 <IconCheck size="1.2rem" />
               </ActionIcon>
            </Tooltip>

            <Tooltip label="Save (Writes changes to disk)">
               <Button 
                  leftSection={<IconDeviceFloppy size="1rem" />} 
                  color="blue" 
                  onClick={() => saveMutation.mutate(false)} 
                  loading={saveMutation.isPending} 
                  disabled={!hasLoaded || syntaxOk === false || isCheckingSyntax}
                >
                  Save
               </Button>
            </Tooltip>

            <Tooltip label="Rulesreload (Triggers !rulesreload: reloads all rules from disk into memory, same as the IRC command)">
               <Button variant="subtle" color="gray" leftSection={<IconRefresh size="1rem" />} onClick={() => reloadMutation.mutate()} loading={reloadMutation.isPending}>
                 Rulesreload
               </Button>
            </Tooltip>
          </Group>
        </Group>

        {/* Validation Errors */}
        {errors.length > 0 && (
          <Alert icon={<IconAlertCircle size="1rem" />} title="Validation Errors" color="red" mt="md" variant="light" withCloseButton onClose={() => setErrors([])}>
            <ScrollArea.Autosize mah={100}>
              {errors.map((e, idx) => (
                <Text
                  key={idx}
                  size="sm"
                  style={{ cursor: 'pointer' }}
                  onClick={() => focusLine(e.line)}
                  c="red.7"
                  fw={500}
                >
                  Line {e.line}: {e.message}
                </Text>
              ))}
            </ScrollArea.Autosize>
          </Alert>
        )}
      </Paper>

      <Paper p="sm" shadow="sm" radius="md" withBorder h="100%">
        <Tabs value={activeTab} onChange={setActiveTab} keepMounted={false}>
          <Tabs.List mb="xs">
            <Tabs.Tab value="editor" leftSection={<IconCode size="0.8rem" />}>Editor</Tabs.Tab>
            <Tabs.Tab value="snapshot" leftSection={<IconFileText size="0.8rem" />}>Snapshot (Site Rules)</Tabs.Tab>
            <Tabs.Tab value="conditions" leftSection={<IconSearch size="0.8rem" />}>Conditions</Tabs.Tab>
          </Tabs.List>

          <Tabs.Panel value="editor">
            {editorPanel}
            {hasLoaded && (
              <Text size="xs" c="dimmed" mt={4} ta="right">
                File: {rtplPath}
              </Text>
            )}
          </Tabs.Panel>

          <Tabs.Panel value="snapshot">
            {snapshotPanel}
          </Tabs.Panel>

          <Tabs.Panel value="conditions">
             <Stack h="100%" gap="md">
                <TextInput
                  placeholder="Search conditions..."
                  leftSection={<IconSearch size="0.8rem" />}
                  value={conditionSearch}
                  onChange={(e) => setConditionSearch(e.currentTarget.value)}
                />
                
                <Text size="xs" c="dimmed">
                   Click a condition to insert it into the editor (switches to Editor tab).
                </Text>

                <ScrollArea style={{ height: 'calc(100vh - 400px)', minHeight: '400px' }}>
                  <Grid gutter="sm">
                    {filteredConditions.map((c) => (
                      <Grid.Col key={c.name} span={{ base: 12, md: 6, lg: 4 }}>
                        <Paper
                          withBorder
                          p="sm"
                          radius="sm"
                          style={{ cursor: 'pointer', transition: 'background-color 0.2s', height: '100%' }}
                          onClick={() => {
                             insertAtCursorOrReplaceSelection(`if ${c.name} `);
                             setActiveTab('editor');
                          }}
                          className="condition-card"
                        >
                          <Group justify="space-between" align="start" wrap="nowrap" mb={4}>
                             <Text size="sm" fw={700} style={{ fontFamily: 'monospace' }}>{c.name}</Text>
                             <IconArrowRight size="0.8rem" style={{ opacity: 0.5 }} />
                          </Group>
                          <Text size="xs" c="dimmed" lh={1.3}>
                            {c.description}
                          </Text>
                        </Paper>
                      </Grid.Col>
                    ))}
                  </Grid>
                  {filteredConditions.length === 0 && (
                    <Text size="sm" c="dimmed" ta="center" py="xl">No matches found</Text>
                  )}
                </ScrollArea>
             </Stack>
          </Tabs.Panel>
        </Tabs>
      </Paper>
    </Stack>
  );
}
