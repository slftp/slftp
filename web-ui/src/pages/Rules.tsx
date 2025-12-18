import { Alert, Badge, Button, Card, Center, Divider, Group, Loader, ScrollArea, Select, Stack, Text, Textarea, TextInput, Title } from '@mantine/core';
import { useMutation, useQuery } from '@tanstack/react-query';
import { useEffect, useMemo, useRef, useState } from 'react';
import { useSearchParams } from 'react-router-dom';
import { notifications } from '@mantine/notifications';
import { apiClient } from '../api/client';
import type { Site } from '../api/client';

type RuleError = { line: number; message: string };
type RuleCondition = { name: string; ops: string; description: string };

export function Rules() {
  const CONDITIONS_VISIBLE = 3;
  const CONDITION_ROW_HEIGHT = 68;
  const CONDITION_ROW_GAP = 6;
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
    const el = rtplTextareaRef.current;
    if (!el) return;
    if (lineNumber <= 0) return;
    const lines = rtplContent.split('\n');
    const idx = Math.min(lineNumber - 1, lines.length - 1);
    let offset = 0;
    for (let i = 0; i < idx; i++) offset += lines[i].length + 1;
    requestAnimationFrame(() => {
      el.focus();
      el.setSelectionRange(offset, offset);
    });
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

  useEffect(() => {
    // no-op; selection is not required for click-to-insert
  }, [filteredConditions]);

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

  return (
    <Card shadow="sm" padding="lg" radius="md" withBorder>
      <Group justify="space-between" align="flex-end" mb="md">
        <Title order={3}>Rules</Title>
        <Group align="flex-end">
          <Group gap="xs" align="center">
            <Text fw={500} size="sm">Site</Text>
            <Select
              aria-label="Site"
              placeholder="Site"
              value={siteName}
              data={siteOptions}
              onChange={(v) => v && setSiteName(v)}
              searchable
              w={260}
            />
          </Group>
          <Button variant="default" loading={loadMutation.isPending} onClick={() => loadMutation.mutate(siteName)} disabled={!siteName}>
            Load
          </Button>
          <Button variant="default" loading={isCheckingSyntax} onClick={() => runSyntaxCheck(rtplContent, true)} disabled={!hasLoaded}>
            Syntax check
          </Button>
          <Button variant="outline" loading={reloadMutation.isPending} onClick={() => reloadMutation.mutate()}>
            Reload rules
          </Button>
          <Button loading={saveMutation.isPending} onClick={() => saveMutation.mutate(true)} disabled={!hasLoaded || syntaxOk === false || isCheckingSyntax}>
            Save
          </Button>
        </Group>
      </Group>

      <Stack gap="sm">
        {hasLoaded && (
          <Group>
            {isCheckingSyntax && <Badge variant="light">Checking syntax…</Badge>}
            {syntaxOk === true && <Badge color="green" variant="light">Syntax OK</Badge>}
            {syntaxOk === false && <Badge color="red" variant="light">Syntax errors</Badge>}
            {syntaxOk === false && errors.length > 0 && (
              <Text size="xs" c="dimmed">
                {errors.length} error(s) – fix them or re-run syntax check
              </Text>
            )}
          </Group>
        )}

        {hasLoaded && (
          <Group gap="xl" wrap="nowrap">
            <Text
              size="xs"
              c="dimmed"
              style={{ flex: 1, minWidth: 0 }}
              lineClamp={1}
              title={rtplPath}
            >
              rtpl file: {rtplPath}
            </Text>
            {siteName !== '*' && (
              <Text
                size="xs"
                c="dimmed"
                style={{ flex: 1, minWidth: 0 }}
                lineClamp={1}
                title={siteRulesSnapshotPath}
              >
                snapshot: {siteRulesSnapshotPath}
              </Text>
            )}
          </Group>
        )}

        {errors.length > 0 && (
          <Alert color="red" title="Validation errors">
            <ScrollArea h={160}>
              {errors.slice(0, 250).map((e) => (
                <Text
                  key={`${e.line}-${e.message}`}
                  size="sm"
                  style={{ cursor: 'pointer' }}
                  onClick={() => focusLine(e.line)}
                >
                  Line {e.line}: {e.message}
                </Text>
              ))}
            </ScrollArea>
          </Alert>
        )}

        <Divider label="Conditions" />
        <Group justify="space-between" align="flex-end">
          <TextInput
            label="Search"
            placeholder="Filter conditions by name/description (e.g. year, tag, mp3language)"
            value={conditionSearch}
            onChange={(e) => setConditionSearch(e.currentTarget.value)}
            w={520}
          />
          <Group>
            <Badge variant="light">
              {filteredConditions.length}/{(conditions || []).length}
            </Badge>
          </Group>
        </Group>

        <Card withBorder padding="sm" radius="md">
          <Stack gap="xs">
            <Text fw={600} size="sm">Click a condition to insert `if …`</Text>
            <ScrollArea h={CONDITIONS_VISIBLE * CONDITION_ROW_HEIGHT + (CONDITIONS_VISIBLE - 1) * CONDITION_ROW_GAP}>
              <Stack gap={CONDITION_ROW_GAP}>
                {filteredConditions.map((c) => (
                  <Card
                    key={c.name}
                    withBorder
                    padding="xs"
                    radius="sm"
                    style={{ cursor: 'pointer', height: CONDITION_ROW_HEIGHT, overflow: 'hidden' }}
                    onClick={() => insertAtCursorOrReplaceSelection(`if ${c.name} `)}
                  >
                    <Group justify="space-between" align="flex-start">
                      <Stack gap={2} style={{ flex: 1 }}>
                        <Group gap="xs">
                          <Text fw={600} size="sm">{c.name}</Text>
                        </Group>
                        <Text size="xs" c="dimmed" lineClamp={2}>{c.description}</Text>
                      </Stack>
                    </Group>
                  </Card>
                ))}
                {filteredConditions.length === 0 && (
                  <Text size="sm" c="dimmed">No matches.</Text>
                )}
              </Stack>
            </ScrollArea>
          </Stack>
        </Card>

        <Group align="flex-start" grow>
          <Stack gap={6} style={{ flex: 1 }}>
            <Text fw={500} size="sm">Incoming rules (*.rtpl)</Text>
            <div
              style={{
                border: '1px solid var(--mantine-color-default-border)',
                borderRadius: 'var(--mantine-radius-md)',
                overflow: 'hidden',
                display: 'flex',
                width: '100%',
                background: 'var(--mantine-color-default)',
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
                  background: 'var(--mantine-color-default-hover)',
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
                autosize
                minRows={24}
                variant="unstyled"
                ref={rtplTextareaRef}
                styles={{
                  root: { flex: 1 },
                  input: {
                    padding: '12px 12px 12px 10px',
                    fontFamily:
                      'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
                    fontSize: 13,
                    lineHeight: 1.45,
                  },
                }}
              />
            </div>
          </Stack>
          <Stack gap="xs">
            <Textarea
              label="SITE RULES snapshot (read-only)"
              value={siteRulesSnapshotContent}
              readOnly
              autosize
              minRows={24}
            />
            {siteName === '*' && <Text size="xs" c="dimmed">No SITE RULES snapshot for global rules.</Text>}
          </Stack>
        </Group>
      </Stack>
    </Card>
  );
}
