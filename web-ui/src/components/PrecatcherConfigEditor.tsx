import { Alert, Badge, Button, Card, Center, Group, Loader, ScrollArea, Stack, Text, Textarea, TextInput, Title } from '@mantine/core';
import { useMutation } from '@tanstack/react-query';
import { useEffect, useMemo, useRef, useState } from 'react';
import { notifications } from '@mantine/notifications';
import { apiClient } from '../api/client';

type ConfigError = { line: number; message: string };

export function PrecatcherConfigEditor() {
  const [configContent, setConfigContent] = useState('');
  const [configMd5, setConfigMd5] = useState('');
  const [configPath, setConfigPath] = useState('');
  const [errors, setErrors] = useState<ConfigError[]>([]);
  const [syntaxOk, setSyntaxOk] = useState<boolean | null>(null);
  const [isCheckingSyntax, setIsCheckingSyntax] = useState(false);
  const [hasLoaded, setHasLoaded] = useState(false);
  const [testReleaseName, setTestReleaseName] = useState('');

  const textareaRef = useRef<HTMLTextAreaElement | null>(null);
  const lineNumbersRef = useRef<HTMLPreElement | null>(null);
  const highlightRef = useRef<HTMLPreElement | null>(null);

  // Line numbers memoization
  const configLines = useMemo(() => configContent.split('\n'), [configContent]);
  const lineCount = Math.max(1, configLines.length);

  // Load config
  const loadMutation = useMutation({
    mutationFn: async () => {
      const res = await apiClient.post('/ApiPrecatcherService/GetPrecatcherConfig', {});
      const info = res.data.result?.[0] || res.data;
      return info;
    },
    onSuccess: (info) => {
      setConfigContent(info.Content || '');
      setConfigMd5(info.Md5 || '');
      setConfigPath(info.Path || '');
      setHasLoaded(true);
      setErrors([]);
      notifications.show({ title: 'Loaded', message: 'Precatcher config loaded.', color: 'green' });
    },
    onError: (err: any) => notifications.show({ title: 'Error', message: err.message, color: 'red' }),
  });

  // Validate config
  const validateMutation = useMutation({
    mutationFn: async (content: string) => {
      const res = await apiClient.post('/ApiPrecatcherService/ValidatePrecatcherConfig', { Content: content });
      return res.data.result?.[0] || res.data;
    },
  });

  // Save config
  const saveMutation = useMutation({
    mutationFn: async (reload: boolean) => {
      const res = await apiClient.post('/ApiPrecatcherService/SavePrecatcherConfig', {
        Content: configContent,
        ExpectedMd5: configMd5,
        Reload: reload,
      });
      return res.data.result?.[0] || res.data;
    },
    onSuccess: (data) => {
      if (!data.Ok) {
        let parsed: ConfigError[] = [];
        try {
          const raw = data.Errors;
          const arr = typeof raw === 'string' ? JSON.parse(raw) : raw;
          parsed = Array.isArray(arr) ? arr : [];
        } catch { parsed = []; }
        setErrors(parsed);
        notifications.show({ title: 'Save failed', message: data.Message || 'Could not save.', color: 'red' });
        return;
      }
      setErrors([]);
      setSyntaxOk(true);
      setConfigMd5(data.Md5 || '');
      notifications.show({ title: 'Saved', message: 'Precatcher config saved.', color: 'green' });
    },
    onError: (err: any) => notifications.show({ title: 'Error', message: err.message, color: 'red' }),
  });

  // Reload precatcher
  const reloadMutation = useMutation({
    mutationFn: async () => {
      await apiClient.post('/ApiPrecatcherService/ReloadPrecatcher', {});
    },
    onSuccess: () => notifications.show({ title: 'Reloaded', message: 'Precatcher reloaded.', color: 'green' }),
    onError: (err: any) => notifications.show({ title: 'Error', message: err.message, color: 'red' }),
  });

  // Syntax check runner
  const runSyntaxCheck = (content: string, showToast: boolean) => {
    setIsCheckingSyntax(true);
    setSyntaxOk(null);
    validateMutation.mutate(content, {
      onSuccess: (data) => {
        let parsed: ConfigError[] = [];
        try {
          const raw = data.Errors;
          const arr = typeof raw === 'string' ? JSON.parse(raw) : raw;
          parsed = Array.isArray(arr) ? arr : [];
        } catch { parsed = []; }
        setErrors(parsed);
        setSyntaxOk(Boolean(data.Ok));
        setIsCheckingSyntax(false);
        if (showToast) {
          if (data.Ok) notifications.show({ title: 'Syntax OK', message: 'No errors.', color: 'green' });
          else notifications.show({ title: 'Syntax errors', message: `${parsed.length} error(s).`, color: 'red' });
        }
      },
    });
  };

  // Debounced syntax check
  useEffect(() => {
    if (!hasLoaded) return;
    const t = setTimeout(() => runSyntaxCheck(configContent, false), 600);
    return () => clearTimeout(t);
  }, [hasLoaded, configContent]);

  // Auto-load on mount
  useEffect(() => {
    loadMutation.mutate();
  }, []);


  const focusLine = (lineNumber: number) => {
    const el = textareaRef.current;
    if (!el || lineNumber <= 0) return;
    const idx = Math.min(lineNumber - 1, configLines.length - 1);
    let offset = 0;
    for (let i = 0; i < idx; i++) offset += configLines[i].length + 1;
    requestAnimationFrame(() => {
      el.focus();
      const lineLength = configLines[idx]?.length ?? 0;
      el.setSelectionRange(offset, offset + lineLength);
    });
  };

  const syncScroll = () => {
    const el = textareaRef.current;
    const gutter = lineNumbersRef.current;
    const highlight = highlightRef.current;
    if (!el || !gutter || !highlight) return;
    gutter.scrollTop = el.scrollTop;
    highlight.style.transform = `translate(${-el.scrollLeft}px, ${-el.scrollTop}px)`;
  };

  const matchedLines = useMemo(() => {
    if (!testReleaseName.trim()) return [];
    const results: { line: number; text: string }[] = [];
    for (let i = 0; i < configLines.length; i++) {
      const line = configLines[i];
      const trimmed = line.trim();
      if (!trimmed || trimmed.startsWith('#')) continue;
      const firstSep = line.indexOf(';');
      if (firstSep !== 0) continue;
      const secondSep = line.indexOf(';', firstSep + 1);
      if (secondSep === -1) continue;
      const regexText = line.slice(secondSep + 1).trim();
      if (!regexText.startsWith('/')) continue;
      const lastSlash = (() => {
        for (let j = regexText.length - 1; j > 0; j -= 1) {
          if (regexText[j] !== '/') continue;
          let backslashes = 0;
          let k = j - 1;
          while (k >= 0 && regexText[k] === '\\') {
            backslashes += 1;
            k -= 1;
          }
          if (backslashes % 2 === 0) return j;
        }
        return -1;
      })();
      if (lastSlash <= 0) continue;
      const pattern = regexText.slice(1, lastSlash);
      const flags = regexText.slice(lastSlash + 1);
      try {
        const re = new RegExp(pattern, flags);
        if (re.test(testReleaseName)) results.push({ line: i + 1, text: line });
      } catch {
        continue;
      }
    }
    return results.length > 0 ? [results[0]] : [];
  }, [configLines, testReleaseName]);

  const matchedLineSet = useMemo(() => {
    const set = new Set<number>();
    matchedLines.forEach((m) => set.add(m.line));
    return set;
  }, [matchedLines]);

  useEffect(() => {
    syncScroll();
  }, [configContent, matchedLineSet]);

  useEffect(() => {
    if (matchedLines.length > 0) focusLine(matchedLines[0].line);
  }, [matchedLines]);

  const lineHeightPx = 20;

  if (loadMutation.isPending) return <Center h={400}><Loader size="xl" /></Center>;

  return (
    <Stack gap="md">
      <Group justify="space-between">
        <Title order={3}>Precatcher Configuration</Title>
        <Group>
          <Button variant="default" loading={loadMutation.isPending} onClick={() => loadMutation.mutate()}>
            Reload from disk
          </Button>
          <Button variant="default" loading={isCheckingSyntax} onClick={() => runSyntaxCheck(configContent, true)} disabled={!hasLoaded}>
            Syntax check
          </Button>
          <Button variant="outline" loading={reloadMutation.isPending} onClick={() => reloadMutation.mutate()}>
            Reload precatcher
          </Button>
          <Button loading={saveMutation.isPending} onClick={() => saveMutation.mutate(true)} disabled={!hasLoaded || syntaxOk === false || isCheckingSyntax}>
            Save
          </Button>
        </Group>
      </Group>

      {hasLoaded && (
        <Group>
          {isCheckingSyntax && <Badge variant="light">Checking syntax…</Badge>}
          {syntaxOk === true && <Badge color="green" variant="light">Syntax OK</Badge>}
          {syntaxOk === false && <Badge color="red" variant="light">Syntax errors</Badge>}
        </Group>
      )}

      {hasLoaded && (
        <Text size="xs" c="dimmed" lineClamp={1} title={configPath}>
          File: {configPath}
        </Text>
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

      <Card withBorder padding="sm" radius="md">
        <Stack gap="xs">
          <Text fw={600} size="sm">Test release name</Text>
          <TextInput
            placeholder="Paste a release name..."
            value={testReleaseName}
            onChange={(e) => setTestReleaseName(e.currentTarget.value)}
          />
          <Text size="xs" c="dimmed">
            {matchedLines.length === 0 ? 'No match.' : 'First match highlighted.'}
          </Text>
          {matchedLines.length > 0 && (
            <Card
              withBorder
              padding="xs"
              radius="sm"
              style={{ cursor: 'pointer' }}
              onClick={() => focusLine(matchedLines[0].line)}
            >
              <Text fw={600} size="sm">Line {matchedLines[0].line}</Text>
              <Text size="xs" c="dimmed">{matchedLines[0].text}</Text>
            </Card>
          )}
        </Stack>
      </Card>

      <div
        style={{
          border: '1px solid var(--mantine-color-default-border)',
          borderRadius: 'var(--mantine-radius-md)',
          overflow: 'hidden',
          display: 'flex',
          background: 'var(--mantine-color-default)',
        }}
      >
        <pre
          ref={lineNumbersRef}
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
            lineHeight: `${lineHeightPx}px`,
            overflow: 'hidden',
            userSelect: 'none',
          }}
        >
          {Array.from({ length: lineCount }, (_, idx) => {
            const lineNumber = idx + 1;
            const isMatched = matchedLineSet.has(lineNumber);
            return (
              <div
                key={lineNumber}
                style={{
                  background: isMatched ? 'var(--mantine-color-yellow-light)' : 'transparent',
                  color: isMatched ? 'var(--mantine-color-yellow-8)' : 'inherit',
                  lineHeight: `${lineHeightPx}px`,
                }}
              >
                {lineNumber}
              </div>
            );
          })}
        </pre>
        <div style={{ position: 'relative', flex: 1 }}>
          <pre
            ref={highlightRef}
            style={{
              position: 'absolute',
              top: 0,
              left: 0,
              right: 0,
              bottom: 0,
              margin: 0,
              padding: '12px 12px 12px 10px',
              fontFamily: 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
              fontSize: 13,
              lineHeight: `${lineHeightPx}px`,
              whiteSpace: 'pre',
              pointerEvents: 'none',
              color: 'transparent',
              zIndex: 2,
            }}
          >
            {Array.from({ length: lineCount }, (_, idx) => {
              const lineNumber = idx + 1;
              const isMatched = matchedLineSet.has(lineNumber);
              return (
                <div
                  key={`hl-${lineNumber}`}
                  style={{
                    background: isMatched ? 'var(--mantine-color-yellow-light)' : 'transparent',
                    lineHeight: `${lineHeightPx}px`,
                  }}
                >
                  {configLines[idx] ?? ''}
                </div>
              );
            })}
          </pre>
          <Textarea
            value={configContent}
            onChange={(e) => setConfigContent(e.currentTarget.value)}
            onScroll={syncScroll}
            autosize
            minRows={30}
            wrap="off"
            variant="unstyled"
            ref={textareaRef}
            styles={{
              root: { position: 'relative', zIndex: 1 },
              input: {
                padding: '12px 12px 12px 10px',
                fontFamily: 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
                fontSize: 13,
                lineHeight: `${lineHeightPx}px`,
                whiteSpace: 'pre',
                overflowX: 'auto',
                background: 'transparent',
              },
            }}
          />
        </div>
      </div>
    </Stack>
  );
}
