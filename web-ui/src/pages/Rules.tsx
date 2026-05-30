import {
  Alert,
  Badge,
  Button,
  Center,
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
  Code,
  useMantineColorScheme,
} from '@mantine/core';
import { useMutation, useQuery } from '@tanstack/react-query';
import { useEffect, useMemo, useRef, useState } from 'react';
import { useSearchParams } from 'react-router-dom';
import { notifications } from '@mantine/notifications';
import { IconCheck, IconCode, IconDeviceFloppy, IconFileText, IconRefresh, IconSearch, IconAlertCircle, IconArrowRight, IconBulb } from '@tabler/icons-react';
import Editor from '@monaco-editor/react';
import DOMPurify from 'dompurify';
import { apiClient } from '../api/client';
import type { Site } from '../api/client';
import { RulesExamples } from '../components/RulesExamples';

type RuleError = { line: number; message: string };
type RuleCondition = { name: string; ops: string; description: string };

function buildConditionExample(condition: RuleCondition, site: string): string {
  const cond = (condition.name || '').trim();
  const condLower = cond.toLowerCase();
  const ops = ((condition.ops || '').trim() + ' ').replace(/\s+/g, ' ').trim();
  const siteToken = (site && site !== '*') ? site : 'EXAMPLESITE';
  const section = '*';
  const prefix = `${siteToken} ${section} if `;

  if (condLower === 'default') {
    return `${prefix}default then ALLOW`;
  }

  if (ops === '') {
    return `${prefix}${cond} then DROP\n${siteToken} ${section} if default then ALLOW`;
  }

  if (condLower === 'section') {
    return `${prefix}section =~ /^(TV|X265|BLURAY)/i then ALLOW`;
  }

  if (ops.includes('=~') || ops.includes('!~')) {
    if (condLower.includes('release')) {
      return `${prefix}${cond} =~ /[-._](GERMAN|FRENCH)[-._]/i then DROP\n${siteToken} ${section} if default then ALLOW`;
    }
    return `${prefix}${cond} =~ /pattern/i then DROP\n${siteToken} ${section} if default then ALLOW`;
  }

  if (ops.includes('notin') || ops.includes('in')) {
    if (condLower.includes('language') || condLower.includes('lang')) {
      return `${prefix}${cond} notin English, German then DROP\n${siteToken} ${section} if default then ALLOW`;
    }
    if (condLower.includes('country')) {
      return `${prefix}${cond} notin USA, UK then DROP\n${siteToken} ${section} if default then ALLOW`;
    }
    if (condLower.includes('genre')) {
      return `${prefix}${cond} in Documentary, Sports then DROP\n${siteToken} ${section} if default then ALLOW`;
    }
    if (condLower.includes('group')) {
      return `${prefix}${cond} in GROUP1, GROUP2, GROUP3 then DROP\n${siteToken} ${section} if default then ALLOW`;
    }
    return `${prefix}${cond} in VALUE1, VALUE2 then DROP\n${siteToken} ${section} if default then ALLOW`;
  }

  if (ops.includes('<') || ops.includes('>') || ops.includes('>=') || ops.includes('<=')) {
    if (condLower.includes('year')) {
      return `${prefix}${cond} < 2020 then DROP\n${siteToken} ${section} if default then ALLOW`;
    }
    if (condLower.includes('rating')) {
      return `${prefix}${cond} < 65 then DROP\n${siteToken} ${section} if default then ALLOW`;
    }
    if (condLower.includes('votes')) {
      return `${prefix}${cond} < 500 then DROP\n${siteToken} ${section} if default then ALLOW`;
    }
    if (condLower.includes('size') || condLower.includes('files') || condLower.includes('kb') || condLower.includes('disk') || condLower.includes('age')) {
      return `${prefix}${cond} > 500 then DROP\n${siteToken} ${section} if default then ALLOW`;
    }
    return `${prefix}${cond} > 0 then DROP\n${siteToken} ${section} if default then ALLOW`;
  }

  if (ops.includes('!=') || ops.includes('=')) {
    if (condLower.includes('language') || condLower.includes('lang')) {
      return `${prefix}${cond} != English then DROP\n${siteToken} ${section} if default then ALLOW`;
    }
    if (condLower.includes('country')) {
      return `${prefix}${cond} = USA then ALLOW`;
    }
    if (condLower.includes('year')) {
      return `${prefix}${cond} = 2024 then ALLOW`;
    }
    return `${prefix}${cond} = VALUE then DROP\n${siteToken} ${section} if default then ALLOW`;
  }

  return `${prefix}${cond} then DROP\n${siteToken} ${section} if default then ALLOW`;
}

export function Rules() {
  const { colorScheme } = useMantineColorScheme();
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
  
  const editorRef = useRef<any>(null);
  const monacoRef = useRef<any>(null);
  const conditionClickTimeoutRef = useRef<number | null>(null);
  
  const [hasLoaded, setHasLoaded] = useState(false);
  const [activeTab, setActiveTab] = useState<string | null>('editor');

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
    // Deduplicate sites by value
    const uniqueList = Array.from(new Map(list.map(item => [item.value, item])).values());
    uniqueList.sort((a, b) => a.label.localeCompare(b.label));
    return opts.concat(uniqueList);
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

  const handleEditorWillMount = (monaco: any) => {
    // Register a new language
    monaco.languages.register({ id: 'slftp-rules' });
    monaco.languages.setLanguageConfiguration('slftp-rules', {
      brackets: [
        ['{', '}'],
        ['[', ']'],
        ['(', ')'],
      ],
      autoClosingPairs: [
        { open: '{', close: '}' },
        { open: '[', close: ']' },
        { open: '(', close: ')' },
        { open: '"', close: '"' },
        { open: "'", close: "'" },
      ],
      colorizedBracketPairs: [
        ['{', '}'],
        ['[', ']'],
        ['(', ')'],
      ],
    });

    // Register a tokens provider for the language
    monaco.languages.setMonarchTokensProvider('slftp-rules', {
      tokenizer: {
        root: [
          [/\b(if|then|and|or|not|in|notin)\b/i, 'keyword'],
          [/\b(DROP|ALLOW)\b/i, 'type'],
          [/\b(default)\b/i, 'constant'],
          [/\b(group|age|releasename|section|tag|year|mp3language|mp3year|mp3numdisks|imdblookupdone|imdblanguages|imdbgenre|imdbgenres|imdbrating|imdbyear|imdbvotes|imdbcountry|tvlookupdone|tvlanguage|tvcountry|tvgenres|tvscripted|tvrunning|tvstatus|tvclassification|tvepisodeagedays|tvseason|tvep|nfolookupdone|internal|files|size|disk|kb)\b/i, 'variable'],
          [/#.*$/, 'comment'],
          [/"[^"]*"/, 'string'],
          [/'[^']*'/, 'string'],
          [/\b\d+\b/, 'number'],
          [/[{}()\[\]]/, '@brackets'],
          [/[\/\\]/, 'delimiter'],
          [/!~|=~/, { token: 'operator', next: '@afterRegexOp' }],
          [/[<>!=~]+/, 'operator'],
          [/&&|\|\|/, 'operator'],
        ],
        afterRegexOp: [
          [/\s+/, 'white'],
          [/\/(?!\s)/, { token: 'delimiter.regexp', next: '@regex' }],
          [/./, { token: '', next: '@pop' }],
        ],
        regex: [
          [/[^\\/()[\]{}]+/, 'regexp'],
          [/\\./, 'regexp'],
          [/[(){}\[\]]/, '@brackets'],
          [/\/[gimuy]*/, { token: 'delimiter.regexp', next: '@pop' }],
        ]
      }
    });

    const commonRules = [
      { token: 'keyword', fontStyle: 'bold' },
      { token: 'type', fontStyle: 'bold' },
      { token: 'comment', fontStyle: 'italic' },
    ];

    // Define Dark theme
    monaco.editor.defineTheme('slftp-theme-dark', {
      base: 'vs-dark',
      inherit: true,
      rules: [
        ...commonRules,
        { token: 'keyword', foreground: '569cd6' },
        { token: 'type', foreground: '4ec9b0' },
        { token: 'constant', foreground: '4fc1ff' },
        { token: 'variable', foreground: '9cdcfe' },
        { token: 'comment', foreground: '6a9955' },
        { token: 'string', foreground: 'ce9178' },
        { token: 'regexp', foreground: 'd16969' },
        { token: 'number', foreground: 'b5cea8' },
        { token: 'operator', foreground: 'd4d4d4' },
        { token: 'delimiter', foreground: 'd7ba7d' },
        { token: 'delimiter.regexp', foreground: 'ffff00', fontStyle: 'bold underline' },
        { token: 'delimiter.regexp.slftp-rules', foreground: 'ffff00', fontStyle: 'bold underline' },
      ],
      colors: {
        'editor.background': '#1a1b1e',
        'editor.selectionBackground': '#264f78',
        'editor.lineHighlightBackground': '#2b2d30',
        'editorCursor.foreground': '#aeafad',
        'editorWhitespace.foreground': '#3b3a32',
        'editorIndentGuide.background': '#404040',
        'editorSelectionHighlightBackground': '#add6ff26',
        'editorBracketHighlightForeground1': '#d7ba7d',
        'editorBracketHighlightForeground2': '#4ec9b0',
        'editorBracketHighlightForeground3': '#569cd6',
        'editorBracketHighlightForeground4': '#ce9178',
        'editorBracketHighlightForeground5': '#b5cea8',
        'editorBracketHighlightForeground6': '#c586c0',
        'editorBracketPairGuide.activeBackground1': '#d7ba7d40',
        'editorBracketPairGuide.activeBackground2': '#4ec9b040',
        'editorBracketPairGuide.activeBackground3': '#569cd640',
        'editorBracketPairGuide.activeBackground4': '#ce917840',
        'editorBracketPairGuide.activeBackground5': '#b5cea840',
        'editorBracketPairGuide.activeBackground6': '#c586c040',
      }
    });

    // Define Light theme
    monaco.editor.defineTheme('slftp-theme-light', {
      base: 'vs',
      inherit: true,
      rules: [
        ...commonRules,
        { token: 'keyword', foreground: '0000ff' },
        { token: 'type', foreground: '267f99' },
        { token: 'constant', foreground: '0070c1' },
        { token: 'variable', foreground: '001080' },
        { token: 'comment', foreground: '008000' },
        { token: 'string', foreground: 'a31515' },
        { token: 'regexp', foreground: '811f3f' },
        { token: 'number', foreground: '098658' },
        { token: 'operator', foreground: '000000' },
        { token: 'delimiter', foreground: 'a31515' },
        { token: 'delimiter.regexp', foreground: 'c00000', fontStyle: 'bold underline' },
        { token: 'delimiter.regexp.slftp-rules', foreground: 'c00000', fontStyle: 'bold underline' },
      ],
      colors: {
        'editor.background': '#ffffff',
        'editor.lineHighlightBackground': '#f3f3f3',
        'editorBracketHighlightForeground1': '#a31515',
        'editorBracketHighlightForeground2': '#0451a5',
        'editorBracketHighlightForeground3': '#098658',
        'editorBracketHighlightForeground4': '#795e26',
        'editorBracketHighlightForeground5': '#267f99',
        'editorBracketHighlightForeground6': '#811f3f',
        'editorBracketPairGuide.activeBackground1': '#a3151540',
        'editorBracketPairGuide.activeBackground2': '#0451a540',
        'editorBracketPairGuide.activeBackground3': '#09865840',
        'editorBracketPairGuide.activeBackground4': '#795e2640',
        'editorBracketPairGuide.activeBackground5': '#267f9940',
        'editorBracketPairGuide.activeBackground6': '#811f3f40',
      }
    });
  };

  const handleEditorDidMount = (editor: any, monaco: any) => {
    editorRef.current = editor;
    monacoRef.current = monaco;
  };

  const insertAtCursorOrReplaceSelection = (insertText: string) => {
    const editor = editorRef.current;
    const canEditInMonaco = (activeTab === 'editor' || activeTab === 'split') && !!editor && !!editor.getModel?.();

    if (!canEditInMonaco) {
      setRtplContent((prev) => prev + insertText);
      return;
    }

    try {
      const selection = editor.getSelection();
      const op = { range: selection, text: insertText, forceMoveMarkers: true };
      editor.executeEdits('my-source', [op]);
      editor.focus();
      setRtplContent(editor.getValue());
    } catch {
      setRtplContent((prev) => prev + insertText);
    }
  };

  const focusLine = (lineNumber: number) => {
    // Switch to editor tab if not active
    if (activeTab !== 'editor' && activeTab !== 'split') {
      setActiveTab('editor');
    }
    
    // Allow tab switch to happen
    setTimeout(() => {
      const editor = editorRef.current;
      if (!editor) return;
      if (lineNumber <= 0) return;
      
      editor.revealLineInCenter(lineNumber);
      editor.setPosition({ column: 1, lineNumber: lineNumber });
      editor.focus();
    }, 100);
  };

  const filteredConditions = useMemo(() => {
    const list = conditions || [];
    if (!conditionSearch.trim()) return list;
    const q = conditionSearch.trim().toLowerCase();
    return list.filter((c) => c.name.toLowerCase().includes(q) || c.description.toLowerCase().includes(q));
  }, [conditions, conditionSearch]);

  const getConditionExample = (c: RuleCondition): string => {
    return buildConditionExample(c, siteName);
  };

  const insertConditionFromCard = (c: RuleCondition, fullExample: boolean) => {
    const example = getConditionExample(c);
    const text = fullExample ? `${example}\n` : `${example.split('\n')[0]}\n`;
    insertAtCursorOrReplaceSelection(text);
    setActiveTab('editor');
  };

  const handleConditionCardClick = (c: RuleCondition) => {
    if (conditionClickTimeoutRef.current !== null) {
      window.clearTimeout(conditionClickTimeoutRef.current);
    }

    conditionClickTimeoutRef.current = window.setTimeout(() => {
      insertConditionFromCard(c, false);
      conditionClickTimeoutRef.current = null;
    }, 220);
  };

  const handleConditionCardDoubleClick = (c: RuleCondition) => {
    if (conditionClickTimeoutRef.current !== null) {
      window.clearTimeout(conditionClickTimeoutRef.current);
      conditionClickTimeoutRef.current = null;
    }
    insertConditionFromCard(c, true);
  };

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

  const statusDisplay = useMemo(() => {
    if (!hasLoaded) return null;
    if (isCheckingSyntax) return <Badge color="yellow" variant="light">Checking...</Badge>;
    
    if (syntaxOk === true) return (
      <Group gap={6}>
        <IconCheck size={18} color="var(--mantine-color-green-6)" />
        <Text size="sm" c="green" fw={500} style={{ whiteSpace: 'nowrap' }}>Syntax OK</Text>
      </Group>
    );
    
    if (errors.length > 0) {
      const firstError = errors[0];
      return (
        <Tooltip label={errors.map(e => `Line ${e.line}: ${e.message}`).join('\n')} multiline position="bottom-end">
          <Group gap={6} style={{ cursor: 'pointer', maxWidth: '100%' }} wrap="nowrap" onClick={() => focusLine(firstError.line)}>
            <IconAlertCircle size={18} color="var(--mantine-color-red-6)" style={{ minWidth: 18 }} />
            <Text size="sm" c="red" fw={500} lineClamp={1}>
              Line {firstError.line}: <span dangerouslySetInnerHTML={{ __html: DOMPurify.sanitize(firstError.message) }} />
              {errors.length > 1 && ` (+${errors.length - 1})`}
            </Text>
          </Group>
        </Tooltip>
      );
    }
    
    return <Badge color="gray" variant="light">Ready</Badge>;
  }, [hasLoaded, isCheckingSyntax, syntaxOk, errors]);

  useEffect(() => {
    return () => {
      if (conditionClickTimeoutRef.current !== null) {
        window.clearTimeout(conditionClickTimeoutRef.current);
      }
    };
  }, []);

  if (isLoading) return <Center h={400}><Loader size="xl" /></Center>;
  if (error) return <Alert color="red" title="Error">Could not load sites</Alert>;

  const editorPanel = (
    <div
      style={{
        border: '1px solid var(--mantine-color-default-border)',
        borderRadius: 'var(--mantine-radius-md)',
        overflow: 'hidden',
        height: 'calc(100vh - 300px)',
        minHeight: '500px',
        background: colorScheme === 'dark' ? '#1a1b1e' : '#ffffff',
      }}
    >
      <Editor
        height="100%"
        language="slftp-rules"
        theme={colorScheme === 'dark' ? 'slftp-theme-dark' : 'slftp-theme-light'}
        value={rtplContent}
        onChange={(value) => setRtplContent(value || '')}
        beforeMount={handleEditorWillMount}
        onMount={handleEditorDidMount}
        options={{
          minimap: { enabled: false },
          scrollBeyondLastLine: false,
          fontSize: 13,
          fontFamily: 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
          wordWrap: 'on',
          automaticLayout: true,
          renderWhitespace: 'none',
          occurrencesHighlight: 'singleFile',
          selectionHighlight: true,
          renderLineHighlight: 'all',
          matchBrackets: 'always',
          bracketPairColorization: { enabled: true, independentColorPoolPerBracketType: false },
          guides: { bracketPairs: true, bracketPairsHorizontal: false },
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
      </Paper>

      <Paper p="sm" shadow="sm" radius="md" withBorder h="100%">
        <Tabs value={activeTab} onChange={setActiveTab} keepMounted={false}>
          <Group justify="space-between" align="center" mb="xs" wrap="nowrap">
            <Tabs.List style={{ borderBottom: 'none', flexShrink: 0 }}>
              <Tabs.Tab value="editor" leftSection={<IconCode size="0.8rem" />}>Editor</Tabs.Tab>
              <Tabs.Tab value="snapshot" leftSection={<IconFileText size="0.8rem" />}>Snapshot (Site Rules)</Tabs.Tab>
              <Tabs.Tab value="conditions" leftSection={<IconSearch size="0.8rem" />}>Conditions</Tabs.Tab>
              <Tabs.Tab value="examples" leftSection={<IconBulb size="0.8rem" />}>Examples</Tabs.Tab>
            </Tabs.List>
            
            <Group gap="xs" pr="xs" style={{ flexGrow: 1, justifyContent: 'flex-end', minWidth: 0 }}>
              {statusDisplay}
            </Group>
          </Group>
          
          <Divider mb="xs" />

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
                   Click inserts a quick rule line. Double-click inserts a full example rule block.
                </Text>

                <ScrollArea style={{ height: 'calc(100vh - 400px)', minHeight: '400px' }}>
                  <Grid gutter="sm">
                    {filteredConditions.map((c) => (
                      <Grid.Col key={c.name} span={{ base: 12, md: 6, lg: 4 }}>
                        {(() => {
                          const conditionExample = getConditionExample(c);
                          const shortExample = conditionExample.split('\n')[0];
                          return (
                        <Paper
                          withBorder
                          p="sm"
                          radius="sm"
                          style={{ cursor: 'pointer', transition: 'background-color 0.2s', height: '100%' }}
                          onClick={() => handleConditionCardClick(c)}
                          onDoubleClick={() => handleConditionCardDoubleClick(c)}
                          className="condition-card"
                        >
                          <Group justify="space-between" align="start" wrap="nowrap" mb={4}>
                             <Text size="sm" fw={700} style={{ fontFamily: 'monospace' }}>{c.name}</Text>
                             <IconArrowRight size="0.8rem" style={{ opacity: 0.5 }} />
                          </Group>
                          <Text size="xs" c="dimmed" lh={1.3}>
                            {c.description}
                          </Text>
                          <Tooltip label={conditionExample} withArrow multiline w={420}>
                            <Text size="xs" mt={6} style={{ fontFamily: 'monospace' }} c="blue">
                              {shortExample}
                            </Text>
                          </Tooltip>
                        </Paper>
                          );
                        })()}
                      </Grid.Col>
                    ))}
                  </Grid>
                  {filteredConditions.length === 0 && (
                    <Text size="sm" c="dimmed" ta="center" py="xl">No matches found</Text>
                  )}
                </ScrollArea>
             </Stack>
          </Tabs.Panel>

          <Tabs.Panel value="examples">
             <RulesExamples conditions={conditions || []} />
          </Tabs.Panel>
        </Tabs>
      </Paper>
    </Stack>
  );
}
