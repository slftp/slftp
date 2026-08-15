import {
  Alert,
  Badge,
  Button,
  Center,
  Divider,
  Grid,
  Group,
  Loader,
  Modal,
  Paper,
  ScrollArea,
  Select,
  Stack,
  Tabs,
  Text,
  TextInput,
  Title,
  Tooltip,
  ActionIcon,
  ThemeIcon,
  Code,
  useMantineColorScheme,
} from '@mantine/core';
import { useMutation, useQuery } from '@tanstack/react-query';
import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { useSearchParams } from 'react-router-dom';
import { notifications } from '@mantine/notifications';
import { IconCheck, IconCode, IconDeviceFloppy, IconFileText, IconRefresh, IconSearch, IconAlertCircle, IconArrowRight, IconBulb } from '@tabler/icons-react';
import Editor from '@monaco-editor/react';
import type { Monaco } from '@monaco-editor/react';
import type { editor as MonacoEditor, Position as MonacoPosition } from 'monaco-editor';
import DOMPurify from 'dompurify';
import { apiClient } from '../api/client';
import type { Site } from '../api/client';
import { RulesExamples } from '../components/RulesExamples';

// ponytail: this repo has no density/style theme switch (unlike the branch this
// editor was ported from) — fixed "md" sizing and the "modern" (glow/gradient)
// look are used unconditionally. Reintroduce isClassic/currentDensity if/when
// this repo grows an equivalent theme setting.
const EDITOR_FONT = { fontSize: 13.5, lineHeight: 22 };

type RuleError = { line: number; message: string };
type RuleCondition = { name: string; ops: string; description: string; values?: string };

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

// Clause connectors offered after a value/regex. slftp's parser only accepts the
// symbolic forms (TAndOperator.Name='&&', TOrOperator.Name='||'); the words
// "and"/"or" do NOT parse, so we suggest && / || (real rules use these exclusively).
const CLAUSE_CONNECTORS = [
  { label: '&&', insert: '&& ', doc: 'AND — both conditions must match.' },
  { label: '||', insert: '|| ', doc: 'OR — either condition matches.' },
  { label: 'then', insert: 'then ', doc: 'End the conditions and specify the action (ALLOW/DROP).' },
];

// Fallback condition names used for syntax highlighting until the backend list loads
const FALLBACK_CONDITION_NAMES = ['group', 'age', 'releasename', 'section', 'tag', 'year', 'mp3language', 'mp3year', 'mp3numdisks', 'imdblookupdone', 'imdblanguages', 'imdbgenre', 'imdbgenres', 'imdbrating', 'imdbyear', 'imdbvotes', 'imdbcountry', 'tvlookupdone', 'tvlanguage', 'tvcountry', 'tvgenres', 'tvscripted', 'tvrunning', 'tvstatus', 'tvclassification', 'tvepisodeagedays', 'tvseason', 'tvep', 'nfolookupdone', 'internal', 'files', 'size', 'disk', 'kb'];

// Builds the Monarch tokenizer; condition names and section names are dynamic so
// every backend condition gets the 'variable' color and every known section the
// 'section' color (a misspelled section stays neutral = built-in validation).
function buildSlftpTokenizer(condNames: string[], sectionNames: string[] = [], siteNames: string[] = []) {
  const names = (condNames.length ? condNames : FALLBACK_CONDITION_NAMES)
    .filter((n) => /^[a-z0-9]+$/i.test(n))
    .sort((a, b) => b.length - a.length); // longest first so e.g. tvepisodeagedays wins over tvep
  const secs = sectionNames
    .filter((n) => /^[\w-]+$/.test(n))
    .sort((a, b) => b.length - a.length); // longest first so TV-1080P wins over TV
  const sectionRule: [RegExp, string][] = secs.length
    ? [[new RegExp('\\b(' + secs.join('|') + ')\\b', 'i'), 'section']]
    : [];
  const sites = siteNames
    .filter((n) => /^[\w-]+$/.test(n))
    .sort((a, b) => b.length - a.length);
  // a rule line starts with the sitename; color known sites so an existing/valid
  // one is visible (a typo stays neutral and is flagged by the inline markers).
  const siteRule: [RegExp, string][] = sites.length
    ? [[new RegExp('\\b(' + sites.join('|') + ')\\b', 'i'), 'site']]
    : [];
  return {
    tokenizer: {
      root: [
        ...siteRule,
        // "section in/notin ..." routes to a section-aware list (@sectionList):
        // known sections stay gold, unknown ones stay NEUTRAL (not red) and are
        // flagged by a red marker instead — same treatment as an invalid sitename.
        [/\bsection\b(?=\s+(?:not)?in\b)/i, { token: 'variable', next: '@sectionPre' }],
        // any other 'in'/'notin' opens a generic value list → @inList (values red,
        // commas yellow), the same visual language used inside a /regex/.
        [/\b(in|notin)\b/i, { token: 'keyword', next: '@inList' }],
        // NOTE: 'and'/'or' are deliberately NOT keywords — the parser only accepts
        // '&&'/'||'; leaving them uncolored (plus a red marker) shows they're invalid.
        [/\b(if|then|not)\b/i, 'keyword'],
        [/\b(DROP|ALLOW)\b/i, 'type'],
        [/\b(default)\b/i, 'constant'],
        [new RegExp('\\b(' + names.join('|') + ')\\b', 'i'), 'variable'],
        // known sections get their own color; must precede the generic identifier
        // rule below so a valid section isn't swallowed as a neutral operand.
        ...sectionRule,
        [/#.*$/, 'comment'],
        [/"[^"]*"/, 'string'],
        [/'[^']*'/, 'string'],
        // operand identifiers (release values / unknown sections like X264, 0DAY,
        // TV-1080P): any word/dash run containing at least one letter, consumed
        // whole BEFORE the number rule. Monaco re-anchors each rule at the cursor,
        // so without this the "264" in "X264" would start a fresh match and be
        // colored as a number. Pure-digit tokens fall through to 'number'.
        [/[\w-]*[A-Za-z][\w-]*/, ''],
        [/\b\d+\b/, 'number'],
        [/[{}()[\]]/, '@brackets'],
        [/[/\\]/, 'delimiter'],
        [/!~|=~/, { token: 'operator', next: '@afterRegexOp' }],
        [/[<>!=~]+/, 'operator'],
        [/&&|\|\|/, 'operator'],
        [/\*/, 'operator'], // section/site wildcard → same purple as operators
      ],
      afterRegexOp: [
        [/\s+/, 'white'],
        [/\/(?!\s)/, { token: 'delimiter.regexp', next: '@regex' }],
        [/./, { token: '', next: '@pop' }],
      ],
      regex: [
        [/[^\\/()[\]{}]+/, 'regexp'],
        [/\\./, 'regexp'],
        [/[(){}[\]]/, '@brackets'],
        // '@popall' (not '@pop') returns straight to 'root' after the closing
        // slash; popping only one level would leave us in 'afterRegexOp', whose
        // catch-all then swallows the first '&' of a following '&&' (so it would
        // not be colored). popall keeps && / || highlighted after a regex.
        [/\/[gimuy]*/, { token: 'delimiter.regexp', next: '@popall' }],
      ],
      // value list after in/notin: entries red ('regexp'), separators yellow
      // ('delimiter.regexp') — same palette as a /regex/. Closes back to root on
      // 'then', '&&'/'||' or a closing ')', so it never bleeds past the clause.
      inList: [
        [/[ \t]+/, 'white'],
        [/,/, 'list.comma'],
        [/\b(then)\b/i, { token: 'keyword', next: '@popall' }],
        [/&&|\|\|/, { token: 'operator', next: '@popall' }],
        [/\)/, { token: '@brackets', next: '@popall' }],
        [/[^\s,)]+/, 'regexp'],
      ],
      // bridge: we consumed the 'section' condition; consume the in/notin operator
      // and enter the section-aware list.
      sectionPre: [
        [/[ \t]+/, 'white'],
        [/\b(?:not)?in\b/i, { token: 'keyword', next: '@sectionList' }],
        [/./, { token: '@rematch', next: '@pop' }],
      ],
      // section value list: known sections gold, unknown ones neutral (a red
      // marker flags them), commas yellow. Closes on then / && / || / ).
      sectionList: [
        [/[ \t]+/, 'white'],
        [/,/, 'list.comma'],
        [/\b(then)\b/i, { token: 'keyword', next: '@popall' }],
        [/&&|\|\|/, { token: 'operator', next: '@popall' }],
        [/\)/, { token: '@brackets', next: '@popall' }],
        ...sectionRule,
        [/[^\s,)]+/, ''],
      ],
    },
  };
}

export function Rules() {
  const { colorScheme } = useMantineColorScheme();
  const editorFont = EDITOR_FONT;
  const [searchParams, setSearchParams] = useSearchParams();
  const [siteName, setSiteName] = useState<string>('');
  const [rtplContent, setRtplContent] = useState('');
  const [rtplMd5, setRtplMd5] = useState('');
  const [reloadPromptOpen, setReloadPromptOpen] = useState(false);
  const [rtplPath, setRtplPath] = useState('');
  const [siteRulesSnapshotContent, setSiteRulesSnapshotContent] = useState('');
  const [siteRulesSnapshotPath, setSiteRulesSnapshotPath] = useState('');
  const [errors, setErrors] = useState<RuleError[]>([]);
  const [syntaxOk, setSyntaxOk] = useState<boolean | null>(null);
  const [isCheckingSyntax, setIsCheckingSyntax] = useState(false);
  const [conditionSearch, setConditionSearch] = useState('');
  
  const editorRef = useRef<MonacoEditor.IStandaloneCodeEditor | null>(null);
  const monacoRef = useRef<Monaco | null>(null);
  const conditionClickTimeoutRef = useRef<number | null>(null);
  const conditionsRef = useRef<RuleCondition[]>([]);
  const sectionsRef = useRef<string[]>([]);
  const siteNamesRef = useRef<string[]>([]);
  const disposablesRef = useRef<{ dispose: () => void }[]>([]);
  const tokensDisposableRef = useRef<{ dispose: () => void } | null>(null);
  
  const [hasLoaded, setHasLoaded] = useState(false);
  // The site whose rtpl is actually in the editor. Only set by "Load rtpl" —
  // the Select can point elsewhere without affecting validation or Save, so
  // switching the dropdown neither red-flags the loaded content nor lets a
  // Save write the old content under the newly selected site.
  const [loadedSite, setLoadedSite] = useState('');
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

  // Uppercased valid site names (the rule parser uppercases the sitename token);
  // used both for highlighting and for the "rule must start with a valid/edited
  // site" marker check.
  const siteNames = useMemo(
    () => (sites || []).filter((s) => s.name.toLowerCase() !== 'slftp').map((s) => s.name.toUpperCase()),
    [sites],
  );

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

  // All sections defined in slftp (kb_sections) → used to color correctly-spelled
  // sections and, by omission, leave typos neutral (a lightweight validation hint).
  const { data: sections } = useQuery({
    queryKey: ['available-sections'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSitesService/GetAvailableSections', {});
      const raw = res.data.result?.[0] || res.data;
      const arr = typeof raw === 'string' ? JSON.parse(raw) : raw;
      return Array.isArray(arr) ? (arr as string[]) : [];
    },
    refetchInterval: 60000,
    refetchOnWindowFocus: false,
  });

  useEffect(() => {
    if (!siteName && siteOptions.length > 0) setSiteName(siteOptions[0].value);
  }, [siteName, siteOptions]);

  // Keep the completion/hover providers reading fresh conditions (they are
  // registered once in beforeMount, so they must read via a ref, not a closure).
  useEffect(() => {
    conditionsRef.current = conditions || [];
  }, [conditions]);

  useEffect(() => {
    sectionsRef.current = sections || [];
  }, [sections]);

  useEffect(() => {
    siteNamesRef.current = siteNames;
  }, [siteNames]);

  // Re-register syntax highlighting once the backend condition/section/site lists
  // are available, so every condition name, known section and known site gets its
  // color (not just the hardcoded fallback).
  useEffect(() => {
    const monaco = monacoRef.current;
    if (!monaco || !conditions || conditions.length === 0) return;
    tokensDisposableRef.current?.dispose?.();
    tokensDisposableRef.current = monaco.languages.setMonarchTokensProvider(
      'slftp-rules',
      buildSlftpTokenizer(conditions.map((c) => c.name), sections || [], siteNames),
    );
  }, [conditions, sections, siteNames]);

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
    onSuccess: ({ rtplInfo, snapInfo }, selectedSite) => {
      setErrors([]);
      setLoadedSite(selectedSite);
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
  }, [searchParams, siteOptions, siteName, loadMutation, setSearchParams]);

  const handleEditorWillMount = (monaco: Monaco) => {
    // Register a new language
    monaco.languages.register({ id: 'slftp-rules' });
    monaco.languages.setLanguageConfiguration('slftp-rules', {
      // release-style tokens (X264-1080P-GROUP) contain '-'; Monaco's default
      // wordPattern treats '-' as a separator, which breaks autocomplete's word
      // range mid-token (e.g. deleting the last char of "X264-1080P-X" only
      // targets "X", not the whole value). Keep everything but whitespace/
      // separators/operators as one word instead.
      wordPattern: /[^\s,()&|!=<>~]+/,
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

    // Register a (condition-aware) tokens provider for the language
    tokensDisposableRef.current = monaco.languages.setMonarchTokensProvider(
      'slftp-rules',
      buildSlftpTokenizer(conditionsRef.current.map((c) => c.name), sectionsRef.current, siteNamesRef.current),
    );

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
        // Harmonized "One Dark"-aligned palette: roles unchanged, but all hues
        // share a consistent saturation/brightness so the editor reads as one set.
        { token: 'keyword', foreground: 'd19a66' },   // if/then/in/notin/not — warm orange
        { token: 'type', foreground: '56b6c2' },      // ALLOW/DROP — cyan
        { token: 'constant', foreground: '61afef' },  // default — blue
        { token: 'variable', foreground: '61afef' },  // conditions — same blue as default
        { token: 'site', foreground: '3fdf6b', fontStyle: 'bold' },     // sites — vivid green (stands out from gold sections)
        { token: 'list.comma', foreground: 'e5c07b', fontStyle: 'bold' }, // list separators — soft yellow (no underline)
        { token: 'section', foreground: 'dcdcaa', fontStyle: 'bold' },   // sections — pale gold
        { token: 'comment', foreground: '7f848e' },   // muted grey
        { token: 'string', foreground: 'ce9178' },
        { token: 'regexp', foreground: 'e06c75' },     // regex/list values — red
        { token: 'number', foreground: 'b5cea8' },
        { token: 'operator', foreground: 'c678dd', fontStyle: 'bold' },  // operators + '*' — purple
        { token: 'delimiter', foreground: 'abb2bf' },
        { token: 'delimiter.regexp', foreground: 'e5c07b', fontStyle: 'bold underline' },
        { token: 'delimiter.regexp.slftp-rules', foreground: 'e5c07b', fontStyle: 'bold underline' },
      ],
      colors: {
        'editor.background': '#1a1b1e',
        'editor.selectionBackground': '#3e4451',
        'editor.lineHighlightBackground': '#22242a',
        'editorCursor.foreground': '#61afef',
        'editorLineNumber.foreground': '#4b5263',
        'editorLineNumber.activeForeground': '#abb2bf',
        'editorWhitespace.foreground': '#3b3a32',
        'editorIndentGuide.background': '#2c2e34',
        'editorIndentGuide.activeBackground': '#3e4451',
        'editorSelectionHighlightBackground': '#abb2bf26',
        'editorBracketHighlightForeground1': '#d19a66',
        'editorBracketHighlightForeground2': '#56b6c2',
        'editorBracketHighlightForeground3': '#61afef',
        'editorBracketHighlightForeground4': '#98c379',
        'editorBracketHighlightForeground5': '#c678dd',
        'editorBracketHighlightForeground6': '#e06c75',
        'editorBracketPairGuide.activeBackground1': '#d19a6640',
        'editorBracketPairGuide.activeBackground2': '#56b6c240',
        'editorBracketPairGuide.activeBackground3': '#61afef40',
        'editorBracketPairGuide.activeBackground4': '#98c37940',
        'editorBracketPairGuide.activeBackground5': '#c678dd40',
        'editorBracketPairGuide.activeBackground6': '#e06c7540',
      }
    });

    // Define Light theme
    monaco.editor.defineTheme('slftp-theme-light', {
      base: 'vs',
      inherit: true,
      rules: [
        ...commonRules,
        // Harmonized "One Light"-aligned palette (same roles as the dark theme).
        { token: 'keyword', foreground: 'b25000' },   // if/then/in/notin/not — warm orange
        { token: 'type', foreground: '0184bc' },      // ALLOW/DROP — cyan
        { token: 'constant', foreground: '4078f2' },  // default — blue
        { token: 'variable', foreground: '4078f2' },  // conditions — same blue as default
        { token: 'site', foreground: '1a7f37', fontStyle: 'bold' },     // sites — vivid green (stands out from olive sections)
        { token: 'list.comma', foreground: '9a6700', fontStyle: 'bold' }, // list separators — amber (no underline)
        { token: 'section', foreground: '795e26', fontStyle: 'bold' },   // sections — olive
        { token: 'comment', foreground: 'a0a1a7' },   // muted grey
        { token: 'string', foreground: 'a31515' },
        { token: 'regexp', foreground: 'e45649' },     // regex/list values — red
        { token: 'number', foreground: '098658' },
        { token: 'operator', foreground: 'a626a4', fontStyle: 'bold' },  // operators + '*' — purple
        { token: 'delimiter', foreground: '696c77' },
        { token: 'delimiter.regexp', foreground: '9a6700', fontStyle: 'bold underline' },
        { token: 'delimiter.regexp.slftp-rules', foreground: '9a6700', fontStyle: 'bold underline' },
      ],
      colors: {
        'editor.background': '#ffffff',
        'editor.lineHighlightBackground': '#f3f4f6',
        'editorCursor.foreground': '#4078f2',
        'editorLineNumber.foreground': '#9d9d9f',
        'editorLineNumber.activeForeground': '#383a42',
        'editorIndentGuide.activeBackground': '#c2c2c3',
        'editorBracketHighlightForeground1': '#b25000',
        'editorBracketHighlightForeground2': '#0184bc',
        'editorBracketHighlightForeground3': '#4078f2',
        'editorBracketHighlightForeground4': '#50a14f',
        'editorBracketHighlightForeground5': '#a626a4',
        'editorBracketHighlightForeground6': '#e45649',
        'editorBracketPairGuide.activeBackground1': '#b2500040',
        'editorBracketPairGuide.activeBackground2': '#0184bc40',
        'editorBracketPairGuide.activeBackground3': '#4078f240',
        'editorBracketPairGuide.activeBackground4': '#50a14f40',
        'editorBracketPairGuide.activeBackground5': '#a626a440',
        'editorBracketPairGuide.activeBackground6': '#e4564940',
      }
    });

    // --- Phase 1: context-aware autocomplete (conditions/operators/actions) ---
    const completionDisposable = monaco.languages.registerCompletionItemProvider('slftp-rules', {
      triggerCharacters: [' ', '=', '!', '<', '>', '~', '/', '('],
      provideCompletionItems: (model: MonacoEditor.ITextModel, position: MonacoPosition) => {
        const conds = conditionsRef.current;
        const word = model.getWordUntilPosition(position);
        const range = {
          startLineNumber: position.lineNumber,
          endLineNumber: position.lineNumber,
          startColumn: word.startColumn,
          endColumn: word.endColumn,
        };
        const prefix = model.getValueInRange({
          startLineNumber: position.lineNumber,
          startColumn: 1,
          endLineNumber: position.lineNumber,
          endColumn: position.column,
        });
        const Kind = monaco.languages.CompletionItemKind;
        const SnippetRule = monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet;
        const suggestions: unknown[] = [];

        // after "then " → actions
        if (/\bthen\s+\S*$/i.test(prefix)) {
          for (const a of ['ALLOW', 'DROP']) {
            suggestions.push({ label: a, kind: Kind.Keyword, insertText: a, range });
          }
          return { suggestions };
        }

        // Locate the cursor within the clause grammar:
        //   <condition> <operator> <value> [&& | || …]  then  <action>
        // '&&'/'||' are the real combiners; 'and'/'or' kept only so a clause still
        // resets if a user typed those (even though the parser rejects them).
        // '(' opens a grouped sub-expression, so it also starts a fresh clause →
        // a condition can be suggested right after "( ".
        const byName = new Map(conds.map((c) => [c.name.toLowerCase(), c] as const));
        const connectors = new Set(['if', 'and', 'or', 'not', '&&', '||', '(']);
        const allTokens = prefix.trim().split(/\s+/).filter(Boolean);
        const endsWithSpace = /\s$/.test(prefix) || prefix.trim() === '';
        // tokens already completed (exclude the partial word being typed now)
        const completed = endsWithSpace ? allTokens : allTokens.slice(0, -1);
        // current clause = tokens since the last connector / line start
        let lastConnector = '';
        const clause: string[] = [];
        for (let i = completed.length - 1; i >= 0; i--) {
          const t = completed[i].toLowerCase();
          if (connectors.has(t) || t === 'then') { lastConnector = t; break; }
          clause.unshift(completed[i]);
        }

        // after "then" → actions
        if (lastConnector === 'then') {
          for (const a of ['ALLOW', 'DROP']) {
            suggestions.push({ label: a, kind: Kind.Keyword, insertText: a, range });
          }
          return { suggestions };
        }

        const clauseCond = clause.length >= 1 ? byName.get(clause[0].toLowerCase()) : undefined;
        const hasOperator = clause.length >= 2; // condition + operator already present

        // condition slot: nothing typed yet in this clause → condition catalogue
        if (clause.length === 0) {
          suggestions.push({
            label: 'default',
            kind: Kind.Constant,
            insertText: 'default then ALLOW',
            documentation: 'Fallback rule — matches when no other rule did.',
            range,
          });
          for (const c of conds) {
            suggestions.push({
              label: c.name,
              kind: Kind.Property,
              insertText: c.name + ' ',
              detail: c.ops || undefined,
              documentation: c.description || undefined,
              range,
              // auto-open the operator list right after picking a condition
              command: { id: 'editor.action.triggerSuggest', title: 'suggest' },
            });
          }
          return { suggestions };
        }

        // operator slot: a known condition is typed but no operator yet.
        // The user may have already typed part of the operator (=, !, <, >, ~);
        // replace that run rather than append (so "=" + "=~" ≠ "==~").
        if (clauseCond && !hasOperator) {
          const opMatch = prefix.match(/[=!<>~]+$/);
          const typed = opMatch ? opMatch[0] : '';
          const opRange = {
            startLineNumber: position.lineNumber,
            endLineNumber: position.lineNumber,
            startColumn: opMatch ? position.column - typed.length : range.startColumn,
            endColumn: position.column,
          };
          const ops = (clauseCond.ops || '').trim().split(/\s+/)
            .filter((op) => op && (!typed || op.startsWith(typed)));
          // boolean conditions take no operator/value (e.g. "if tvscripted then …")
          // → ops is empty; fall through to offer the connectors below.
          if (ops.length > 0) {
            const hasValues = (clauseCond.values || '').trim() !== '';
            // preferred order: pattern pair first, then list pair, then exact pair
            // (=~ before =, !~ before !=) — grouped by pairs and by how often used.
            const OP_ORDER = ['=~', '!~', 'in', 'notin', '=', '!='];
            for (const op of ops) {
              const isRegexOp = op === '=~' || op === '!~';
              const rank = OP_ORDER.indexOf(op);
              const item: Record<string, unknown> = {
                label: op,
                kind: Kind.Operator,
                // regex operators just insert "op " and open the regex-template
                // list (see value slot); "in/notin" insert "op " and open the
                // value list; enumerable "=" likewise. No hardcoded skeleton here.
                insertText: op + ' ',
                // filterText = the exact text already in the range, so Monaco never
                // drops the item (operator chars confuse its fuzzy matcher); we have
                // pre-filtered the list ourselves via startsWith(typed) above.
                filterText: typed || op,
                sortText: (rank < 0 ? 99 : rank).toString().padStart(2, '0'),
                range: opRange,
              };
              // auto-open the follow-up list after the operator: regex templates for
              // =~/!~, otherwise the value list when the condition is enumerable.
              if (isRegexOp || hasValues) {
                item.command = { id: 'editor.action.triggerSuggest', title: 'suggest' };
              }
              suggestions.push(item);
            }
            return { suggestions };
          }
        }

        // regex value slot: after a =~/!~ operator, offer ready-made /pattern/i
        // skeletons. Most rules use a separator-bounded alternation, so that is
        // the first (selected) template; a plain pattern is the fallback. The
        // ${1:…} tabstop lands pre-selected, so the user just types e.g.
        // "Foo|Bar" over it. Triggered both by picking the operator and by '/'.
        // clause.length === 2 means condition + operator are in, but no value yet
        // (a fully typed /regex/ becomes clause[2], so this won't re-fire then).
        const opTok = clause.length === 2 ? clause[1].toLowerCase() : '';
        if (opTok === '=~' || opTok === '!~') {
          // if a leading "/" run is already typed, replace it rather than append
          const slash = prefix.match(/\/\S*$/);
          const rxRange = slash
            ? { startLineNumber: position.lineNumber, endLineNumber: position.lineNumber,
                startColumn: position.column - slash[0].length, endColumn: position.column }
            : range;
          const templates = [
            { label: '/[-._](…)[-._]/i — bounded',
              insert: '/[-._](${1:Value1|Value2})[-._]/i',
              doc: 'Matches one of Value1|Value2 bounded by a dot, dash or underscore (the common form).' },
            { label: '/^(…)$/i — exact match',
              insert: '/^(${1:Value1|Value2})$/i',
              doc: 'Matches only if the whole value equals one of Value1|Value2 — no partial/surrounding text allowed.' },
            { label: '/…/i — free-form',
              insert: '/${1:pattern}/i',
              doc: 'A free-form case-insensitive regex, no template structure.' },
          ];
          templates.forEach((t, i) => suggestions.push({
            label: t.label, kind: Kind.Snippet, insertText: t.insert,
            insertTextRules: SnippetRule, documentation: t.doc, sortText: '0' + i, range: rxRange,
          }));
          CLAUSE_CONNECTORS.forEach((k, i) => suggestions.push({
            label: k.label, kind: Kind.Keyword, insertText: k.insert,
            documentation: k.doc, sortText: '1' + i, range,
          }));
          return { suggestions };
        }

        // value slot (condition + operator present) → offer the condition's
        // enumerable values (if any) + connectors; never conditions (so typing a
        // value like "1080P" does not suggest "pre…").
        // sortText prefixes keep the enumerable values grouped at the top ('0…')
        // and the structural connectors below them ('1…'); without this Monaco
        // sorts alphabetically and interleaves "and/or/then" among the values.
        // Only offer the value list at a *fresh* value position: right after the
        // operator (in/notin/=/!=) or after a comma — not after an already-entered
        // value followed by a space (there the user wants a connector, not values).
        const lastTok = completed[completed.length - 1] || '';
        const freshValue = /,$/.test(lastTok)
          || ['in', 'notin', '=', '==', '!='].includes(lastTok.toLowerCase());
        const accepted = (clauseCond?.values || '').trim();
        if (accepted && freshValue) {
          // don't re-suggest values already present earlier in this same list
          // (e.g. "section in SX, X264, " shouldn't offer SX/X264 again)
          const alreadyUsed = new Set(
            clause.slice(2).join(' ').split(',').map((s) => s.trim().toUpperCase()).filter(Boolean),
          );
          for (const v of accepted.split(',').map((s) => s.trim()).filter(Boolean)) {
            if (alreadyUsed.has(v.toUpperCase())) continue;
            // no trailing space: lets the user continue with "," (list) or " " (done)
            // in a single keystroke, instead of having to delete a space first
            suggestions.push({ label: v, kind: Kind.Value, insertText: v, sortText: '0' + v, range });
          }
        }
        CLAUSE_CONNECTORS.forEach((k, i) => suggestions.push({
          label: k.label, kind: Kind.Keyword, insertText: k.insert,
          documentation: k.doc, sortText: '1' + i, range,
        }));
        return { suggestions };
      },
    });

    // --- Phase 3 (hover): show a condition's operators + description on hover ---
    const hoverDisposable = monaco.languages.registerHoverProvider('slftp-rules', {
      provideHover: (model: MonacoEditor.ITextModel, position: MonacoPosition) => {
        const word = model.getWordAtPosition(position);
        if (!word) return null;
        const c = conditionsRef.current.find((x) => x.name.toLowerCase() === word.word.toLowerCase());
        if (!c) return null;
        const contents = [{ value: '**' + c.name + '**' }];
        if (c.ops && c.ops.trim()) contents.push({ value: '`' + c.ops.trim() + '`' });
        if (c.description) contents.push({ value: c.description });
        return {
          range: new monaco.Range(position.lineNumber, word.startColumn, position.lineNumber, word.endColumn),
          contents,
        };
      },
    });

    disposablesRef.current.push(completionDisposable, hoverDisposable);
  };

  const handleEditorDidMount = (editor: MonacoEditor.IStandaloneCodeEditor, monaco: Monaco) => {
    editorRef.current = editor;
    monacoRef.current = monaco;
  };

  const insertAtCursorOrReplaceSelection = (insertText: string) => {
    const editor = editorRef.current;
    const canEditInMonaco = activeTab === 'editor' && !!editor && !!editor.getModel?.();

    if (!canEditInMonaco) {
      setRtplContent((prev) => prev + insertText);
      return;
    }

    try {
      const selection = editor.getSelection();
      if (!selection) throw new Error('no selection');
      const op = { range: selection, text: insertText, forceMoveMarkers: true };
      editor.executeEdits('my-source', [op]);
      editor.focus();
      setRtplContent(editor.getValue());
    } catch {
      setRtplContent((prev) => prev + insertText);
    }
  };

  const focusLine = useCallback((lineNumber: number) => {
    // Switch to editor tab if not active
    if (activeTab !== 'editor') {
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
  }, [activeTab]);

  const filteredConditions = useMemo(() => {
    const list = conditions || [];
    if (!conditionSearch.trim()) return list;
    const q = conditionSearch.trim().toLowerCase();
    return list.filter((c) => c.name.toLowerCase().includes(q) || c.description.toLowerCase().includes(q));
  }, [conditions, conditionSearch]);

  const getConditionExample = (c: RuleCondition): string => {
    // examples must name the site whose rtpl is in the editor, not the (possibly
    // different) dropdown selection — otherwise inserting one triggers the
    // wrong-site marker.
    return buildConditionExample(c, loadedSite || siteName);
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

  const validateMutation = useMutation({
    mutationFn: async (content: string) => {
      const res = await apiClient.post('/ApiSitesService/ValidateRtpl', { Content: content });
      return res.data.result?.[0] || res.data;
    },
  });

  const saveMutation = useMutation({
    mutationFn: async (reload: boolean) => {
      const res = await apiClient.post('/ApiSitesService/SaveSiteRtpl', { SiteName: loadedSite, Content: rtplContent, ExpectedMd5: rtplMd5, Reload: reload });
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
      // saved to disk, but not yet live in memory → offer to reload right away
      setReloadPromptOpen(true);
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

  const runSyntaxCheck = useCallback((content: string, showToast: boolean) => {
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
  }, [validateMutation]);

  // ref avoids retriggering the debounce on every validateMutation status flip
  const runSyntaxCheckRef = useRef(runSyntaxCheck);
  useEffect(() => {
    runSyntaxCheckRef.current = runSyntaxCheck;
  }, [runSyntaxCheck]);

  useEffect(() => {
    if (!hasLoaded) return;
    const t = setTimeout(() => {
      runSyntaxCheckRef.current(rtplContent, false);
    }, 600);
    return () => clearTimeout(t);
  }, [hasLoaded, rtplContent]);

  // Phase 3: surface parser errors as inline editor markers (red squiggles +
  // hover message), in addition to the header status badge.
  useEffect(() => {
    const monaco = monacoRef.current;
    const editor = editorRef.current;
    if (!monaco || !editor) return;
    const model = editor.getModel();
    if (!model) return;
    const markers = (errors || []).map((e) => {
      const line = Math.max(1, Math.min(e.line || 1, model.getLineCount()));
      return {
        severity: monaco.MarkerSeverity.Error,
        startLineNumber: line,
        endLineNumber: line,
        startColumn: 1,
        endColumn: model.getLineMaxColumn(line),
        message: (e.message || '').replace(/<[^>]*>/g, ''),
      };
    });

    // Hard validation of the leading sitename: every rule line must start with a
    // valid site, and — when editing a concrete site — with exactly that site.
    // The '*' (global) view skips the equality check. Typos/wrong sites → red.
    const validSites = new Set(siteNames);
    const validSections = new Set((sections || []).map((s) => s.toUpperCase()));
    // Conditions with a fixed value set (backend AcceptedValuesAsText -> 'values').
    // Used for a soft "unknown value" warning — NOT a hard error, since some of
    // these lists (languages/countries) are curated and may be incomplete.
    const enumVals = new Map<string, Set<string>>();
    for (const c of conditions || []) {
      // 'section' already gets dedicated hard validation above; skip it here to
      // avoid a duplicate (Error + Warning) marker on the same unknown token.
      if (c.name.toLowerCase() === 'section') continue;
      const vals = (c.values || '').split(',').map((v) => v.trim()).filter(Boolean);
      if (vals.length) enumVals.set(c.name.toLowerCase(), new Set(vals.map((v) => v.toUpperCase())));
    }
    const enumNames = [...enumVals.keys()];
    const expected = loadedSite && loadedSite !== '*' ? loadedSite.toUpperCase() : '';
    const lineCount = model.getLineCount();
    for (let ln = 1; ln <= lineCount; ln++) {
      const raw = model.getLineContent(ln);
      const trimmed = raw.trim();
      if (!trimmed || trimmed.startsWith('#')) continue;
      const first = trimmed.split(/\s+/)[0];
      const firstUp = first.toUpperCase();
      const startCol = raw.indexOf(first) + 1;
      let message = '';
      if (expected && validSites.size > 0) {
        if (firstUp !== expected) {
          message = validSites.has(firstUp)
            ? `Rule targets a different site "${first}" — expected "${expected}" (the site being edited).`
            : `Unknown site "${first}" — expected "${expected}" (the site being edited).`;
        }
      } else if (firstUp !== '*' && !validSites.has(firstUp) && validSites.size > 0) {
        message = `Unknown site "${first}" — no such site is configured.`;
      }
      if (message) {
        markers.push({
          severity: monaco.MarkerSeverity.Error,
          startLineNumber: ln,
          endLineNumber: ln,
          startColumn: startCol,
          endColumn: startCol + first.length,
          message,
        });
      }

      // Validate the section slot (2nd token, e.g. "SX X264-SD-DE if ..."); '*'
      // means all sections. An unknown name gets a red marker like a bad sitename.
      if (validSections.size > 0) {
        const second = trimmed.split(/\s+/)[1];
        const secLc = (second || '').toLowerCase();
        if (second && second !== '*' && secLc !== 'if' && secLc !== 'ifnot'
            && !validSections.has(second.toUpperCase())) {
          const col = raw.indexOf(second, raw.indexOf(first) + first.length) + 1;
          markers.push({
            severity: monaco.MarkerSeverity.Error,
            startLineNumber: ln,
            endLineNumber: ln,
            startColumn: col,
            endColumn: col + second.length,
            message: `Unknown section "${second}" — no such section is configured.`,
          });
        }
      }

      // Validate "section in/notin <list>" members against the known sections;
      // unknown ones (left neutral by the tokenizer) get a red marker here.
      if (validSections.size > 0) {
        const secRe = /\bsection\s+(?:not)?in\s+/gi;
        let m: RegExpExecArray | null;
        while ((m = secRe.exec(raw)) !== null) {
          let pos = m.index + m[0].length;
          while (pos < raw.length) {
            while (pos < raw.length && /\s/.test(raw[pos])) pos++;
            const rest = raw.slice(pos);
            if (!rest || /^(?:then\b|&&|\|\||\))/i.test(rest)) break;
            const tok = (rest.match(/^[^\s,)]+/) || [''])[0];
            if (!tok) break;
            if (!validSections.has(tok.toUpperCase())) {
              markers.push({
                severity: monaco.MarkerSeverity.Error,
                startLineNumber: ln,
                endLineNumber: ln,
                startColumn: pos + 1,
                endColumn: pos + 1 + tok.length,
                message: `Unknown section "${tok}" — no such section is configured.`,
              });
            }
            pos += tok.length;
            while (pos < raw.length && /\s/.test(raw[pos])) pos++;
            if (raw[pos] === ',') { pos++; continue; }
            break;
          }
        }
      }

      // The parser only accepts '&&'/'||' as connectors — a literal "and"/"or"
      // looks plausible (other rule engines use it) but fails to parse. Flag it
      // red right away. Regex bodies and quoted strings are blanked out first so
      // e.g. /and/i or a value containing "or" is not falsely flagged.
      // Connectors are standalone tokens surrounded by whitespace/line ends —
      // require that (not just a \b word boundary), so scene-tag tokens like
      // "-OR-" or "Fast.AND.Furious" (hyphen/dot are non-word chars too) don't
      // false-positive.
      {
        const scrubbed = raw
          .replace(/\/(?:\\.|[^/\\])*\/[a-z]*/gi, (s: string) => ' '.repeat(s.length))
          .replace(/"[^"]*"|'[^']*'/g, (s: string) => ' '.repeat(s.length));
        const connRe = /(?<=^|\s)(and|or)(?=\s|$)/gi;
        let cm: RegExpExecArray | null;
        while ((cm = connRe.exec(scrubbed)) !== null) {
          markers.push({
            severity: monaco.MarkerSeverity.Error,
            startLineNumber: ln,
            endLineNumber: ln,
            startColumn: cm.index + 1,
            endColumn: cm.index + 1 + cm[1].length,
            message: `"${cm[1]}" is not a valid connector — use "${cm[1].toLowerCase() === 'and' ? '&&' : '||'}" instead.`,
          });
        }
      }

      // Soft validation of enumerable condition values ("<cond> = / in / notin
      // <value…>"): values outside the condition's known set get a yellow warning
      // (not an error — these lists may be curated/incomplete). Regex ops skipped.
      if (enumNames.length) {
        const enumRe = new RegExp('\\b(' + enumNames.join('|') + ')\\s+(?:==|!=|=|in|notin)\\b', 'gi');
        let em: RegExpExecArray | null;
        while ((em = enumRe.exec(raw)) !== null) {
          const set = enumVals.get(em[1].toLowerCase());
          if (!set) continue;
          let pos = em.index + em[0].length;
          while (pos < raw.length) {
            while (pos < raw.length && /\s/.test(raw[pos])) pos++;
            const rest = raw.slice(pos);
            if (!rest || /^(?:then\b|&&|\|\||\))/i.test(rest)) break;
            const tok = (rest.match(/^[^\s,)]+/) || [''])[0];
            if (!tok) break;
            if (!set.has(tok.toUpperCase())) {
              markers.push({
                severity: monaco.MarkerSeverity.Warning,
                startLineNumber: ln,
                endLineNumber: ln,
                startColumn: pos + 1,
                endColumn: pos + 1 + tok.length,
                message: `"${tok}" is not a known value for ${em[1]}.`,
              });
            }
            pos += tok.length;
            while (pos < raw.length && /\s/.test(raw[pos])) pos++;
            if (raw[pos] === ',') { pos++; continue; }
            break;
          }
        }
      }
    }
    monaco.editor.setModelMarkers(model, 'slftp-rules', markers);
  }, [errors, syntaxOk, hasLoaded, rtplContent, loadedSite, siteNames, sections, conditions]);

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
        <Tooltip classNames={{ tooltip: 'tip-themed' }} label={errors.map(e => `Line ${e.line}: ${e.message}`).join('\n')} multiline position="bottom-end">
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
  }, [hasLoaded, isCheckingSyntax, syntaxOk, errors, focusLine]);

  useEffect(() => {
    return () => {
      if (conditionClickTimeoutRef.current !== null) {
        window.clearTimeout(conditionClickTimeoutRef.current);
      }
      disposablesRef.current.forEach((d) => d?.dispose?.());
      disposablesRef.current = [];
      tokensDisposableRef.current?.dispose?.();
      tokensDisposableRef.current = null;
    };
  }, []);

  if (isLoading) return <Center h={400}><Loader size="xl" /></Center>;
  if (error) return <Alert color="red" title="Error">Could not load sites</Alert>;

  const editorPanel = (
    <div
      style={{
        border: '1px solid color-mix(in srgb, var(--mantine-primary-color-filled) 60%, var(--mantine-color-default-border))',
        borderRadius: 'var(--mantine-radius-md)',
        overflow: 'hidden',
        height: 'calc(100vh - 300px)',
        minHeight: '500px',
        background: colorScheme === 'dark' ? '#1a1b1e' : '#ffffff',
        transition: 'box-shadow 150ms ease, border-color 150ms ease',
        // soft accent glow around the editor (matches the cbftp / Stats glass-card look)
        boxShadow: '0 0 0 1px color-mix(in srgb, var(--mantine-primary-color-filled) 40%, transparent), 0 0 22px color-mix(in srgb, var(--mantine-primary-color-filled) 45%, transparent)',
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
          // typography: roomier line-height, subtle tracking and ligature-capable
          // mono fonts (used when present on the system, otherwise the stack falls
          // back gracefully) for a calmer, more polished editor surface.
          fontSize: editorFont.fontSize,
          lineHeight: editorFont.lineHeight,
          letterSpacing: 0.3,
          fontLigatures: true,
          fontFamily: '"JetBrains Mono", "Fira Code", ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
          padding: { top: 14, bottom: 14 },
          wordWrap: 'on',
          automaticLayout: true,
          renderWhitespace: 'none',
          occurrencesHighlight: 'singleFile',
          selectionHighlight: true,
          renderLineHighlight: 'all',
          roundedSelection: true,
          smoothScrolling: true,
          cursorBlinking: 'smooth',
          cursorSmoothCaretAnimation: 'on',
          scrollbar: { verticalScrollbarSize: 9, horizontalScrollbarSize: 9, useShadows: false },
          matchBrackets: 'always',
          bracketPairColorization: { enabled: true, independentColorPoolPerBracketType: false },
          guides: { bracketPairs: true, bracketPairsHorizontal: false },
          quickSuggestions: { other: true, comments: false, strings: false },
          quickSuggestionsDelay: 0,
          suggestOnTriggerCharacters: true,
          tabCompletion: 'on',
          wordBasedSuggestions: 'off',
        }}
      />
    </div>
  );

  const snapshotPanel = (
    <Stack gap="xs" h="100%">
       <Group justify="space-between">
         <Text size="sm" fw={500}>Current Site Rules Snapshot</Text>
         {siteRulesSnapshotPath && <Code>{siteRulesSnapshotPath}</Code>}
       </Group>
      {siteRulesSnapshotContent ? (
        // plaintext, not slftp-rules — this is the site's raw SITE RULES text, not rule syntax
        <div
          style={{
            border: '1px solid var(--mantine-color-default-border)',
            borderRadius: 'var(--mantine-radius-md)',
            overflow: 'hidden',
            height: 'calc(100vh - 340px)',
            minHeight: '460px',
            background: colorScheme === 'dark' ? '#1a1b1e' : '#ffffff',
          }}
        >
          <Editor
            height="100%"
            language="plaintext"
            theme={colorScheme === 'dark' ? 'slftp-theme-dark' : 'slftp-theme-light'}
            value={siteRulesSnapshotContent}
            beforeMount={(monaco) => {
              // the main editor (default tab) normally registers the language and
              // themes; guard for the edge case that this tab mounts first.
              if (!monaco.languages.getLanguages().some((l: { id: string }) => l.id === 'slftp-rules')) {
                handleEditorWillMount(monaco);
              }
            }}
            options={{
              readOnly: true,
              domReadOnly: true,
              minimap: { enabled: false },
              scrollBeyondLastLine: false,
              fontSize: editorFont.fontSize,
              lineHeight: editorFont.lineHeight,
              fontLigatures: true,
              fontFamily: '"JetBrains Mono", "Fira Code", ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
              padding: { top: 14, bottom: 14 },
              wordWrap: 'on',
              automaticLayout: true,
              renderLineHighlight: 'none',
              occurrencesHighlight: 'off',
            }}
          />
        </div>
      ) : (
        <Alert color="gray" variant="light" icon={<IconFileText size={18} />}>
          {!hasLoaded
            ? 'Load a site first — the snapshot is fetched together with "Load rtpl".'
            : loadedSite === '*'
              ? 'Global rules (*) have no per-site snapshot — this view only exists for concrete sites.'
              : `No snapshot file exists for "${loadedSite}" yet. slftp writes this file when it generates the active rules for the site; until then there is nothing to show here.`}
        </Alert>
      )}
    </Stack>
  );

  return (
    <Stack gap="md" h="100%" className="fade-in">
      <Modal
        opened={reloadPromptOpen}
        onClose={() => setReloadPromptOpen(false)}
        title="Rules saved"
        centered
        radius="md"
        overlayProps={{ backgroundOpacity: 0.55, blur: 3 }}
      >
        <Stack gap="md">
          <Group gap="sm" wrap="nowrap" align="flex-start">
            <ThemeIcon
              size={40}
              radius="md"
              variant="gradient"
              gradient={{ from: 'blue', to: 'cyan', deg: 135 }}
              className="glow-primary"
            >
              <IconRefresh size={22} />
            </ThemeIcon>
            <Text size="sm">
              Saved to disk — <b>not active yet</b>. Run <Code>Rulesreload</Code> now?
            </Text>
          </Group>
          <Group justify="flex-end" gap="xs">
            <Button variant="default" className="hover-lift" onClick={() => setReloadPromptOpen(false)}>
              Not now
            </Button>
            <Button
              className="glow-primary hover-lift"
              color="blue"
              leftSection={<IconRefresh size="1rem" />}
              loading={reloadMutation.isPending}
              onClick={() => { reloadMutation.mutate(); setReloadPromptOpen(false); }}
            >
              Reload now
            </Button>
          </Group>
        </Stack>
      </Modal>

      <Paper className="vision-card" p="md" radius="md">
        <Group justify="space-between" align="center">
          <Group gap="sm">
            <ThemeIcon
              size={38}
              radius="md"
              variant="gradient"
              gradient={{ from: 'blue', to: 'cyan', deg: 135 }}
              className="glow-primary"
            >
              <IconCode size={22} />
            </ThemeIcon>
            <Title order={3} className="gradient-text">Rules Editor</Title>
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
            <Tooltip classNames={{ tooltip: 'tip-themed' }} label="Load rtpl (Loads the actual file content from the server into this editor)">
              <Button className="hover-lift glow-btn" variant="default" onClick={() => loadMutation.mutate(siteName)} loading={loadMutation.isPending} disabled={!siteName}>
                Load rtpl
              </Button>
            </Tooltip>
            
            <Divider orientation="vertical" />

            <Tooltip classNames={{ tooltip: 'tip-themed' }} label="Syntax check (Checks the current editor content for syntax errors without saving)">
               <ActionIcon className="hover-lift" variant="light" color="blue" size="lg" onClick={() => runSyntaxCheck(rtplContent, true)} loading={isCheckingSyntax} disabled={!hasLoaded}>
                 <IconCheck size="1.2rem" />
               </ActionIcon>
            </Tooltip>

            <Tooltip classNames={{ tooltip: 'tip-themed' }} label="Save (Writes changes to disk)">
               <Button
                  className="hover-lift glow-btn"
                  leftSection={<IconDeviceFloppy size="1rem" />}
                  color="blue"
                  onClick={() => saveMutation.mutate(false)}
                  loading={saveMutation.isPending}
                  disabled={!hasLoaded || syntaxOk === false || isCheckingSyntax}
                >
                  Save
               </Button>
            </Tooltip>

            <Tooltip classNames={{ tooltip: 'tip-themed' }} label="Rulesreload (Triggers !rulesreload: reloads all rules from disk into memory, same as the IRC command)">
               <Button className="hover-lift" variant="subtle" color="gray" leftSection={<IconRefresh size="1rem" />} onClick={() => reloadMutation.mutate()} loading={reloadMutation.isPending}>
                 Rulesreload
               </Button>
            </Tooltip>
          </Group>
        </Group>
      </Paper>

      <Paper className="vision-card no-lift" p="sm" radius="md" h="100%">
        <Tabs variant="pills" classNames={{ tab: 'pill-tab', list: 'pill-tab-list' }} color="blue" value={activeTab} onChange={setActiveTab} keepMounted={false}>
          <Group justify="space-between" align="center" mb="xs" wrap="nowrap">
            <Tabs.List style={{ borderBottom: 'none', flexShrink: 0 }}>
              <Tabs.Tab value="editor" leftSection={<IconCode size="0.8rem" />}>Editor</Tabs.Tab>
              <Tabs.Tab value="snapshot" leftSection={<IconFileText size="0.8rem" />}>Snapshot (Site Rules)</Tabs.Tab>
              <Tabs.Tab value="conditions" leftSection={<IconSearch size="0.8rem" />}>Conditions</Tabs.Tab>
              <Tabs.Tab value="examples" leftSection={<IconBulb size="0.8rem" />}>Examples</Tabs.Tab>
            </Tabs.List>
            
            <Group gap="xs" pr="xs" style={{ flexGrow: 1, justifyContent: 'flex-end', minWidth: 0 }}>
              {hasLoaded && siteName !== loadedSite && (
                <Tooltip classNames={{ tooltip: 'tip-themed' }} label={`The editor still shows the rules of "${loadedSite}". Click "Load rtpl" to switch to "${siteName}".`}>
                  <Badge color="yellow" variant="light" style={{ textTransform: 'none' }}>
                    Editing {loadedSite} — load rtpl for {siteName}
                  </Badge>
                </Tooltip>
              )}
              {statusDisplay}
            </Group>
          </Group>
          
          <Divider mb="xs" />

          <Tabs.Panel value="editor">
            {editorPanel}
            {hasLoaded && (
              <Group justify="flex-end" gap={6} mt={6} wrap="nowrap">
                <IconFileText size={12} style={{ color: 'var(--mantine-color-dimmed)', flexShrink: 0 }} />
                <Text
                  size="xs"
                  c="dimmed"
                  ff="monospace"
                  title={rtplPath}
                  style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}
                >
                  {rtplPath}
                </Text>
              </Group>
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
                          <Tooltip classNames={{ tooltip: 'tip-themed' }} label={conditionExample} withArrow multiline w={420}>
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
