import { Card, Title, Table, Alert, Loader, Center, TextInput, Button, Stack, Group, Text, ScrollArea, Badge, Switch, Tooltip, Modal, MultiSelect, ActionIcon, Breadcrumbs, Box } from '@mantine/core';
import { IconChevronRight, IconSearch, IconDeviceFloppy, IconPin, IconPlus, IconFolderOpen, IconArrowUp, IconRefresh, IconCheck, IconChevronUp, IconChevronDown, IconSelector } from '@tabler/icons-react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useState, useMemo, useRef, useEffect } from 'react';
import { apiClient, fetchBrowserPath, fetchConfigContent } from '../api/client';
import { notifications } from '@mantine/notifications';
import { sortBrowserDirs, type BrowserDirSortBy, type BrowserSortDir } from '../utils/browserDates';

const BROWSER_PENDING_POLL_MS = 120;

interface SectionData {
  section: string;
  dir: string;
}

function toggleBrowserSort(currentBy: BrowserDirSortBy, currentDir: BrowserSortDir, nextBy: BrowserDirSortBy): { by: BrowserDirSortBy; dir: BrowserSortDir } {
  if (currentBy !== nextBy) return { by: nextBy, dir: nextBy === 'modified' ? 'desc' : 'asc' };
  return { by: currentBy, dir: currentDir === 'asc' ? 'desc' : 'asc' };
}

function browserSortIndicator(active: boolean, dir: BrowserSortDir) {
  if (!active) return <IconSelector size="0.9rem" />;
  return dir === 'asc' ? <IconChevronUp size="0.9rem" /> : <IconChevronDown size="0.9rem" />;
}

export function SectionDirectories() {
  const queryClient = useQueryClient();
  const [selectedSite, setSelectedSite] = useState<string | null>(null);
  const [sectionDirs, setSectionDirs] = useState<Record<string, string>>({});
  const [originalDirs, setOriginalDirs] = useState<Record<string, string>>({});
  const [searchQuery, setSearchQuery] = useState('');
  const [showOnlySet, setShowOnlySet] = useState(true);
  const [preserveSection, setPreserveSection] = useState<string | null>(null);
  const [markedSection, setMarkedSection] = useState<string | null>(null);
  const viewportRef = useRef<HTMLDivElement>(null);
  const [browserOpen, setBrowserOpen] = useState(false);
  const [browserPath, setBrowserPath] = useState('/');
  const [browserSortBy, setBrowserSortBy] = useState<BrowserDirSortBy>('modified');
  const [browserSortDir, setBrowserSortDir] = useState<BrowserSortDir>('desc');
  const [quickSection, setQuickSection] = useState<string[]>([]);
  const [quickPath, setQuickPath] = useState('');

  const { data: sitesData, isLoading: sitesLoading } = useQuery({
    queryKey: ['sites'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: '' });
      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }
      const rawSites = responseData.Sites;
      let parsedSites: any[] = [];
      try {
        if (typeof rawSites === 'string') {
          parsedSites = JSON.parse(rawSites);
        } else if (Array.isArray(rawSites)) {
          parsedSites = rawSites;
        }
      } catch (e) {
        console.error('Failed to parse sites JSON', e);
      }
      return parsedSites.filter(site => site.name.toLowerCase() !== 'slftp');
    },
    refetchOnWindowFocus: false,
  });

  const { data: sectionsData, isLoading: sectionsLoading } = useQuery({
    queryKey: ['sections', selectedSite],
    queryFn: async () => {
      if (!selectedSite) return [];
      const res = await apiClient.post('/ApiSitesService/GetSiteSections', { SiteName: selectedSite });

      let sections: SectionData[] = [];
      try {
        if (typeof res.data === 'string') {
          sections = JSON.parse(res.data);
        } else if (Array.isArray(res.data)) {
          sections = res.data;
        } else if (res.data.result) {
          const resultData = Array.isArray(res.data.result) ? res.data.result[0] : res.data.result;
          if (typeof resultData === 'string') {
            sections = JSON.parse(resultData);
          } else if (Array.isArray(resultData)) {
            sections = resultData;
          }
        }
      } catch (e) {
        console.error('Failed to parse sections:', e);
        return [];
      }

      const dirs: Record<string, string> = {};
      sections.forEach(s => {
        dirs[s.section] = s.dir || '';
      });
      setSectionDirs(dirs);
      setOriginalDirs({ ...dirs });

      return sections;
    },
    enabled: !!selectedSite,
    refetchOnWindowFocus: false,
    refetchOnReconnect: false,
  });

  const { data: availableSectionsData } = useQuery({
    queryKey: ['sections-available'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSitesService/GetAvailableSections', {});
      let available: string[] = [];
      try {
        if (typeof res.data === 'string') {
          available = JSON.parse(res.data);
        } else if (Array.isArray(res.data)) {
          available = res.data;
        } else if (res.data.result) {
          const resultData = Array.isArray(res.data.result) ? res.data.result[0] : res.data.result;
          if (typeof resultData === 'string') {
            available = JSON.parse(resultData);
          } else if (Array.isArray(resultData)) {
            available = resultData;
          }
        }
      } catch (e) {
        console.error('Failed to parse available sections:', e);
        return [];
      }
      return available;
    },
    refetchOnWindowFocus: false,
    refetchOnReconnect: false,
  });

  const { data: browserData, isLoading: browserLoading, isRefetching: browserRefetching, refetch: refetchBrowser } = useQuery({
    queryKey: ['sections-browser', selectedSite, browserPath],
    queryFn: async () => {
      if (!selectedSite) return null;
      return fetchBrowserPath(selectedSite, browserPath);
    },
    enabled: !!selectedSite && browserOpen,
  });

  useEffect(() => {
    if (!browserOpen || !selectedSite) return;
    if (browserData?.status !== 'pending') return;
    if (browserRefetching) return;

    const timer = window.setTimeout(() => {
      void refetchBrowser();
    }, BROWSER_PENDING_POLL_MS);

    return () => window.clearTimeout(timer);
  }, [browserOpen, selectedSite, browserData?.status, browserRefetching, refetchBrowser]);

  const { data: precatcherConfig } = useQuery({
    queryKey: ['slftp-precatcher-config'],
    queryFn: async () => fetchConfigContent('slftp.precatcher'),
    enabled: browserOpen,
    refetchOnWindowFocus: false,
  });

  const { data: precatcherIni } = useQuery({
    queryKey: ['slftp-precatcher-ini'],
    queryFn: async () => fetchConfigContent('slftp.ini'),
    enabled: browserOpen,
    refetchOnWindowFocus: false,
  });

  useEffect(() => {
    if (!availableSectionsData || availableSectionsData.length === 0) return;
    setSectionDirs(prev => {
      const next = { ...prev };
      for (const section of availableSectionsData) {
        if (!(section in next)) {
          next[section] = '';
        }
      }
      return next;
    });
    setOriginalDirs(prev => {
      const next = { ...prev };
      for (const section of availableSectionsData) {
        if (!(section in next)) {
          next[section] = '';
        }
      }
      return next;
    });
  }, [availableSectionsData, sectionsData]);

  const saveAllMutation = useMutation({
    mutationFn: async () => {
      const changes = Object.keys(sectionDirs).filter(
        section => sectionDirs[section] !== originalDirs[section]
      );

      for (const section of changes) {
        await apiClient.post('/ApiSitesService/SetSiteSection', {
          SiteName: selectedSite,
          Section: section,
          Dir: sectionDirs[section],
        });
      }

      return changes.length;
    },
    onSuccess: (count) => {
      setOriginalDirs({ ...sectionDirs });
      notifications.show({
        title: 'Saved',
        message: `${count} section(s) updated`,
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['sections', selectedSite] });
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.message,
        color: 'red',
      });
    },
  });

  const handleSaveAll = () => {
    saveAllMutation.mutate();
  };

  const normalizePath = (value: string) => {
    let p = value.trim();
    if (!p) return '';
    if (!p.startsWith('/')) p = '/' + p;
    if (p.length > 1 && p.endsWith('/')) p = p.slice(0, -1);
    return p;
  };

  const parsePrecatcherConfig = (content: string) => {
    const sections: Array<{ section: string; alias: string }> = [];
    const mappings: Array<{ orig: string; target: string; mask: string }> = [];
    let block = '';
    const lines = content.split(/\r?\n/);

    const isLineCommented = (line: string) => {
      const t = line.trim();
      return t.startsWith('#') || t.startsWith('//');
    };

    for (const line of lines) {
      const trimmed = line.trim();
      if (!trimmed) continue;
      if (trimmed.startsWith('[') && trimmed.endsWith(']')) {
        block = trimmed.toLowerCase();
        continue;
      }
      if (isLineCommented(line)) continue;

      if (block === '[sections]') {
        const idx = line.indexOf('=');
        if (idx <= 0) continue;
        const section = line.slice(0, idx).trim().toUpperCase();
        const aliases = line.slice(idx + 1).split(',');
        for (const alias of aliases) {
          const a = alias.trim();
          if (a) sections.push({ section, alias: ` ${a} ` });
        }
      } else if (block === '[mappings]') {
        const parts = line.split(';');
        if (parts.length < 3) continue;
        const orig = parts[0].trim().toUpperCase();
        const target = parts[1].trim().toUpperCase();
        const rhs = parts.slice(2).join(';').trim();
        if (!target || rhs.length === 0) continue;

        const regexMatches = rhs.match(/\/.*?\/i?/g);
        if (regexMatches && regexMatches.length > 0) {
          for (const match of regexMatches) {
            mappings.push({ orig, target, mask: match });
          }
        } else {
          const masks = rhs.split(',');
          for (const mask of masks) {
            const m = mask.trim();
            if (m) mappings.push({ orig, target, mask: m });
          }
        }
      }
    }

    return { sections, mappings };
  };

  const parsePrecatcherIni = (content: string) => {
    const lines = content.split(/\r?\n/);
    let block = '';
    for (const line of lines) {
      const trimmed = line.trim();
      if (!trimmed || trimmed.startsWith('#') || trimmed.startsWith('//')) continue;
      if (trimmed.startsWith('[') && trimmed.endsWith(']')) {
        block = trimmed.toLowerCase();
        continue;
      }
      if (block === '[precatcher]') {
        const idx = trimmed.indexOf('=');
        if (idx <= 0) continue;
        const key = trimmed.slice(0, idx).trim().toLowerCase();
        const val = trimmed.slice(idx + 1).trim().toLowerCase();
        if (key === 'recursiv_mapping') {
          return val === '1' || val === 'true' || val === 'yes';
        }
      }
    }
    return false;
  };

  const precatcherRules = useMemo(() => {
    if (!precatcherConfig) return { sections: [], mappings: [] };
    return parsePrecatcherConfig(precatcherConfig);
  }, [precatcherConfig]);

  const recursiveMappingEnabled = useMemo(() => {
    if (!precatcherIni) return false;
    return parsePrecatcherIni(precatcherIni);
  }, [precatcherIni]);

  const cleanReleaseName = (name: string) => {
    const cleaned = name.replace(/[^A-Za-z0-9]+/g, ' ').trim();
    return ` ${cleaned} `;
  };

  const maskMatches = (mask: string, input: string) => {
    if (!mask) return false;
    const isRegex = mask.startsWith('/') && (mask.endsWith('/') || mask.endsWith('/i'));
    if (isRegex) {
      const hasI = mask.endsWith('/i');
      const pattern = mask.slice(1, hasI ? -2 : -1);
      try {
        const re = new RegExp(pattern, hasI ? 'i' : undefined);
        return re.test(input);
      } catch {
        return false;
      }
    }

    const escaped = mask.replace(/[.+^${}()|[\]\\]/g, '\\$&');
    const glob = '^' + escaped.replace(/\*/g, '.*').replace(/\?/g, '.') + '$';
    try {
      return new RegExp(glob, 'i').test(input);
    } catch {
      return false;
    }
  };

  const findSectionFromAliases = (releaseName: string) => {
    const cleaned = cleanReleaseName(releaseName).toLowerCase();
    for (const entry of precatcherRules.sections) {
      if (cleaned.includes(entry.alias.toLowerCase())) {
        return entry.section;
      }
    }
    return '';
  };

  const applyMappings = (releaseName: string, initialSection: string, depth: number = 0): string => {
    const currentDepth = depth + 1;

    if (currentDepth > 500) {
      console.error('[applyMappings] Max recursion depth reached for:', releaseName);
      return '';
    }

    let section = initialSection || '';

    for (const mapping of precatcherRules.mappings) {
      // Global mappings (orig='') only apply on first call (depth=1)
      const isGlobal = mapping.orig === '' && currentDepth === 1;
      const matchesSection = mapping.orig === section;

      if (!(isGlobal || matchesSection)) continue;
      if (!maskMatches(mapping.mask, releaseName)) continue;

      // Found a match
      if (recursiveMappingEnabled && mapping.target !== 'TRASH') {
        // Recursive mapping: continue mapping with new section
        return applyMappings(releaseName, mapping.target, currentDepth);
      } else {
        // Non-recursive: return immediately
        return mapping.target;
      }
    }

    return section;
  };

  const autoMapFromReleases = () => {
    if (!precatcherConfig) {
      notifications.show({
        title: 'Precatcher config',
        message: 'Precatcher config is not loaded yet.',
        color: 'yellow',
      });
      return;
    }
    if (!browserData?.files) {
      notifications.show({
        title: 'No data',
        message: 'No directory listing available yet.',
        color: 'yellow',
      });
      return;
    }
    const dirs = browserData.files.filter((f) => f.is_dir);
    if (dirs.length === 0) {
      notifications.show({
        title: 'No folders',
        message: 'No folders found in this path.',
        color: 'yellow',
      });
      return;
    }

    const counts = new Map<string, number>();
    for (const dir of dirs) {
      const detected = findSectionFromAliases(dir.name);
      const mapped = applyMappings(dir.name, detected);
      if (mapped) counts.set(mapped, (counts.get(mapped) || 0) + 1);
    }

    if (counts.size === 0) {
      notifications.show({
        title: 'No matches',
        message: 'No sections matched from release names.',
        color: 'yellow',
      });
      return;
    }

    const sections = Array.from(counts.keys());
    const path = normalizePath(browserPath);
    const summary = Array.from(counts.entries())
      .sort((a, b) => b[1] - a[1])
      .slice(0, 6)
      .map(([section, count]) => `${section} (${count})`)
      .join(', ');

    const notificationId = `auto-map-${Date.now()}`;

    const handleApply = () => {
      setSectionDirs((prev) => {
        const next = { ...prev };
        for (const section of sections) {
          next[section] = path;
        }
        return next;
      });
      notifications.hide(notificationId);
      notifications.show({
        title: 'Applied',
        message: `Set ${sections.length} section(s) to ${path}`,
        color: 'green',
      });
    };

    notifications.show({
      id: notificationId,
      title: 'Auto-mapped sections',
      message: (
        <Box>
          <Text size="sm" mb="xs">Found {counts.size} section(s). {summary}</Text>
          <Button size="xs" leftSection={<IconCheck size="1rem" />} onClick={handleApply}>
            Apply to {path}
          </Button>
        </Box>
      ),
      color: 'green',
      autoClose: false,
      withCloseButton: true,
    });
  };

  const handleQuickAdd = () => {
    const sections = quickSection.map((s) => s.trim()).filter(Boolean);
    const path = normalizePath(quickPath);
    if (sections.length === 0 || !path) {
      notifications.show({
        title: 'Missing data',
        message: 'Select one or more sections and a directory path first.',
        color: 'yellow',
      });
      return;
    }
    setSectionDirs((prev) => {
      const next = { ...prev };
      for (const section of sections) {
        next[section] = path;
      }
      return next;
    });
  };

  const browserDirs = useMemo(() => {
    const files = browserData?.files || [];
    return sortBrowserDirs(files.filter((f) => f.is_dir), browserSortBy, browserSortDir);
  }, [browserData, browserSortBy, browserSortDir]);

  const breadcrumbItems = useMemo(() => {
    const parts = browserPath === '/' ? [] : browserPath.split('/').filter(Boolean);
    const items = [
      { label: '/', path: '/' },
      ...parts.map((part, idx) => ({
        label: part,
        path: '/' + parts.slice(0, idx + 1).join('/'),
      })),
    ];
    return items;
  }, [browserPath]);

  const navigateBrowserPath = (value: string) => {
    const next = normalizePath(value);
    setBrowserPath(next || '/');
  };

  const handleBrowserSort = (nextBy: BrowserDirSortBy) => {
    const next = toggleBrowserSort(browserSortBy, browserSortDir, nextBy);
    setBrowserSortBy(next.by);
    setBrowserSortDir(next.dir);
  };

  const prefetchBrowserPath = (pathValue: string, forceRefresh: boolean) => {
    if (!selectedSite) return;
    const targetPath = normalizePath(pathValue) || '/';
    fetchBrowserPath(selectedSite, targetPath, forceRefresh).then(() => {
      queryClient.invalidateQueries({ queryKey: ['sections-browser', selectedSite, targetPath] });
    });
  };

  const openBrowser = () => {
    const startPath = normalizePath(quickPath) || '/';
    setBrowserPath(startPath);
    setBrowserOpen(true);
    prefetchBrowserPath(startPath, true);
  };

  const handleBrowserRefresh = () => {
    if (!selectedSite) return;
    fetchBrowserPath(selectedSite, browserPath, true).then(() => {
      queryClient.invalidateQueries({ queryKey: ['sections-browser', selectedSite, browserPath] });
    });
  };

  const filteredSections = useMemo(() => {
    if (!sectionsData) return [];

    const available = availableSectionsData || [];
    const combined: SectionData[] = [];
    const seen = new Set<string>();

    for (const section of available) {
      combined.push({ section, dir: sectionDirs[section] || '' });
      seen.add(section);
    }

    for (const sectionData of sectionsData) {
      if (!seen.has(sectionData.section)) {
        combined.push({
          section: sectionData.section,
          dir: sectionDirs[sectionData.section] || sectionData.dir || '',
        });
        seen.add(sectionData.section);
      }
    }

    let data = combined.length > 0 ? combined : [...sectionsData];
    
    // Sort sections alphabetically by name
    data.sort((a, b) => a.section.localeCompare(b.section));

    if (showOnlySet) {
      data = data.filter(s => {
        const dir = sectionDirs[s.section] || s.dir || '';
        return dir.trim().length > 0;
      });
    }

    if (!searchQuery) return data;

    const query = searchQuery.toLowerCase();
    return data.filter(s => s.section.toLowerCase().includes(query));
  }, [sectionsData, availableSectionsData, searchQuery, showOnlySet, sectionDirs]);

  const hasChanges = useMemo(() => {
    return Object.keys(sectionDirs).some(
      section => sectionDirs[section] !== originalDirs[section]
    );
  }, [sectionDirs, originalDirs]);

  const changesCount = useMemo(() => {
    return Object.keys(sectionDirs).filter(
      section => sectionDirs[section] !== originalDirs[section]
    ).length;
  }, [sectionDirs, originalDirs]);

  const handleToggleShowOnlySet = (event: React.ChangeEvent<HTMLInputElement>) => {
    const isChecked = event.currentTarget.checked;

    if (markedSection) {
       setPreserveSection(markedSection);
    } else if (viewportRef.current) {
      const viewport = viewportRef.current;
      const scrollTop = viewport.scrollTop;

      let candidate = null;
      let minDiff = Infinity;

      // Find the section closest to the top of the viewport
      for (const s of filteredSections) {
        const el = document.getElementById(`section-row-${s.section}`);
        if (el) {
          const diff = Math.abs(el.offsetTop - scrollTop);
          if (diff < minDiff) {
            minDiff = diff;
            candidate = s.section;
          }
        }
      }

      if (candidate) {
        setPreserveSection(candidate);
      }
    }

    setShowOnlySet(isChecked);
  };

  useEffect(() => {
    if (preserveSection) {
      // Small delay to ensure the DOM has updated with the new filter state
      const timer = setTimeout(() => {
        const el = document.getElementById(`section-row-${preserveSection}`);
        if (el) {
          el.scrollIntoView({ block: 'center' });
        }
        setPreserveSection(null);
      }, 100);
      return () => clearTimeout(timer);
    }
  }, [filteredSections, preserveSection]);

  if (sitesLoading) return <Center h={400}><Loader size="xl" /></Center>;

  if (!selectedSite) {
    return (
      <Card shadow="sm" padding="lg" radius="md" withBorder>
        <Title order={3} mb="md">Sections Manager</Title>
        <Text size="sm" c="dimmed" mb="md">Select a site to manage sections</Text>

        <Table highlightOnHover>
          <Table.Thead>
            <Table.Tr>
              <Table.Th>Site</Table.Th>
              <Table.Th>Actions</Table.Th>
            </Table.Tr>
          </Table.Thead>
          <Table.Tbody>
            {sitesData?.map((site: any) => (
              <Table.Tr key={site.name} style={{ cursor: 'pointer' }} onClick={() => setSelectedSite(site.name)}>
                <Table.Td fw={600}>{site.name}</Table.Td>
                <Table.Td>
                  <Button variant="light" size="xs" rightSection={<IconChevronRight size="1rem" />}>
                    Manage Sections
                  </Button>
                </Table.Td>
              </Table.Tr>
            ))}
          </Table.Tbody>
        </Table>
      </Card>
    );
  }

  return (
    <Card shadow="sm" padding="lg" radius="md" withBorder>
      <Stack gap="md">
        <Group justify="space-between">
          <Title order={3}>Sections for {selectedSite}</Title>
          <Button variant="outline" onClick={() => { setSelectedSite(null); setSearchQuery(''); }}>
            Back to Sites
          </Button>
        </Group>

        {sectionsLoading ? (
          <Center h={300}><Loader size="lg" /></Center>
        ) : (
          <>
            <Group align="end" wrap="wrap">
              <Stack gap={4} style={{ minWidth: 200, flex: '0 0 220px' }}>
                <Text size="sm" fw={500}>Section</Text>
                <MultiSelect
                  data={(availableSectionsData || []).map(s => ({ value: s, label: s }))}
                  value={quickSection}
                  onChange={setQuickSection}
                  placeholder="Choose sections"
                  searchable
                  clearSearchOnChange={false}
                  maxDropdownHeight={360}
                  clearable
                />
              </Stack>
              <Stack gap={4} style={{ flex: 1, minWidth: 240 }}>
                <Text size="sm" fw={500}>Directory path</Text>
                <TextInput
                  placeholder="/path/to/dir"
                  value={quickPath}
                  onChange={(e) => setQuickPath(e.currentTarget.value)}
                  onFocus={() => prefetchBrowserPath(quickPath, true)}
                  rightSection={
                    <ActionIcon variant="subtle" onClick={openBrowser} aria-label="Browse">
                      <IconFolderOpen size="1rem" />
                    </ActionIcon>
                  }
                />
              </Stack>
              <Button
                leftSection={<IconPlus size="1rem" />}
                onClick={handleQuickAdd}
              >
                Add section path
              </Button>
            </Group>
            <Text size="xs" c="dimmed">
              Quick add lets you select one or more precatcher sections and set a directory path. Use the folder icon to browse remote paths.
            </Text>

            <Group>
              <TextInput
                placeholder="Search sections..."
                leftSection={<IconSearch size="1rem" />}
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.currentTarget.value)}
                style={{ flex: 1, maxWidth: 400 }}
              />
              <Switch
                label="Show configured only"
                checked={showOnlySet}
                onChange={handleToggleShowOnlySet}
              />
            </Group>

            <Text size="xs" c="dimmed">
              Click on a row to mark it. The view will try to keep the marked row visible when toggling filters.
            </Text>

            {filteredSections.length === 0 && searchQuery && (
              <Alert color="yellow" title="No results">
                No sections found matching "{searchQuery}"
              </Alert>
            )}

            {filteredSections.length === 0 && !searchQuery && showOnlySet && (
              <Alert color="blue" title="No configured sections">
                No sections have a directory path configured. Uncheck "Show configured only" to see all available sections.
              </Alert>
            )}

            {filteredSections.length === 0 && !searchQuery && !showOnlySet && (
              <Alert color="yellow" title="No sections">
                No sections found in slftp.precatcher configuration.
              </Alert>
            )}

            {filteredSections.length > 0 && (
              <>
                <Table withTableBorder withColumnBorders style={{ tableLayout: 'fixed', borderBottom: 'none' }}>
                  <Table.Thead>
                    <Table.Tr>
                      <Table.Th style={{ width: 200 }}>Section</Table.Th>
                      <Table.Th>Directory Path</Table.Th>
                    </Table.Tr>
                  </Table.Thead>
                </Table>
                <ScrollArea h={600} viewportRef={viewportRef} type="always">
                  <Table striped highlightOnHover withTableBorder withColumnBorders style={{ tableLayout: 'fixed', borderTop: 'none' }}>
                    <Table.Tbody>
                      {filteredSections.map((sectionData: SectionData) => {
                        const isModified = sectionDirs[sectionData.section] !== originalDirs[sectionData.section];
                        const isMarked = markedSection === sectionData.section;
                        return (
                          <Table.Tr
                            key={sectionData.section}
                            id={`section-row-${sectionData.section}`}
                            onClick={() => setMarkedSection(isMarked ? null : sectionData.section)}
                            style={{
                              cursor: 'pointer',
                              backgroundColor: isMarked ? 'var(--nav-hover-bg)' : undefined,
                              scrollMarginTop: '45px'
                            }}
                          >
                            <Table.Td style={{ width: 200 }}>
                              <Group gap="xs" justify="space-between">
                                <Group gap="xs">
                                  <Text fw={600}>{sectionData.section}</Text>
                                  {isModified && <Badge size="xs" color="yellow">modified</Badge>}
                                </Group>
                                {isMarked && (
                                  <Tooltip label="Marked for scroll preservation">
                                    <IconPin size="1rem" color="var(--primary-light)" />
                                  </Tooltip>
                                )}
                              </Group>
                            </Table.Td>
                            <Table.Td onClick={(e) => e.stopPropagation()}>
                              <TextInput
                                value={sectionDirs[sectionData.section] || ''}
                                onChange={(e) => setSectionDirs({ ...sectionDirs, [sectionData.section]: e.currentTarget.value })}
                                placeholder="/path/to/section/"
                                size="xs"
                              />
                            </Table.Td>
                          </Table.Tr>
                        );
                      })}
                    </Table.Tbody>
                  </Table>
                </ScrollArea>
              </>
            )}

            <Group justify="space-between" align="center">
              <Text size="sm" c="dimmed">
                Showing {filteredSections.length} of {sectionsData?.length || 0} sections
                {hasChanges && (
                  <Text span c="yellow" fw={600} ml="md">
                    • {changesCount} unsaved change(s)
                  </Text>
                )}
              </Text>
              {hasChanges && (
                <Button
                  className="floating-save-action"
                  leftSection={<IconDeviceFloppy size="1rem" />}
                  onClick={handleSaveAll}
                  loading={saveAllMutation.isPending}
                  color="green"
                  size="md"
                >
                  Save All Changes
                </Button>
              )}
            </Group>
          </>
        )}
      </Stack>
      <Modal opened={browserOpen} onClose={() => setBrowserOpen(false)} title="Browse site directory" size="xl">
        <Stack gap="sm">
          <Group justify="space-between">
            <Group gap="xs">
              <ActionIcon
                variant="light"
                onClick={() => navigateBrowserPath(browserPath.split('/').slice(0, -1).join('/') || '/')}
                aria-label="Up one level"
              >
                <IconArrowUp size="1rem" />
              </ActionIcon>
              <ActionIcon
                variant="light"
                onClick={handleBrowserRefresh}
                aria-label="Refresh"
              >
                <IconRefresh size="1rem" />
              </ActionIcon>
            </Group>
            <Group gap="xs">
              <Button
                variant="light"
                onClick={autoMapFromReleases}
              >
                Auto map from releases (beta)
              </Button>
              <Button
                leftSection={<IconPlus size="1rem" />}
                onClick={() => {
                  setQuickPath(browserPath);
                  setBrowserOpen(false);
                }}
              >
                Use this path
              </Button>
            </Group>
          </Group>

          <Breadcrumbs>
            {breadcrumbItems.map((item) => (
              <Button
                key={item.path}
                variant="subtle"
                size="compact-sm"
                onClick={() => navigateBrowserPath(item.path)}
              >
                {item.label}
              </Button>
            ))}
          </Breadcrumbs>
          <Text size="xs" c="dimmed">
            Auto map scans folder names against `slftp.precatcher` sections/mappings and suggests which sections to add.
          </Text>

          {(browserLoading || browserRefetching) && (
            <Center h={120}><Loader size="md" /></Center>
          )}

          {!browserLoading && browserData?.status === 'error' && (
            <Alert color="red" title="Browser error">
              {browserData.message || 'Failed to load directory.'}
            </Alert>
          )}

          {!browserLoading && browserData?.status !== 'error' && (
            <Table striped highlightOnHover withTableBorder withColumnBorders>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th
                    style={{ cursor: 'pointer', userSelect: 'none' }}
                    onClick={() => handleBrowserSort('name')}
                  >
                    <Group gap={4} wrap="nowrap">
                      <Text size="sm" fw={600}>Directory</Text>
                      {browserSortIndicator(browserSortBy === 'name', browserSortDir)}
                    </Group>
                  </Table.Th>
                  <Table.Th
                    style={{ cursor: 'pointer', userSelect: 'none', whiteSpace: 'nowrap' }}
                    onClick={() => handleBrowserSort('modified')}
                  >
                    <Group gap={4} wrap="nowrap">
                      <Text size="sm" fw={600}>Modified</Text>
                      {browserSortIndicator(browserSortBy === 'modified', browserSortDir)}
                    </Group>
                  </Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {browserDirs.length === 0 && (
                  <Table.Tr>
                    <Table.Td colSpan={2}>
                      <Text size="sm" c="dimmed">No folders in this path.</Text>
                    </Table.Td>
                  </Table.Tr>
                )}
                {browserDirs.map((dir) => (
                  <Table.Tr key={dir.name} style={{ cursor: 'pointer' }} onClick={() => navigateBrowserPath(`${browserPath === '/' ? '' : browserPath}/${dir.name}`)}>
                    <Table.Td>
                      <Group gap="xs">
                        <IconFolderOpen size="1rem" />
                        <Text fw={600}>{dir.name}</Text>
                      </Group>
                    </Table.Td>
                    <Table.Td>
                      <Text size="sm" c="dimmed">{dir.date || '-'}</Text>
                    </Table.Td>
                  </Table.Tr>
                ))}
              </Table.Tbody>
            </Table>
          )}
        </Stack>
      </Modal>
    </Card>
  );
}
