import { Card, Title, Stack, Group, Text, NumberInput, Button, Checkbox, ScrollArea, Badge, Table, Loader, Center, MultiSelect, SegmentedControl, Tooltip, Modal, ActionIcon, TextInput, CloseButton, Paper, FileButton } from '@mantine/core';
import { IconRoute, IconRefresh, IconGridDots, IconBolt, IconEdit, IconSearch, IconDownload, IconUpload } from '@tabler/icons-react';
import { useQuery, useMutation } from '@tanstack/react-query';
import { useMemo, useRef, useState } from 'react';
import { apiClient } from '../api/client';
import type { Site, RouteEntry } from '../api/client';
import { notifications } from '@mantine/notifications';

function RouteChip({
  label,
  speed,
  locked,
  affilOnly,
  noAffil,
  speedColor,
  onEdit,
  onDelete,
}: {
  label: string;
  speed: number;
  locked?: boolean;
  affilOnly?: boolean;
  noAffil?: boolean;
  speedColor: string;
  onEdit: () => void;
  onDelete: () => void;
}) {
  const hasFlags = !!locked || !!affilOnly || !!noAffil;
  const borderColor = `var(--mantine-color-${speedColor}-6)`;

  return (
    <Paper
      withBorder
      p={6}
      radius="sm"
      style={{ minWidth: 150, borderColor, borderWidth: 1 }}
    >
      <Stack gap={4}>
        <Group justify="space-between" gap={8} wrap="nowrap">
          <Text size="xs" fw={600} truncate>
            {label}
          </Text>
          <Badge size="xs" color={speedColor} variant="filled">
            {speed}
          </Badge>
        </Group>

        <Group gap={3} wrap="wrap">
          {locked && <Badge size="xs" color="gray" variant="light">🔒 Locked</Badge>}
          {affilOnly && <Badge size="xs" color="blue" variant="light">👥 Affil</Badge>}
          {noAffil && <Badge size="xs" color="red" variant="light">🚫 NoAffil</Badge>}
          {!hasFlags && <Text size="xs" c="dimmed">No flags</Text>}
        </Group>

        <Group justify="flex-end" gap={4}>
          <ActionIcon size="xs" variant="subtle" color="blue" onClick={onEdit}>
            <IconEdit size="0.65rem" />
          </ActionIcon>
          <ActionIcon size="xs" variant="subtle" color="red" onClick={onDelete}>
            ×
          </ActionIcon>
        </Group>
      </Stack>
    </Paper>
  );
}

function IncomingRoutesList({ destSite, quickSpeed, onRouteDeleted, allRoutes }: { destSite: string; quickSpeed: number; onRouteDeleted: () => void; allRoutes: Map<string, RouteEntry[]> }) {
  const [editingRoute, setEditingRoute] = useState<RouteEntry & { source: string } | null>(null);
  const [editSpeed, setEditSpeed] = useState<number | ''>(5);
  const [editLocked, setEditLocked] = useState(false);
  const [editAffilOnly, setEditAffilOnly] = useState(false);
  const [editNoAffil, setEditNoAffil] = useState(false);

  // Filter incoming routes from the pre-loaded map
  const incomingRoutes: Array<RouteEntry & { source: string }> = [];
  allRoutes.forEach((routes, sourceSite) => {
    if (sourceSite === destSite) return;
    routes.filter(r => r.dest === destSite).forEach(r => {
      incomingRoutes.push({ ...r, source: sourceSite });
    });
  });

  const setRouteMutation = useMutation({
    mutationFn: async ({ source, dest, speed }: { source: string; dest: string; speed: number }) => {
      await apiClient.post('/ApiSitesService/SetSiteRoute', {
        SourceSite: source,
        DestSite: dest,
        Speed: speed,
        Locked: false,
        AffilOnly: false,
        NoAffil: false
      });
    },
    onSuccess: () => {
      onRouteDeleted();
    }
  });

  const handleDrop = async (e: React.DragEvent, destSite: string) => {
    e.preventDefault();
    const sourceSite = e.dataTransfer.getData('text/plain');

    if (sourceSite === destSite) {
      notifications.show({ title: 'Error', message: 'Cannot create route to same site', color: 'red' });
      return;
    }

    await setRouteMutation.mutateAsync({
      source: sourceSite,
      dest: destSite,
      speed: quickSpeed
    });

    notifications.show({
      title: 'Route created',
      message: `${sourceSite} → ${destSite} [${quickSpeed}]`,
      color: 'green'
    });
  };

  const handleDeleteRoute = async (source: string) => {
    await setRouteMutation.mutateAsync({
      source: source,
      dest: destSite,
      speed: 0
    });
    notifications.show({ title: 'Route deleted', message: `${source} → ${destSite}`, color: 'orange' });
  };

  const openEditModal = (route: RouteEntry & { source: string }) => {
    setEditingRoute(route);
    setEditSpeed(route.speed);
    setEditLocked(route.locked || false);
    setEditAffilOnly(route.affil_only || false);
    setEditNoAffil(route.no_affil || false);
  };

  const handleSaveEdit = async () => {
    if (!editingRoute || editSpeed === '') return;

    await apiClient.post('/ApiSitesService/SetSiteRoute', {
      SourceSite: editingRoute.source,
      DestSite: destSite,
      Speed: editSpeed,
      Locked: editLocked,
      AffilOnly: editAffilOnly,
      NoAffil: editNoAffil
    });

    notifications.show({
      title: 'Route updated',
      message: `${editingRoute.source} → ${destSite} [${editSpeed}]`,
      color: 'blue'
    });

    setEditingRoute(null);
    onRouteDeleted();
  };

  const getSpeedColor = (speed: number) => {
    if (speed >= 8) return 'green';
    if (speed >= 5) return 'blue';
    if (speed >= 3) return 'yellow';
    return 'red';
  };

	  return (
	    <>
	      <Group
	        gap="xs"
	        style={{ minHeight: 40 }}
	        onDragOver={(e) => e.preventDefault()}
	        onDrop={(e) => handleDrop(e, destSite)}
	        wrap="wrap"
	      >
	        {incomingRoutes && incomingRoutes.length > 0 ? (
	          incomingRoutes.map((route) => (
	            <Tooltip
	              key={route.source}
	              label={
	                <Stack gap={4}>
	                  <Text size="xs">{route.source} → {destSite}</Text>
	                  <Text size="xs">Speed: {route.speed}</Text>
	                  {route.locked && <Text size="xs" c="red">🔒 Locked</Text>}
	                  {route.affil_only && <Text size="xs" c="blue">👥 Affil Only</Text>}
	                  {route.no_affil && <Text size="xs" c="gray">🚫 No Affil</Text>}
	                </Stack>
	              }
	            >
	              <div>
	                <RouteChip
	                  label={route.source}
	                  speed={route.speed}
	                  locked={route.locked || false}
	                  affilOnly={route.affil_only || false}
	                  noAffil={route.no_affil || false}
	                  speedColor={getSpeedColor(route.speed)}
	                  onEdit={() => openEditModal(route)}
	                  onDelete={() => handleDeleteRoute(route.source)}
	                />
	              </div>
	            </Tooltip>
	          ))
	        ) : (
	          <Text size="sm" c="dimmed">Drop sites here to create incoming routes</Text>
	        )}
	      </Group>

      <Modal
        opened={!!editingRoute}
        onClose={() => setEditingRoute(null)}
        title={`Edit Route: ${editingRoute?.source} → ${destSite}`}
        centered
      >
        <Stack gap="md">
          <NumberInput
            label="Speed (1-9)"
            value={editSpeed}
            onChange={(val) => setEditSpeed(typeof val === 'number' ? val : '')}
            min={1}
            max={9}
            size="lg"
          />

          <Stack gap="xs">
            <Checkbox
              label="🔒 Locked"
              checked={editLocked}
              onChange={(e) => setEditLocked(e.currentTarget.checked)}
              description="Lock this route to prevent auto-changes"
            />
            <Checkbox
              label="👥 Affil Only"
              checked={editAffilOnly}
              onChange={(e) => setEditAffilOnly(e.currentTarget.checked)}
              description="Only use for affil releases"
            />
            <Checkbox
              label="🚫 No Affil"
              checked={editNoAffil}
              onChange={(e) => setEditNoAffil(e.currentTarget.checked)}
              description="Don't use for affil releases"
            />
          </Stack>

          <Group justify="flex-end">
            <Button variant="default" onClick={() => setEditingRoute(null)}>
              Cancel
            </Button>
            <Button onClick={handleSaveEdit} disabled={editSpeed === ''}>
              Save Changes
            </Button>
          </Group>
        </Stack>
      </Modal>
    </>
  );
}

function RoutesList({ sourceSite, quickSpeed, onRouteDeleted }: { sourceSite: string; quickSpeed: number; onRouteDeleted: () => void }) {
  const [editingRoute, setEditingRoute] = useState<RouteEntry | null>(null);
  const [editSpeed, setEditSpeed] = useState<number | ''>(5);
  const [editLocked, setEditLocked] = useState(false);
  const [editAffilOnly, setEditAffilOnly] = useState(false);
  const [editNoAffil, setEditNoAffil] = useState(false);

  const { data: routes, isLoading, refetch } = useQuery({
    queryKey: ['routes', sourceSite],
    queryFn: async (): Promise<RouteEntry[]> => {
      const res = await apiClient.post('/ApiSitesService/GetSiteRoutes', { SiteName: sourceSite });
      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }
      const rawRoutes = responseData.Routes;
      if (!rawRoutes) return [];
      try {
        if (typeof rawRoutes === 'string') {
          return JSON.parse(rawRoutes);
        }
        if (Array.isArray(rawRoutes)) {
          return rawRoutes;
        }
      } catch {
        return [];
      }
      return [];
    }
  });

  const setRouteMutation = useMutation({
    mutationFn: async ({ source, dest, speed }: { source: string; dest: string; speed: number }) => {
      await apiClient.post('/ApiSitesService/SetSiteRoute', {
        SourceSite: source,
        DestSite: dest,
        Speed: speed,
        Locked: false,
        AffilOnly: false,
        NoAffil: false
      });
    },
    onSuccess: () => {
      refetch();
      onRouteDeleted();
    }
  });

  const handleDrop = async (e: React.DragEvent, sourceSite: string) => {
    e.preventDefault();
    const destSite = e.dataTransfer.getData('text/plain');

    if (sourceSite === destSite) {
      notifications.show({ title: 'Error', message: 'Cannot create route to same site', color: 'red' });
      return;
    }

    await setRouteMutation.mutateAsync({
      source: sourceSite,
      dest: destSite,
      speed: quickSpeed
    });

    notifications.show({
      title: 'Route created',
      message: `${sourceSite} → ${destSite} [${quickSpeed}]`,
      color: 'green'
    });
  };

  const handleDeleteRoute = async (dest: string) => {
    await setRouteMutation.mutateAsync({
      source: sourceSite,
      dest: dest,
      speed: 0
    });
    notifications.show({ title: 'Route deleted', message: `${sourceSite} → ${dest}`, color: 'orange' });
  };

  const openEditModal = (route: RouteEntry) => {
    setEditingRoute(route);
    setEditSpeed(route.speed);
    setEditLocked(route.locked || false);
    setEditAffilOnly(route.affil_only || false);
    setEditNoAffil(route.no_affil || false);
  };

  const handleSaveEdit = async () => {
    if (!editingRoute || editSpeed === '') return;

    await setRouteMutation.mutateAsync({
      source: sourceSite,
      dest: editingRoute.dest,
      speed: editSpeed as number
    });

    await apiClient.post('/ApiSitesService/SetSiteRoute', {
      SourceSite: sourceSite,
      DestSite: editingRoute.dest,
      Speed: editSpeed,
      Locked: editLocked,
      AffilOnly: editAffilOnly,
      NoAffil: editNoAffil
    });

    notifications.show({
      title: 'Route updated',
      message: `${sourceSite} → ${editingRoute.dest} [${editSpeed}]`,
      color: 'blue'
    });

    setEditingRoute(null);
    refetch();
    onRouteDeleted();
  };

  const getSpeedColor = (speed: number) => {
    if (speed >= 8) return 'green';
    if (speed >= 5) return 'blue';
    if (speed >= 3) return 'yellow';
    return 'red';
  };

  if (isLoading) return <Loader size="xs" />;

	  return (
	    <>
	      <Group
	        gap="xs"
	        style={{ minHeight: 40 }}
	        onDragOver={(e) => e.preventDefault()}
	        onDrop={(e) => handleDrop(e, sourceSite)}
	        wrap="wrap"
	      >
	        {routes && routes.length > 0 ? (
	        routes.map((route) => (
	          <Tooltip
	            key={route.dest}
	            label={
	              <Stack gap={4}>
	                <Text size="xs">{sourceSite} → {route.dest}</Text>
	                <Text size="xs">Speed: {route.speed}</Text>
	                {route.locked && <Text size="xs" c="red">🔒 Locked</Text>}
	                {route.affil_only && <Text size="xs" c="blue">👥 Affil Only</Text>}
	                {route.no_affil && <Text size="xs" c="gray">🚫 No Affil</Text>}
	              </Stack>
	            }
	          >
	            <div>
	              <RouteChip
	                label={route.dest}
	                speed={route.speed}
	                locked={route.locked || false}
	                affilOnly={route.affil_only || false}
	                noAffil={route.no_affil || false}
	                speedColor={getSpeedColor(route.speed)}
	                onEdit={() => openEditModal(route)}
	                onDelete={() => handleDeleteRoute(route.dest)}
	              />
	            </div>
	          </Tooltip>
	        ))
	        ) : (
	          <Text size="sm" c="dimmed">Drop sites here to create routes</Text>
	        )}
	      </Group>

      <Modal
        opened={!!editingRoute}
        onClose={() => setEditingRoute(null)}
        title={`Edit Route: ${sourceSite} → ${editingRoute?.dest}`}
        centered
      >
        <Stack gap="md">
          <NumberInput
            label="Speed (1-9)"
            value={editSpeed}
            onChange={(val) => setEditSpeed(typeof val === 'number' ? val : '')}
            min={1}
            max={9}
            size="lg"
          />

          <Stack gap="xs">
            <Checkbox
              label="🔒 Locked"
              checked={editLocked}
              onChange={(e) => setEditLocked(e.currentTarget.checked)}
              description="Lock this route to prevent auto-changes"
            />
            <Checkbox
              label="👥 Affil Only"
              checked={editAffilOnly}
              onChange={(e) => setEditAffilOnly(e.currentTarget.checked)}
              description="Only use for affil releases"
            />
            <Checkbox
              label="🚫 No Affil"
              checked={editNoAffil}
              onChange={(e) => setEditNoAffil(e.currentTarget.checked)}
              description="Don't use for affil releases"
            />
          </Stack>

          <Group justify="flex-end">
            <Button variant="default" onClick={() => setEditingRoute(null)}>
              Cancel
            </Button>
            <Button onClick={handleSaveEdit} disabled={editSpeed === ''}>
              Save Changes
            </Button>
          </Group>
        </Stack>
      </Modal>
    </>
  );
}

export function Routes() {
  const [selectedSources, setSelectedSources] = useState<string[]>([]);
  const [selectedDests, setSelectedDests] = useState<string[]>([]);
  const [bulkSpeed, setBulkSpeed] = useState<number | ''>(5);
  const [addBackRoutes, setAddBackRoutes] = useState(false);
  const [bulkLocked, setBulkLocked] = useState(false);
  const [bulkAffilOnly, setBulkAffilOnly] = useState(false);
  const [bulkNoAffil, setBulkNoAffil] = useState(false);
  const [viewMode, setViewMode] = useState<string>('bulk');
  const quickSpeed = 7;
  const [refreshKey, setRefreshKey] = useState(0);
  const [routeDirection, setRouteDirection] = useState<'outgoing' | 'incoming'>('incoming');
  const [paletteFilter, setPaletteFilter] = useState('');
  const matrixViewportRef = useRef<HTMLDivElement>(null);
  const isDraggingSiteRef = useRef(false);

  const [exportLoading, setExportLoading] = useState(false);
  const [importLoading, setImportLoading] = useState(false);

  const { data: sites, isLoading: sitesLoading } = useQuery({
    queryKey: ['sites'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSitesService/GetSites', { Filter: '' });
      let responseData = res.data;
      if (res.data.result && Array.isArray(res.data.result)) {
        responseData = res.data.result[0];
      }
      const rawSites = responseData.Sites;
      if (!rawSites) return [];
      try {
        if (typeof rawSites === 'string') {
          return JSON.parse(rawSites) as Site[];
        }
        if (Array.isArray(rawSites)) {
          return rawSites as Site[];
        }
      } catch (err) {
        console.error('Failed to parse sites:', err);
        return [];
      }
      return [];
    }
  });

  const handleExport = async () => {
    if (!sites) return;
    setExportLoading(true);
    try {
      const exportData: Record<string, RouteEntry[]> = {};
      for (const site of sites) {
        const res = await apiClient.post('/ApiSitesService/GetSiteRoutes', { SiteName: site.name });
        let responseData = res.data;
        if (res.data.result && Array.isArray(res.data.result)) {
          responseData = res.data.result[0];
        }
        if (responseData.Routes) {
          let routes: RouteEntry[] = [];
          if (typeof responseData.Routes === 'string') {
            routes = JSON.parse(responseData.Routes);
          } else if (Array.isArray(responseData.Routes)) {
            routes = responseData.Routes;
          }
          exportData[site.name] = routes;
        } else {
          exportData[site.name] = [];
        }
      }

      const jsonStr = JSON.stringify(exportData, null, 2);
      const blob = new Blob([jsonStr], { type: 'application/json' });
      const url = URL.createObjectURL(blob);
      const link = document.createElement('a');
      link.href = url;
      link.download = 'slftp-routes-backup.json';
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
      URL.revokeObjectURL(url);
      
      notifications.show({ title: 'Success', message: 'Routes exported successfully.', color: 'green' });
    } catch (e) {
      console.error('Export failed', e);
      notifications.show({ title: 'Error', message: 'Failed to export routes', color: 'red' });
    } finally {
      setExportLoading(false);
    }
  };

  const handleImport = async (file: File | null) => {
    if (!file) return;
    setImportLoading(true);
    try {
      const text = await file.text();
      const importData = JSON.parse(text) as Record<string, RouteEntry[]>;
      let count = 0;

      for (const [sourceSite, routes] of Object.entries(importData)) {
        for (const route of routes) {
          await setRouteMutation.mutateAsync({
            source: sourceSite,
            dest: route.dest,
            speed: route.speed,
            locked: route.locked || false,
            affilOnly: route.affil_only || false,
            noAffil: route.no_affil || false
          });
          count++;
        }
      }

      notifications.show({ title: 'Success', message: `Imported ${count} routes successfully.`, color: 'green' });
      setRefreshKey(prev => prev + 1);
    } catch (e) {
      console.error('Import failed', e);
      notifications.show({ title: 'Error', message: 'Failed to import routes. Ensure file is valid JSON.', color: 'red' });
    } finally {
      setImportLoading(false);
    }
  };

  // Load all routes once for incoming view
  const { data: allRoutesMap, isLoading: allRoutesLoading } = useQuery({
    queryKey: ['all-routes', refreshKey],
    queryFn: async (): Promise<Map<string, RouteEntry[]>> => {
      const routesMap = new Map<string, RouteEntry[]>();
      if (!sites) return routesMap;

      const results = await Promise.all(
        sites.map(async (site): Promise<[string, RouteEntry[]]> => {
          try {
            const res = await apiClient.post('/ApiSitesService/GetSiteRoutes', { SiteName: site.name });
            let responseData = res.data;
            if (res.data.result && Array.isArray(res.data.result)) {
              responseData = res.data.result[0];
            }
            const rawRoutes = responseData.Routes;

            if (rawRoutes) {
              let routes: RouteEntry[] = [];
              if (typeof rawRoutes === 'string') {
                routes = JSON.parse(rawRoutes);
              } else if (Array.isArray(rawRoutes)) {
                routes = rawRoutes;
              }
              return [site.name, routes];
            } else {
              return [site.name, []];
            }
          } catch (e) {
            console.error(`Failed to fetch routes for ${site.name}`, e);
            return [site.name, []];
          }
        })
      );

      for (const [name, routes] of results) {
        routesMap.set(name, routes);
      }

      return routesMap;
    },
    enabled: !!sites && routeDirection === 'incoming' && viewMode === 'matrix'
  });

  const setRouteMutation = useMutation({
    mutationFn: async ({ source, dest, speed, locked, affilOnly, noAffil }: { source: string; dest: string; speed: number; locked: boolean; affilOnly: boolean; noAffil: boolean }) => {
      await apiClient.post('/ApiSitesService/SetSiteRoute', { SourceSite: source, DestSite: dest, Speed: speed, Locked: locked, AffilOnly: affilOnly, NoAffil: noAffil });
    },
    onError: (err) => notifications.show({ title: 'Error', message: err.message, color: 'red' })
  });

  const handleBulkSetRoutes = async () => {
    if (selectedSources.length === 0 || selectedDests.length === 0 || bulkSpeed === '') {
      notifications.show({ title: 'Error', message: 'Please select sources, destinations and speed', color: 'red' });
      return;
    }

    let count = 0;
    for (const source of selectedSources) {
      for (const dest of selectedDests) {
        if (source === dest) continue;

        try {
          await setRouteMutation.mutateAsync({
            source,
            dest,
            speed: bulkSpeed as number,
            locked: bulkLocked,
            affilOnly: bulkAffilOnly,
            noAffil: bulkNoAffil
          });
          count++;

          if (addBackRoutes) {
            await setRouteMutation.mutateAsync({
              source: dest,
              dest: source,
              speed: bulkSpeed as number,
              locked: bulkLocked,
              affilOnly: bulkAffilOnly,
              noAffil: bulkNoAffil
            });
            count++;
          }
        } catch (e) {
          console.error('Failed to set route', e);
        }
      }
    }

    notifications.show({
      title: 'Bulk operation completed',
      message: `${count} routes created successfully`,
      color: 'green'
    });

    setSelectedSources([]);
    setSelectedDests([]);
  };

  const visibleSites = useMemo(() => {
    return (sites || []).filter((s) => s.name.toLowerCase() !== 'slftp');
  }, [sites]);
  const siteOptions = useMemo(() => visibleSites.map((s) => s.name), [visibleSites]);
  const paletteSites = useMemo(() => {
    const q = paletteFilter.trim().toLowerCase();
    if (!q) return visibleSites;
    return visibleSites.filter((s) => s.name.toLowerCase().includes(q));
  }, [paletteFilter, visibleSites]);

  if (sitesLoading) return <Center h={400}><Loader size="xl" /></Center>;

  const matrixHeight = 'calc(100vh - 220px)';

  const handleMatrixDragOver = (e: React.DragEvent) => {
    if (!isDraggingSiteRef.current) return;
    const viewport = matrixViewportRef.current;
    if (!viewport) return;

    const rect = viewport.getBoundingClientRect();
    const y = e.clientY - rect.top;
    const edge = 60;
    const step = 18;

    if (y < edge) viewport.scrollTop -= step;
    else if (y > rect.height - edge) viewport.scrollTop += step;
  };

  return (
    <Stack gap="md">
      <Group justify="space-between" align="center">
        <Group gap="xs">
          <IconRoute size="2rem" />
          <Title order={2}>Routes Management</Title>
          <Group gap="xs" ml="lg">
            <Button
              variant="default"
              size="sm"
              leftSection={<IconDownload size="1rem" />}
              loading={exportLoading}
              onClick={handleExport}
            >
              Export JSON
            </Button>
            <FileButton onChange={handleImport} accept="application/json">
              {(props) => (
                <Button
                  {...props}
                  variant="default"
                  size="sm"
                  leftSection={<IconUpload size="1rem" />}
                  loading={importLoading}
                >
                  Import JSON
                </Button>
              )}
            </FileButton>
          </Group>
        </Group>
        <SegmentedControl
          value={viewMode}
          onChange={setViewMode}
          data={[
            { label: 'Bulk Operations', value: 'bulk' },
            { label: 'Matrix View', value: 'matrix' },
          ]}
        />
      </Group>

      {viewMode === 'bulk' && (
        <Stack gap="md">
          <Card shadow="sm" padding="lg" radius="md" withBorder>
            <Stack gap="md">
              <Group gap="xs">
                <IconBolt size="1.5rem" />
                <Text fw={600} size="lg">Bulk Route Operations</Text>
              </Group>
              <Text size="sm" c="gray.6">Create multiple routes at once. Select sources and destinations, then apply speed to all combinations.</Text>

              <Group grow align="flex-start">
                <Stack gap="xs">
                  <Text fw={500}>Source Sites ({selectedSources.length} selected)</Text>
                  <MultiSelect
                    data={siteOptions}
                    value={selectedSources}
                    onChange={setSelectedSources}
                    placeholder="Select source sites..."
                    searchable
                    clearable
                    maxDropdownHeight={300}
                  />
                </Stack>

                <Stack gap="xs">
                  <Text fw={500}>Destination Sites ({selectedDests.length} selected)</Text>
                  <MultiSelect
                    data={siteOptions}
                    value={selectedDests}
                    onChange={setSelectedDests}
                    placeholder="Select destination sites..."
                    searchable
                    clearable
                    maxDropdownHeight={300}
                  />
                </Stack>
              </Group>

              <Group align="flex-end">
                <NumberInput
                  label="Speed (1-9)"
                  value={bulkSpeed}
                  onChange={(val) => setBulkSpeed(typeof val === 'number' ? val : '')}
                  min={1}
                  max={9}
                  w={120}
                  size="sm"
                />
                <Checkbox
                  label="Add back routes (bidirectional)"
                  checked={addBackRoutes}
                  onChange={(e) => setAddBackRoutes(e.currentTarget.checked)}
                  description="Creates routes in both directions"
                />
              </Group>

              <Group gap="md">
                <Checkbox label="Locked" checked={bulkLocked} onChange={(e) => setBulkLocked(e.currentTarget.checked)} />
                <Checkbox label="Affil Only" checked={bulkAffilOnly} onChange={(e) => setBulkAffilOnly(e.currentTarget.checked)} />
                <Checkbox label="No Affil" checked={bulkNoAffil} onChange={(e) => setBulkNoAffil(e.currentTarget.checked)} />
              </Group>

              <Group>
                <Button
                  onClick={handleBulkSetRoutes}
                  disabled={selectedSources.length === 0 || selectedDests.length === 0 || bulkSpeed === ''}
                  loading={setRouteMutation.isPending}
                  size="lg"
                  leftSection={<IconBolt size="1.2rem" />}
                >
                  Create {selectedSources.length * selectedDests.length * (addBackRoutes ? 2 : 1)} Routes
                </Button>
                <Button
                  variant="light"
                  onClick={() => {
                    setSelectedSources([]);
                    setSelectedDests([]);
                    setBulkSpeed(5);
                    setAddBackRoutes(false);
                    setBulkLocked(false);
                    setBulkAffilOnly(false);
                    setBulkNoAffil(false);
                  }}
                >
                  Clear All
                </Button>
              </Group>

              {selectedSources.length > 0 && selectedDests.length > 0 && (
                <Card withBorder bg="blue.0" p="sm">
                  <Text size="sm" fw={500}>Preview:</Text>
                  <Text size="xs" c="gray.7">
                    Will create routes from {selectedSources.join(', ')} → {selectedDests.join(', ')}
                    {addBackRoutes && ' (and back routes)'}
                  </Text>
                </Card>
              )}
            </Stack>
          </Card>
        </Stack>
      )}

      {viewMode === 'matrix' && (
        <Group align="flex-start" gap="md">
          <Card shadow="sm" padding="md" radius="md" withBorder style={{ width: 200, position: 'sticky', top: 'var(--mantine-spacing-md)' }}>
            <Stack gap="md">
              <Group gap="xs">
                <Text fw={600}>Sites Palette</Text>
              </Group>
              <TextInput
                placeholder="Filter sites..."
                value={paletteFilter}
                onChange={(e) => setPaletteFilter(e.currentTarget.value)}
                leftSection={<IconSearch size="1rem" />}
                rightSection={
                  paletteFilter ? (
                    <CloseButton aria-label="Clear filter" onClick={() => setPaletteFilter('')} />
                  ) : null
                }
                size="xs"
              />
              <Text size="xs" c="gray.6">Drag sites to rows →</Text>
              <ScrollArea h={matrixHeight}>
                <Stack gap="xs">
                  {paletteSites.map((site) => (
                    <Badge
                      key={site.name}
                      size="lg"
                      variant="light"
                      color="cyan"
                      draggable
                      onDragStart={(e) => {
                        isDraggingSiteRef.current = true;
                        e.dataTransfer.setData('text/plain', site.name);
                        e.dataTransfer.effectAllowed = 'copy';
                      }}
                      onDragEnd={() => {
                        isDraggingSiteRef.current = false;
                      }}
                      style={{ cursor: 'grab', width: '100%', justifyContent: 'center' }}
                    >
                      {site.name}
                    </Badge>
                  ))}
                </Stack>
              </ScrollArea>
            </Stack>
          </Card>

          <Card shadow="sm" padding="lg" radius="md" withBorder style={{ flex: 1 }}>
            <Stack gap="md">
              <Group justify="space-between">
                <Group gap="xs">
                  <IconGridDots size="1.5rem" />
                  <Text fw={600} size="lg">Route Matrix</Text>
                </Group>
                <Group gap="md">
                  <Group gap="xs">
                    <Text size="sm" fw={500}>Direction:</Text>
                    <SegmentedControl
                      value={routeDirection}
                      onChange={(val) => setRouteDirection(val as 'outgoing' | 'incoming')}
                      data={[
                        { label: 'Outgoing →', value: 'outgoing' },
                        { label: '← Incoming', value: 'incoming' },
                      ]}
                      size="xs"
                    />
                  </Group>
                  <Button
                    variant="light"
                    size="xs"
                    onClick={() => setRefreshKey(prev => prev + 1)}
                    leftSection={<IconRefresh size="1rem" />}
                  >
                    Refresh
                  </Button>
                </Group>
              </Group>
              <Text size="sm" c="gray.6">
                🎯 Drag sites from palette to rows to create routes • {routeDirection === 'outgoing' ? 'Click pencil to edit speed/locked/affil flags' : 'Click pencil to edit incoming route flags'} • Click × to delete
              </Text>

              <ScrollArea h={matrixHeight} viewportRef={matrixViewportRef}>
                <div onDragOver={handleMatrixDragOver}>
                  <Table striped highlightOnHover withTableBorder withColumnBorders>
                    <Table.Thead>
                      <Table.Tr>
                        <Table.Th style={{ position: 'sticky', left: 0, background: 'var(--mantine-color-body)', zIndex: 1, minWidth: 120 }}>
                          <Text fw={700}>{routeDirection === 'outgoing' ? 'Source' : 'Destination'}</Text>
                        </Table.Th>
                        <Table.Th>
                          <Text fw={700}>
                            {routeDirection === 'outgoing'
                              ? 'Destinations → Routes (drop here to create)'
                              : '← Sources / Incoming Routes (drop here to create)'}
                          </Text>
                        </Table.Th>
                      </Table.Tr>
                    </Table.Thead>
                    <Table.Tbody>
                      {visibleSites.map((site) => (
                        <Table.Tr key={`${site.name}-${refreshKey}-${routeDirection}`}>
                          <Table.Td style={{ position: 'sticky', left: 0, background: 'var(--mantine-color-body)', zIndex: 1, fontWeight: 600 }}>
                            <Badge size="lg" variant="dot" color="blue">{site.name}</Badge>
                          </Table.Td>
                          <Table.Td style={{ minWidth: 400 }}>
                            {routeDirection === 'outgoing' ? (
                              <RoutesList
                                sourceSite={site.name}
                                quickSpeed={quickSpeed}
                                onRouteDeleted={() => setRefreshKey(prev => prev + 1)}
                              />
                            ) : allRoutesLoading ? (
                              <Loader size="xs" />
                            ) : (
                              <IncomingRoutesList
                                destSite={site.name}
                                quickSpeed={quickSpeed}
                                onRouteDeleted={() => setRefreshKey(prev => prev + 1)}
                                allRoutes={allRoutesMap || new Map()}
                              />
                            )}
                          </Table.Td>
                        </Table.Tr>
                      ))}
                    </Table.Tbody>
                  </Table>
                </div>
              </ScrollArea>
            </Stack>
          </Card>
        </Group>
      )}
    </Stack>
  );
}
