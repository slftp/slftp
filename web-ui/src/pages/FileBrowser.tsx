import { useState, useEffect, useRef } from 'react';
import { Title, Group, Button, Grid, Paper, Stack, Text, Badge, Tooltip, Menu, Tabs } from '@mantine/core';
import { IconArrowRight, IconArrowLeft, IconArrowsExchange, IconDots, IconBolt, IconFolders, IconArrowsLeftRight } from '@tabler/icons-react';
import { FileBrowserPane } from './FileBrowserPane';
import { apiClient } from '../api/client';
import type { FileEntry } from '../api/client';
import { notifications } from '@mantine/notifications';
import { useQueryClient } from '@tanstack/react-query';
import { fetchBrowserPath } from '../api/client';

export function FileBrowser() {
  const queryClient = useQueryClient();
  const [activeTab, setActiveTab] = useState<string | null>('fxp');
  const [leftSite, setLeftSite] = useState<string | null>(null);
  const [rightSite, setRightSite] = useState<string | null>(null);
  const [leftPath, setLeftPath] = useState('/');
  const [rightPath, setRightPath] = useState('/');
  const [leftSelection, setLeftSelection] = useState<FileEntry[]>([]);
  const [rightSelection, setRightSelection] = useState<FileEntry[]>([]);
  const [transferring, setTransferring] = useState(false);
  const mountedRef = useRef(true);
  useEffect(() => {
    return () => { mountedRef.current = false; };
  }, []);

  const refreshPanesOnce = async () => {
    const ls = leftSite;
    const rs = rightSite;
    const lp = leftPath;
    const rp = rightPath;
    if (ls) await fetchBrowserPath(ls, lp, true);
    if (rs && activeTab === 'fxp') await fetchBrowserPath(rs, rp, true);
    if (ls) queryClient.invalidateQueries({ queryKey: ['browser', ls, lp] });
    if (rs && activeTab === 'fxp') queryClient.invalidateQueries({ queryKey: ['browser', rs, rp] });
  };

  const sleep = (ms: number) => new Promise<void>((resolve) => window.setTimeout(resolve, ms));

  const waitForTasksAndRefresh = async (taskUids: number[]) => {
    if (taskUids.length === 0) return;

    const deadline = Date.now() + 15 * 60 * 1000;
    const remaining = new Set(taskUids);

    while (remaining.size > 0 && Date.now() < deadline) {
      if (!mountedRef.current) return;
      for (const uid of Array.from(remaining)) {
        if (!mountedRef.current) return;
        const res = await apiClient.post('/ApiQueueService/GetTask', { TaskUid: uid });
        const info = Array.isArray(res.data?.result) ? res.data.result[0] : res.data?.result?.[0] ?? res.data?.result ?? res.data;
        const status = String(info?.Status ?? '').toLowerCase();

        if (status === 'completed') {
          remaining.delete(uid);
          continue;
        }
        if (status === 'failed') {
          const reason = info?.Error || `(uid ${uid})`;
          throw new Error(`Transfer task failed: ${reason}`);
        }
      }
      await sleep(1500);
    }

    if (!mountedRef.current) return;
    if (remaining.size > 0) {
      throw new Error('Timed out waiting for transfer completion');
    }

    await refreshPanesOnce();
  };

  const canCopyToRight = !!leftSite && !!rightSite && leftSelection.length > 0 && !transferring;
  const canCopyToLeft = !!leftSite && !!rightSite && rightSelection.length > 0 && !transferring;

  const handleTransfer = async (source: 'left' | 'right') => {
    const srcSite = source === 'left' ? leftSite : rightSite;
    const dstSite = source === 'left' ? rightSite : leftSite;
    const srcPath = source === 'left' ? leftPath : rightPath;
    const dstPath = source === 'left' ? rightPath : leftPath;
    const selection = source === 'left' ? leftSelection : rightSelection;

    if (!srcSite || !dstSite || selection.length === 0) return;

    setTransferring(true);
    let queued = 0;
    const taskUids: number[] = [];

    try {
      for (const file of selection) {
        if (file.is_dir) {
          // Use the new Release Transfer API for directories
          // This delegates recursion and transfer logic to the backend (TPazoDirlistTask)
          // similar to the !transfer IRC command.
          const res = await apiClient.post('/ApiQueueService/CreateReleaseTransferTask', {
            SourceSite: srcSite,
            DestSite: dstSite,
            SourceDir: srcPath,
            DestDir: dstPath,
            RlsName: file.name
          });
          const taskUid = Array.isArray(res.data?.result) ? res.data.result[0] : res.data?.result ?? res.data;
          if (!taskUid || taskUid === 0) {
             throw new Error(`Directory transfer task for ${file.name} was not queued`);
          }
          taskUids.push(Number(taskUid));
          queued++;
        } else {
          // Standard file transfer
          const fullSrcPath = srcPath + (srcPath === '/' ? '' : '/') + file.name;
          const res = await apiClient.post('/ApiQueueService/CreateTransferTask', {
            SourceSite: srcSite,
            DestSite: dstSite,
            Section: 'FXP',
            Dir: dstPath,
            FileName: fullSrcPath,
          });
          const taskUid = Array.isArray(res.data?.result) ? res.data.result[0] : res.data?.result ?? res.data;
          if (!taskUid || taskUid === 0) {
            throw new Error(`Transfer task for ${file.name} was not queued`);
          }
          taskUids.push(Number(taskUid));
          queued++;
        }
      }

      notifications.show({
        title: 'Transfer Queued',
        message: `${queued} files queued for transfer from ${srcSite} to ${dstSite}.`,
        color: 'green',
      });

      // Clear selection and refresh once when tasks report completion.
      if (source === 'left') setLeftSelection([]);
      else setRightSelection([]);

      await waitForTasksAndRefresh(taskUids);
    } catch (e: unknown) {
      console.error(e);
      const message = e instanceof Error ? e.message : 'Failed to queue transfer';
      notifications.show({ title: 'Transfer Error', message, color: 'red' });
    } finally {
      setTransferring(false);
    }
  };

  return (
    <Stack style={{ height: 'calc(100dvh - 60px - 2rem)' }} mih={0} gap="md">
      <Group justify="space-between" wrap="wrap" gap="sm">
        <div>
          <Group gap="xs">
            <Title order={2}>Browser</Title>
          </Group>
          <Text size="sm" c="dimmed">
            Browse sites and manage files
          </Text>
        </div>
      </Group>

      <Tabs value={activeTab} onChange={setActiveTab} variant="default">
        <Tabs.List>
          <Tabs.Tab value="fxp" leftSection={<IconArrowsLeftRight size={16} />}>FXP</Tabs.Tab>
          <Tabs.Tab value="browse" leftSection={<IconFolders size={16} />}>Browse</Tabs.Tab>
        </Tabs.List>
      </Tabs>

      <Paper withBorder radius="md" p="sm">
        <Group justify="space-between" wrap="wrap" gap="sm">
          <Group gap="xs" wrap="wrap">
            {activeTab === 'fxp' && (
              <>
                <Tooltip label="Queue transfer left → right (FXP)" withArrow withinPortal>
                  <Button
                    variant="light"
                    leftSection={<IconArrowRight size="1.1rem" />}
                    disabled={!canCopyToRight}
                    onClick={() => handleTransfer('left')}
                  >
                    FXP L→R
                  </Button>
                </Tooltip>

                <Tooltip label="Queue transfer right → left (FXP)" withArrow withinPortal>
                  <Button
                    variant="light"
                    leftSection={<IconArrowLeft size="1.1rem" />}
                    disabled={!canCopyToLeft}
                    onClick={() => handleTransfer('right')}
                  >
                    FXP R→L
                  </Button>
                </Tooltip>

                <Tooltip label="Swap panes" withArrow withinPortal>
                  <Button
                    variant="default"
                    leftSection={<IconArrowsExchange size="1.1rem" />}
                    disabled={transferring}
                    onClick={() => {
                      setLeftSite(rightSite);
                      setRightSite(leftSite);
                      setLeftPath(rightPath);
                      setRightPath(leftPath);
                      setLeftSelection(rightSelection);
                      setRightSelection(leftSelection);
                    }}
                  >
                    Swap
                  </Button>
                </Tooltip>
              </>
            )}

            <Menu withinPortal position="bottom-start">
              <Menu.Target>
                <Button variant="default" leftSection={<IconDots size="1.1rem" />}>
                  Commands
                </Button>
              </Menu.Target>
              <Menu.Dropdown>
                <Menu.Label>Planned</Menu.Label>
                <Menu.Item leftSection={<IconBolt size="1.0rem" />} disabled>
                  More commands (delete, mkdir, pre, nuke, …)
                </Menu.Item>
              </Menu.Dropdown>
            </Menu>
          </Group>

          <Group gap="xs" wrap="wrap">
            <Badge variant="light">{leftSelection.length} selected {activeTab === 'fxp' ? '(L)' : ''}</Badge>
            {activeTab === 'fxp' && (
              <Badge variant="light">{rightSelection.length} selected (R)</Badge>
            )}
          </Group>
        </Group>
      </Paper>

      <Grid style={{ flex: 1, minHeight: 0 }} gutter="md">
        <Grid.Col span={activeTab === 'browse' ? 12 : { base: 12, md: 6 }} style={{ height: '100%', minHeight: 0 }}>
          <FileBrowserPane
            site={leftSite}
            path={leftPath}
            onSiteChange={setLeftSite}
            onPathChange={setLeftPath}
            onSelectionChange={setLeftSelection}
          />
        </Grid.Col>

        {activeTab === 'fxp' && (
          <Grid.Col span={{ base: 12, md: 6 }} style={{ height: '100%', minHeight: 0 }}>
            <FileBrowserPane
              site={rightSite}
              path={rightPath}
              onSiteChange={setRightSite}
              onPathChange={setRightPath}
              onSelectionChange={setRightSelection}
            />
          </Grid.Col>
        )}
      </Grid>
    </Stack>
  );
}
