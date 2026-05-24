import { useMemo, useState, useEffect } from 'react';
import { ActionIcon, Alert, Badge, Button, Group, Loader, Modal, ScrollArea, Stack, Table, TextInput, Text, Tooltip, Select, MultiSelect, Switch, Box, Paper } from '@mantine/core';
import { useDisclosure } from '@mantine/hooks';
import { useForm } from '@mantine/form';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { IconAlertCircle, IconEye, IconPlus, IconSearch, IconRefresh, IconPlayerStop } from '@tabler/icons-react';
import { notifications } from '@mantine/notifications';
import {
  getSpreadJobs,
  getSpreadJob,
  startSpreadJob,
  resetSpreadJob,
  abortSpreadJob,
  getSites,
  getSections,
  getCbftpLatencies,
} from '../../api/cbftpClient';
import type {
  CbftpSpreadJob,
  SpreadJobFilters,
  StartSpreadJobRequest,
  ResetSpreadJobRequest,
  AbortSpreadJobRequest,
  CbftpLatencyEntry,
} from '../../api/cbftpClient';

export function SpreadJobs() {
  const queryClient = useQueryClient();
  const [search, setSearch] = useState('');
  const [statusFilter, setStatusFilter] = useState<string>('');
  const [profileFilter, setProfileFilter] = useState<string>('');
  const [detailsOpened, { open: openDetails, close: closeDetails }] = useDisclosure(false);
  const [startOpened, { open: openStart, close: closeStart }] = useDisclosure(false);
  const [resetOpened, { open: openReset, close: closeReset }] = useDisclosure(false);
  const [abortOpened, { open: openAbort, close: closeAbort }] = useDisclosure(false);
  const [selectedJob, setSelectedJob] = useState<string | null>(null);

  const filters: SpreadJobFilters = {
    status: statusFilter || undefined,
    profile: profileFilter || undefined,
    name: search || undefined,
  };

  const { data: jobNamesRaw, isLoading, error } = useQuery<string[]>({
    queryKey: ['cbftp-spread-jobs', filters],
    queryFn: () => getSpreadJobs(filters) as Promise<string[]>,
    refetchInterval: 30000,
  });

  const JOBS_LIMIT = 500;

  const jobNames = useMemo(() => {
    if (!jobNamesRaw) return [];
    return [...jobNamesRaw].reverse().slice(0, JOBS_LIMIT);
  }, [jobNamesRaw]);

  const { data: jobDetailsList } = useQuery<CbftpSpreadJob[]>({
    queryKey: ['cbftp-spread-jobs-details', jobNames],
    queryFn: async () => {
      if (!jobNames || jobNames.length === 0) {
        return [];
      }
      const results = await Promise.all(
        jobNames.map((jobName) =>
          getSpreadJob(jobName).catch(() => null)
        )
      );
      return results.filter((job): job is CbftpSpreadJob => job !== null);
    },
    enabled: !!jobNames?.length,
    refetchInterval: jobNames && jobNames.length > 0 ? 30000 : false,
  });

  const jobDetailsByName = useMemo(() => {
    const entries = jobDetailsList?.map((job) => [job.name, job] as const) ?? [];
    return new Map(entries);
  }, [jobDetailsList]);

  const { data: jobDetails, error: detailsError } = useQuery<CbftpSpreadJob>({
    queryKey: ['cbftp-spread-job', selectedJob],
    queryFn: () => getSpreadJob(selectedJob!),
    enabled: !!selectedJob,
    refetchInterval: (query) => {
      return query.state.data?.status === 'RUNNING' ? 30000 : false;
    },
    retry: false, // Don't retry if a job is gone
  });

  // Effect to handle details error
  useEffect(() => {
    if (detailsError && detailsOpened) {
      notifications.show({ 
        title: 'Job Not Found', 
        message: 'The selected spread job is no longer available in history.', 
        color: 'red' 
      });
      closeDetails();
      setSelectedJob(null);
    }
  }, [detailsError, detailsOpened, closeDetails]);

  const { data: siteNames } = useQuery<string[]>({
    queryKey: ['cbftp-sites-for-spread'],
    queryFn: () => getSites(),
  });

  const { data: sectionNames } = useQuery<string[]>({
    queryKey: ['cbftp-sections-for-spread'],
    queryFn: () => getSections(),
  });

  const { data: latencies } = useQuery<CbftpLatencyEntry[]>({
    queryKey: ['cbftp-latencies'],
    queryFn: () => getCbftpLatencies(),
    refetchInterval: 30000,
  });

  const latencyByName = useMemo(() => {
    const map = new Map<string, CbftpLatencyEntry>();
    latencies?.forEach((item) => map.set(item.name, item));
    return map;
  }, [latencies]);

  const startMutation = useMutation({
    mutationFn: startSpreadJob,
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Spread job started', color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['cbftp-spread-jobs'] });
      closeStart();
      form.reset();
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const resetMutation = useMutation({
    mutationFn: ({ name, options }: { name: string; options?: ResetSpreadJobRequest }) =>
      resetSpreadJob(name, options),
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Spread job reset', color: 'blue' });
      queryClient.invalidateQueries({ queryKey: ['cbftp-spread-jobs'] });
      closeReset();
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const abortMutation = useMutation({
    mutationFn: ({ name, options }: { name: string; options?: AbortSpreadJobRequest }) =>
      abortSpreadJob(name, options),
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Spread job aborted', color: 'orange' });
      queryClient.invalidateQueries({ queryKey: ['cbftp-spread-jobs'] });
      closeAbort();
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const form = useForm({
    initialValues: {
      section: '',
      name: '',
      profile: 'RACE' as 'RACE' | 'DISTRIBUTE' | 'PREPARE',
      sites: [] as string[],
      sites_dlonly: [] as string[],
      sites_all: false,
      reset: false,
    },
    validate: {
      section: (value) => (value.trim() ? null : 'Section is required'),
      name: (value) => (value.trim() ? null : 'Name is required'),
    },
  });

  const resetForm = useForm({
    initialValues: {
      hard: false,
    },
  });

  const abortForm = useForm({
    initialValues: {
      deleteOption: 'NONE' as 'NONE' | 'INCOMPLETE' | 'OWN' | 'ALL',
      removeSites: [] as string[],
    },
  });

  const handleViewDetails = (jobName: string) => {
    setSelectedJob(jobName);
    openDetails();
  };

  const handleResetClick = (jobName: string) => {
    setSelectedJob(jobName);
    resetForm.reset();
    openReset();
  };

  const handleAbortClick = (jobName: string) => {
    setSelectedJob(jobName);
    abortForm.reset();
    openAbort();
  };

  const handleSubmit = (values: typeof form.values) => {
    const request: StartSpreadJobRequest = {
      section: values.section,
      name: values.name,
      profile: values.profile,
      sites_all: values.sites_all,
      reset: values.reset,
    };

    if (!values.sites_all) {
      request.sites = values.sites;
      request.sites_dlonly = values.sites_dlonly;
    }

    startMutation.mutate(request);
  };

  const handleResetSubmit = (values: typeof resetForm.values) => {
    if (selectedJob) {
      resetMutation.mutate({
        name: selectedJob,
        options: { hard: values.hard },
      });
    }
  };

  const handleAbortSubmit = (values: typeof abortForm.values) => {
    if (selectedJob) {
      const options: AbortSpreadJobRequest = {};

      if (values.removeSites.length > 0) {
        options.sites = values.removeSites;
      } else if (values.deleteOption !== 'NONE') {
        options.delete = values.deleteOption;
      }

      abortMutation.mutate({ name: selectedJob, options });
    }
  };

  const getProfileBadge = (profile: string) => {
    const color = profile === 'RACE' ? 'blue' : profile === 'DISTRIBUTE' ? 'green' : 'orange';
    return <Badge size="xs" color={color} variant="light">{profile}</Badge>;
  };

  const getStatusBadge = (status: string) => {
    switch (status) {
      case 'RUNNING': return <Badge size="xs" color="blue">Running</Badge>;
      case 'DONE': return <Badge size="xs" color="green">Done</Badge>;
      case 'ABORTED': return <Badge size="xs" color="red">Aborted</Badge>;
      case 'PREPARED': return <Badge size="xs" color="yellow">Prepared</Badge>;
      default: return <Badge size="xs">{status}</Badge>;
    }
  };

  if (isLoading) {
    return <Loader />;
  }

  if (error) {
    return (
      <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
        {error instanceof Error ? error.message : 'Failed to fetch spread jobs'}
      </Alert>
    );
  }

  return (
    <>
      <Group justify="apart" mb="md">
        <Group gap="sm">
          <TextInput
            placeholder="Search jobs..."
            leftSection={<IconSearch size={16} />}
            value={search}
            onChange={(e) => setSearch(e.currentTarget.value)}
            style={{ width: 200 }}
          />
          <Select
            placeholder="Status"
            clearable
            data={[
              { value: 'RUNNING', label: 'Running' },
              { value: 'DONE', label: 'Done' },
              { value: 'ABORTED', label: 'Aborted' },
              { value: 'PREPARED', label: 'Prepared' },
            ]}
            value={statusFilter}
            onChange={(value) => setStatusFilter(value || '')}
            style={{ width: 150 }}
          />
          <Select
            placeholder="Profile"
            clearable
            data={[
              { value: 'RACE', label: 'Race' },
              { value: 'DISTRIBUTE', label: 'Distribute' },
              { value: 'PREPARE', label: 'Prepare' },
            ]}
            value={profileFilter}
            onChange={(value) => setProfileFilter(value || '')}
            style={{ width: 150 }}
          />
        </Group>
        <Button leftSection={<IconPlus size={16} />} onClick={openStart}>
          Start Spread Job
        </Button>
      </Group>

      <Text mb="md" size="sm" c="dimmed">
        Showing {jobNames?.length || 0} of {jobNamesRaw?.length || 0} jobs (newest first, max {JOBS_LIMIT})
      </Text>

      <ScrollArea>
        <Table striped highlightOnHover fz="xs" verticalSpacing={4} style={{ tableLayout: 'fixed', width: '100%' }}>
          <Table.Thead>
            <Table.Tr>
              <Table.Th style={{ width: '35%' }}>Name</Table.Th>
              <Table.Th style={{ width: '12%' }}>Section</Table.Th>
              <Table.Th style={{ width: '8%' }}>Profile</Table.Th>
              <Table.Th style={{ width: '8%' }}>Status</Table.Th>
              <Table.Th style={{ width: '5%' }}>Sites</Table.Th>
              <Table.Th style={{ width: '12%' }}>Latency</Table.Th>
              <Table.Th style={{ width: '20%' }}>Actions</Table.Th>
            </Table.Tr>
          </Table.Thead>
          <Table.Tbody>
            {jobNames?.map((jobName) => {
              const details = jobDetailsByName.get(jobName);
              return (
                <Table.Tr key={jobName}>
                  <Table.Td style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                    <Tooltip label={jobName} openDelay={400}>
                      <Text size="xs" style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                        {jobName}
                      </Text>
                    </Tooltip>
                  </Table.Td>
                  <Table.Td style={{ overflow: 'hidden' }}>
                    {details?.section
                      ? <Badge size="xs" variant="light">{details.section}</Badge>
                      : <Badge size="xs" color="gray" variant="light">-</Badge>}
                  </Table.Td>
                  <Table.Td>
                    {details?.profile ? getProfileBadge(details.profile) : <Badge size="xs" color="gray" variant="light">-</Badge>}
                  </Table.Td>
                  <Table.Td>
                    {details?.status ? getStatusBadge(details.status) : <Badge size="xs" color="gray" variant="light">-</Badge>}
                  </Table.Td>
                  <Table.Td>
                    <Text size="xs">{typeof details?.sites?.length === 'number' ? details.sites.length : '-'}</Text>
                  </Table.Td>
                  <Table.Td>
                    {(() => {
                      const lat = latencyByName.get(jobName);
                      if (!lat) return <Text size="xs" c="dimmed">-</Text>;
                      return (
                        <Tooltip label={`PRE: ${lat.pre_time} / UDP: ${lat.udp_time}`} openDelay={400}>
                          <Text size="xs" fw={500} c={lat.latency_ms < 1000 ? 'green' : lat.latency_ms < 5000 ? 'yellow' : 'red'}>
                            {lat.latency_ms} ms
                          </Text>
                        </Tooltip>
                      );
                    })()}
                  </Table.Td>
                  <Table.Td style={{ whiteSpace: 'nowrap' }}>
                    <Group gap={4} wrap="nowrap">
                      <Tooltip label="View Details">
                        <ActionIcon size="sm" variant="light" onClick={() => handleViewDetails(jobName)}>
                          <IconEye size={14} />
                        </ActionIcon>
                      </Tooltip>
                      <Tooltip label="Reset">
                        <ActionIcon size="sm" variant="light" color="blue" onClick={() => handleResetClick(jobName)}>
                          <IconRefresh size={14} />
                        </ActionIcon>
                      </Tooltip>
                      <Tooltip label="Abort">
                        <ActionIcon size="sm" variant="light" color="red" onClick={() => handleAbortClick(jobName)}>
                          <IconPlayerStop size={14} />
                        </ActionIcon>
                      </Tooltip>
                    </Group>
                  </Table.Td>
                </Table.Tr>
              );
            })}
          </Table.Tbody>
        </Table>
      </ScrollArea>

      {/* Details Modal */}
      <Modal opened={detailsOpened} onClose={closeDetails} title={`Job Details: ${selectedJob}`} size="lg">
        {jobDetails ? (
          <Stack gap="sm">
            <Group justify="apart">
              <Text fw={500}>Section:</Text>
              <Badge>{jobDetails.section}</Badge>
            </Group>
            <Group justify="apart">
              <Text fw={500}>Profile:</Text>
              {getProfileBadge(jobDetails.profile)}
            </Group>
            <Group justify="apart">
              <Text fw={500}>Status:</Text>
              {getStatusBadge(jobDetails.status)}
            </Group>
            <Group justify="apart">
              <Text fw={500}>Sites:</Text>
              <Text>{jobDetails.sites?.length || 0}</Text>
            </Group>
            {jobDetails.sites && jobDetails.sites.length > 0 && (
              <Box>
                <Text fw={500} mb="xs">Site List:</Text>
                <Group gap="xs">
                  {jobDetails.sites.map((site) => (
                    <Badge key={site} color={jobDetails.sites_dlonly?.includes(site) ? 'orange' : 'blue'}>
                      {site}
                      {jobDetails.sites_dlonly?.includes(site) && ' (DL Only)'}
                    </Badge>
                  ))}
                </Group>
              </Box>
            )}
            {jobDetails.progress && (
              <Box>
                <Text fw={500} mb="xs">Per-Site Progress:</Text>
                <Stack gap="xs">
                  {Object.entries(jobDetails.progress).map(([site, progress]) => (
                    <Paper key={site} p="xs" withBorder>
                      <Group justify="apart">
                        <Text size="sm">{site}</Text>
                        <Text size="sm" c="dimmed">
                          {progress.files_done}/{progress.total_files} files
                        </Text>
                      </Group>
                    </Paper>
                  ))}
                </Stack>
              </Box>
            )}
          </Stack>
        ) : (
          <Loader />
        )}
      </Modal>

      {/* Start Job Modal */}
      <Modal opened={startOpened} onClose={closeStart} title="Start Spread Job" size="lg">
        <form onSubmit={form.onSubmit(handleSubmit)}>
          <Stack gap="md">
            <Select
              label="Section"
              required
              data={sectionNames || []}
              searchable
              {...form.getInputProps('section')}
            />

            <TextInput label="Name" required {...form.getInputProps('name')} />

            <Select
              label="Profile"
              data={[
                { value: 'RACE', label: 'Race' },
                { value: 'DISTRIBUTE', label: 'Distribute' },
                { value: 'PREPARE', label: 'Prepare' },
              ]}
              {...form.getInputProps('profile')}
            />

            <Switch
              label="Use All Sites"
              {...form.getInputProps('sites_all', { type: 'checkbox' })}
            />

            {!form.values.sites_all && (
              <>
                <MultiSelect
                  label="Sites"
                  data={siteNames || []}
                  searchable
                  {...form.getInputProps('sites')}
                />

                <MultiSelect
                  label="Download-Only Sites (optional)"
                  data={siteNames || []}
                  searchable
                  {...form.getInputProps('sites_dlonly')}
                />
              </>
            )}

            <Switch
              label="Reset if exists"
              {...form.getInputProps('reset', { type: 'checkbox' })}
            />

            <Group justify="flex-end" mt="md">
              <Button variant="default" onClick={closeStart}>
                Cancel
              </Button>
              <Button type="submit" loading={startMutation.isPending}>
                Start Job
              </Button>
            </Group>
          </Stack>
        </form>
      </Modal>

      {/* Reset Modal */}
      <Modal opened={resetOpened} onClose={closeReset} title="Reset Spread Job" size="sm">
        <form onSubmit={resetForm.onSubmit(handleResetSubmit)}>
          <Stack gap="md">
            <Text>Reset job: <Text span fw={700}>{selectedJob}</Text></Text>

            <Switch
              label="Hard Reset"
              description="Re-create directories (hard reset)"
              {...resetForm.getInputProps('hard', { type: 'checkbox' })}
            />

            <Group justify="flex-end" mt="md">
              <Button variant="default" onClick={closeReset}>
                Cancel
              </Button>
              <Button type="submit" loading={resetMutation.isPending}>
                Reset
              </Button>
            </Group>
          </Stack>
        </form>
      </Modal>

      {/* Abort Modal */}
      <Modal opened={abortOpened} onClose={closeAbort} title="Abort Spread Job" size="sm">
        <form onSubmit={abortForm.onSubmit(handleAbortSubmit)}>
          <Stack gap="md">
            <Text>Abort job: <Text span fw={700}>{selectedJob}</Text></Text>

            <Select
              label="Delete Files"
              data={[
                { value: 'NONE', label: 'None' },
                { value: 'INCOMPLETE', label: 'Incomplete' },
                { value: 'OWN', label: 'Own' },
                { value: 'ALL', label: 'All' },
              ]}
              {...abortForm.getInputProps('deleteOption')}
            />

            <Text size="sm" c="dimmed">OR</Text>

            <MultiSelect
              label="Remove Sites (instead of aborting)"
              data={jobDetails?.sites || []}
              searchable
              {...abortForm.getInputProps('removeSites')}
            />

            <Group justify="flex-end" mt="md">
              <Button variant="default" onClick={closeAbort}>
                Cancel
              </Button>
              <Button color="red" type="submit" loading={abortMutation.isPending}>
                Abort
              </Button>
            </Group>
          </Stack>
        </form>
      </Modal>
    </>
  );
}
