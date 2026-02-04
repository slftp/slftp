import { useMemo, useState } from 'react';
import { ActionIcon, Alert, Badge, Button, Group, Loader, Modal, Stack, Table, TextInput, Text, Tooltip, Select, Tabs, Progress } from '@mantine/core';
import { useDisclosure } from '@mantine/hooks';
import { useForm } from '@mantine/form';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { IconAlertCircle, IconEye, IconPlus, IconSearch, IconRefresh, IconPlayerStop, IconArrowsRightLeft, IconDownload, IconUpload } from '@tabler/icons-react';
import { notifications } from '@mantine/notifications';
import {
  getTransferJobs,
  getTransferJob,
  startTransferJob,
  resetTransferJob,
  abortTransferJob,
  getSites,
  getSections,
} from '../../api/cbftpClient';
import type {
  CbftpTransferJob,
  TransferJobFilters,
  StartTransferJobRequest,
} from '../../api/cbftpClient';

export function TransferJobs() {
  const queryClient = useQueryClient();
  const [search, setSearch] = useState('');
  const [statusFilter, setStatusFilter] = useState<string>('');
  const [typeFilter, setTypeFilter] = useState<string>('');
  const [detailsOpened, { open: openDetails, close: closeDetails }] = useDisclosure(false);
  const [startOpened, { open: openStart, close: closeStart }] = useDisclosure(false);
  const [selectedJob, setSelectedJob] = useState<string | null>(null);

  const filters: TransferJobFilters = {
    status: statusFilter || undefined,
    type: typeFilter || undefined,
    name: search || undefined,
  };

  const { data: jobNamesRaw, isLoading, error } = useQuery<string[]>({
    queryKey: ['cbftp-transfer-jobs', filters],
    queryFn: () => getTransferJobs(filters),
    refetchInterval: 30000,
  });

  const jobNames = useMemo(() => {
    if (!jobNamesRaw) return [];
    return [...jobNamesRaw].reverse();
  }, [jobNamesRaw]);

  const { data: jobDetailsList } = useQuery<CbftpTransferJob[]>({
    queryKey: ['cbftp-transfer-jobs-details', jobNames],
    queryFn: async () => {
      if (!jobNames || jobNames.length === 0) {
        return [];
      }
      return Promise.all(jobNames.map((jobName) => getTransferJob(jobName)));
    },
    enabled: !!jobNames?.length,
    refetchInterval: jobNames && jobNames.length > 0 ? 30000 : false,
  });

  const jobDetailsByName = useMemo(() => {
    const entries = jobDetailsList?.map((job) => [job.name, job] as const) ?? [];
    return new Map(entries);
  }, [jobDetailsList]);

  const { data: jobDetails } = useQuery<CbftpTransferJob>({
    queryKey: ['cbftp-transfer-job', selectedJob],
    queryFn: () => getTransferJob(selectedJob!),
    enabled: !!selectedJob,
  });

  const { data: siteNames } = useQuery<string[]>({
    queryKey: ['cbftp-sites-for-jobs'],
    queryFn: () => getSites(),
  });

  const { data: sectionNames } = useQuery<string[]>({
    queryKey: ['cbftp-sections-for-jobs'],
    queryFn: () => getSections(),
  });

  const startMutation = useMutation({
    mutationFn: startTransferJob,
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Transfer job started', color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['cbftp-transfer-jobs'] });
      closeStart();
      form.reset();
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const resetMutation = useMutation({
    mutationFn: (nameOrId: string) => resetTransferJob(nameOrId),
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Transfer job reset', color: 'blue' });
      queryClient.invalidateQueries({ queryKey: ['cbftp-transfer-jobs'] });
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const abortMutation = useMutation({
    mutationFn: (nameOrId: string) => abortTransferJob(nameOrId),
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Transfer job aborted', color: 'orange' });
      queryClient.invalidateQueries({ queryKey: ['cbftp-transfer-jobs'] });
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const form = useForm({
    initialValues: {
      jobType: 'FXP' as 'FXP' | 'DOWNLOAD' | 'UPLOAD',
      name: '',
      src_site: '',
      src_pathType: 'section' as 'section' | 'path',
      src_section: '',
      src_path: '',
      dst_site: '',
      dst_pathType: 'section' as 'section' | 'path',
      dst_section: '',
      dst_path: '',
    },
    validate: {
      name: (value) => (value.trim() ? null : 'Name is required'),
    },
  });

  const handleViewDetails = (jobName: string) => {
    setSelectedJob(jobName);
    openDetails();
  };

  const handleReset = (jobName: string) => {
    resetMutation.mutate(jobName);
  };

  const handleAbort = (jobName: string) => {
    abortMutation.mutate(jobName);
  };

  const handleSubmit = (values: typeof form.values) => {
    const request: StartTransferJobRequest = {
      name: values.name,
    };

    if (values.jobType === 'FXP') {
      request.src_site = values.src_site;
      request.dst_site = values.dst_site;

      if (values.src_pathType === 'section') {
        request.src_section = values.src_section;
      } else {
        request.src_path = values.src_path;
      }

      if (values.dst_pathType === 'section') {
        request.dst_section = values.dst_section;
      } else {
        request.dst_path = values.dst_path;
      }
    } else if (values.jobType === 'DOWNLOAD') {
      request.src_site = values.src_site;

      if (values.src_pathType === 'section') {
        request.src_section = values.src_section;
      } else {
        request.src_path = values.src_path;
      }

      if (values.dst_path) {
        request.dst_path = values.dst_path;
      }
    } else if (values.jobType === 'UPLOAD') {
      request.dst_site = values.dst_site;

      if (values.dst_pathType === 'section') {
        request.dst_section = values.dst_section;
      } else {
        request.dst_path = values.dst_path;
      }

      if (values.src_path) {
        request.src_path = values.src_path;
      }
    }

    startMutation.mutate(request);
  };

  const getTypeIcon = (type: string) => {
    switch (type) {
      case 'FXP': return <IconArrowsRightLeft size={16} />;
      case 'DOWNLOAD': return <IconDownload size={16} />;
      case 'UPLOAD': return <IconUpload size={16} />;
      default: return null;
    }
  };

  const getTypeBadge = (type: string) => {
    const color = type === 'FXP' ? 'blue' : type === 'DOWNLOAD' ? 'green' : 'orange';
    return <Badge color={color} leftSection={getTypeIcon(type)}>{type}</Badge>;
  };

  const getStatusBadge = (status: string) => {
    switch (status) {
      case 'RUNNING': return <Badge color="blue">Running</Badge>;
      case 'DONE': return <Badge color="green">Done</Badge>;
      case 'ABORTED': return <Badge color="red">Aborted</Badge>;
      default: return <Badge>{status}</Badge>;
    }
  };

  if (isLoading) {
    return <Loader />;
  }

  if (error) {
    return (
      <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
        {error instanceof Error ? error.message : 'Failed to fetch transfer jobs'}
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
            ]}
            value={statusFilter}
            onChange={(value) => setStatusFilter(value || '')}
            style={{ width: 150 }}
          />
          <Select
            placeholder="Type"
            clearable
            data={[
              { value: 'FXP', label: 'FXP' },
              { value: 'DOWNLOAD', label: 'Download' },
              { value: 'UPLOAD', label: 'Upload' },
            ]}
            value={typeFilter}
            onChange={(value) => setTypeFilter(value || '')}
            style={{ width: 150 }}
          />
        </Group>
        <Button leftSection={<IconPlus size={16} />} onClick={openStart}>
          Start Transfer Job
        </Button>
      </Group>

      <Text mb="md">Total Jobs: {jobNames?.length || 0}</Text>

      <Table striped highlightOnHover>
        <Table.Thead>
          <Table.Tr>
            <Table.Th>Name</Table.Th>
            <Table.Th>Type</Table.Th>
            <Table.Th>Status</Table.Th>
            <Table.Th>Source</Table.Th>
            <Table.Th>Destination</Table.Th>
            <Table.Th>Actions</Table.Th>
          </Table.Tr>
        </Table.Thead>
        <Table.Tbody>
          {jobNames?.map((jobName) => {
            const details = jobDetailsByName.get(jobName);
            const source = details?.src_site || details?.src_section || details?.src_path;
            const destination = details?.dst_site || details?.dst_section || details?.dst_path;
            return (
              <Table.Tr key={jobName}>
                <Table.Td>{jobName}</Table.Td>
                <Table.Td>
                  {details?.type ? getTypeBadge(details.type) : <Badge color="gray">Unknown</Badge>}
                </Table.Td>
                <Table.Td>
                  {details?.status ? getStatusBadge(details.status) : <Badge color="gray">Unknown</Badge>}
                </Table.Td>
                <Table.Td>{source || '-'}</Table.Td>
                <Table.Td>{destination || '-'}</Table.Td>
                <Table.Td>
                  <Group gap="xs">
                    <Tooltip label="View Details">
                      <ActionIcon variant="light" onClick={() => handleViewDetails(jobName)}>
                        <IconEye size={16} />
                      </ActionIcon>
                    </Tooltip>
                    <Tooltip label="Reset">
                      <ActionIcon variant="light" color="blue" onClick={() => handleReset(jobName)}>
                        <IconRefresh size={16} />
                      </ActionIcon>
                    </Tooltip>
                    <Tooltip label="Abort">
                      <ActionIcon variant="light" color="red" onClick={() => handleAbort(jobName)}>
                        <IconPlayerStop size={16} />
                      </ActionIcon>
                    </Tooltip>
                  </Group>
                </Table.Td>
              </Table.Tr>
            );
          })}
        </Table.Tbody>
      </Table>

      {/* Details Modal */}
      <Modal opened={detailsOpened} onClose={closeDetails} title={`Job Details: ${selectedJob}`} size="lg">
        {jobDetails ? (
          <Stack gap="sm">
            <Group justify="apart">
              <Text fw={500}>Type:</Text>
              {getTypeBadge(jobDetails.type)}
            </Group>
            <Group justify="apart">
              <Text fw={500}>Status:</Text>
              {getStatusBadge(jobDetails.status)}
            </Group>
            <Group justify="apart">
              <Text fw={500}>Source Site:</Text>
              <Text>{jobDetails.src_site || 'N/A'}</Text>
            </Group>
            <Group justify="apart">
              <Text fw={500}>Source Path:</Text>
              <Text>{jobDetails.src_path || jobDetails.src_section || 'N/A'}</Text>
            </Group>
            <Group justify="apart">
              <Text fw={500}>Destination Site:</Text>
              <Text>{jobDetails.dst_site || 'N/A'}</Text>
            </Group>
            <Group justify="apart">
              <Text fw={500}>Destination Path:</Text>
              <Text>{jobDetails.dst_path || jobDetails.dst_section || 'N/A'}</Text>
            </Group>
            {jobDetails.progress !== undefined && (
              <>
                <Text fw={500}>Progress:</Text>
                <Progress value={jobDetails.progress} />
              </>
            )}
            <Group justify="flex-end" mt="md">
              <Button variant="light" onClick={() => selectedJob && handleReset(selectedJob)}>
                Reset
              </Button>
              <Button color="red" onClick={() => selectedJob && handleAbort(selectedJob)}>
                Abort
              </Button>
            </Group>
          </Stack>
        ) : (
          <Loader />
        )}
      </Modal>

      {/* Start Job Modal */}
      <Modal opened={startOpened} onClose={closeStart} title="Start Transfer Job" size="lg">
        <form onSubmit={form.onSubmit(handleSubmit)}>
          <Tabs value={form.values.jobType} onChange={(value) => form.setFieldValue('jobType', value as any)}>
            <Tabs.List>
              <Tabs.Tab value="FXP" leftSection={<IconArrowsRightLeft size={16} />}>
                FXP Transfer
              </Tabs.Tab>
              <Tabs.Tab value="DOWNLOAD" leftSection={<IconDownload size={16} />}>
                Download
              </Tabs.Tab>
              <Tabs.Tab value="UPLOAD" leftSection={<IconUpload size={16} />}>
                Upload
              </Tabs.Tab>
            </Tabs.List>

            <Tabs.Panel value="FXP" pt="md">
              <Stack gap="md">
                <TextInput label="Job Name" required {...form.getInputProps('name')} />

                <Select
                  label="Source Site"
                  data={siteNames || []}
                  searchable
                  {...form.getInputProps('src_site')}
                />

                <Select
                  label="Source Type"
                  data={[
                    { value: 'section', label: 'Section' },
                    { value: 'path', label: 'Path' },
                  ]}
                  {...form.getInputProps('src_pathType')}
                />

                {form.values.src_pathType === 'section' ? (
                  <Select
                    label="Source Section"
                    data={sectionNames || []}
                    searchable
                    {...form.getInputProps('src_section')}
                  />
                ) : (
                  <TextInput label="Source Path" {...form.getInputProps('src_path')} />
                )}

                <Select
                  label="Destination Site"
                  data={siteNames || []}
                  searchable
                  {...form.getInputProps('dst_site')}
                />

                <Select
                  label="Destination Type"
                  data={[
                    { value: 'section', label: 'Section' },
                    { value: 'path', label: 'Path' },
                  ]}
                  {...form.getInputProps('dst_pathType')}
                />

                {form.values.dst_pathType === 'section' ? (
                  <Select
                    label="Destination Section"
                    data={sectionNames || []}
                    searchable
                    {...form.getInputProps('dst_section')}
                  />
                ) : (
                  <TextInput label="Destination Path" {...form.getInputProps('dst_path')} />
                )}
              </Stack>
            </Tabs.Panel>

            <Tabs.Panel value="DOWNLOAD" pt="md">
              <Stack gap="md">
                <TextInput label="Job Name" required {...form.getInputProps('name')} />

                <Select
                  label="Source Site"
                  data={siteNames || []}
                  searchable
                  {...form.getInputProps('src_site')}
                />

                <Select
                  label="Source Type"
                  data={[
                    { value: 'section', label: 'Section' },
                    { value: 'path', label: 'Path' },
                  ]}
                  {...form.getInputProps('src_pathType')}
                />

                {form.values.src_pathType === 'section' ? (
                  <Select
                    label="Source Section"
                    data={sectionNames || []}
                    searchable
                    {...form.getInputProps('src_section')}
                  />
                ) : (
                  <TextInput label="Source Path" {...form.getInputProps('src_path')} />
                )}

                <TextInput
                  label="Local Destination Path (optional)"
                  {...form.getInputProps('dst_path')}
                />
              </Stack>
            </Tabs.Panel>

            <Tabs.Panel value="UPLOAD" pt="md">
              <Stack gap="md">
                <TextInput label="Job Name" required {...form.getInputProps('name')} />

                <TextInput
                  label="Local Source Path (optional)"
                  {...form.getInputProps('src_path')}
                />

                <Select
                  label="Destination Site"
                  data={siteNames || []}
                  searchable
                  {...form.getInputProps('dst_site')}
                />

                <Select
                  label="Destination Type"
                  data={[
                    { value: 'section', label: 'Section' },
                    { value: 'path', label: 'Path' },
                  ]}
                  {...form.getInputProps('dst_pathType')}
                />

                {form.values.dst_pathType === 'section' ? (
                  <Select
                    label="Destination Section"
                    data={sectionNames || []}
                    searchable
                    {...form.getInputProps('dst_section')}
                  />
                ) : (
                  <TextInput label="Destination Path" {...form.getInputProps('dst_path')} />
                )}
              </Stack>
            </Tabs.Panel>
          </Tabs>

          <Group justify="flex-end" mt="md">
            <Button variant="default" onClick={closeStart}>
              Cancel
            </Button>
            <Button type="submit" loading={startMutation.isPending}>
              Start Job
            </Button>
          </Group>
        </form>
      </Modal>
    </>
  );
}
