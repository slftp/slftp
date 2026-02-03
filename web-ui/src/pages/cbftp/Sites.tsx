import { useMemo, useState } from 'react';
import { ActionIcon, Alert, Badge, Button, Group, Loader, Modal, Stack, Table, TextInput, Text, Tooltip, Tabs, Select, Switch, NumberInput, ScrollArea, Paper } from '@mantine/core';
import { useDisclosure } from '@mantine/hooks';
import { useForm } from '@mantine/form';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { IconAlertCircle, IconEye, IconEdit, IconTrash, IconPlus, IconSearch, IconFolder } from '@tabler/icons-react';
import { notifications } from '@mantine/notifications';
import { getSites, getSite, createSite, updateSite, deleteSite, getSiteSections, createSiteSection, updateSiteSection, deleteSiteSection } from '../../api/cbftpClient';
import type { CbftpSite, SiteSection } from '../../api/cbftpClient';

export function Sites() {
  const queryClient = useQueryClient();
  const [search, setSearch] = useState('');
  const [detailsOpened, { open: openDetails, close: closeDetails }] = useDisclosure(false);
  const [formOpened, { open: openForm, close: closeForm }] = useDisclosure(false);
  const [deleteOpened, { open: openDelete, close: closeDelete }] = useDisclosure(false);
  const [sectionFormOpened, { open: openSectionForm, close: closeSectionForm }] = useDisclosure(false);
  const [deleteSectionOpened, { open: openDeleteSection, close: closeDeleteSection }] = useDisclosure(false);
  const [selectedSite, setSelectedSite] = useState<string | null>(null);
  const [editMode, setEditMode] = useState(false);
  const [selectedSection, setSelectedSection] = useState<SiteSection | null>(null);
  const [sectionEditMode, setSectionEditMode] = useState(false);

  const { data: siteNames, isLoading, error } = useQuery<string[]>({
    queryKey: ['cbftp-sites'],
    queryFn: () => getSites(),
    refetchInterval: 30000,
  });

  const { data: siteDetailsList } = useQuery<CbftpSite[]>({
    queryKey: ['cbftp-sites-details', siteNames],
    queryFn: async () => {
      if (!siteNames || siteNames.length === 0) {
        return [];
      }
      return Promise.all(siteNames.map((siteName) => getSite(siteName)));
    },
    enabled: !!siteNames?.length,
    refetchInterval: siteNames && siteNames.length > 0 ? 30000 : false,
  });

  const siteDetailsByName = useMemo(() => {
    const entries = siteDetailsList?.map((site) => [site.name, site] as const) ?? [];
    return new Map(entries);
  }, [siteDetailsList]);

  const { data: siteDetails } = useQuery<CbftpSite>({
    queryKey: ['cbftp-site', selectedSite],
    queryFn: () => getSite(selectedSite!),
    enabled: !!selectedSite,
  });

  const createMutation = useMutation({
    mutationFn: createSite,
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Site created successfully', color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['cbftp-sites'] });
      closeForm();
      form.reset();
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const updateMutation = useMutation({
    mutationFn: ({ name, updates }: { name: string; updates: Partial<CbftpSite> }) => updateSite(name, updates),
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Site updated successfully', color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['cbftp-sites'] });
      queryClient.invalidateQueries({ queryKey: ['cbftp-site', selectedSite] });
      closeForm();
      form.reset();
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const deleteMutation = useMutation({
    mutationFn: deleteSite,
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Site deleted successfully', color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['cbftp-sites'] });
      closeDelete();
      setSelectedSite(null);
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  // Site Sections query and mutations
  const { data: siteSections, refetch: refetchSections } = useQuery<SiteSection[]>({
    queryKey: ['cbftp-site-sections', selectedSite],
    queryFn: () => getSiteSections(selectedSite!),
    enabled: !!selectedSite && (formOpened || detailsOpened),
  });

  const createSectionMutation = useMutation({
    mutationFn: ({ siteName, section }: { siteName: string; section: SiteSection }) =>
      createSiteSection(siteName, section),
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Section added successfully', color: 'green' });
      refetchSections();
      queryClient.invalidateQueries({ queryKey: ['cbftp-site', selectedSite] });
      closeSectionForm();
      sectionForm.reset();
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const updateSectionMutation = useMutation({
    mutationFn: ({ siteName, sectionName, updates }: { siteName: string; sectionName: string; updates: Partial<SiteSection> }) =>
      updateSiteSection(siteName, sectionName, updates),
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Section updated successfully', color: 'green' });
      refetchSections();
      queryClient.invalidateQueries({ queryKey: ['cbftp-site', selectedSite] });
      closeSectionForm();
      sectionForm.reset();
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const deleteSectionMutation = useMutation({
    mutationFn: ({ siteName, sectionName }: { siteName: string; sectionName: string }) =>
      deleteSiteSection(siteName, sectionName),
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Section deleted successfully', color: 'green' });
      refetchSections();
      queryClient.invalidateQueries({ queryKey: ['cbftp-site', selectedSite] });
      closeDeleteSection();
      setSelectedSection(null);
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const sectionForm = useForm({
    initialValues: {
      name: '',
      path: '',
    },
    validate: {
      name: (value) => (value.trim() ? null : 'Section name is required'),
      path: (value) => (value.trim() ? null : 'Path is required'),
    },
  });

  const form = useForm({
    initialValues: {
      name: '',
      addresses: [] as string[],
      user: '',
      password: '',
      tls_mode: 'NONE' as 'NONE' | 'AUTH_TLS' | 'IMPLICIT',
      transfer_protocol: 'IPV4_ONLY' as 'IPV4_ONLY' | 'PREFER_IPV4' | 'PREFER_IPV6' | 'IPV6_ONLY',
      max_logins: 3,
      max_sim_down: 2,
      max_sim_up: 3,
      leave_free_slot: true,
      priority: 'NORMAL' as 'VERY_LOW' | 'LOW' | 'NORMAL' | 'HIGH' | 'VERY_HIGH',
      list_command: 'STAT_L' as 'STAT_L' | 'LIST',
      list_frequency: 'AUTO' as 'VERY_LOW' | 'FIXED_LOW' | 'FIXED_AVERAGE' | 'FIXED_HIGH' | 'FIXED_VERY_HIGH' | 'AUTO' | 'DYNAMIC_LOW' | 'DYNAMIC_AVERAGE' | 'DYNAMIC_HIGH' | 'DYNAMIC_VERY_HIGH',
      allow_download: 'YES' as 'YES' | 'NO' | 'MATCH_ONLY',
      allow_upload: 'YES' as 'YES' | 'NO',
      disabled: false,
      cepr: true,
      cpsv: true,
      pret: false,
      sscn: false,
      xdupe: true,
      broken_pasv: false,
    },
    validate: {
      name: (value) => (value.trim() ? null : 'Site name is required'),
      addresses: (value) => (value.length > 0 ? null : 'At least one address is required'),
    },
  });

  const handleViewDetails = (siteName: string) => {
    setSelectedSite(siteName);
    openDetails();
  };

  const handleEdit = (siteName: string) => {
    setSelectedSite(siteName);
    setEditMode(true);
    const site = siteDetailsByName.get(siteName);
    if (site) {
      form.setValues({
        name: site.name,
        addresses: site.addresses || [],
        user: site.user || '',
        password: site.password || '',
        tls_mode: site.tls_mode || 'NONE',
        transfer_protocol: site.transfer_protocol || 'IPV4_ONLY',
        max_logins: site.max_logins || 3,
        max_sim_down: site.max_sim_down || 2,
        max_sim_up: site.max_sim_up || 3,
        leave_free_slot: site.leave_free_slot !== undefined ? site.leave_free_slot : true,
        priority: site.priority || 'NORMAL',
        list_command: site.list_command || 'STAT_L',
        list_frequency: site.list_frequency || 'AUTO',
        allow_download: site.allow_download || 'YES',
        allow_upload: site.allow_upload || 'YES',
        disabled: site.disabled || false,
        cepr: site.cepr !== undefined ? site.cepr : true,
        cpsv: site.cpsv !== undefined ? site.cpsv : true,
        pret: site.pret || false,
        sscn: site.sscn || false,
        xdupe: site.xdupe !== undefined ? site.xdupe : true,
        broken_pasv: site.broken_pasv || false,
      });
    }
    openForm();
  };

  const handleAdd = () => {
    setEditMode(false);
    setSelectedSite(null);
    form.reset();
    openForm();
  };

  const handleDelete = (siteName: string) => {
    setSelectedSite(siteName);
    openDelete();
  };

  const handleSubmit = (values: typeof form.values) => {
    if (editMode && selectedSite) {
      updateMutation.mutate({ name: selectedSite, updates: values });
    } else {
      createMutation.mutate(values);
    }
  };

  const handleAddSection = () => {
    setSectionEditMode(false);
    setSelectedSection(null);
    sectionForm.reset();
    openSectionForm();
  };

  const handleEditSection = (section: SiteSection) => {
    setSectionEditMode(true);
    setSelectedSection(section);
    sectionForm.setValues({
      name: section.name,
      path: section.path,
    });
    openSectionForm();
  };

  const handleDeleteSection = (section: SiteSection) => {
    setSelectedSection(section);
    openDeleteSection();
  };

  const handleSectionSubmit = (values: typeof sectionForm.values) => {
    if (!selectedSite) return;
    if (sectionEditMode && selectedSection) {
      updateSectionMutation.mutate({
        siteName: selectedSite,
        sectionName: selectedSection.name,
        updates: { path: values.path },
      });
    } else {
      createSectionMutation.mutate({
        siteName: selectedSite,
        section: values,
      });
    }
  };

  const filteredSites = (siteNames || []).filter((name) =>
    typeof name === 'string' && name.toLowerCase().includes(search.toLowerCase())
  );

  if (isLoading) {
    return <Loader />;
  }

  if (error) {
    return (
      <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
        {error instanceof Error ? error.message : 'Failed to fetch sites'}
      </Alert>
    );
  }

  const formatTlsMode = (value?: string) => {
    switch (value) {
      case 'AUTH_TLS':
        return 'AUTH TLS';
      case 'IMPLICIT':
        return 'Implicit';
      case 'NONE':
        return 'None';
      default:
        return value ? value.replace(/_/g, ' ') : 'Unknown';
    }
  };

  const formatPriority = (value?: string) => {
    return value ? value.replace(/_/g, ' ') : 'Unknown';
  };

  return (
    <>
      <Group justify="apart" mb="md">
        <TextInput
          placeholder="Search sites..."
          leftSection={<IconSearch size={16} />}
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
          style={{ width: 300 }}
        />
        <Button leftSection={<IconPlus size={16} />} onClick={handleAdd}>
          Add Site
        </Button>
      </Group>

      <Text mb="md">Total Sites: {siteNames?.length || 0}</Text>

      <Table striped highlightOnHover>
        <Table.Thead>
          <Table.Tr>
            <Table.Th>Name</Table.Th>
            <Table.Th>Status</Table.Th>
            <Table.Th>TLS Mode</Table.Th>
            <Table.Th>Priority</Table.Th>
            <Table.Th>Actions</Table.Th>
          </Table.Tr>
        </Table.Thead>
        <Table.Tbody>
          {filteredSites.map((siteName) => {
            const details = siteDetailsByName.get(siteName);
            const isDisabled = details?.disabled;
            return (
            <Table.Tr key={siteName}>
              <Table.Td>{siteName}</Table.Td>
              <Table.Td>
                {details ? (
                  <Badge color={isDisabled ? 'red' : 'green'}>
                    {isDisabled ? 'Disabled' : 'Enabled'}
                  </Badge>
                ) : (
                  <Badge color="gray">Unknown</Badge>
                )}
              </Table.Td>
              <Table.Td>
                <Badge color="blue">{formatTlsMode(details?.tls_mode)}</Badge>
              </Table.Td>
              <Table.Td>
                <Badge>{formatPriority(details?.priority)}</Badge>
              </Table.Td>
              <Table.Td>
                <Group gap="xs">
                  <Tooltip label="View Details">
                    <ActionIcon variant="light" onClick={() => handleViewDetails(siteName)}>
                      <IconEye size={16} />
                    </ActionIcon>
                  </Tooltip>
                  <Tooltip label="Edit">
                    <ActionIcon variant="light" color="blue" onClick={() => handleEdit(siteName)}>
                      <IconEdit size={16} />
                    </ActionIcon>
                  </Tooltip>
                  <Tooltip label="Delete">
                    <ActionIcon variant="light" color="red" onClick={() => handleDelete(siteName)}>
                      <IconTrash size={16} />
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
      <Modal opened={detailsOpened} onClose={closeDetails} title={`Site Details: ${selectedSite}`} size="lg">
        {siteDetails ? (
          <ScrollArea h={500}>
            <Stack gap="sm">
              <Group justify="apart">
                <Text fw={500}>Name:</Text>
                <Text>{siteDetails.name}</Text>
              </Group>
              <Group justify="apart">
                <Text fw={500}>Addresses:</Text>
                <Text>{siteDetails.addresses?.join(', ') || 'N/A'}</Text>
              </Group>
              <Group justify="apart">
                <Text fw={500}>User:</Text>
                <Text>{siteDetails.user || 'N/A'}</Text>
              </Group>
              <Group justify="apart">
                <Text fw={500}>TLS Mode:</Text>
                <Badge>{siteDetails.tls_mode || 'NONE'}</Badge>
              </Group>
              <Group justify="apart">
                <Text fw={500}>Priority:</Text>
                <Badge>{siteDetails.priority || 'NORMAL'}</Badge>
              </Group>
              <Group justify="apart">
                <Text fw={500}>Max Logins:</Text>
                <Text>{siteDetails.max_logins || 'N/A'}</Text>
              </Group>
              <Group justify="apart">
                <Text fw={500}>Disabled:</Text>
                <Badge color={siteDetails.disabled ? 'red' : 'green'}>
                  {siteDetails.disabled ? 'Yes' : 'No'}
                </Badge>
              </Group>
              {siteDetails.sections && siteDetails.sections.length > 0 && (
                <>
                  <Text fw={500}>Sections:</Text>
                  {siteDetails.sections.map((section) => (
                    <Group key={section.name} gap="xs">
                      <Badge>{section.name}</Badge>
                      <Text size="sm" c="dimmed">{section.path}</Text>
                    </Group>
                  ))}
                </>
              )}
            </Stack>
          </ScrollArea>
        ) : (
          <Loader />
        )}
      </Modal>

      {/* Add/Edit Form Modal */}
      <Modal
        opened={formOpened}
        onClose={closeForm}
        title={editMode ? 'Edit Site' : 'Add Site'}
        size="xl"
      >
        <form onSubmit={form.onSubmit(handleSubmit)}>
          <Tabs defaultValue="basic">
            <Tabs.List>
              <Tabs.Tab value="basic">Basic</Tabs.Tab>
              <Tabs.Tab value="connection">Connection</Tabs.Tab>
              <Tabs.Tab value="slots">Slots</Tabs.Tab>
              <Tabs.Tab value="preferences">Preferences</Tabs.Tab>
              <Tabs.Tab value="policies">Policies</Tabs.Tab>
              <Tabs.Tab value="advanced">Advanced</Tabs.Tab>
              {editMode && <Tabs.Tab value="sections" leftSection={<IconFolder size={14} />}>Sections</Tabs.Tab>}
            </Tabs.List>

            <Tabs.Panel value="basic" pt="md">
              <Stack gap="md">
                <TextInput
                  label="Name"
                  required
                  disabled={editMode}
                  {...form.getInputProps('name')}
                />
                <TextInput
                  label="Addresses (comma-separated)"
                  placeholder="host:port, host2:port2"
                  value={form.values.addresses.join(', ')}
                  onChange={(e) => {
                    const addresses = e.currentTarget.value.split(',').map(a => a.trim()).filter(a => a);
                    form.setFieldValue('addresses', addresses);
                  }}
                />
                <TextInput label="Username" {...form.getInputProps('user')} />
                <TextInput label="Password" type="password" {...form.getInputProps('password')} />
              </Stack>
            </Tabs.Panel>

            <Tabs.Panel value="connection" pt="md">
              <Stack gap="md">
                <Select
                  label="TLS Mode"
                  data={[
                    { value: 'NONE', label: 'None' },
                    { value: 'AUTH_TLS', label: 'AUTH TLS' },
                    { value: 'IMPLICIT', label: 'Implicit' },
                  ]}
                  {...form.getInputProps('tls_mode')}
                />
                <Select
                  label="Transfer Protocol"
                  data={[
                    { value: 'IPV4_ONLY', label: 'IPv4 Only' },
                    { value: 'PREFER_IPV4', label: 'Prefer IPv4' },
                    { value: 'PREFER_IPV6', label: 'Prefer IPv6' },
                    { value: 'IPV6_ONLY', label: 'IPv6 Only' },
                  ]}
                  {...form.getInputProps('transfer_protocol')}
                />
                <NumberInput
                  label="Max Logins"
                  min={1}
                  {...form.getInputProps('max_logins')}
                />
              </Stack>
            </Tabs.Panel>

            <Tabs.Panel value="slots" pt="md">
              <Stack gap="md">
                <NumberInput
                  label="Max Simultaneous Downloads"
                  min={0}
                  {...form.getInputProps('max_sim_down')}
                />
                <NumberInput
                  label="Max Simultaneous Uploads"
                  min={0}
                  {...form.getInputProps('max_sim_up')}
                />
                <Switch
                  label="Leave Free Slot"
                  {...form.getInputProps('leave_free_slot', { type: 'checkbox' })}
                />
              </Stack>
            </Tabs.Panel>

            <Tabs.Panel value="preferences" pt="md">
              <Stack gap="md">
                <Select
                  label="Priority"
                  data={[
                    { value: 'VERY_LOW', label: 'Very Low' },
                    { value: 'LOW', label: 'Low' },
                    { value: 'NORMAL', label: 'Normal' },
                    { value: 'HIGH', label: 'High' },
                    { value: 'VERY_HIGH', label: 'Very High' },
                  ]}
                  {...form.getInputProps('priority')}
                />
                <Select
                  label="List Command"
                  data={[
                    { value: 'STAT_L', label: 'STAT -L' },
                    { value: 'LIST', label: 'LIST' },
                  ]}
                  {...form.getInputProps('list_command')}
                />
                <Select
                  label="List Frequency"
                  data={[
                    { value: 'VERY_LOW', label: 'Very Low' },
                    { value: 'FIXED_LOW', label: 'Fixed Low' },
                    { value: 'FIXED_AVERAGE', label: 'Fixed Average' },
                    { value: 'FIXED_HIGH', label: 'Fixed High' },
                    { value: 'FIXED_VERY_HIGH', label: 'Fixed Very High' },
                    { value: 'AUTO', label: 'Auto' },
                    { value: 'DYNAMIC_LOW', label: 'Dynamic Low' },
                    { value: 'DYNAMIC_AVERAGE', label: 'Dynamic Average' },
                    { value: 'DYNAMIC_HIGH', label: 'Dynamic High' },
                    { value: 'DYNAMIC_VERY_HIGH', label: 'Dynamic Very High' },
                  ]}
                  {...form.getInputProps('list_frequency')}
                />
              </Stack>
            </Tabs.Panel>

            <Tabs.Panel value="policies" pt="md">
              <Stack gap="md">
                <Select
                  label="Allow Download"
                  data={[
                    { value: 'YES', label: 'Yes' },
                    { value: 'NO', label: 'No' },
                    { value: 'MATCH_ONLY', label: 'Match Only' },
                  ]}
                  {...form.getInputProps('allow_download')}
                />
                <Select
                  label="Allow Upload"
                  data={[
                    { value: 'YES', label: 'Yes' },
                    { value: 'NO', label: 'No' },
                  ]}
                  {...form.getInputProps('allow_upload')}
                />
                <Switch
                  label="Disabled"
                  {...form.getInputProps('disabled', { type: 'checkbox' })}
                />
              </Stack>
            </Tabs.Panel>

            <Tabs.Panel value="advanced" pt="md">
              <Stack gap="md">
                <Switch label="CEPR" {...form.getInputProps('cepr', { type: 'checkbox' })} />
                <Switch label="CPSV" {...form.getInputProps('cpsv', { type: 'checkbox' })} />
                <Switch label="PRET" {...form.getInputProps('pret', { type: 'checkbox' })} />
                <Switch label="SSCN" {...form.getInputProps('sscn', { type: 'checkbox' })} />
                <Switch label="XDUPE" {...form.getInputProps('xdupe', { type: 'checkbox' })} />
                <Switch label="Broken PASV" {...form.getInputProps('broken_pasv', { type: 'checkbox' })} />
              </Stack>
            </Tabs.Panel>

            {editMode && (
              <Tabs.Panel value="sections" pt="md">
                <Stack gap="md">
                  <Group justify="space-between">
                    <Text fw={500}>Site Sections</Text>
                    <Button size="xs" leftSection={<IconPlus size={14} />} onClick={handleAddSection}>
                      Add Section
                    </Button>
                  </Group>

                  {siteSections && siteSections.length > 0 ? (
                    <Table striped highlightOnHover>
                      <Table.Thead>
                        <Table.Tr>
                          <Table.Th>Name</Table.Th>
                          <Table.Th>Path</Table.Th>
                          <Table.Th style={{ width: 100 }}>Actions</Table.Th>
                        </Table.Tr>
                      </Table.Thead>
                      <Table.Tbody>
                        {siteSections.map((section) => (
                          <Table.Tr key={section.name}>
                            <Table.Td>
                              <Badge variant="light">{section.name}</Badge>
                            </Table.Td>
                            <Table.Td>
                              <Text size="sm" c="dimmed">{section.path}</Text>
                            </Table.Td>
                            <Table.Td>
                              <Group gap="xs">
                                <Tooltip label="Edit">
                                  <ActionIcon size="sm" variant="light" color="blue" onClick={() => handleEditSection(section)}>
                                    <IconEdit size={14} />
                                  </ActionIcon>
                                </Tooltip>
                                <Tooltip label="Delete">
                                  <ActionIcon size="sm" variant="light" color="red" onClick={() => handleDeleteSection(section)}>
                                    <IconTrash size={14} />
                                  </ActionIcon>
                                </Tooltip>
                              </Group>
                            </Table.Td>
                          </Table.Tr>
                        ))}
                      </Table.Tbody>
                    </Table>
                  ) : (
                    <Paper p="md" withBorder>
                      <Text c="dimmed" ta="center">No sections configured for this site</Text>
                    </Paper>
                  )}
                </Stack>
              </Tabs.Panel>
            )}
          </Tabs>

          <Group justify="flex-end" mt="md">
            <Button variant="default" onClick={closeForm}>
              Cancel
            </Button>
            <Button type="submit" loading={createMutation.isPending || updateMutation.isPending}>
              {editMode ? 'Update' : 'Create'}
            </Button>
          </Group>
        </form>
      </Modal>

      {/* Delete Confirmation Modal */}
      <Modal opened={deleteOpened} onClose={closeDelete} title="Delete Site" size="sm">
        <Text>
          Are you sure you want to delete site <Text span c="red" fw={700}>{selectedSite}</Text>?
        </Text>
        <Group justify="flex-end" mt="md">
          <Button variant="default" onClick={closeDelete}>
            Cancel
          </Button>
          <Button
            color="red"
            loading={deleteMutation.isPending}
            onClick={() => selectedSite && deleteMutation.mutate(selectedSite)}
          >
            Delete
          </Button>
        </Group>
      </Modal>

      {/* Section Add/Edit Modal */}
      <Modal
        opened={sectionFormOpened}
        onClose={closeSectionForm}
        title={sectionEditMode ? 'Edit Section' : 'Add Section'}
        size="md"
      >
        <form onSubmit={sectionForm.onSubmit(handleSectionSubmit)}>
          <Stack gap="md">
            <TextInput
              label="Section Name"
              placeholder="e.g., MP3, FLAC, TV-HD"
              required
              disabled={sectionEditMode}
              {...sectionForm.getInputProps('name')}
            />
            <TextInput
              label="Path"
              placeholder="e.g., /incoming/mp3"
              required
              {...sectionForm.getInputProps('path')}
            />
          </Stack>
          <Group justify="flex-end" mt="md">
            <Button variant="default" onClick={closeSectionForm}>
              Cancel
            </Button>
            <Button
              type="submit"
              loading={createSectionMutation.isPending || updateSectionMutation.isPending}
            >
              {sectionEditMode ? 'Update' : 'Add'}
            </Button>
          </Group>
        </form>
      </Modal>

      {/* Section Delete Confirmation Modal */}
      <Modal opened={deleteSectionOpened} onClose={closeDeleteSection} title="Delete Section" size="sm">
        <Text>
          Are you sure you want to delete section <Text span c="red" fw={700}>{selectedSection?.name}</Text> from this site?
        </Text>
        <Group justify="flex-end" mt="md">
          <Button variant="default" onClick={closeDeleteSection}>
            Cancel
          </Button>
          <Button
            color="red"
            loading={deleteSectionMutation.isPending}
            onClick={() => selectedSite && selectedSection && deleteSectionMutation.mutate({
              siteName: selectedSite,
              sectionName: selectedSection.name,
            })}
          >
            Delete
          </Button>
        </Group>
      </Modal>
    </>
  );
}
