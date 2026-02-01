import { useState } from 'react';
import { ActionIcon, Alert, Badge, Button, Group, Loader, Modal, Stack, Table, TextInput, Text, Tooltip, Select, Switch, Box, Paper } from '@mantine/core';
import { useDisclosure } from '@mantine/hooks';
import { useForm } from '@mantine/form';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { IconAlertCircle, IconTrash, IconPlus, IconSearch, IconEdit, IconX } from '@tabler/icons-react';
import { notifications } from '@mantine/notifications';
import { getSections, getSection, createSection, updateSection, deleteSection } from '../../api/cbftpClient';
import type { CbftpSection, Skiplist } from '../../api/cbftpClient';

export function Sections() {
  const queryClient = useQueryClient();
  const [search, setSearch] = useState('');
  const [formOpened, { open: openForm, close: closeForm }] = useDisclosure(false);
  const [deleteOpened, { open: openDelete, close: closeDelete }] = useDisclosure(false);
  const [selectedSection, setSelectedSection] = useState<string | null>(null);
  const [editMode, setEditMode] = useState(false);

  const { data: sectionNames, isLoading, error } = useQuery<Array<string | CbftpSection>>({
    queryKey: ['cbftp-sections'],
    queryFn: getSections,
    refetchInterval: 30000,
  });


  const createMutation = useMutation({
    mutationFn: createSection,
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Section created successfully', color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['cbftp-sections'] });
      closeForm();
      form.reset();
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const updateMutation = useMutation({
    mutationFn: ({ name, updates }: { name: string; updates: Partial<CbftpSection> }) =>
      updateSection(name, updates),
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Section updated successfully', color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['cbftp-sections'] });
      closeForm();
      form.reset();
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const deleteMutation = useMutation({
    mutationFn: deleteSection,
    onSuccess: () => {
      notifications.show({ title: 'Success', message: 'Section deleted successfully', color: 'green' });
      queryClient.invalidateQueries({ queryKey: ['cbftp-sections'] });
      closeDelete();
      setSelectedSection(null);
    },
    onError: (error: Error) => {
      notifications.show({ title: 'Error', message: error.message, color: 'red' });
    },
  });

  const form = useForm({
    initialValues: {
      name: '',
      hotkey: undefined as number | undefined,
      skiplist: [] as Skiplist[],
    },
    validate: {
      name: (value) => (value.trim() ? null : 'Section name is required'),
    },
  });

  const handleAdd = () => {
    setEditMode(false);
    setSelectedSection(null);
    form.reset();
    openForm();
  };

  const handleEdit = async (sectionName: string) => {
    setSelectedSection(sectionName);
    setEditMode(true);

    // Wait for section details to load
    const section = await getSection(sectionName);
    form.setValues({
      name: section.name,
      hotkey: section.hotkey,
      skiplist: section.skiplist || [],
    });

    openForm();
  };

  const handleDelete = (sectionName: string) => {
    setSelectedSection(sectionName);
    openDelete();
  };

  const handleSubmit = (values: typeof form.values) => {
    if (editMode && selectedSection) {
      updateMutation.mutate({ name: selectedSection, updates: values });
    } else {
      createMutation.mutate(values);
    }
  };

  const addSkiplistRule = () => {
    form.setFieldValue('skiplist', [
      ...form.values.skiplist,
      {
        action: 'DENY' as const,
        dir: false,
        file: true,
        pattern: '',
        regex: false,
        scope: 'ALL' as const,
      },
    ]);
  };

  const removeSkiplistRule = (index: number) => {
    form.setFieldValue(
      'skiplist',
      form.values.skiplist.filter((_, i) => i !== index)
    );
  };

  const normalizedSections = (sectionNames || [])
    .map((entry) => (typeof entry === 'string' ? entry : entry?.name))
    .filter((name): name is string => typeof name === 'string' && name.length > 0);

  const filteredSections = normalizedSections.filter((name) =>
    name.toLowerCase().includes(search.toLowerCase())
  );

  if (isLoading) {
    return <Loader />;
  }

  if (error) {
    return (
      <Alert icon={<IconAlertCircle size="1rem" />} title="Error" color="red">
        {error instanceof Error ? error.message : 'Failed to fetch sections'}
      </Alert>
    );
  }

  return (
    <>
      <Group justify="apart" mb="md">
        <TextInput
          placeholder="Search sections..."
          leftSection={<IconSearch size={16} />}
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
          style={{ width: 300 }}
        />
        <Button leftSection={<IconPlus size={16} />} onClick={handleAdd}>
          Add Section
        </Button>
      </Group>

      <Text mb="md">Total Sections: {normalizedSections.length}</Text>

      <Table striped highlightOnHover>
        <Table.Thead>
          <Table.Tr>
            <Table.Th>Name</Table.Th>
            <Table.Th>Hotkey</Table.Th>
            <Table.Th>Actions</Table.Th>
          </Table.Tr>
        </Table.Thead>
        <Table.Tbody>
          {filteredSections.map((sectionName) => (
            <Table.Tr key={sectionName}>
              <Table.Td>{sectionName}</Table.Td>
              <Table.Td>
                <Badge color="gray">N/A</Badge>
              </Table.Td>
              <Table.Td>
                <Group gap="xs">
                  <Tooltip label="Edit">
                    <ActionIcon variant="light" color="blue" onClick={() => handleEdit(sectionName)}>
                      <IconEdit size={16} />
                    </ActionIcon>
                  </Tooltip>
                  <Tooltip label="Delete">
                    <ActionIcon variant="light" color="red" onClick={() => handleDelete(sectionName)}>
                      <IconTrash size={16} />
                    </ActionIcon>
                  </Tooltip>
                </Group>
              </Table.Td>
            </Table.Tr>
          ))}
        </Table.Tbody>
      </Table>

      {/* Add/Edit Form Modal */}
      <Modal
        opened={formOpened}
        onClose={closeForm}
        title={editMode ? 'Edit Section' : 'Add Section'}
        size="xl"
      >
        <form onSubmit={form.onSubmit(handleSubmit)}>
          <Stack gap="md">
            <TextInput
              label="Name"
              required
              disabled={editMode}
              {...form.getInputProps('name')}
            />

            <Select
              label="Hotkey (0-9)"
              placeholder="None"
              clearable
              data={[
                { value: '0', label: '0' },
                { value: '1', label: '1' },
                { value: '2', label: '2' },
                { value: '3', label: '3' },
                { value: '4', label: '4' },
                { value: '5', label: '5' },
                { value: '6', label: '6' },
                { value: '7', label: '7' },
                { value: '8', label: '8' },
                { value: '9', label: '9' },
              ]}
              value={form.values.hotkey?.toString()}
              onChange={(value) => form.setFieldValue('hotkey', value ? parseInt(value) : undefined)}
            />

            <Box>
              <Group justify="apart" mb="sm">
                <Text fw={500}>Skiplist Rules</Text>
                <Button size="xs" onClick={addSkiplistRule}>
                  Add Rule
                </Button>
              </Group>

              <Stack gap="sm">
                {form.values.skiplist.map((rule, index) => (
                  <Paper key={index} p="sm" withBorder>
                    <Stack gap="xs">
                      <Group justify="apart">
                        <Text size="sm" fw={500}>Rule {index + 1}</Text>
                        <ActionIcon
                          color="red"
                          variant="subtle"
                          onClick={() => removeSkiplistRule(index)}
                        >
                          <IconX size={16} />
                        </ActionIcon>
                      </Group>

                      <Group grow>
                        <Select
                          label="Action"
                          data={[
                            { value: 'ALLOW', label: 'Allow' },
                            { value: 'DENY', label: 'Deny' },
                            { value: 'UNIQUE', label: 'Unique' },
                            { value: 'SIMILAR', label: 'Similar' },
                          ]}
                          value={rule.action}
                          onChange={(value) =>
                            form.setFieldValue(`skiplist.${index}.action`, value as any)
                          }
                        />
                        <Select
                          label="Scope"
                          data={[
                            { value: 'IN_RACE', label: 'In Race' },
                            { value: 'ALL', label: 'All' },
                          ]}
                          value={rule.scope}
                          onChange={(value) =>
                            form.setFieldValue(`skiplist.${index}.scope`, value as any)
                          }
                        />
                      </Group>

                      <TextInput
                        label="Pattern"
                        placeholder="*.nfo or regex pattern"
                        value={rule.pattern}
                        onChange={(e) =>
                          form.setFieldValue(`skiplist.${index}.pattern`, e.currentTarget.value)
                        }
                      />

                      <Group>
                        <Switch
                          label="Directory"
                          checked={rule.dir}
                          onChange={(e) =>
                            form.setFieldValue(`skiplist.${index}.dir`, e.currentTarget.checked)
                          }
                        />
                        <Switch
                          label="File"
                          checked={rule.file}
                          onChange={(e) =>
                            form.setFieldValue(`skiplist.${index}.file`, e.currentTarget.checked)
                          }
                        />
                        <Switch
                          label="Regex"
                          checked={rule.regex}
                          onChange={(e) =>
                            form.setFieldValue(`skiplist.${index}.regex`, e.currentTarget.checked)
                          }
                        />
                      </Group>
                    </Stack>
                  </Paper>
                ))}

                {form.values.skiplist.length === 0 && (
                  <Text size="sm" c="dimmed">
                    No skiplist rules defined. Click "Add Rule" to create one.
                  </Text>
                )}
              </Stack>
            </Box>

            <Group justify="flex-end" mt="md">
              <Button variant="default" onClick={closeForm}>
                Cancel
              </Button>
              <Button type="submit" loading={createMutation.isPending || updateMutation.isPending}>
                {editMode ? 'Update' : 'Create'}
              </Button>
            </Group>
          </Stack>
        </form>
      </Modal>

      {/* Delete Confirmation Modal */}
      <Modal opened={deleteOpened} onClose={closeDelete} title="Delete Section" size="sm">
        <Text>
          Are you sure you want to delete section <Text span c="red" fw={700}>{selectedSection}</Text>?
        </Text>
        <Group justify="flex-end" mt="md">
          <Button variant="default" onClick={closeDelete}>
            Cancel
          </Button>
          <Button
            color="red"
            loading={deleteMutation.isPending}
            onClick={() => selectedSection && deleteMutation.mutate(selectedSection)}
          >
            Delete
          </Button>
        </Group>
      </Modal>
    </>
  );
}
