import { useMemo, useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Card,
  Table,
  TextInput,
  NumberInput,
  Group,
  Stack,
  Button,
  Modal,
  ScrollArea,
  Text,
  Select,
  ActionIcon,
  Badge,
  Tooltip,
  Pagination,
} from '@mantine/core';
import { notifications } from '@mantine/notifications';
import { IconSearch, IconPlus, IconEdit, IconTrash, IconDeviceTv, IconChevronUp, IconChevronDown } from '@tabler/icons-react';
import { apiClient } from '../api/client';

function parseMaybeJsonArray(value: unknown): any[] {
  if (Array.isArray(value)) return value;
  if (typeof value === 'string') {
    try {
      const parsed = JSON.parse(value);
      return Array.isArray(parsed) ? parsed : [];
    } catch {
      return [];
    }
  }
  return [];
}

function buildTvMazeSlug(showname: string): string {
  const base = showname.trim().toLowerCase();
  if (!base) return '';
  const slug = base.replace(/[^a-z0-9]+/g, '-').replace(/^-+|-+$/g, '');
  return slug;
}

function TruncatedCell({ children, width }: { children: React.ReactNode; width: number }) {
  const text = String(children || '');
  const widthPx = `${width}px`;
  return (
    <Table.Td style={{ maxWidth: widthPx, width: widthPx, minWidth: widthPx }}>
      <Tooltip label={text} disabled={!text} withArrow>
        <div style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
          {children}
        </div>
      </Tooltip>
    </Table.Td>
  );
}

type SortField = 'TVMazeId' | 'Showname' | 'Country' | 'Status' | 'Network' | 'Genre' | 'PremieredYear' | 'Rating' | 'LastUpdated' | 'CreatedAt';
type SortDirection = 'asc' | 'desc';

function SortableHeader({
  children,
  field,
  currentField,
  direction,
  onClick,
  width
}: {
  children: React.ReactNode;
  field: SortField;
  currentField: SortField;
  direction: SortDirection;
  onClick: (field: SortField) => void;
  width: string;
}) {
  const isActive = currentField === field;
  return (
    <Table.Th
      style={{ width, cursor: 'pointer', userSelect: 'none' }}
      onClick={() => onClick(field)}
    >
      <Group gap={4} wrap="nowrap">
        <span>{children}</span>
        {isActive && (
          direction === 'asc' ?
            <IconChevronUp size={14} /> :
            <IconChevronDown size={14} />
        )}
      </Group>
    </Table.Th>
  );
}

// Helper function for filtering
function matchesFilter(value: string, filter: string): boolean {
  if (!filter) return true;

  const filterTrimmed = filter.trim();
  const valueLower = value.toLowerCase();
  const filterLower = filterTrimmed.toLowerCase();

  // Wildcard mode: contains *
  if (filterTrimmed.includes('*')) {
    const pattern = filterTrimmed
      .replace(/[.*+?^${}()|[\]\\]/g, '\\$&') // Escape special chars
      .replace(/\\\*/g, '.*'); // Convert * to .*
    try {
      const regex = new RegExp(`^${pattern}$`, 'i');
      return regex.test(value);
    } catch {
      return false;
    }
  }

  // Default: exact match (case insensitive)
  return valueLower === filterLower;
}

interface TVRecord {
  TVMazeId: number;
  Showname: string;
  Country: string;
  Status: string;
  Classification: string;
  Network: string;
  Genre: string;
  Language: string;
  PremieredYear: number;
  Rating: number;
  LastUpdated: number;
  CreatedAt: number;
}

interface FormData {
  TVMazeId: string;
  Showname: string;
  Country: string;
  Status: string;
  Classification: string;
  Network: string;
  Genre: string;
  Language: string;
  PremieredYear: number;
  Rating: number;
}

const emptyFormData: FormData = {
  TVMazeId: '',
  Showname: '',
  Country: '',
  Status: 'Running',
  Classification: '',
  Network: '',
  Genre: '',
  Language: 'English',
  PremieredYear: new Date().getFullYear(),
  Rating: 0,
};

export function TV() {
  const queryClient = useQueryClient();

  // Filter states
  const [shownameFilter, setShownameFilter] = useState('');
  const [countryFilter, setCountryFilter] = useState('');
  const [statusFilter, setStatusFilter] = useState<string>('');
  const [networkFilter, setNetworkFilter] = useState('');
  const [genreFilter, setGenreFilter] = useState('');
  const [yearMin, setYearMin] = useState<number | string>('');
  const [yearMax, setYearMax] = useState<number | string>('');
  const [ratingMin, setRatingMin] = useState<number | string>('');

  // Sort state
  const [sortField, setSortField] = useState<SortField>('LastUpdated');
  const [sortDirection, setSortDirection] = useState<SortDirection>('desc');

  // Pagination state
  const [page, setPage] = useState(1);
  const itemsPerPage = 100;

  // Modal states
  const [modalOpened, setModalOpened] = useState(false);
  const [editMode, setEditMode] = useState(false);
  const [formData, setFormData] = useState<FormData>(emptyFormData);

  // Fetch all TV records
  const { data, isLoading, error, refetch } = useQuery({
    queryKey: ['tv-records'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiTVService/GetAllTVRecords', {});

      if (res.data?.result && Array.isArray(res.data.result) && res.data.result[0]?.Records) {
        const records = parseMaybeJsonArray(res.data.result[0].Records);
        return records as TVRecord[];
      }
      return [];
    },
    refetchInterval: 60000, // Refresh every minute
    refetchOnWindowFocus: false,
  });

  // Handler for column sorting
  const handleSort = (field: SortField) => {
    console.log('Sorting by:', field, 'current:', sortField, sortDirection);
    if (sortField === field) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      setSortField(field);
      setSortDirection('asc');
    }
  };

  // Client-side filtering and sorting
  const filtered = useMemo(() => {
    let result = data || [];

    // Apply filters
    if (shownameFilter) {
      result = result.filter(r =>
        r.Showname.toLowerCase().includes(shownameFilter.toLowerCase()) ||
        String(r.TVMazeId).includes(shownameFilter)
      );
    }

    if (countryFilter) {
      result = result.filter(r => matchesFilter(r.Country, countryFilter));
    }

    if (statusFilter) {
      result = result.filter(r => r.Status.toLowerCase() === statusFilter.toLowerCase());
    }

    if (networkFilter) {
      result = result.filter(r =>
        r.Network.toLowerCase().includes(networkFilter.toLowerCase())
      );
    }

    if (genreFilter) {
      result = result.filter(r => matchesFilter(r.Genre, genreFilter));
    }

    if (yearMin !== '' && yearMin !== null) {
      result = result.filter(r => r.PremieredYear >= Number(yearMin));
    }

    if (yearMax !== '' && yearMax !== null) {
      result = result.filter(r => r.PremieredYear <= Number(yearMax));
    }

    if (ratingMin !== '' && ratingMin !== null) {
      result = result.filter(r => r.Rating >= Number(ratingMin));
    }

    // Apply sorting (create new array to ensure React detects change)
    const sorted = [...result].sort((a, b) => {
      let aVal = a[sortField];
      let bVal = b[sortField];

      // Handle string comparison case-insensitively
      if (typeof aVal === 'string' && typeof bVal === 'string') {
        aVal = aVal.toLowerCase();
        bVal = bVal.toLowerCase();
      }

      if (aVal < bVal) return sortDirection === 'asc' ? -1 : 1;
      if (aVal > bVal) return sortDirection === 'asc' ? 1 : -1;
      return 0;
    });

    return sorted;
  }, [data, shownameFilter, countryFilter, statusFilter, networkFilter, genreFilter, yearMin, yearMax, ratingMin, sortField, sortDirection]);

  // Pagination
  const totalPages = Math.ceil(filtered.length / itemsPerPage);
  const paginatedData = useMemo(() => {
    const start = (page - 1) * itemsPerPage;
    const end = start + itemsPerPage;
    return filtered.slice(start, end);
  }, [filtered, page, itemsPerPage]);

  // Reset to page 1 when filters change
  const resetPage = () => setPage(1);

  // Create mutation
  const createMutation = useMutation({
    mutationFn: async (data: FormData) => {
      await apiClient.post('/ApiTVService/CreateTVRecord', {
        TVMazeId: data.TVMazeId,
        Showname: data.Showname,
        Country: data.Country,
        Status: data.Status,
        Classification: data.Classification,
        Network: data.Network,
        Genre: data.Genre,
        Language: data.Language,
        PremieredYear: data.PremieredYear,
        Rating: data.Rating,
      });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Success',
        message: 'TV record created successfully',
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['tv-records'] });
      setModalOpened(false);
      setFormData(emptyFormData);
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.response?.data?.message || err.message || 'Failed to create record',
        color: 'red',
      });
    },
  });

  // Update mutation
  const updateMutation = useMutation({
    mutationFn: async (data: FormData) => {
      await apiClient.post('/ApiTVService/UpdateTVRecord', {
        TVMazeId: data.TVMazeId,
        Showname: data.Showname,
        Country: data.Country,
        Status: data.Status,
        Classification: data.Classification,
        Network: data.Network,
        Genre: data.Genre,
        Language: data.Language,
        PremieredYear: data.PremieredYear,
        Rating: data.Rating,
      });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Success',
        message: 'TV record updated successfully',
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['tv-records'] });
      setModalOpened(false);
      setFormData(emptyFormData);
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.response?.data?.message || err.message || 'Failed to update record',
        color: 'red',
      });
    },
  });

  // Delete mutation
  const deleteMutation = useMutation({
    mutationFn: async (tvMazeId: string) => {
      await apiClient.post('/ApiTVService/DeleteTVRecord', { TVMazeId: tvMazeId });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Success',
        message: 'TV record deleted successfully',
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['tv-records'] });
    },
    onError: (err: any) => {
      notifications.show({
        title: 'Error',
        message: err.response?.data?.message || err.message || 'Failed to delete record',
        color: 'red',
      });
    },
  });

  const handleAddNew = () => {
    setEditMode(false);
    setFormData(emptyFormData);
    setModalOpened(true);
  };

  const handleEdit = (record: TVRecord) => {
    setEditMode(true);
    setFormData({
      TVMazeId: String(record.TVMazeId),
      Showname: record.Showname,
      Country: record.Country,
      Status: record.Status,
      Classification: record.Classification,
      Network: record.Network,
      Genre: record.Genre,
      Language: record.Language,
      PremieredYear: record.PremieredYear,
      Rating: record.Rating,
    });
    setModalOpened(true);
  };

  const handleDelete = (tvMazeId: number) => {
    if (confirm(`Are you sure you want to delete TV show ${tvMazeId}?`)) {
      deleteMutation.mutate(String(tvMazeId));
    }
  };

  const handleSubmit = () => {
    if (editMode) {
      updateMutation.mutate(formData);
    } else {
      createMutation.mutate(formData);
    }
  };

  if (error) {
    return (
      <Stack>
        <Text c="red">Error loading TV data: {(error as Error).message}</Text>
        <Button onClick={() => refetch()}>Retry</Button>
      </Stack>
    );
  }

  return (
    <Stack gap="md">
      <Group justify="space-between">
        <Group>
          <IconDeviceTv size={32} />
          <Text size="xl" fw={700}>TV Database</Text>
          <Badge>{filtered.length} / {data?.length || 0} records</Badge>
        </Group>
        <Button leftSection={<IconPlus size={16} />} onClick={handleAddNew}>
          Add New
        </Button>
      </Group>

      {/* Filters */}
      <Card withBorder padding="md">
        <Stack gap="sm">
          <Text size="sm" fw={500}>Filters</Text>
          <Group grow>
            <TextInput
              placeholder="Search by show name or TVMaze ID..."
              leftSection={<IconSearch size={16} />}
              value={shownameFilter}
              onChange={(e) => {
                setShownameFilter(e.currentTarget.value);
                resetPage();
              }}
            />
            <Select
              placeholder="Status"
              clearable
              value={statusFilter}
              onChange={(val) => {
                setStatusFilter(val || '');
                resetPage();
              }}
              data={[
                { value: 'Running', label: 'Running' },
                { value: 'Ended', label: 'Ended' },
                { value: 'To Be Determined', label: 'To Be Determined' },
                { value: 'In Development', label: 'In Development' },
              ]}
            />
          </Group>
          <Group grow>
            <TextInput
              placeholder="Country (USA or *USA*)"
              value={countryFilter}
              onChange={(e) => {
                setCountryFilter(e.currentTarget.value);
                resetPage();
              }}
            />
            <TextInput
              placeholder="Network"
              value={networkFilter}
              onChange={(e) => {
                setNetworkFilter(e.currentTarget.value);
                resetPage();
              }}
            />
            <TextInput
              placeholder="Genre (Drama or *Drama*)"
              value={genreFilter}
              onChange={(e) => {
                setGenreFilter(e.currentTarget.value);
                resetPage();
              }}
            />
          </Group>
          <Group grow>
            <NumberInput
              placeholder="Year Min"
              value={yearMin}
              onChange={(val) => {
                setYearMin(val);
                resetPage();
              }}
              min={1900}
              max={2100}
            />
            <NumberInput
              placeholder="Year Max"
              value={yearMax}
              onChange={(val) => {
                setYearMax(val);
                resetPage();
              }}
              min={1900}
              max={2100}
            />
            <NumberInput
              placeholder="Rating Min (0-100)"
              value={ratingMin}
              onChange={(val) => {
                setRatingMin(val);
                resetPage();
              }}
              min={0}
              max={100}
            />
          </Group>
        </Stack>
      </Card>

      {/* Table */}
      <Card withBorder radius="md" p={0}>
        <ScrollArea h="calc(100vh - 400px)">
          <Table striped highlightOnHover withTableBorder>
            <Table.Thead>
              <Table.Tr>
                <SortableHeader field="TVMazeId" currentField={sortField} direction={sortDirection} onClick={handleSort} width="100px">
                  TVMaze ID
                </SortableHeader>
                <SortableHeader field="Showname" currentField={sortField} direction={sortDirection} onClick={handleSort} width="250px">
                  Show Name
                </SortableHeader>
                <SortableHeader field="Country" currentField={sortField} direction={sortDirection} onClick={handleSort} width="100px">
                  Country
                </SortableHeader>
                <SortableHeader field="Status" currentField={sortField} direction={sortDirection} onClick={handleSort} width="100px">
                  Status
                </SortableHeader>
                <SortableHeader field="Network" currentField={sortField} direction={sortDirection} onClick={handleSort} width="150px">
                  Network
                </SortableHeader>
                <SortableHeader field="Genre" currentField={sortField} direction={sortDirection} onClick={handleSort} width="120px">
                  Genre
                </SortableHeader>
                <SortableHeader field="PremieredYear" currentField={sortField} direction={sortDirection} onClick={handleSort} width="70px">
                  Year
                </SortableHeader>
                <SortableHeader field="Rating" currentField={sortField} direction={sortDirection} onClick={handleSort} width="70px">
                  Rating
                </SortableHeader>
                <SortableHeader field="CreatedAt" currentField={sortField} direction={sortDirection} onClick={handleSort} width="100px">
                  Created
                </SortableHeader>
                <SortableHeader field="LastUpdated" currentField={sortField} direction={sortDirection} onClick={handleSort} width="100px">
                  Updated
                </SortableHeader>
                <Table.Th style={{ width: '80px' }}>Actions</Table.Th>
              </Table.Tr>
            </Table.Thead>
            <Table.Tbody>
              {isLoading ? (
                <Table.Tr>
                  <Table.Td colSpan={11}>
                    <Text ta="center">Loading...</Text>
                  </Table.Td>
                </Table.Tr>
              ) : filtered.length === 0 ? (
                <Table.Tr>
                  <Table.Td colSpan={11}>
                    <Text ta="center">No records found</Text>
                  </Table.Td>
                </Table.Tr>
              ) : (
                paginatedData.map((record) => (
                  <Table.Tr key={record.TVMazeId}>
                    <Table.Td style={{ width: '100px' }}>
                      {buildTvMazeSlug(record.Showname) ? (
                        <a
                          href={`https://www.tvmaze.com/shows/${record.TVMazeId}/${buildTvMazeSlug(record.Showname)}`}
                          target="_blank"
                          rel="noopener noreferrer"
                        >
                          {record.TVMazeId}
                        </a>
                      ) : (
                        record.TVMazeId
                      )}
                    </Table.Td>
                    <TruncatedCell width={250}>{record.Showname}</TruncatedCell>
                    <Table.Td style={{ width: '100px' }}>{record.Country}</Table.Td>
                    <Table.Td style={{ width: '100px' }}>{record.Status}</Table.Td>
                    <TruncatedCell width={150}>{record.Network}</TruncatedCell>
                    <TruncatedCell width={120}>{record.Genre}</TruncatedCell>
                    <Table.Td style={{ width: '70px' }}>{record.PremieredYear}</Table.Td>
                    <Table.Td style={{ width: '70px' }}>{record.Rating}</Table.Td>
                    <Table.Td style={{ width: '100px' }}>
                      {record.CreatedAt && record.CreatedAt > 0
                        ? new Date(record.CreatedAt * 1000).toLocaleDateString('de-DE')
                        : 'N/A'}
                    </Table.Td>
                    <Table.Td style={{ width: '100px' }}>
                      {record.LastUpdated && record.LastUpdated > 0
                        ? new Date(record.LastUpdated * 1000).toLocaleDateString('de-DE')
                        : 'Never'}
                    </Table.Td>
                    <Table.Td style={{ width: '80px' }}>
                      <Group gap="xs" wrap="nowrap">
                        <ActionIcon
                          variant="subtle"
                          color="blue"
                          onClick={() => handleEdit(record)}
                        >
                          <IconEdit size={16} />
                        </ActionIcon>
                        <ActionIcon
                          variant="subtle"
                          color="red"
                          onClick={() => handleDelete(record.TVMazeId)}
                        >
                          <IconTrash size={16} />
                        </ActionIcon>
                      </Group>
                    </Table.Td>
                  </Table.Tr>
                ))
              )}
            </Table.Tbody>
          </Table>
        </ScrollArea>
      </Card>

      {/* Pagination */}
      {totalPages > 1 && (
        <Group justify="center" mt="md">
          <Pagination
            value={page}
            onChange={setPage}
            total={totalPages}
            boundaries={1}
            siblings={1}
          />
        </Group>
      )}

      {/* Add/Edit Modal */}
      <Modal
        opened={modalOpened}
        onClose={() => {
          setModalOpened(false);
          setFormData(emptyFormData);
        }}
        title={editMode ? 'Edit TV Show' : 'Add New TV Show'}
        size="lg"
      >
        <Stack gap="md">
          <NumberInput
            label="TVMaze ID"
            placeholder="12345"
            required
            value={formData.TVMazeId}
            onChange={(val) => setFormData({ ...formData, TVMazeId: String(val) || '' })}
            disabled={editMode}
          />
          <TextInput
            label="Show Name"
            placeholder="Breaking Bad"
            required
            value={formData.Showname}
            onChange={(e) => setFormData({ ...formData, Showname: e.currentTarget.value })}
          />
          <Group grow>
            <TextInput
              label="Country"
              placeholder="USA"
              value={formData.Country}
              onChange={(e) => setFormData({ ...formData, Country: e.currentTarget.value })}
            />
            <Select
              label="Status"
              placeholder="Select status"
              value={formData.Status}
              onChange={(val) => setFormData({ ...formData, Status: val || 'Running' })}
              data={[
                { value: 'Running', label: 'Running' },
                { value: 'Ended', label: 'Ended' },
                { value: 'To Be Determined', label: 'To Be Determined' },
                { value: 'In Development', label: 'In Development' },
              ]}
            />
          </Group>
          <Group grow>
            <TextInput
              label="Classification"
              placeholder="Scripted"
              value={formData.Classification}
              onChange={(e) => setFormData({ ...formData, Classification: e.currentTarget.value })}
            />
            <TextInput
              label="Network"
              placeholder="AMC"
              value={formData.Network}
              onChange={(e) => setFormData({ ...formData, Network: e.currentTarget.value })}
            />
          </Group>
          <Group grow>
            <TextInput
              label="Genre"
              placeholder="Drama"
              value={formData.Genre}
              onChange={(e) => setFormData({ ...formData, Genre: e.currentTarget.value })}
            />
            <TextInput
              label="Language"
              placeholder="English"
              value={formData.Language}
              onChange={(e) => setFormData({ ...formData, Language: e.currentTarget.value })}
            />
          </Group>
          <Group grow>
            <NumberInput
              label="Premiered Year"
              placeholder="2008"
              required
              value={formData.PremieredYear}
              onChange={(val) => setFormData({ ...formData, PremieredYear: Number(val) || 0 })}
              min={1900}
              max={2100}
            />
            <NumberInput
              label="Rating (0-100)"
              placeholder="85"
              value={formData.Rating}
              onChange={(val) => setFormData({ ...formData, Rating: Number(val) || 0 })}
              min={0}
              max={100}
            />
          </Group>
          <Group justify="flex-end" mt="md">
            <Button
              variant="default"
              onClick={() => {
                setModalOpened(false);
                setFormData(emptyFormData);
              }}
            >
              Cancel
            </Button>
            <Button
              onClick={handleSubmit}
              loading={createMutation.isPending || updateMutation.isPending}
            >
              {editMode ? 'Update' : 'Create'}
            </Button>
          </Group>
        </Stack>
      </Modal>
    </Stack>
  );
}
