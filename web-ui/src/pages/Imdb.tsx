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
import { IconSearch, IconPlus, IconEdit, IconTrash, IconMovie, IconChevronUp, IconChevronDown } from '@tabler/icons-react';
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

type SortField = 'ImdbId' | 'Title' | 'Year' | 'Rating' | 'Votes' | 'Genres' | 'Countries' | 'Languages' | 'ImdbType' | 'CreationTime' | 'UpdatedTime';
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
// - No wildcard: exact match (Spanish matches only "Spanish", not "English,Spanish")
// - With wildcard: pattern match (*Spanish* matches "English,Spanish")
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

interface ImdbRecord {
  ImdbId: string;
  Title: string;
  Year: number;
  Rating: number;
  Votes: number;
  Genres: string;
  Countries: string;
  Languages: string;
  ImdbType: string;
  CreationTime: number;
  UpdatedTime: number;
}

interface FormData {
  ImdbId: string;
  Title: string;
  Year: number;
  Rating: number;
  Votes: number;
  Genres: string;
  Countries: string;
  Languages: string;
  ImdbType: string;
}

const emptyFormData: FormData = {
  ImdbId: '',
  Title: '',
  Year: new Date().getFullYear(),
  Rating: 0,
  Votes: 0,
  Genres: '',
  Countries: '',
  Languages: '',
  ImdbType: 'movie',
};

export function Imdb() {
  const queryClient = useQueryClient();

  // Filter states
  const [titleFilter, setTitleFilter] = useState('');
  const [yearMin, setYearMin] = useState<number | string>('');
  const [yearMax, setYearMax] = useState<number | string>('');
  const [ratingMin, setRatingMin] = useState<number | string>('');
  const [genreFilter, setGenreFilter] = useState('');
  const [countryFilter, setCountryFilter] = useState('');
  const [languageFilter, setLanguageFilter] = useState('');
  const [typeFilter, setTypeFilter] = useState<string>('');

  // Sort state
  const [sortField, setSortField] = useState<SortField>('UpdatedTime');
  const [sortDirection, setSortDirection] = useState<SortDirection>('desc');

  // Pagination state
  const [page, setPage] = useState(1);
  const itemsPerPage = 100;

  // Modal states
  const [modalOpened, setModalOpened] = useState(false);
  const [editMode, setEditMode] = useState(false);
  const [formData, setFormData] = useState<FormData>(emptyFormData);

  // Fetch all IMDB records
  const { data, isLoading, error, refetch } = useQuery({
    queryKey: ['imdb-records'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiImdbService/GetAllImdbRecords', {});

      if (res.data?.result && Array.isArray(res.data.result) && res.data.result[0]?.Records) {
        const records = parseMaybeJsonArray(res.data.result[0].Records);
        return records as ImdbRecord[];
      }
      return [];
    },
    refetchInterval: 30000,
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
    if (titleFilter) {
      result = result.filter(r =>
        r.Title.toLowerCase().includes(titleFilter.toLowerCase()) ||
        r.ImdbId.toLowerCase().includes(titleFilter.toLowerCase())
      );
    }

    if (yearMin !== '' && yearMin !== null) {
      result = result.filter(r => r.Year >= Number(yearMin));
    }

    if (yearMax !== '' && yearMax !== null) {
      result = result.filter(r => r.Year <= Number(yearMax));
    }

    if (ratingMin !== '' && ratingMin !== null) {
      result = result.filter(r => r.Rating >= Number(ratingMin));
    }

    if (genreFilter) {
      result = result.filter(r => matchesFilter(r.Genres, genreFilter));
    }

    if (countryFilter) {
      result = result.filter(r => matchesFilter(r.Countries, countryFilter));
    }

    if (languageFilter) {
      result = result.filter(r => matchesFilter(r.Languages, languageFilter));
    }

    if (typeFilter) {
      result = result.filter(r => r.ImdbType.toLowerCase() === typeFilter.toLowerCase());
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
  }, [data, titleFilter, yearMin, yearMax, ratingMin, genreFilter, countryFilter, languageFilter, typeFilter, sortField, sortDirection]);

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
      await apiClient.post('/ApiImdbService/CreateImdbRecord', {
        ImdbId: data.ImdbId,
        Title: data.Title,
        Year: data.Year,
        Rating: data.Rating,
        Votes: data.Votes,
        Genres: data.Genres,
        Countries: data.Countries,
        Languages: data.Languages,
        ImdbType: data.ImdbType,
      });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Success',
        message: 'IMDB record created successfully',
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['imdb-records'] });
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
      await apiClient.post('/ApiImdbService/UpdateImdbRecord', {
        ImdbId: data.ImdbId,
        Title: data.Title,
        Year: data.Year,
        Rating: data.Rating,
        Votes: data.Votes,
        Genres: data.Genres,
        Countries: data.Countries,
        Languages: data.Languages,
        ImdbType: data.ImdbType,
      });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Success',
        message: 'IMDB record updated successfully',
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['imdb-records'] });
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
    mutationFn: async (imdbId: string) => {
      await apiClient.post('/ApiImdbService/DeleteImdbRecord', { ImdbId: imdbId });
    },
    onSuccess: () => {
      notifications.show({
        title: 'Success',
        message: 'IMDB record deleted successfully',
        color: 'green',
      });
      queryClient.invalidateQueries({ queryKey: ['imdb-records'] });
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

  const handleEdit = (record: ImdbRecord) => {
    setEditMode(true);
    setFormData({
      ImdbId: record.ImdbId,
      Title: record.Title,
      Year: record.Year,
      Rating: record.Rating,
      Votes: record.Votes,
      Genres: record.Genres,
      Countries: record.Countries,
      Languages: record.Languages,
      ImdbType: record.ImdbType,
    });
    setModalOpened(true);
  };

  const handleDelete = (imdbId: string) => {
    if (confirm(`Are you sure you want to delete IMDB record ${imdbId}?`)) {
      deleteMutation.mutate(imdbId);
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
        <Text c="red">Error loading IMDB data: {(error as Error).message}</Text>
        <Button onClick={() => refetch()}>Retry</Button>
      </Stack>
    );
  }

  return (
    <Stack gap="md">
      <Group justify="space-between">
        <Group>
          <IconMovie size={32} />
          <Text size="xl" fw={700}>IMDB Database</Text>
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
              placeholder="Search by title or IMDB ID..."
              leftSection={<IconSearch size={16} />}
              value={titleFilter}
              onChange={(e) => {
                setTitleFilter(e.currentTarget.value);
                resetPage();
              }}
            />
            <Select
              placeholder="Type"
              clearable
              value={typeFilter}
              onChange={(val) => {
                setTypeFilter(val || '');
                resetPage();
              }}
              data={[
                { value: 'movie', label: 'Movie' },
                { value: 'series', label: 'Series' },
                { value: 'tvMovie', label: 'TV Movie' },
                { value: 'tvSeries', label: 'TV Series' },
              ]}
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
          <Group grow>
            <TextInput
              placeholder="Genre (Action or *Action*)"
              value={genreFilter}
              onChange={(e) => {
                setGenreFilter(e.currentTarget.value);
                resetPage();
              }}
            />
            <TextInput
              placeholder="Country (USA or *USA*)"
              value={countryFilter}
              onChange={(e) => {
                setCountryFilter(e.currentTarget.value);
                resetPage();
              }}
            />
            <TextInput
              placeholder="Language (Spanish or *Spanish*)"
              value={languageFilter}
              onChange={(e) => {
                setLanguageFilter(e.currentTarget.value);
                resetPage();
              }}
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
                <SortableHeader field="ImdbId" currentField={sortField} direction={sortDirection} onClick={handleSort} width="100px">
                  IMDB ID
                </SortableHeader>
                <SortableHeader field="Title" currentField={sortField} direction={sortDirection} onClick={handleSort} width="300px">
                  Title
                </SortableHeader>
                <SortableHeader field="Year" currentField={sortField} direction={sortDirection} onClick={handleSort} width="70px">
                  Year
                </SortableHeader>
                <SortableHeader field="Rating" currentField={sortField} direction={sortDirection} onClick={handleSort} width="70px">
                  Rating
                </SortableHeader>
                <SortableHeader field="Votes" currentField={sortField} direction={sortDirection} onClick={handleSort} width="90px">
                  Votes
                </SortableHeader>
                <SortableHeader field="Genres" currentField={sortField} direction={sortDirection} onClick={handleSort} width="150px">
                  Genres
                </SortableHeader>
                <SortableHeader field="Countries" currentField={sortField} direction={sortDirection} onClick={handleSort} width="130px">
                  Countries
                </SortableHeader>
                <SortableHeader field="Languages" currentField={sortField} direction={sortDirection} onClick={handleSort} width="120px">
                  Languages
                </SortableHeader>
                <SortableHeader field="ImdbType" currentField={sortField} direction={sortDirection} onClick={handleSort} width="80px">
                  Type
                </SortableHeader>
                <SortableHeader field="CreationTime" currentField={sortField} direction={sortDirection} onClick={handleSort} width="100px">
                  Created
                </SortableHeader>
                <SortableHeader field="UpdatedTime" currentField={sortField} direction={sortDirection} onClick={handleSort} width="100px">
                  Updated
                </SortableHeader>
                <Table.Th style={{ width: '80px' }}>Actions</Table.Th>
              </Table.Tr>
            </Table.Thead>
            <Table.Tbody>
              {isLoading ? (
                <Table.Tr>
                  <Table.Td colSpan={12}>
                    <Text ta="center">Loading...</Text>
                  </Table.Td>
                </Table.Tr>
              ) : filtered.length === 0 ? (
                <Table.Tr>
                  <Table.Td colSpan={12}>
                    <Text ta="center">No records found</Text>
                  </Table.Td>
                </Table.Tr>
              ) : (
                paginatedData.map((record) => (
                  <Table.Tr key={record.ImdbId}>
                    <Table.Td style={{ width: '100px' }}>
                      <a
                        href={`https://www.imdb.com/title/${record.ImdbId}/`}
                        target="_blank"
                        rel="noopener noreferrer"
                      >
                        {record.ImdbId}
                      </a>
                    </Table.Td>
                    <TruncatedCell width={300}>{record.Title}</TruncatedCell>
                    <Table.Td style={{ width: '70px' }}>{record.Year}</Table.Td>
                    <Table.Td style={{ width: '70px' }}>{record.Rating}</Table.Td>
                    <Table.Td style={{ width: '90px' }}>{record.Votes.toLocaleString()}</Table.Td>
                    <TruncatedCell width={150}>{record.Genres}</TruncatedCell>
                    <TruncatedCell width={130}>{record.Countries}</TruncatedCell>
                    <TruncatedCell width={120}>{record.Languages}</TruncatedCell>
                    <Table.Td style={{ width: '80px' }}>{record.ImdbType}</Table.Td>
                    <Table.Td style={{ width: '100px' }}>
                      {record.CreationTime && record.CreationTime > 0
                        ? new Date(record.CreationTime * 1000).toLocaleDateString('de-DE')
                        : 'N/A'}
                    </Table.Td>
                    <Table.Td style={{ width: '100px' }}>
                      {record.UpdatedTime && record.UpdatedTime > 0
                        ? new Date(record.UpdatedTime * 1000).toLocaleDateString('de-DE')
                        : 'N/A'}
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
                          onClick={() => handleDelete(record.ImdbId)}
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
        title={editMode ? 'Edit IMDB Record' : 'Add New IMDB Record'}
        size="lg"
      >
        <Stack gap="md">
          <TextInput
            label="IMDB ID"
            placeholder="tt1234567"
            required
            value={formData.ImdbId}
            onChange={(e) => setFormData({ ...formData, ImdbId: e.currentTarget.value })}
            disabled={editMode}
          />
          <TextInput
            label="Title"
            placeholder="Movie Title"
            required
            value={formData.Title}
            onChange={(e) => setFormData({ ...formData, Title: e.currentTarget.value })}
          />
          <Group grow>
            <NumberInput
              label="Year"
              placeholder="2024"
              required
              value={formData.Year}
              onChange={(val) => setFormData({ ...formData, Year: Number(val) || 0 })}
              min={1900}
              max={2100}
            />
            <NumberInput
              label="Rating (0-100)"
              placeholder="75"
              value={formData.Rating}
              onChange={(val) => setFormData({ ...formData, Rating: Number(val) || 0 })}
              min={0}
              max={100}
            />
          </Group>
          <NumberInput
            label="Votes"
            placeholder="1000000"
            value={formData.Votes}
            onChange={(val) => setFormData({ ...formData, Votes: Number(val) || 0 })}
            min={0}
          />
          <TextInput
            label="Genres"
            placeholder="Action,Thriller,Drama"
            value={formData.Genres}
            onChange={(e) => setFormData({ ...formData, Genres: e.currentTarget.value })}
          />
          <TextInput
            label="Countries"
            placeholder="USA,UK"
            value={formData.Countries}
            onChange={(e) => setFormData({ ...formData, Countries: e.currentTarget.value })}
          />
          <TextInput
            label="Languages"
            placeholder="English,Spanish"
            value={formData.Languages}
            onChange={(e) => setFormData({ ...formData, Languages: e.currentTarget.value })}
          />
          <Select
            label="Type"
            placeholder="Select type"
            required
            value={formData.ImdbType}
            onChange={(val) => setFormData({ ...formData, ImdbType: val || 'movie' })}
            data={[
              { value: 'movie', label: 'Movie' },
              { value: 'series', label: 'Series' },
              { value: 'tvMovie', label: 'TV Movie' },
              { value: 'tvSeries', label: 'TV Series' },
            ]}
          />
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
