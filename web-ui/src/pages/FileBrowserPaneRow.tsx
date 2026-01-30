import { memo } from 'react';
import { Table, Checkbox, Group, ThemeIcon, Tooltip, Text } from '@mantine/core';
import { IconFolder, IconFile } from '@tabler/icons-react';
import type { FileEntry } from '../api/client';

interface FileRowProps {
  file: FileEntry;
  modifiedMs: number | null;
  selected: boolean;
  onToggle: (fileName: string) => void;
  onNavigate: (path: string) => void;
  currentPath: string;
}

function _splitFilename(aName: string): { base: string; ext: string } {
  const idx = aName.lastIndexOf('.');
  if (idx <= 0 || idx === aName.length - 1) return { base: aName, ext: '' };
  return { base: aName.slice(0, idx), ext: aName.slice(idx) };
}

function _formatModified(aMs: number | null): string {
  if (!aMs) return '—';
  try {
    return new Intl.DateTimeFormat(undefined, {
      year: '2-digit',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
    }).format(new Date(aMs));
  } catch {
    return new Date(aMs).toLocaleString();
  }
}

function _formatSize(bytes: number) {
  if (bytes === 0) return '0 B';
  const k = 1024;
  const sizes = ['B', 'KB', 'MB', 'GB', 'TB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
}

export const FileRow = memo(function FileRow({
  file: f,
  modifiedMs,
  selected,
  onToggle,
  onNavigate,
  currentPath,
}: FileRowProps) {
  // Debug log to check for unnecessary re-renders
  console.log(`[FileRow] Rendering: ${f.name} (selected: ${selected})`);

  return (
    <Table.Tr
      style={{ cursor: f.is_dir ? 'pointer' : 'default', userSelect: 'none' }}
      data-selected={selected || undefined}
      onClick={(e) => {
        if (e.ctrlKey || e.metaKey) {
          onToggle(f.name);
          return;
        }
        if (f.is_dir) {
          onNavigate(currentPath + (currentPath === '/' ? '' : '/') + f.name);
        } else {
          onToggle(f.name);
        }
      }}
    >
      <Table.Td>
        <Checkbox
          checked={selected}
          onChange={() => onToggle(f.name)}
          onClick={(e) => e.stopPropagation()}
          size="xs"
        />
      </Table.Td>
      <Table.Td>
        <Group gap="xs" wrap="nowrap" style={{ minWidth: 0 }}>
          <ThemeIcon color={f.is_dir ? 'blue' : 'gray'} variant="light" size="sm">
            {f.is_dir ? <IconFolder size="0.8rem" /> : <IconFile size="0.8rem" />}
          </ThemeIcon>
          <Tooltip label={f.name} withArrow withinPortal>
            {f.is_dir ? (
              <Text size="sm" fw={600} truncate style={{ minWidth: 0, flex: 1 }}>
                {f.name}
              </Text>
            ) : (
              (() => {
                const parts = _splitFilename(f.name);
                return (
                  <Text
                    size="sm"
                    fw={400}
                    component="span"
                    style={{ minWidth: 0, flex: 1, display: 'flex' }}
                  >
                    <span style={{ minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                      {parts.base}
                    </span>
                    {parts.ext && (
                      <span style={{ flex: '0 0 auto', whiteSpace: 'nowrap' }}>
                        {parts.ext}
                      </span>
                    )}
                  </Text>
                );
              })()
            )}
          </Tooltip>
        </Group>
      </Table.Td>
      <Table.Td>
        {(() => {
          const uid = (f.user || '').toString();
          const display = uid.length > 6 ? `${uid.slice(0, 6)}…` : (uid || '—');
          return (
            <Tooltip label={uid || '—'} withArrow withinPortal>
              <Text size="sm" c="dimmed" ta="right">
                {display}
              </Text>
            </Tooltip>
          );
        })()}
      </Table.Td>
      <Table.Td>
        <Text size="sm" c="dimmed" ta="right">
          {f.is_dir ? '—' : _formatSize(f.size)}
        </Text>
      </Table.Td>
      <Table.Td>
        <Text size="sm" c="dimmed">
          {_formatModified(modifiedMs)}
        </Text>
      </Table.Td>
    </Table.Tr>
  );
});
