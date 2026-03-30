import { useState, useEffect } from 'react';
import { Paper, Select, Button, Group, Textarea, LoadingOverlay, Stack } from '@mantine/core';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { fetchConfigList, fetchConfigContent, saveConfigContent, reloadConfig } from '../api/client';
import { notifications } from '@mantine/notifications';
import { IconCheck, IconX, IconRefresh } from '@tabler/icons-react';

export function ConfigEditor() {
  const [selectedFile, setSelectedFile] = useState<string | null>(null);
  const [content, setContent] = useState('');
  const [isDirty, setIsDirty] = useState(false);
  const queryClient = useQueryClient();

  const { data: files, isLoading: isLoadingFiles } = useQuery({
    queryKey: ['configFiles'],
    queryFn: fetchConfigList
  });

  const { data: fileContent, isFetching: isFetchingContent } = useQuery({
    queryKey: ['configContent', selectedFile],
    queryFn: () => fetchConfigContent(selectedFile!),
    enabled: !!selectedFile
  });

  useEffect(() => {
    if (fileContent !== undefined) {
      setContent(fileContent);
      setIsDirty(false);
    }
  }, [fileContent]);

  const saveMutation = useMutation({
    mutationFn: () => saveConfigContent(selectedFile!, content),
    onSuccess: (success) => {
      if (success) {
        setIsDirty(false);
        notifications.show({
          title: 'Saved',
          message: `${selectedFile} saved successfully`,
          color: 'green',
          icon: <IconCheck size="1.1rem" />,
        });
        queryClient.invalidateQueries({ queryKey: ['configContent', selectedFile] });
      } else {
        notifications.show({
          title: 'Error',
          message: 'Failed to save file',
          color: 'red',
          icon: <IconX size="1.1rem" />,
        });
      }
    }
  });

  const reloadMutation = useMutation({
      mutationFn: () => reloadConfig(selectedFile!),
      onSuccess: (success) => {
          if (success) {
              notifications.show({
                  title: 'Reloaded',
                  message: `${selectedFile} configuration reloaded successfully`,
                  color: 'green',
                  icon: <IconCheck size="1.1rem" />,
              });
          } else {
              const needsRestart = ['slftp.ini', 'slftp.scheduler'].includes(selectedFile || '');
              notifications.show({
                  title: needsRestart ? 'Restart Required' : 'Info',
                  message: needsRestart 
                    ? `${selectedFile} changes require a full restart to take effect.`
                    : `Reload not supported or failed for ${selectedFile}.`,
                  color: needsRestart ? 'orange' : 'blue',
                  icon: <IconRefresh size="1.1rem" />,
              });
          }
      }
  });

  return (
    <Stack gap="md" style={{ height: 'calc(100vh - 160px)' }}>
      <Paper p="md" withBorder style={{ flex: 1, display: 'flex', flexDirection: 'column', position: 'relative' }}>
        <LoadingOverlay visible={isLoadingFiles || isFetchingContent || saveMutation.isPending} />
        
        <Group justify="space-between" mb="md">
          <Select
            label="Select Configuration File"
            placeholder="Pick a file"
            data={files || []}
            value={selectedFile}
            onChange={(value) => {
                if (isDirty) {
                    if (!confirm('You have unsaved changes. Discard them?')) return;
                }
                setSelectedFile(value);
            }}
            style={{ flex: 1 }}
          />
          <Group align="end">
              <Button 
                  onClick={() => saveMutation.mutate()} 
                  loading={saveMutation.isPending} 
                  disabled={!isDirty || !selectedFile}
              >
                  Save
              </Button>
               <Button 
                  variant="light"
                  onClick={() => reloadMutation.mutate()} 
                  loading={reloadMutation.isPending} 
                  disabled={!selectedFile}
                  leftSection={<IconRefresh size="1rem" />}
              >
                  Reload
              </Button>
          </Group>
        </Group>

        {selectedFile && (
          <Textarea
            value={content}
            onChange={(event) => {
                setContent(event.currentTarget.value);
                setIsDirty(true);
            }}
            autosize={false}
            styles={{ input: { fontFamily: 'monospace', height: '100%' }, wrapper: { height: '100%' } }}
            style={{ flex: 1, display: 'flex', flexDirection: 'column' }}
          />
        )}
      </Paper>
    </Stack>
  );
}
