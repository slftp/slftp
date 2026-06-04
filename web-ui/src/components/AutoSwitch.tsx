import { Switch, Tooltip, Loader, rem } from '@mantine/core';
import { IconCheck, IconX } from '@tabler/icons-react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '../api/client';

export function AutoSwitch() {
  const queryClient = useQueryClient();

  const { data: autoStatus, isLoading } = useQuery({
    queryKey: ['autoStatus'],
    queryFn: async () => {
      const res = await apiClient.post('/ApiSystemService/GetAutoStatus');
      // mORMot wraps boolean result in an array
      if (res.data && res.data.result && typeof res.data.result[0] === 'boolean') {
        return res.data.result[0];
      }
      // Fallback if the structure is not as expected
      return false;
    },
    refetchOnWindowFocus: false,
  });

  const mutation = useMutation({
    mutationFn: (enabled: boolean) => {
      return apiClient.post('/ApiSystemService/SetAutoStatus', { Enabled: enabled });
    },
    onSuccess: () => {
      // Invalidate and refetch the query to get the new state
      queryClient.invalidateQueries({ queryKey: ['autoStatus'] });
    },
  });

  const handleToggle = (event: React.ChangeEvent<HTMLInputElement>) => {
    mutation.mutate(event.currentTarget.checked);
  };

  if (isLoading) {
    return <Loader size="xs" />;
  }

  const onIcon = (
    <IconCheck
      style={{ width: rem(12), height: rem(12) }}
      stroke={2.5}
    />
  );

  const offIcon = (
    <IconX
      style={{ width: rem(12), height: rem(12) }}
      stroke={2.5}
    />
  );

  return (
    <Tooltip label="Turn Auto On/Off" withArrow withinPortal zIndex={999}>
      <div>
        <Switch
          checked={autoStatus}
          onChange={handleToggle}
          disabled={mutation.isPending}
          size="md"
          onLabel={onIcon}
          offLabel={offIcon}
        />
      </div>
    </Tooltip>
  );
}