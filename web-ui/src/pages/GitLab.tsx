import { useQuery } from '@tanstack/react-query';
import {
  Alert,
  Anchor,
  Badge,
  Box,
  Card,
  Center,
  Collapse,
  Group,
  Loader,
  Stack,
  Text,
  Title,
  UnstyledButton,
} from '@mantine/core';
import { useDisclosure } from '@mantine/hooks';
import {
  IconAlertCircle,
  IconBrandGitlab,
  IconCheck,
  IconChevronRight,
  IconGitCommit,
  IconRefreshAlert,
} from '@tabler/icons-react';
import { fetchSystemStatus } from '../api/client';
import {
  commitBody,
  commitsBehind,
  gitlabCommitsQuery,
  parseRunningCommit,
  GITLAB_BRANCH_LABEL,
  GITLAB_COMMIT_LIMIT,
  GITLAB_PROJECT_PATH_LABEL,
  GITLAB_PROJECT_URL,
} from '../api/gitlabClient';
import type { GitlabCommit } from '../api/gitlabClient';

const GITLAB_COLOR = '#fc6d26';

export function GitLab() {
  const { data: status } = useQuery({
    queryKey: ['systemStatus'],
    queryFn: fetchSystemStatus,
    refetchOnWindowFocus: false,
  });

  const { data: commits, isLoading, error } = useQuery(gitlabCommitsQuery);

  const running = parseRunningCommit(status?.Version);
  const behind = commitsBehind(commits, running);

  return (
    <Stack gap="lg">
      <Group justify="space-between" align="center">
        <Group gap="sm">
          <IconBrandGitlab size="2rem" color={GITLAB_COLOR} stroke={1.5} />
          <Box>
            <Title order={2} style={{ fontSize: '1.4rem' }}>
              GitLab
            </Title>
            <Anchor
              href={GITLAB_PROJECT_URL}
              target="_blank"
              rel="noopener noreferrer"
              size="sm"
              c="dimmed"
            >
              {GITLAB_PROJECT_PATH_LABEL} &middot; {GITLAB_BRANCH_LABEL}
            </Anchor>
          </Box>
        </Group>
        <Badge
          size="lg"
          variant="light"
          style={{ fontFamily: 'monospace', textTransform: 'none' }}
        >
          {status?.Version ?? 'unknown'}
        </Badge>
      </Group>

      <UpdateStatus behind={behind} running={running} hasCommits={!!commits?.length} />

      {error && (
        <Alert
          className="alert-glass-warn"
          icon={<IconAlertCircle size="1.25rem" />}
          title="GitLab not reachable"
          radius="lg"
        >
          <Text size="sm">
            {(error as Error).message}. The commit list needs direct access to{' '}
            {GITLAB_PROJECT_URL.replace(/^https:\/\//, '').split('/')[0]} from your browser.
          </Text>
        </Alert>
      )}

      {isLoading ? (
        <Center h={160}>
          <Loader />
        </Center>
      ) : (
        <Stack gap="xs">
          {commits?.map((commit) => (
            <CommitRow
              key={commit.id}
              commit={commit}
              isRunning={!!running && commit.id.toLowerCase().startsWith(running)}
            />
          ))}
        </Stack>
      )}

      {!!commits?.length && (
        <Text size="xs" c="dimmed" ta="center">
          Showing the latest {Math.min(GITLAB_COMMIT_LIMIT, commits.length)} commits of the{' '}
          {GITLAB_BRANCH_LABEL} branch.
        </Text>
      )}
    </Stack>
  );
}

interface CommitRowProps {
  commit: GitlabCommit;
  isRunning: boolean;
}

function CommitRow({ commit, isRunning }: CommitRowProps) {
  const [opened, { toggle }] = useDisclosure(false);

  // Single-line commits have no body, those stay non-expandable.
  const body = commitBody(commit);
  const hasBody = body.length > 0;

  const heading = (
    <Group gap="sm" wrap="nowrap" align="flex-start" style={{ minWidth: 0 }}>
      {hasBody ? (
        <IconChevronRight
          size="1.1rem"
          stroke={1.5}
          color={isRunning ? GITLAB_COLOR : 'var(--nav-label-inactive)'}
          style={{
            marginTop: 2,
            flexShrink: 0,
            transform: opened ? 'rotate(90deg)' : 'none',
            transition: 'transform 0.15s ease',
          }}
        />
      ) : (
        <IconGitCommit
          size="1.1rem"
          stroke={1.5}
          color={isRunning ? GITLAB_COLOR : 'var(--nav-label-inactive)'}
          style={{ marginTop: 2, flexShrink: 0 }}
        />
      )}
      <Box style={{ minWidth: 0 }}>
        <Text size="sm" fw={500} style={{ wordBreak: 'break-word', textAlign: 'left' }}>
          {commit.title}
        </Text>
        <Text size="xs" c="dimmed" style={{ textAlign: 'left' }}>
          {commit.author_name} &middot; {new Date(commit.committed_date).toLocaleString()}
        </Text>
      </Box>
    </Group>
  );

  return (
    <Card
      radius="md"
      p="sm"
      className="glass"
      style={{
        border: isRunning ? `1px solid ${GITLAB_COLOR}80` : '1px solid var(--border)',
        background: isRunning ? `${GITLAB_COLOR}12` : undefined,
      }}
    >
      <Group justify="space-between" wrap="nowrap" align="flex-start" gap="sm">
        {hasBody ? (
          <UnstyledButton
            onClick={toggle}
            aria-expanded={opened}
            style={{ minWidth: 0, flex: 1 }}
          >
            {heading}
          </UnstyledButton>
        ) : (
          heading
        )}
        <Group gap="xs" wrap="nowrap" style={{ flexShrink: 0 }}>
          {isRunning && (
            <Badge size="sm" variant="light" color="orange">
              running
            </Badge>
          )}
          <Anchor
            href={commit.web_url}
            target="_blank"
            rel="noopener noreferrer"
            size="xs"
            style={{ fontFamily: 'monospace' }}
          >
            {commit.short_id}
          </Anchor>
        </Group>
      </Group>

      {hasBody && (
        <Collapse in={opened}>
          <Text
            size="xs"
            c="dimmed"
            mt="sm"
            pl={30}
            style={{ whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}
          >
            {body}
          </Text>
        </Collapse>
      )}
    </Card>
  );
}

interface UpdateStatusProps {
  behind: number | undefined;
  running: string | undefined;
  hasCommits: boolean;
}

function UpdateStatus({ behind, running, hasCommits }: UpdateStatusProps) {
  if (!hasCommits) return null;

  if (!running) {
    return (
      <Alert
        className="alert-glass-warn"
        icon={<IconAlertCircle size="1.25rem" />}
        title="No revision in this build"
        radius="lg"
      >
        <Text size="sm">
          This binary was built without the Makefile, so <code>SL_REV</code> is empty and there
          is nothing to compare against. Build with <code>make</code> to embed the commit hash.
        </Text>
      </Alert>
    );
  }

  if (behind === undefined) {
    return (
      <Alert
        className="alert-glass-warn"
        icon={<IconAlertCircle size="1.25rem" />}
        title="Running commit not found"
        radius="lg"
      >
        <Text size="sm">
          Commit <code>{running}</code> is not among the latest {GITLAB_COMMIT_LIMIT} commits —
          this build is either far behind or from another branch.
        </Text>
      </Alert>
    );
  }

  if (behind === 0) {
    return (
      <Alert icon={<IconCheck size="1.25rem" />} title="Up to date" color="teal" radius="lg">
        <Text size="sm">You are running the latest commit of the {GITLAB_BRANCH_LABEL} branch.</Text>
      </Alert>
    );
  }

  return (
    <Alert
      className="alert-glass-warn"
      icon={<IconRefreshAlert size="1.25rem" />}
      title={`Update available — ${behind} commit${behind === 1 ? '' : 's'} behind`}
      radius="lg"
    >
      <Text size="sm">
        Pull the latest changes and rebuild to catch up. The commits you are missing are listed
        above the <Badge size="xs" variant="light" color="orange">running</Badge> marker.
      </Text>
    </Alert>
  );
}
