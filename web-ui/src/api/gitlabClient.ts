// The slftp project on gitlab.com is public: the v4 API answers anonymously
// and sends "access-control-allow-origin: *". The browser therefore queries it directly,
// without proxying through slftp and without a token.
const GITLAB_HOST = 'https://gitlab.com';
const GITLAB_PROJECT_PATH = 'slftp/slftp';
// GitLab's default branch for this project is "dev", not the branch this build tracks,
// so the branch must be requested explicitly rather than relying on GitLab's default-branch pick.
const GITLAB_BRANCH = 'api_and_webui';

export const GITLAB_PROJECT_URL = `${GITLAB_HOST}/${GITLAB_PROJECT_PATH}/-/tree/${GITLAB_BRANCH}`;

const PROJECT_API = `${GITLAB_HOST}/api/v4/projects/${encodeURIComponent(GITLAB_PROJECT_PATH)}`;

export const GITLAB_COMMIT_LIMIT = 30;
export const GITLAB_PROJECT_PATH_LABEL = GITLAB_PROJECT_PATH;
export const GITLAB_BRANCH_LABEL = GITLAB_BRANCH;

export interface GitlabCommit {
  id: string;
  short_id: string;
  title: string;
  message: string;
  author_name: string;
  committed_date: string;
  web_url: string;
}

/**
 * Latest commits of the GITLAB_BRANCH branch.
 */
export const fetchGitlabCommits = async (): Promise<GitlabCommit[]> => {
  const res = await fetch(
    `${PROJECT_API}/repository/commits?ref_name=${encodeURIComponent(GITLAB_BRANCH)}&per_page=${GITLAB_COMMIT_LIMIT}`,
  );
  if (!res.ok) throw new Error(`GitLab API returned ${res.status}`);
  return res.json();
};

/**
 * Extracts the commit hash from the version string of the slftp API.
 * "1.5.11b6 (git# DEADC0DE-1aec5ee4)" -> "1aec5ee4"
 *
 * SL_REV is only patched in by the Makefile. On a build without the Makefile it is
 * empty, the string then contains no "git#" and we return undefined -> no update
 * comparison instead of a false report.
 */
export const parseRunningCommit = (version?: string): string | undefined =>
  version?.match(/git#\s*(?:\S+-)?([0-9a-f]{7,40})\b/i)?.[1]?.toLowerCase();

/**
 * The commit message without its subject line, i.e. the part worth expanding in the UI.
 * Returns an empty string for single-line commits.
 *
 * GitLab truncates "title" for very long subject lines, so when the message does not
 * start with the title we fall back to the full message rather than slicing blindly.
 */
export const commitBody = (commit: GitlabCommit): string =>
  commit.message.startsWith(commit.title)
    ? commit.message.slice(commit.title.length).trim()
    : commit.message.trim();

/**
 * How many commits is the running instance behind?
 *
 * undefined = no statement possible (list missing, SL_REV empty, or the running commit
 * is so old it is no longer within the latest GITLAB_COMMIT_LIMIT entries).
 * 0 = up to date.
 */
export const commitsBehind = (
  commits: GitlabCommit[] | undefined,
  running: string | undefined,
): number | undefined => {
  if (!commits?.length || !running) return undefined;
  // The full hash starts with the short hash from SL_REV, no matter how far it was cut.
  const idx = commits.findIndex((c) => c.id.toLowerCase().startsWith(running));
  return idx === -1 ? undefined : idx;
};

/**
 * Shared query options: staleTime Infinity means one fetch per web-UI load, no polling.
 * Dashboard and GitLab page share the same request through the query key.
 */
export const gitlabCommitsQuery = {
  queryKey: ['gitlabCommits'],
  queryFn: fetchGitlabCommits,
  staleTime: Infinity,
  refetchOnWindowFocus: false,
  retry: false,
} as const;
