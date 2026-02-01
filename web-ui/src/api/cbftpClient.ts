import { apiClient } from './client';

// Use slftp API as proxy to cbftp
// All requests will go to /cbftp/* which will be proxied to cbftp REST API
export const cbftpClient = apiClient;

// Types based on API documentation

export interface Skiplist {
  action: 'ALLOW' | 'DENY' | 'UNIQUE' | 'SIMILAR';
  dir: boolean;
  file: boolean;
  pattern: string;
  regex: boolean;
  scope: 'IN_RACE' | 'ALL';
}

export interface SiteSection {
  name: string;
  path: string;
}

export interface CbftpSite {
  name: string;
  addresses?: string[];
  affils?: string[];
  allow_download?: 'YES' | 'NO' | 'MATCH_ONLY';
  allow_upload?: 'YES' | 'NO';
  avg_speed?: Record<string, number>;
  base_path?: string;
  broken_pasv?: boolean;
  cepr?: boolean;
  cpsv?: boolean;
  disabled?: boolean;
  except_source_sites?: string[];
  except_target_sites?: string[];
  force_binary_mode?: boolean;
  leave_free_slot?: boolean;
  list_command?: 'STAT_L' | 'LIST';
  max_idle_time?: number;
  max_logins?: number;
  max_sim_down?: number;
  max_sim_down_complete?: number;
  max_sim_down_pre?: number;
  max_sim_down_transferjob?: number;
  max_sim_up?: number;
  password?: string;
  pret?: boolean;
  priority?: 'VERY_LOW' | 'LOW' | 'NORMAL' | 'HIGH' | 'VERY_HIGH';
  list_frequency?: 'VERY_LOW' | 'FIXED_LOW' | 'FIXED_AVERAGE' | 'FIXED_HIGH' | 'FIXED_VERY_HIGH' | 'AUTO' | 'DYNAMIC_LOW' | 'DYNAMIC_AVERAGE' | 'DYNAMIC_HIGH' | 'DYNAMIC_VERY_HIGH';
  proxy_name?: string;
  proxy_type?: 'GLOBAL' | 'NONE' | 'USE';
  sections?: SiteSection[];
  skiplist?: Skiplist[];
  sscn?: boolean;
  stay_logged_in?: boolean;
  tls_mode?: 'NONE' | 'AUTH_TLS' | 'IMPLICIT';
  tls_transfer_policy?: 'ALWAYS_OFF' | 'PREFER_OFF' | 'PREFER_ON' | 'ALWAYS_ON';
  transfer_protocol?: 'IPV4_ONLY' | 'PREFER_IPV4' | 'PREFER_IPV6' | 'IPV6_ONLY';
  transfer_source_policy?: 'ALLOW' | 'BLOCK';
  transfer_target_policy?: 'ALLOW' | 'BLOCK';
  user?: string;
  xdupe?: boolean;
}

export interface CbftpSection {
  name: string;
  hotkey?: number;
  num_jobs?: number;
  skiplist?: Skiplist[];
}

export interface JobProgress {
  files_done: number;
  total_files: number;
  bytes_done: number;
  total_bytes: number;
  status: string;
}

export interface CbftpSpreadJob {
  name: string;
  section: string;
  status: 'RUNNING' | 'DONE' | 'ABORTED' | 'PREPARED';
  profile: 'RACE' | 'DISTRIBUTE' | 'PREPARE';
  sites?: string[];
  sites_dlonly?: string[];
  progress?: Record<string, JobProgress>;
  total_files?: number;
  files_done?: number;
  time_started?: string;
  time_spent?: number;
}

export interface CbftpTransferJob {
  name: string;
  id: number;
  status: 'RUNNING' | 'DONE' | 'ABORTED';
  type: 'FXP' | 'DOWNLOAD' | 'UPLOAD';
  src_site?: string;
  src_path?: string;
  src_section?: string;
  dst_site?: string;
  dst_path?: string;
  dst_section?: string;
  progress?: number;
  files_transferred?: number;
  total_files?: number;
  speed?: number;
  time_spent?: number;
}

export interface RawCommandRequest {
  command: string;
  sites?: string[];
  sites_with_sections?: string[];
  sites_all?: boolean;
  path?: string;
  path_section?: string;
  timeout?: number;
  async?: boolean;
}

export interface RawCommandResult {
  site: string;
  success: boolean;
  output: string;
  error?: string;
}

export interface RawCommandResponse {
  id?: number;
  results?: RawCommandResult[];
}

export interface StartSpreadJobRequest {
  section: string;
  name: string;
  sites?: string[];
  sites_dlonly?: string[];
  sites_all?: boolean;
  reset?: boolean;
  profile: 'RACE' | 'DISTRIBUTE' | 'PREPARE';
}

export interface ResetSpreadJobRequest {
  hard?: boolean;
}

export interface AbortSpreadJobRequest {
  delete?: 'NONE' | 'INCOMPLETE' | 'OWN' | 'ALL';
  sites?: string[];
}

export interface StartTransferJobRequest {
  src_site?: string;
  src_section?: string;
  src_path?: string;
  dst_site?: string;
  dst_section?: string;
  dst_path?: string;
  name: string;
}

export interface CbftpInfo {
  version?: string;
  build_date?: string;
  uptime?: number;
  num_sites?: number;
  num_spread_jobs?: number;
  num_transfer_jobs?: number;
  active_spread_jobs?: number;
  active_transfer_jobs?: number;
}

// API Functions

// Sites
export const getSites = async (section?: string): Promise<string[]> => {
  const params = section ? { section } : {};
  const response = await cbftpClient.get<string[]>('/cbftp/sites', { params });
  return response.data;
};

export const getSite = async (name: string): Promise<CbftpSite> => {
  const response = await cbftpClient.get<CbftpSite>(`/cbftp/sites/${name}`);
  const data = response.data || ({} as CbftpSite);
  if (!data.name) {
    return { ...data, name };
  }
  return data;
};

export const createSite = async (site: Partial<CbftpSite>): Promise<void> => {
  await cbftpClient.post('/cbftp/sites', site);
};

export const updateSite = async (name: string, updates: Partial<CbftpSite>): Promise<void> => {
  await cbftpClient.patch(`/cbftp/sites/${name}`, updates);
};

export const deleteSite = async (name: string): Promise<void> => {
  await cbftpClient.delete(`/cbftp/sites/${name}`);
};

export const getSiteSections = async (siteName: string): Promise<SiteSection[]> => {
  const response = await cbftpClient.get<SiteSection[]>(`/cbftp/sites/${siteName}/sections`);
  return response.data;
};

// Sections
export const getSections = async (): Promise<string[]> => {
  const response = await cbftpClient.get<Array<string | CbftpSection>>('/cbftp/sections');
  if (!Array.isArray(response.data)) {
    return [];
  }
  return response.data
    .map((entry) => (typeof entry === 'string' ? entry : entry?.name))
    .filter((name): name is string => typeof name === 'string' && name.length > 0);
};

export const getSection = async (name: string): Promise<CbftpSection> => {
  const response = await cbftpClient.get<CbftpSection>(`/cbftp/sections/${name}`);
  return response.data;
};

export const createSection = async (section: Partial<CbftpSection>): Promise<void> => {
  await cbftpClient.post('/cbftp/sections', section);
};

export const updateSection = async (name: string, updates: Partial<CbftpSection>): Promise<void> => {
  await cbftpClient.patch(`/cbftp/sections/${name}`, updates);
};

export const deleteSection = async (name: string): Promise<void> => {
  await cbftpClient.delete(`/cbftp/sections/${name}`);
};

// Spread Jobs
export interface SpreadJobFilters {
  status?: string;
  profile?: string;
  section?: string;
  site?: string;
  name?: string;
}

export const getSpreadJobs = async (filters?: SpreadJobFilters): Promise<string[]> => {
  const response = await cbftpClient.get<string[]>('/cbftp/spreadjobs', { params: filters });
  return response.data;
};

export const getSpreadJob = async (name: string): Promise<CbftpSpreadJob> => {
  const response = await cbftpClient.get<CbftpSpreadJob>(`/cbftp/spreadjobs/${name}`);
  const data = response.data || ({} as CbftpSpreadJob);
  if (!data.name) {
    return { ...data, name };
  }
  return data;
};

export const startSpreadJob = async (job: StartSpreadJobRequest): Promise<void> => {
  await cbftpClient.post('/cbftp/spreadjobs', job);
};

export const resetSpreadJob = async (name: string, options?: ResetSpreadJobRequest): Promise<void> => {
  await cbftpClient.post(`/cbftp/spreadjobs/${name}/reset`, options || {});
};

export const abortSpreadJob = async (name: string, options?: AbortSpreadJobRequest): Promise<void> => {
  await cbftpClient.post(`/cbftp/spreadjobs/${name}/abort`, options || {});
};

// Transfer Jobs
export interface TransferJobFilters {
  id?: boolean;
  status?: string;
  type?: string;
  src_site?: string;
  dst_site?: string;
  site?: string;
  name?: string;
}

export const getTransferJobs = async (filters?: TransferJobFilters): Promise<string[]> => {
  const response = await cbftpClient.get<string[]>('/cbftp/transferjobs', { params: filters });
  return response.data;
};

export const getTransferJob = async (nameOrId: string, byId: boolean = false): Promise<CbftpTransferJob> => {
  const response = await cbftpClient.get<CbftpTransferJob>(`/cbftp/transferjobs/${nameOrId}`, {
    params: { id: byId }
  });
  const data = response.data || ({} as CbftpTransferJob);
  if (!data.name) {
    return { ...data, name: nameOrId };
  }
  return data;
};

export const startTransferJob = async (job: StartTransferJobRequest): Promise<void> => {
  await cbftpClient.post('/cbftp/transferjobs', job);
};

export const resetTransferJob = async (nameOrId: string, byId: boolean = false): Promise<void> => {
  await cbftpClient.post(`/cbftp/transferjobs/${nameOrId}/reset`, {}, {
    params: { id: byId }
  });
};

export const abortTransferJob = async (nameOrId: string, byId: boolean = false): Promise<void> => {
  await cbftpClient.post(`/cbftp/transferjobs/${nameOrId}/abort`, {}, {
    params: { id: byId }
  });
};

// Raw Commands
export const sendRawCommand = async (request: RawCommandRequest): Promise<RawCommandResponse> => {
  const response = await cbftpClient.post<RawCommandResponse>('/cbftp/raw', request);
  return response.data;
};

export const getRawCommandResult = async (id: number): Promise<RawCommandResponse> => {
  const response = await cbftpClient.get<RawCommandResponse>(`/cbftp/raw/${id}`);
  return response.data;
};

// Info
export const getInfo = async (): Promise<CbftpInfo> => {
  const response = await cbftpClient.get('/cbftp/info');
  const info = response.data as any;
  if (!info || typeof info !== 'object') {
    return {};
  }

  const buildInfo = info.build_info ?? {};
  const stats = info.stats ?? {};

  return {
    version: buildInfo.version_tag || buildInfo.distribution_tag,
    build_date: buildInfo.compile_time,
    uptime: info.uptime ?? stats.uptime,
    num_sites: stats.num_sites ?? stats.sites,
    num_spread_jobs: stats.spread_jobs_all ?? stats.spread_jobs,
    num_transfer_jobs: stats.transfer_jobs_all ?? stats.transfer_jobs,
    active_spread_jobs: stats.spread_jobs_active ?? stats.spread_jobs_running,
    active_transfer_jobs: stats.transfer_jobs_active ?? stats.transfer_jobs_running,
  };
};
