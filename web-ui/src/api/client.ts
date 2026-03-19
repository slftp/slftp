import axios from 'axios';

// Configurable base URL + token via Vite env
const API_BASE_URL = import.meta.env.VITE_API_BASE_URL || '/api';

// Get token from localStorage only
const getApiToken = (): string | null => {
  return localStorage.getItem('apiToken');
};

export const apiClient = axios.create({
  baseURL: API_BASE_URL,
  headers: {
    'Content-Type': 'application/json',
  },
});

// Request Interceptor: Log outgoing requests
apiClient.interceptors.request.use(request => {
  console.log(`[API Request] ${request.method?.toUpperCase()} ${request.url}`, request.params || request.data || '');
  return request;
});

// Response Interceptor: Log incoming responses
apiClient.interceptors.response.use(
  response => {
    console.log(`[API Response] ${response.status} ${response.config.url}`, response.data);
    return response;
  },
  error => {
    console.error(`[API Error] ${error.response?.status} ${error.config?.url}`, error.response?.data || error.message);
    return Promise.reject(error);
  }
);

// Set initial token
const token = getApiToken();
if (token) {
  apiClient.defaults.headers.common.Authorization = `Bearer ${token}`;
}

// Helper function to update token (called after login)
export const setApiToken = (token: string) => {
  localStorage.setItem('apiToken', token);
  apiClient.defaults.headers.common.Authorization = `Bearer ${token}`;
};

// Helper function to clear token (called on logout)
export const clearApiToken = () => {
  localStorage.removeItem('apiToken');
  delete apiClient.defaults.headers.common.Authorization;
};

// Helper function to check if user is authenticated
export const isAuthenticated = (): boolean => {
  return localStorage.getItem('apiToken') !== null;
};

// Types for API responses

export interface SystemStatus {
  Version: string;
  Uptime: number;
  SitesCount: number;
  SitesUp: number;
  SitesDown: number;
  QueueSize: number;
  QueueSizeMax: number;
  ActiveTasks: number;
  LoadAvgCurrent1: number;
  LoadAvgCurrent5: number;
  LoadAvgCurrent15: number;
  LoadAvgPeak1: number;
  LoadAvgPeak5: number;
  LoadAvgPeak15: number;
  LoadAvgAvailable: boolean;
  CpuLoadCurrent: number;
  CpuLoadMax: number;
  CpuLoadAvailable: boolean;
  PerformanceLevel: number;
  DirlistPerSecond: number;
  DirlistPerSecondMax: number;
}

export interface Bnc {
  host: string;
  port: number;
}

export interface Site {
  name: string;
  status: 'UP' | 'DOWN' | 'DOWN_BY_USER' | 'UNKNOWN';
  slots: number;
  freeslots: number;
  ircnick?: string;
  max_dn?: number;
  max_up?: number;
  max_pre_dn?: number;
  num_dn?: number;
  num_up?: number;
  permdown?: boolean;
  autologin?: boolean;
  autorules_interval?: number;
  destination_queue_limit?: number;
}

export interface SitesListResponse {
  Total: number;
  Up: number;
  Down: number;
  Sites: string; // JSON String inside JSON response (based on backend implementation)
}

export interface RouteEntry {
  dest: string;
  speed: number;
  affil_only?: boolean;
  no_affil?: boolean;
  locked?: boolean;
}

export interface IssuesSummary {
  WindowSeconds: number;
  Total: number;
  Skip: number;
  DontMatch: number;
  MissingSection: number;
  Nuke: number;
}

export interface Issue {
  Id: number;
  TsUnix: number;
  IssueType: string;
  Section: string;
  ReleaseName: string;
  SiteName: string;
  Reason: string;
  KbEvent: string;
}

export interface SimulatorSiteResult {
  Sitename: string;
  Section: string;
  Allowed: boolean;
  Reason: string;
  RuleAction: string;
  IsAffil: boolean;
  HasSection: boolean;
  SiteDown: boolean;
  PretimeOk: boolean;
}

export interface SimulatorRouteResult {
  SourceSite: string;
  DestinationSite: string;
  Rank: number;
  RouteWeight: number;
}

export interface SimulatorResult {
  Releasename: string;
  Section: string;
  TotalSites: number;
  AllowedSites: number;
  ErrorMessage: string;
  Sites: SimulatorSiteResult[] | string;
  Routes: SimulatorRouteResult[] | string;
}

export interface FileEntry {
  name: string;
  size: number;
  date: string;
  user: string;
  group: string;
  perm: string;
  is_dir: boolean;
  is_symlink?: boolean;
  symlink_target?: string;
}

export interface BrowserResponse {
  status: 'pending' | 'ready' | 'error';
  files?: FileEntry[];
  message?: string;
  path?: string;
  timestamp?: number;
}

export interface SlotRuntime {
  slot: number;
  name: string;
  status: string;
  task: string;
  task_uid?: number;
  action?: string;
  uploading: boolean;
  downloading: boolean;
  direction: string;
  last_io_sec: number;
  last_task_sec: number;
  last_non_idle_task_sec: number;
  response_code: number;
}

export interface SiteSlotsRuntime {
  site: string;
  site_status: string;
  locked: boolean;
  slots_total: number;
  slots_free: number;
  active_slots: number;
  slots: SlotRuntime[];
}

export const fetchBrowserPath = async (site: string, path: string, refresh: boolean = false): Promise<BrowserResponse> => {
  const response = await apiClient.post<any>('/ApiBrowserService/GetPath', {
    SiteName: site,
    Path: path,
    ForceRefresh: refresh
  });
  
  if (response.data && response.data.result && Array.isArray(response.data.result)) {
    return response.data.result[0];
  }
  return response.data;
};

export const fetchConfigList = async (): Promise<string[]> => {
  const response = await apiClient.post<any>('/ApiConfigService/GetConfigList');
  if (response.data && response.data.result && Array.isArray(response.data.result)) {
    return response.data.result[0];
  }
  return [];
};

export const fetchConfigContent = async (filename: string): Promise<string> => {
  const response = await apiClient.post<any>('/ApiConfigService/GetConfigContent', {
    Filename: filename
  });
  if (response.data && response.data.result && Array.isArray(response.data.result)) {
    return response.data.result[0];
  }
  return '';
};

export const saveConfigContent = async (filename: string, content: string): Promise<boolean> => {
  const response = await apiClient.post<any>('/ApiConfigService/SaveConfigContent', {
    Filename: filename,
    Content: content
  });
  if (response.data && response.data.result && Array.isArray(response.data.result)) {
    return response.data.result[0];
  }
  return false;
};

export const reloadConfig = async (filename: string): Promise<boolean> => {
  const response = await apiClient.post<any>('/ApiConfigService/ReloadConfig', {
    Filename: filename
  });
  if (response.data && response.data.result && Array.isArray(response.data.result)) {
    return response.data.result[0];
  }
  return false;
};

export interface SlotHistory {
  seq: number;
  full: boolean;
  lines: string[];
}

export const fetchSlotsRuntime = async (siteName: string = ''): Promise<SiteSlotsRuntime[]> => {
  const response = await apiClient.post<any>('/ApiSitesService/GetSlotsRuntime', {
    SiteName: siteName
  });

  const payload = response?.data?.result && Array.isArray(response.data.result)
    ? response.data.result[0]
    : response.data;

  if (Array.isArray(payload)) {
    return payload as SiteSlotsRuntime[];
  }

  if (typeof payload === 'string') {
    try {
      const parsed = JSON.parse(payload);
      return Array.isArray(parsed) ? (parsed as SiteSlotsRuntime[]) : [];
    } catch {
      return [];
    }
  }

  return [];
};

export const setSlotMonitoring = async (site: string, slot: number, enabled: boolean): Promise<boolean> => {
  const action = enabled ? 'enable' : 'disable';
  try {
    const response = await apiClient.post(`/monitoring/${action}/${site}/${slot}`);
    return response.status === 200;
  } catch {
    return false;
  }
};

// Section Tester API
export interface SectionTestItem {
  name: string;
  section: string;
}

export interface SectionTestResult {
  releaseName: string;
  expectedSection: string;
  detectedSection: string;
  matched: boolean;
}

export interface SectionTestStats {
  total: number;
  matched: number;
  failed: number;
}

export interface SectionTestResponse {
  success: boolean;
  error?: string;
  results: SectionTestResult[];
  stats: SectionTestStats;
}

export const batchTestSections = async (items: SectionTestItem[]): Promise<SectionTestResponse> => {
  const response = await apiClient.post<any>('/ApiSimulatorService/BatchTestSections', {
    ReleasesJson: JSON.stringify(items)
  });
  if (response.data?.result && Array.isArray(response.data.result)) {
    return response.data.result[0] as SectionTestResponse;
  }
  return response.data as SectionTestResponse;
};

export const saveSectionTesterData = async (content: string): Promise<boolean> => {
  const response = await apiClient.post<any>('/ApiSimulatorService/SaveSectionTesterData', {
    Content: content
  });
  if (response.data?.result && Array.isArray(response.data.result)) {
    return response.data.result[0] as boolean;
  }
  return response.data as boolean;
};

export const loadSectionTesterData = async (): Promise<string> => {
  const response = await apiClient.post<any>('/ApiSimulatorService/LoadSectionTesterData');
  if (response.data?.result && Array.isArray(response.data.result)) {
    return response.data.result[0] as string;
  }
  return response.data || '';
};
