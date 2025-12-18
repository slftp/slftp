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
  ActiveTasks: number;
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
}

export interface BrowserResponse {
  status: 'pending' | 'ready' | 'error';
  files?: FileEntry[];
  message?: string;
  path?: string;
  timestamp?: number;
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
