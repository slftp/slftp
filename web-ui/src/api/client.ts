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
