import { Suspense, lazy, type ReactNode } from 'react';
import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom';
import { Layout } from './components/Layout';
import { Center, Loader } from '@mantine/core';
import { isAuthenticated } from './api/client';

const Dashboard = lazy(() => import('./pages/Dashboard').then((m) => ({ default: m.Dashboard })));
const SitesList = lazy(() => import('./pages/SitesList').then((m) => ({ default: m.SitesList })));
const SiteSettings = lazy(() => import('./pages/SiteSettings').then((m) => ({ default: m.SiteSettings })));
const RoutesPage = lazy(() => import('./pages/Routes').then((m) => ({ default: m.Routes })));
const Rules = lazy(() => import('./pages/Rules').then((m) => ({ default: m.Rules })));
const Sections = lazy(() => import('./pages/Sections').then((m) => ({ default: m.Sections })));
const IRC = lazy(() => import('./pages/IRC').then((m) => ({ default: m.IRC })));
const Stats = lazy(() => import('./pages/Stats').then((m) => ({ default: m.Stats })));
const Logs = lazy(() => import('./pages/Logs').then((m) => ({ default: m.Logs })));
const Issues = lazy(() => import('./pages/Issues').then((m) => ({ default: m.Issues })));
const Tools = lazy(() => import('./pages/Tools').then((m) => ({ default: m.Tools })));
const FileBrowser = lazy(() => import('./pages/FileBrowser').then((m) => ({ default: m.FileBrowser })));
const Pre = lazy(() => import('./pages/Pre').then((m) => ({ default: m.Pre })));
const Races = lazy(() => import('./pages/Races').then((m) => ({ default: m.Races })));
const Databases = lazy(() => import('./pages/Databases').then((m) => ({ default: m.Databases })));
const Help = lazy(() => import('./pages/Help').then((m) => ({ default: m.Help })));
const Cbftp = lazy(() => import('./pages/cbftp/Cbftp').then((m) => ({ default: m.Cbftp })));

const Login = lazy(() => import('./pages/Login'));

// Protected Route wrapper
const ProtectedRoute = ({ children }: { children: ReactNode }) => {
  if (!isAuthenticated()) {
    return <Navigate to="/login" replace />;
  }
  return <>{children}</>;
};

function RouteLoader() {
  return (
    <Center mih="40vh">
      <Loader size="lg" />
    </Center>
  );
}

function App() {
  return (
    <BrowserRouter>
      <Suspense fallback={<RouteLoader />}>
        <Routes>
          <Route path="/login" element={<Login />} />
          <Route path="/" element={
            <ProtectedRoute>
              <Layout />
            </ProtectedRoute>
          }>
            <Route index element={<Dashboard />} />
            <Route path="sites" element={<SitesList />} />
            <Route path="sites/:siteName" element={<SiteSettings />} />
            <Route path="browser" element={<FileBrowser />} />
            <Route path="cbftp" element={<Cbftp />} />
            <Route path="pre" element={<Pre />} />
            <Route path="logs" element={<Logs />} />
            <Route path="races" element={<Races />} />
            <Route path="races/:releaseName" element={<Races />} />

            <Route path="issues" element={<Issues />} />
            <Route path="databases" element={<Databases />} />
            <Route path="routes" element={<RoutesPage />} />
            <Route path="rules" element={<Rules />} />
            <Route path="tools" element={<Tools />} />
            <Route path="help" element={<Help />} />
            <Route path="sections" element={<Sections />} />
            <Route path="irc" element={<IRC />} />
            <Route path="stats" element={<Stats />} />
          </Route>
          <Route path="/index.html" element={<Navigate to="/" replace />} />
        </Routes>
      </Suspense>
    </BrowserRouter>
  );
}

export default App;
