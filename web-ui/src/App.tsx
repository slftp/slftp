import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom';
import { Layout } from './components/Layout';
import { Dashboard } from './pages/Dashboard';
import { SitesList } from './pages/SitesList';
import { SiteSettings } from './pages/SiteSettings';
import { Routes as RoutesPage } from './pages/Routes';
import { Rules } from './pages/Rules';
import { Sections } from './pages/Sections';
import { IRC } from './pages/IRC';
import { Stats } from './pages/Stats';
import { Logs } from './pages/Logs';
import { Issues } from './pages/Issues';
import { Simulator } from './pages/Simulator';
import { FileBrowser } from './pages/FileBrowser';
import { Pre } from './pages/Pre';
import { Races } from './pages/Races';
import { Databases } from './pages/Databases';
import Login from './pages/Login';
import { isAuthenticated } from './api/client';

// Protected Route wrapper
const ProtectedRoute = ({ children }: { children: React.ReactNode }) => {
  if (!isAuthenticated()) {
    return <Navigate to="/login" replace />;
  }
  return <>{children}</>;
};

function App() {
  return (
    <BrowserRouter>
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
          <Route path="pre" element={<Pre />} />
          <Route path="logs" element={<Logs />} />
          <Route path="races" element={<Races />} />
          <Route path="issues" element={<Issues />} />
          <Route path="databases" element={<Databases />} />
          <Route path="routes" element={<RoutesPage />} />
          <Route path="rules" element={<Rules />} />
          <Route path="tools" element={<Simulator />} />
          <Route path="sections" element={<Sections />} />
          <Route path="irc" element={<IRC />} />
          <Route path="stats" element={<Stats />} />
        </Route>
        <Route path="/index.html" element={<Navigate to="/" replace />} />
      </Routes>
    </BrowserRouter>
  );
}

export default App;
