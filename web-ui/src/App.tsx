import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom';
import { Layout } from './components/Layout';
import { Dashboard } from './pages/Dashboard';
import { SitesList } from './pages/SitesList';
import { Routes as RoutesPage } from './pages/Routes';
import { Sections } from './pages/Sections';
import { IRC } from './pages/IRC';
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
          <Route path="routes" element={<RoutesPage />} />
          <Route path="sections" element={<Sections />} />
          <Route path="irc" element={<IRC />} />
        </Route>
        <Route path="/index.html" element={<Navigate to="/" replace />} />
      </Routes>
    </BrowserRouter>
  );
}

export default App;