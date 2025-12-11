import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom';
import { Layout } from './components/Layout';
import { Dashboard } from './pages/Dashboard';
import { SitesList } from './pages/SitesList';
import { Routes as RoutesPage } from './pages/Routes';
import { Sections } from './pages/Sections';

function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Layout />}>
          <Route index element={<Dashboard />} />
          <Route path="sites" element={<SitesList />} />
          <Route path="routes" element={<RoutesPage />} />
          <Route path="sections" element={<Sections />} />
        </Route>
        <Route path="/index.html" element={<Navigate to="/" replace />} />
      </Routes>
    </BrowserRouter>
  );
}

export default App;