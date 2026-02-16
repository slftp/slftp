import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { MantineProvider, createTheme } from '@mantine/core';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { Notifications } from '@mantine/notifications';
import App from './App.tsx';
import { ThemeProvider, useTheme } from './context/ThemeContext';
import '@mantine/core/styles.css';
import '@mantine/notifications/styles.css';
import './index.css';

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      refetchOnWindowFocus: false,
      retry: 2,
      staleTime: 30000,
    },
  },
});

// Inner component that has access to theme context
function ThemedApp() {
  const { theme } = useTheme();
  const mantineTheme = createTheme(theme);

  // Dynamic notification styles based on current theme
  const getNotificationStyles = () => {
    const isMinimal = theme.primaryColor === 'brand' && theme.colors?.brand?.[5] === '#4b5563';
    
    if (isMinimal) {
      return {
        notification: {
          background: 'rgba(17, 24, 39, 0.98)',
          backdropFilter: 'blur(20px)',
          border: '1px solid rgba(75, 85, 99, 0.5)',
          boxShadow: '0 4px 12px rgba(0, 0, 0, 0.3)',
        },
      };
    }
    
    return {
      notification: {
        background: 'linear-gradient(127.09deg, rgba(30, 41, 59, 0.98) 19.41%, rgba(40, 49, 71, 0.95) 76.65%)',
        backdropFilter: 'blur(20px)',
        border: '1px solid rgba(255, 255, 255, 0.12)',
        boxShadow: '0 8px 32px rgba(0, 0, 0, 0.4)',
      },
    };
  };

  return (
    <MantineProvider theme={mantineTheme} defaultColorScheme="dark">
      <Notifications 
        position="top-right"
        styles={getNotificationStyles()}
      />
      <QueryClientProvider client={queryClient}>
        <App />
      </QueryClientProvider>
    </MantineProvider>
  );
}

// Root component with providers
function Root() {
  return (
    <StrictMode>
      <ThemeProvider defaultTheme="vision">
        <ThemedApp />
      </ThemeProvider>
    </StrictMode>
  );
}

createRoot(document.getElementById('root')!).render(<Root />);
