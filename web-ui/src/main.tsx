import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { MantineProvider, createTheme } from '@mantine/core';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { Notifications } from '@mantine/notifications';
import App from './App.tsx';
import '@mantine/core/styles.css';
import '@mantine/notifications/styles.css';
import './index.css';

// Vision UI Theme Configuration - Aufgehellt
const visionTheme = createTheme({
  primaryColor: 'brand',
  colors: {
    brand: [
      '#e0e7ff', // 0 - lightest
      '#c7d2fe', // 1
      '#a5b4fc', // 2
      '#818cf8', // 3
      '#6366f1', // 4
      '#4f46e5', // 5 - primary (indigo-600)
      '#4338ca', // 6
      '#3730a3', // 7
      '#312e81', // 8
      '#1e1b4b', // 9 - darkest
    ],
    dark: [
      '#f8fafc',
      '#f1f5f9',
      '#e2e8f0',
      '#cbd5e1',
      '#94a3b8',
      '#64748b',
      '#475569',
      '#1e293b',
      '#0f172a',
      '#020617',
    ],
    success: [
      '#ecfdf5',
      '#d1fae5',
      '#a7f3d0',
      '#6ee7b7',
      '#34d399',
      '#10b981',
      '#059669',
      '#047857',
      '#065f46',
      '#064e3b',
    ],
    warning: [
      '#fffbeb',
      '#fef3c7',
      '#fde68a',
      '#fcd34d',
      '#fbbf24',
      '#f59e0b',
      '#d97706',
      '#b45309',
      '#92400e',
      '#78350f',
    ],
    danger: [
      '#fef2f2',
      '#fee2e2',
      '#fecaca',
      '#fca5a5',
      '#f87171',
      '#ef4444',
      '#dc2626',
      '#b91c1c',
      '#991b1b',
      '#7f1d1d',
    ],
  },
  primaryShade: 5,
  defaultRadius: 'md',
  fontFamily: "'Inter', 'DM Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
  fontFamilyMonospace: "'JetBrains Mono', 'Fira Code', 'Monaco', monospace",
  headings: {
    fontFamily: "'Inter', 'DM Sans', sans-serif",
    fontWeight: '700',
    sizes: {
      h1: { fontSize: '2.5rem', lineHeight: '1.2' },
      h2: { fontSize: '2rem', lineHeight: '1.3' },
      h3: { fontSize: '1.5rem', lineHeight: '1.4' },
      h4: { fontSize: '1.25rem', lineHeight: '1.5' },
      h5: { fontSize: '1rem', lineHeight: '1.5' },
      h6: { fontSize: '0.875rem', lineHeight: '1.5' },
    },
  },
  fontSizes: {
    xs: '0.75rem',
    sm: '0.875rem',
    md: '1rem',
    lg: '1.125rem',
    xl: '1.25rem',
  },
  spacing: {
    xs: '0.5rem',
    sm: '0.75rem',
    md: '1rem',
    lg: '1.5rem',
    xl: '2rem',
  },
  radius: {
    xs: '4px',
    sm: '8px',
    md: '16px',
    lg: '24px',
    xl: '32px',
  },
  shadows: {
    xs: '0 2px 8px rgba(0, 0, 0, 0.2)',
    sm: '0 4px 12px rgba(0, 0, 0, 0.25)',
    md: '0 8px 32px rgba(0, 0, 0, 0.3)',
    lg: '0 16px 48px rgba(0, 0, 0, 0.35)',
    xl: '0 24px 64px rgba(0, 0, 0, 0.4)',
  },
  components: {
    Button: {
      defaultProps: {
        radius: 'md',
      },
      styles: {
        root: {
          fontWeight: 600,
          transition: 'all 0.3s ease',
          '&:hover': {
            transform: 'translateY(-2px)',
          },
        },
      },
    },
    Card: {
      defaultProps: {
        radius: 'lg',
        padding: 'lg',
      },
      styles: {
        root: {
          background: 'linear-gradient(127.09deg, rgba(30, 41, 59, 0.9) 19.41%, rgba(40, 49, 71, 0.75) 76.65%)',
          backdropFilter: 'blur(20px)',
          border: '1px solid rgba(255, 255, 255, 0.1)',
          boxShadow: '0 8px 32px rgba(0, 0, 0, 0.3), 0 0 60px rgba(79, 70, 229, 0.1)',
          transition: 'all 0.3s ease',
          '&:hover': {
            borderColor: 'rgba(255, 255, 255, 0.15)',
            boxShadow: '0 12px 40px rgba(0, 0, 0, 0.4), 0 0 80px rgba(79, 70, 229, 0.15)',
          },
        },
      },
    },
    Badge: {
      styles: {
        root: {
          fontWeight: 600,
          letterSpacing: '0.02em',
        },
      },
    },
    Modal: {
      styles: {
        content: {
          background: 'linear-gradient(127.09deg, rgba(30, 41, 59, 0.98) 19.41%, rgba(40, 49, 71, 0.9) 76.65%)',
          backdropFilter: 'blur(30px)',
          border: '1px solid rgba(255, 255, 255, 0.12)',
        },
        header: {
          background: 'transparent',
          borderBottom: '1px solid rgba(255, 255, 255, 0.1)',
        },
      },
    },
    Table: {
      styles: {
        table: {
          borderCollapse: 'separate',
          borderSpacing: '0',
        },
        thead: {
          background: 'rgba(30, 41, 59, 0.8)',
        },
        th: {
          color: '#94a3b8',
          fontWeight: 600,
          fontSize: '0.7rem',
          textTransform: 'uppercase',
          letterSpacing: '0.05em',
          padding: '12px 16px',
        },
        td: {
          borderBottom: '1px solid rgba(255, 255, 255, 0.06)',
          padding: '12px 16px',
        },
        tr: {
          transition: 'background 0.2s ease',
          '&:hover': {
            background: 'rgba(79, 70, 229, 0.06)',
          },
        },
        tbody: {
          '& tr:nth-of-type(odd)': {
            background: 'rgba(239, 68, 68, 0.5)',
          },
        },
      },
    },
    NavLink: {
      styles: {
        root: {
          borderRadius: '12px',
          margin: '4px 0',
          transition: 'all 0.3s ease',
          '&[data-active]': {
            background: 'linear-gradient(135deg, rgba(79, 70, 229, 0.2) 0%, rgba(129, 140, 248, 0.12) 100%)',
            border: '1px solid rgba(79, 70, 229, 0.35)',
            boxShadow: '0 0 20px rgba(79, 70, 229, 0.2)',
          },
          '&:hover': {
            background: 'rgba(79, 70, 229, 0.1)',
          },
        },
      },
    },
    Input: {
      styles: {
        input: {
          background: 'rgba(30, 41, 59, 0.6)',
          border: '1px solid rgba(255, 255, 255, 0.1)',
          transition: 'all 0.3s ease',
          '&:focus': {
            borderColor: '#818cf8',
            boxShadow: '0 0 0 3px rgba(79, 70, 229, 0.2)',
          },
        },
      },
    },
    Progress: {
      styles: {
        root: {
          background: 'rgba(255, 255, 255, 0.1)',
        },
        section: {
          transition: 'width 0.5s ease',
        },
      },
    },
  },
});

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      refetchOnWindowFocus: false,
      retry: 2,
      staleTime: 30000,
    },
  },
});

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <MantineProvider theme={visionTheme} defaultColorScheme="dark">
      <Notifications 
        position="top-right"
        styles={{
          notification: {
            background: 'linear-gradient(127.09deg, rgba(30, 41, 59, 0.98) 19.41%, rgba(40, 49, 71, 0.95) 76.65%)',
            backdropFilter: 'blur(20px)',
            border: '1px solid rgba(255, 255, 255, 0.12)',
            boxShadow: '0 8px 32px rgba(0, 0, 0, 0.4)',
          },
        }}
      />
      <QueryClientProvider client={queryClient}>
        <App />
      </QueryClientProvider>
    </MantineProvider>
  </StrictMode>
);
