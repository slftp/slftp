import type { MantineThemeOverride } from '@mantine/core';

// ============================================================
// VISION THEME - Original Blue/Purple Theme
// ============================================================
export const visionTheme: MantineThemeOverride = {
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
            background: 'rgba(15, 23, 42, 0.6)',
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
};

// ============================================================
// MINIMAL GRAY THEME - Neutral Gray Alternative
// ============================================================
export const minimalGrayTheme: MantineThemeOverride = {
  primaryColor: 'brand',
  colors: {
    brand: [
      '#f3f4f6', // 0 - lightest
      '#e5e7eb', // 1
      '#d1d5db', // 2
      '#9ca3af', // 3
      '#6b7280', // 4
      '#4b5563', // 5 - primary (gray-600)
      '#374151', // 6
      '#1f2937', // 7
      '#111827', // 8
      '#030712', // 9 - darkest
    ],
    dark: [
      '#f9fafb',
      '#f3f4f6',
      '#e5e7eb',
      '#d1d5db',
      '#9ca3af',
      '#6b7280',
      '#4b5563',
      '#374151',
      '#1f2937',
      '#111827',
    ],
    success: [
      '#f0fdf4',
      '#dcfce7',
      '#bbf7d0',
      '#86efac',
      '#4ade80',
      '#22c55e',
      '#16a34a',
      '#15803d',
      '#166534',
      '#14532d',
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
    fontWeight: '600',
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
    md: '12px',
    lg: '16px',
    xl: '24px',
  },
  shadows: {
    xs: '0 1px 3px rgba(0, 0, 0, 0.1)',
    sm: '0 2px 6px rgba(0, 0, 0, 0.15)',
    md: '0 4px 12px rgba(0, 0, 0, 0.2)',
    lg: '0 8px 24px rgba(0, 0, 0, 0.25)',
    xl: '0 12px 36px rgba(0, 0, 0, 0.3)',
  },
  components: {
    Button: {
      defaultProps: {
        radius: 'md',
      },
      styles: {
        root: {
          fontWeight: 500,
          transition: 'all 0.2s ease',
          '&:hover': {
            transform: 'translateY(-1px)',
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
          background: 'rgba(31, 41, 55, 0.95)',
          backdropFilter: 'blur(10px)',
          border: '1px solid rgba(75, 85, 99, 0.4)',
          boxShadow: '0 4px 12px rgba(0, 0, 0, 0.2)',
          transition: 'all 0.2s ease',
          '&:hover': {
            borderColor: 'rgba(107, 114, 128, 0.6)',
            boxShadow: '0 6px 16px rgba(0, 0, 0, 0.25)',
          },
        },
      },
    },
    Badge: {
      styles: {
        root: {
          fontWeight: 500,
          letterSpacing: '0.01em',
        },
      },
    },
    Modal: {
      styles: {
        content: {
          background: 'rgba(17, 24, 39, 0.98)',
          backdropFilter: 'blur(20px)',
          border: '1px solid rgba(75, 85, 99, 0.5)',
        },
        header: {
          background: 'transparent',
          borderBottom: '1px solid rgba(75, 85, 99, 0.3)',
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
          background: 'rgba(31, 41, 55, 0.9)',
        },
        th: {
          color: '#9ca3af',
          fontWeight: 500,
          fontSize: '0.7rem',
          textTransform: 'uppercase',
          letterSpacing: '0.05em',
          padding: '12px 16px',
        },
        td: {
          borderBottom: '1px solid rgba(75, 85, 99, 0.3)',
          padding: '12px 16px',
        },
        tr: {
          transition: 'background 0.15s ease',
          '&:hover': {
            background: 'rgba(75, 85, 99, 0.15)',
          },
        },
        tbody: {
          '& tr:nth-of-type(odd)': {
            background: 'rgba(17, 24, 39, 0.5)',
          },
        },
      },
    },
    NavLink: {
      styles: {
        root: {
          borderRadius: '8px',
          margin: '2px 0',
          transition: 'all 0.2s ease',
          '&[data-active]': {
            background: 'rgba(75, 85, 99, 0.3)',
            border: '1px solid rgba(107, 114, 128, 0.5)',
            boxShadow: 'none',
          },
          '&:hover': {
            background: 'rgba(75, 85, 99, 0.2)',
          },
        },
      },
    },
    Input: {
      styles: {
        input: {
          background: 'rgba(17, 24, 39, 0.6)',
          border: '1px solid rgba(75, 85, 99, 0.4)',
          transition: 'all 0.2s ease',
          '&:focus': {
            borderColor: '#6b7280',
            boxShadow: '0 0 0 2px rgba(107, 114, 128, 0.2)',
          },
        },
      },
    },
    Progress: {
      styles: {
        root: {
          background: 'rgba(75, 85, 99, 0.3)',
        },
        section: {
          transition: 'width 0.5s ease',
        },
      },
    },
  },
};

// ============================================================
// LIGHT THEME - High Contrast Light Alternative
// ============================================================
export const lightTheme: MantineThemeOverride = {
  primaryColor: 'brand',
  colors: {
    brand: [
      '#eff6ff', // 0 - lightest
      '#dbeafe', // 1
      '#bfdbfe', // 2
      '#93c5fd', // 3
      '#60a5fa', // 4
      '#3b82f6', // 5 - primary (blue-600)
      '#2563eb', // 6
      '#1d4ed8', // 7
      '#1e40af', // 8
      '#1e3a8a', // 9 - darkest
    ],
    dark: [
      '#f9fafb',
      '#f3f4f6',
      '#e5e7eb',
      '#d1d5db',
      '#9ca3af',
      '#6b7280',
      '#4b5563',
      '#374151',
      '#1f2937',
      '#111827',
    ],
    success: [
      '#f0fdf4',
      '#dcfce7',
      '#bbf7d0',
      '#86efac',
      '#4ade80',
      '#22c55e',
      '#16a34a',
      '#15803d',
      '#166534',
      '#14532d',
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
  primaryShade: 6,
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
    md: '12px',
    lg: '16px',
    xl: '24px',
  },
  shadows: {
    xs: '0 1px 3px rgba(0, 0, 0, 0.1)',
    sm: '0 4px 6px rgba(0, 0, 0, 0.1)',
    md: '0 10px 15px rgba(0, 0, 0, 0.1)',
    lg: '0 20px 25px rgba(0, 0, 0, 0.1)',
    xl: '0 25px 50px rgba(0, 0, 0, 0.1)',
  },
  components: {
    Button: {
      defaultProps: {
        radius: 'md',
      },
      styles: {
        root: {
          fontWeight: 600,
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
          background: '#ffffff',
          border: '1px solid #e5e7eb',
          boxShadow: '0 1px 3px rgba(0, 0, 0, 0.1)',
          '&:hover': {
            borderColor: '#d1d5db',
            boxShadow: '0 4px 6px rgba(0, 0, 0, 0.1)',
          },
        },
      },
    },
    Badge: {
      styles: {
        root: {
          fontWeight: 600,
        },
      },
    },
    Modal: {
      styles: {
        content: {
          background: '#ffffff',
          border: '1px solid #e5e7eb',
        },
        header: {
          borderBottom: '1px solid #e5e7eb',
        },
      },
    },
    Table: {
      styles: {
        thead: {
          background: '#f9fafb',
        },
        th: {
          color: '#4b5563',
          fontWeight: 600,
          borderBottom: '2px solid #e5e7eb',
        },
        td: {
          borderBottom: '1px solid #f3f4f6',
        },
        tr: {
          '&:hover': {
            background: '#f9fafb',
          },
        },
        tbody: {
          '& tr:nth-of-type(odd)': {
            background: '#ffffff',
          },
          '& tr:nth-of-type(even)': {
            background: '#fafafa',
          },
        },
      },
    },
    NavLink: {
      styles: {
        root: {
          borderRadius: '8px',
          '&[data-active]': {
            background: '#eff6ff',
            color: '#1d4ed8',
            fontWeight: 600,
          },
        },
      },
    },
    Input: {
      styles: {
        input: {
          background: '#ffffff',
          border: '1px solid #d1d5db',
          color: '#111827',
          '&:focus': {
            borderColor: '#3b82f6',
          },
        },
      },
    },
    Progress: {
      styles: {
        root: {
          background: '#f3f4f6',
        },
      },
    },
  },
};

export type ThemeType = 'vision' | 'minimal' | 'light';

export const themes = {
  vision: visionTheme,
  minimal: minimalGrayTheme,
  light: lightTheme,
};

export const themeLabels: Record<ThemeType, string> = {
  vision: 'Vision (Blue/Purple)',
  minimal: 'Minimal (Gray)',
  light: 'Light (High Contrast)',
};

export const themeIcons: Record<ThemeType, string> = {
  vision: '🎨',
  minimal: '⬜',
  light: '☀️',
};
