import { createContext, useContext, useState, useEffect, useCallback, type ReactNode } from 'react';
import type { MantineThemeOverride } from '@mantine/core';
import { themes, themeLabels } from '../themes';

export type ThemeType = 'vision' | 'minimal';

interface ThemeContextValue {
  currentTheme: ThemeType;
  theme: MantineThemeOverride;
  setTheme: (theme: ThemeType) => void;
  toggleTheme: () => void;
  themeLabel: string;
  availableThemes: { value: ThemeType; label: string; icon: string }[];
}

const STORAGE_KEY = 'slftp-webui-theme';

const ThemeContext = createContext<ThemeContextValue | undefined>(undefined);

interface ThemeProviderProps {
  children: ReactNode;
  defaultTheme?: ThemeType;
}

export function ThemeProvider({ children, defaultTheme = 'vision' }: ThemeProviderProps) {
  const [currentTheme, setCurrentTheme] = useState<ThemeType>(() => {
    // Try to load from localStorage on initial render
    if (typeof window !== 'undefined') {
      const stored = localStorage.getItem(STORAGE_KEY) as ThemeType | null;
      if (stored && themes[stored]) {
        return stored;
      }
    }
    return defaultTheme;
  });

  // Persist theme to localStorage whenever it changes
  useEffect(() => {
    localStorage.setItem(STORAGE_KEY, currentTheme);
    // Update data-theme attribute on body for CSS selectors
    document.body.setAttribute('data-theme', currentTheme);
  }, [currentTheme]);

  // Initialize data-theme attribute on mount
  useEffect(() => {
    document.body.setAttribute('data-theme', currentTheme);
  }, []);

  const setTheme = useCallback((theme: ThemeType) => {
    if (themes[theme]) {
      setCurrentTheme(theme);
    }
  }, []);

  const toggleTheme = useCallback(() => {
    setCurrentTheme(prev => prev === 'vision' ? 'minimal' : 'vision');
  }, []);

  const theme = themes[currentTheme];
  const themeLabel = themeLabels[currentTheme];

  const availableThemes = Object.keys(themes).map((key) => ({
    value: key as ThemeType,
    label: themeLabels[key as ThemeType],
    icon: key === 'vision' ? '🎨' : '⬜',
  }));

  const value: ThemeContextValue = {
    currentTheme,
    theme,
    setTheme,
    toggleTheme,
    themeLabel,
    availableThemes,
  };

  return (
    <ThemeContext.Provider value={value}>
      {children}
    </ThemeContext.Provider>
  );
}

export function useTheme() {
  const context = useContext(ThemeContext);
  if (context === undefined) {
    throw new Error('useTheme must be used within a ThemeProvider');
  }
  return context;
}

// Hook for components that want to handle theme gracefully if outside provider
export function useThemeSafe(): ThemeContextValue | null {
  return useContext(ThemeContext) ?? null;
}
