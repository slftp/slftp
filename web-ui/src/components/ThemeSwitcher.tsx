import { Menu, ActionIcon, Tooltip } from '@mantine/core';
import { IconPalette } from '@tabler/icons-react';
import { useTheme, type ThemeType } from '../context/ThemeContext';

export function ThemeSwitcher() {
  const { currentTheme, setTheme, themeLabel, availableThemes } = useTheme();

  return (
    <Menu position="bottom-end" offset={4} withArrow>
      <Menu.Target>
        <Tooltip label={`Theme: ${themeLabel}`} position="bottom">
          <ActionIcon
            variant="light"
            size="md"
            radius="md"
            style={{
              background: 'rgba(255, 255, 255, 0.08)',
              border: '1px solid rgba(255, 255, 255, 0.1)',
              color: '#a3aed0',
              transition: 'all 0.2s ease',
            }}
            styles={{
              root: {
                '&:hover': {
                  background: 'rgba(255, 255, 255, 0.12)',
                  color: '#fff',
                },
              },
            }}
          >
            <IconPalette size="1.1rem" />
          </ActionIcon>
        </Tooltip>
      </Menu.Target>

      <Menu.Dropdown
        style={{
          background: 'var(--gradient-card)',
          backdropFilter: 'blur(20px)',
          border: '1px solid var(--border)',
          boxShadow: 'var(--shadow-lg)',
        }}
      >
        <Menu.Label style={{ color: 'var(--text-muted)', fontSize: '0.7rem' }}>
          Select Theme
        </Menu.Label>
        {availableThemes.map((theme) => (
          <Menu.Item
            key={theme.value}
            leftSection={<span style={{ fontSize: '1rem' }}>{theme.icon}</span>}
            onClick={() => setTheme(theme.value as ThemeType)}
            style={{
              background: currentTheme === theme.value 
                ? 'var(--nav-active-bg)' 
                : 'transparent',
              border: currentTheme === theme.value 
                ? '1px solid var(--nav-active-border)' 
                : '1px solid transparent',
              color: currentTheme === theme.value 
                ? 'var(--text-primary)' 
                : 'var(--text-secondary)',
              fontWeight: currentTheme === theme.value ? 600 : 400,
              borderRadius: '6px',
              margin: '2px 4px',
            }}
          >
            {theme.label}
          </Menu.Item>
        ))}
      </Menu.Dropdown>
    </Menu>
  );
}

// Simplified toggle button for quick switching between two themes
export function ThemeToggle() {
  const { currentTheme, toggleTheme } = useTheme();

  return (
    <Tooltip label={`Switch to ${currentTheme === 'vision' ? 'Minimal' : 'Vision'} theme`} position="bottom">
      <ActionIcon
        variant="light"
        size="md"
        radius="md"
        onClick={toggleTheme}
        style={{
          background: 'rgba(255, 255, 255, 0.08)',
          border: '1px solid rgba(255, 255, 255, 0.1)',
          color: '#a3aed0',
          transition: 'all 0.2s ease',
        }}
        styles={{
          root: {
            '&:hover': {
              background: 'rgba(255, 255, 255, 0.12)',
              color: '#fff',
            },
          },
        }}
      >
        {currentTheme === 'vision' ? '🎨' : '⬜'}
      </ActionIcon>
    </Tooltip>
  );
}
