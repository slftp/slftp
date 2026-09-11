import { AppShell, Burger, Group, NavLink, Title, Avatar, Box, ScrollArea, Divider } from '@mantine/core';
import { useDisclosure } from '@mantine/hooks';
import { useQuery } from '@tanstack/react-query';
import { 
  IconDashboard, 
  IconServer, 
  IconRoute, 
  IconFolders,
  IconMessage2,
  IconLogout,
  IconFilter,
  IconChartBar,
  IconFileText,
  IconAlertTriangle,
  IconFlask,
  IconFolder,
  IconDatabase,
  IconTransfer,
  IconActivity,
  IconBrandGitlab,
} from '@tabler/icons-react';
import { useNavigate, useLocation, Outlet } from 'react-router-dom';
import { clearApiToken } from '../api/client';
import { isCbftpEnabled } from '../api/cbftpClient';
import { AutoSwitch } from './AutoSwitch';
import { ThemeSwitcher } from './ThemeSwitcher';
import { useState, useEffect, type ReactNode } from 'react';

interface NavItem {
  icon: typeof IconDashboard;
  label: string;
  to: string;
  color: string;
}

export function Layout() {
  const [opened, { toggle }] = useDisclosure();
  const navigate = useNavigate();
  const location = useLocation();
  const [scrolled, setScrolled] = useState(false);

  const { data: cbftpEnabled } = useQuery({
    queryKey: ['cbftp-enabled'],
    queryFn: isCbftpEnabled,
    staleTime: 5 * 60 * 1000,
    retry: false,
  });

  useEffect(() => {
    let ticking = false;
    let rafId = 0;
    let isMounted = true;

    const handleScroll = () => {
      if (ticking || !isMounted) return;
      ticking = true;

      rafId = window.requestAnimationFrame(() => {
        if (!isMounted) return;
        const isScrolled = window.scrollY > 20;
        setScrolled((prev) => (prev === isScrolled ? prev : isScrolled));
        ticking = false;
      });
    };

    window.addEventListener('scroll', handleScroll, { passive: true });
    handleScroll();

    return () => {
      isMounted = false;
      if (rafId !== 0) {
        window.cancelAnimationFrame(rafId);
      }
      window.removeEventListener('scroll', handleScroll);
    };
  }, []);

  const navItemsBase: NavItem[] = [
    { icon: IconDashboard, label: 'Dashboard', to: '/', color: '#4318ff' },
    { icon: IconActivity, label: 'Races', to: '/races', color: '#00ff88' },
    { icon: IconServer, label: 'Sites', to: '/sites', color: '#00d4ff' },
    { icon: IconRoute, label: 'Routes', to: '/routes', color: '#7b2cbf' },
    { icon: IconFilter, label: 'Rules', to: '/rules', color: '#ff6b6b' },
    { icon: IconFolders, label: 'Sections', to: '/sections', color: '#00ff88' },
    { icon: IconFolder, label: 'Browser', to: '/browser', color: '#feca57' },
    { icon: IconMessage2, label: 'IRC', to: '/irc', color: '#9b59b6' },
    { icon: IconChartBar, label: 'Stats', to: '/stats', color: '#e67e22' },
    { icon: IconDatabase, label: 'Databases', to: '/databases', color: '#3498db' },
    { icon: IconFileText, label: 'Logs', to: '/logs', color: '#95a5a6' },
    { icon: IconAlertTriangle, label: 'Issues', to: '/issues', color: '#e74c3c' },
    { icon: IconFlask, label: 'Tools', to: '/tools', color: '#1abc9c' },
    { icon: IconBrandGitlab, label: 'GitLab', to: '/gitlab', color: '#fc6d26' },
    ...(cbftpEnabled ? [{ icon: IconTransfer, label: 'cbftp', to: '/cbftp', color: '#00d4ff' }] : []),
  ];

  const dashboardItem = navItemsBase.find((item) => item.to === '/');
  const sortedNavItems = navItemsBase
    .filter((item) => item.to !== '/')
    .slice()
    .sort((a, b) => a.label.localeCompare(b.label, 'en', { sensitivity: 'base' }));
  const navItems = dashboardItem ? [dashboardItem, ...sortedNavItems] : sortedNavItems;

  const handleLogout = () => {
    clearApiToken();
    navigate('/login');
  };

  const isActive = (to: string) => {
    if (to === '/') {
      return location.pathname === '/';
    }
    return location.pathname.startsWith(to);
  };

  return (
    <AppShell
      header={{ height: 56 }}
      navbar={{
        width: 200,
        breakpoint: 'sm',
        collapsed: { mobile: !opened },
      }}
      padding="md"
    >
      {/* Header */}
      <AppShell.Header
        style={{
          background: scrolled ? 'var(--gradient-header)' : 'var(--gradient-main)',
          backdropFilter: 'blur(10px)',
          borderBottom: '1px solid var(--border)',
          boxShadow: scrolled ? 'var(--shadow)' : 'none',
          transition: 'background-color 0.2s ease, box-shadow 0.2s ease',
        }}
      >
        <Group h="100%" px="md" justify="space-between">
          <Group gap="sm">
            <Burger 
              opened={opened} 
              onClick={toggle} 
              hiddenFrom="sm" 
              size="sm"
              color="var(--burger-color)"
            />
            <Group gap="xs">
              <Avatar 
                src="/slftp.png" 
                alt="Soulless FTP" 
                size={28} 
                radius="xl"
                style={{
                  boxShadow: 'var(--shadow-glow)',
                }}
              />
              <Title 
                order={5}
                style={{
                  background: 'linear-gradient(135deg, var(--text-gradient-start) 0%, var(--text-gradient-end) 100%)',
                  WebkitBackgroundClip: 'text',
                  WebkitTextFillColor: 'transparent',
                  margin: 0,
                  lineHeight: 1.2,
                }}
              >
                Soulless FTP
              </Title>
            </Group>
          </Group>

          <Group gap="sm">
            <ThemeSwitcher />
            <AutoSwitch />
          </Group>
        </Group>
      </AppShell.Header>

      {/* Sidebar */}
      <AppShell.Navbar
        style={{
          background: 'var(--gradient-main)',
          backdropFilter: 'blur(10px)',
          borderRight: '1px solid var(--border)',
        }}
      >
        <ScrollArea h="100%" type="never" scrollbarSize={0}>
          <Box p="xs">
            <Stack gap={2}>
              {navItems.map((item, index) => {
                const active = isActive(item.to);
                return (
                  <NavLink
                    key={item.label}
                    active={active}
                    label={item.label}
                    leftSection={
                      <item.icon 
                        size="1rem" 
                        stroke={active ? 2.5 : 1.5}
                        color={active ? item.color : 'var(--nav-label-inactive)'}
                      />
                    }
                    onClick={() => {
                      navigate(item.to);
                      if (window.innerWidth < 768) toggle();
                    }}
                    styles={{
                      root: {
                        order: index,
                        borderRadius: '8px',
                        padding: '8px 10px',
                        margin: '1px 0',
                        background: active 
                          ? `linear-gradient(135deg, ${item.color}20 0%, ${item.color}10 100%)`
                          : 'transparent',
                        border: active ? `1px solid ${item.color}40` : '1px solid transparent',
                        boxShadow: active ? `0 0 8px ${item.color}12` : 'none',
                        transition: 'background-color 0.2s ease, border-color 0.2s ease, box-shadow 0.2s ease',
                        '&:hover': {
                          background: active 
                            ? `linear-gradient(135deg, ${item.color}25 0%, ${item.color}15 100%)`
                            : 'var(--nav-hover-bg)',
                        },
                      },
                      label: {
                        color: active ? 'var(--nav-label-active)' : 'var(--nav-label-inactive)',
                        fontWeight: active ? 600 : 500,
                        fontSize: '0.8rem',
                      },
                    }}
                  />
                );
              })}
            </Stack>

            {/* Logout Button */}
            <Box mt="sm">
              <Divider my="sm" style={{ borderColor: 'rgba(255, 255, 255, 0.04)' }} />
              <NavLink
                label="Logout"
                leftSection={<IconLogout size="1rem" stroke={1.5} color="#ff4d4d" />}
                onClick={handleLogout}
                styles={{
                  root: {
                    borderRadius: '8px',
                    padding: '8px 10px',
                    background: 'rgba(255, 77, 77, 0.08)',
                    border: '1px solid rgba(255, 77, 77, 0.15)',
                    transition: 'background-color 0.2s ease, border-color 0.2s ease',
                    '&:hover': {
                      background: 'rgba(255, 77, 77, 0.15)',
                      borderColor: 'rgba(255, 77, 77, 0.25)',
                    },
                  },
                  label: {
                    color: '#ff6b6b',
                    fontWeight: 600,
                    fontSize: '0.8rem',
                  },
                }}
              />
            </Box>
          </Box>
        </ScrollArea>
      </AppShell.Navbar>

      {/* Main Content */}
      <AppShell.Main
        style={{
          background: 'transparent',
          paddingTop: '76px',
        }}
      >
        <Outlet />
      </AppShell.Main>
    </AppShell>
  );
}

function Stack({ children, gap = 0 }: { children: ReactNode; gap?: number }) {
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: `${gap}px` }}>
      {children}
    </div>
  );
}
