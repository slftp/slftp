import { AppShell, Burger, Group, NavLink, Title, useMantineColorScheme, ActionIcon, Avatar, Tooltip } from '@mantine/core';
import { useDisclosure } from '@mantine/hooks';
import { IconDashboard, IconServer, IconMoon, IconSun, IconRoute, IconFolders, IconBrandHipchat, IconLogout, IconFilter, IconChartBar, IconFileText, IconAlertTriangle, IconFlask, IconFolder, IconBolt, IconTrophy, IconDatabase, IconHelpCircle } from '@tabler/icons-react';
import { useNavigate, useLocation, Outlet } from 'react-router-dom';
import { clearApiToken } from '../api/client';
import { AutoSwitch } from './AutoSwitch';

export function Layout() {
  const [opened, { toggle }] = useDisclosure();
  const navigate = useNavigate();
  const location = useLocation();
  const { colorScheme, toggleColorScheme } = useMantineColorScheme();

  interface NavLinkItem {
    icon: any;
    label: string;
    to: string;
    children?: { label: string; to: string }[];
  }

  const links: NavLinkItem[] = [
    { icon: IconDashboard, label: 'Dashboard', to: '/' },
    { icon: IconFolder, label: 'Browser', to: '/browser' },
    { icon: IconBolt, label: 'PRE', to: '/pre' },
    { icon: IconBrandHipchat, label: 'IRC', to: '/irc' },
    { icon: IconDatabase, label: 'Databases', to: '/databases' },
    { icon: IconAlertTriangle, label: 'Issues', to: '/issues' },
    { icon: IconFileText, label: 'Logs', to: '/logs' },
    { icon: IconTrophy, label: 'Races', to: '/races' },
    { icon: IconRoute, label: 'Routes', to: '/routes' },
    { icon: IconFilter, label: 'Rules', to: '/rules' },
    { icon: IconFlask, label: 'Tools', to: '/tools' },
    { icon: IconHelpCircle, label: 'Help', to: '/help' },
    { icon: IconFolders, label: 'Sections', to: '/sections' },
    { icon: IconServer, label: 'Sites Manager', to: '/sites' },
    { icon: IconChartBar, label: 'Stats', to: '/stats' },
  ];

  const items = links.map((link) => {
    if (link.children) {
      return (
        <NavLink
          key={link.label}
          label={link.label}
          leftSection={<link.icon size="1rem" stroke={1.5} />}
          childrenOffset={28}
          defaultOpened={location.pathname.startsWith(link.to)}
        >
          {link.children.map((child) => (
             <NavLink
               key={child.label}
               label={child.label}
               active={location.pathname === child.to}
               onClick={() => {
                 navigate(child.to);
                 if (window.innerWidth < 768) toggle();
               }}
             />
          ))}
        </NavLink>
      );
    }

    return (
      <NavLink
        key={link.label}
        active={location.pathname === link.to}
        label={link.label}
        leftSection={<link.icon size="1rem" stroke={1.5} />}
        onClick={() => {
          navigate(link.to);
          if (window.innerWidth < 768) toggle(); // Close menu on mobile after click
        }}
      />
    );
  });

  const handleLogout = () => {
    clearApiToken();
    navigate('/login');
  };

  return (
      <AppShell
        header={{ height: 60 }}
        navbar={{
        width: 210,
          breakpoint: 'sm',
          collapsed: { mobile: !opened },
        }}
        padding="md"
      >
      <AppShell.Header>
        <Group h="100%" px="md" justify="space-between">
          <Group>
            <Burger opened={opened} onClick={toggle} hiddenFrom="sm" size="sm" />
            <Avatar src="/slftp.png" alt="Soulless FTP" size={28} radius="sm" />
            <Title order={3}>Soulless FTP</Title>
          </Group>
          <Group>
            <AutoSwitch />
            <Tooltip label="Toggle color scheme" withArrow withinPortal zIndex={999}>
              <ActionIcon
                onClick={() => toggleColorScheme()}
                variant="default"
                size="lg"
                aria-label="Toggle color scheme"
              >
                {colorScheme === 'dark' ? <IconSun size="1.2rem" /> : <IconMoon size="1.2rem" />}
              </ActionIcon>
            </Tooltip>
          </Group>
        </Group>
      </AppShell.Header>

      <AppShell.Navbar p="sm">
        {items}
        <NavLink
          label="Logout"
          leftSection={<IconLogout size="1rem" stroke={1.5} />}
          onClick={handleLogout}
          style={{ marginTop: 'auto' }}
          c="red"
        />
      </AppShell.Navbar>

      <AppShell.Main>
        <Outlet />
      </AppShell.Main>
    </AppShell>
  );
}
