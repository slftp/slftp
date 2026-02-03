import { AppShell, Burger, Group, NavLink, Title, useMantineColorScheme, ActionIcon, Avatar, Tooltip } from '@mantine/core';
import { useDisclosure } from '@mantine/hooks';
import { useQuery } from '@tanstack/react-query';
import { IconDashboard, IconServer, IconMoon, IconSun, IconRoute, IconFolders, IconMessage2, IconLogout, IconFilter, IconChartBar, IconFileText, IconAlertTriangle, IconFlask, IconFolder, IconBolt, IconTrophy, IconDatabase, IconHelpCircle, IconCloud } from '@tabler/icons-react';
import { useNavigate, useLocation, Outlet } from 'react-router-dom';
import { clearApiToken } from '../api/client';
import { isCbftpEnabled } from '../api/cbftpClient';
import { AutoSwitch } from './AutoSwitch';

export function Layout() {
  const [opened, { toggle }] = useDisclosure();
  const navigate = useNavigate();
  const location = useLocation();
  const { colorScheme, toggleColorScheme } = useMantineColorScheme();

  const { data: cbftpEnabled } = useQuery({
    queryKey: ['cbftp-enabled'],
    queryFn: isCbftpEnabled,
    staleTime: 5 * 60 * 1000, // Cache for 5 minutes
    retry: false,
  });

  interface NavLinkItem {
    icon: any;
    label: string;
    to: string;
    color?: string;
    children?: { label: string; to: string }[];
  }

  const allLinks: NavLinkItem[] = [
    { icon: IconDashboard, label: 'Dashboard', to: '/', color: 'blue' },
    { icon: IconFolder, label: 'Browser', to: '/browser', color: 'yellow' },
    { icon: IconCloud, label: 'cbftp', to: '/cbftp', color: 'grape' },
    { icon: IconBolt, label: 'PRE', to: '/pre', color: 'orange' },
    { icon: IconMessage2, label: 'IRC', to: '/irc', color: 'violet' },
    { icon: IconDatabase, label: 'Databases', to: '/databases', color: 'cyan' },
    { icon: IconAlertTriangle, label: 'Issues', to: '/issues', color: 'red' },
    { icon: IconFileText, label: 'Logs', to: '/logs', color: 'gray' },
    { icon: IconTrophy, label: 'Races', to: '/races', color: 'green' },
    { icon: IconRoute, label: 'Routes', to: '/routes', color: 'indigo' },
    { icon: IconFilter, label: 'Rules', to: '/rules', color: 'pink' },
    { icon: IconFlask, label: 'Tools', to: '/tools', color: 'teal' },
    { icon: IconHelpCircle, label: 'Help', to: '/help', color: 'violet' },
    { icon: IconFolders, label: 'Sections', to: '/sections', color: 'lime' },
    { icon: IconServer, label: 'Sites Manager', to: '/sites', color: 'blue' },
    { icon: IconChartBar, label: 'Stats', to: '/stats', color: 'orange' },
  ];

  // Filter out cbftp if not enabled
  const links = allLinks.filter(link => link.to !== '/cbftp' || cbftpEnabled);

  const items = links.map((link) => {
    if (link.children) {
      return (
        <NavLink
          key={link.label}
          label={link.label}
          leftSection={<link.icon size="1rem" stroke={1.5} color={link.color} />}
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
        leftSection={<link.icon size="1rem" stroke={1.5} color={link.color} />}
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
