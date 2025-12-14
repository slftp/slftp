import { AppShell, Burger, Group, NavLink, Title, useMantineColorScheme, ActionIcon, Avatar } from '@mantine/core';
import { useDisclosure } from '@mantine/hooks';
import { IconDashboard, IconServer, IconMoon, IconSun, IconRoute, IconFolders, IconBrandHipchat, IconLogout, IconFilter, IconChartBar, IconFileText } from '@tabler/icons-react';
import { useNavigate, useLocation, Outlet } from 'react-router-dom';
import { clearApiToken } from '../api/client';

export function Layout() {
  const [opened, { toggle }] = useDisclosure();
  const navigate = useNavigate();
  const location = useLocation();
  const { colorScheme, toggleColorScheme } = useMantineColorScheme();

  const links = [
    { icon: IconDashboard, label: 'Dashboard', to: '/' },
    { icon: IconBrandHipchat, label: 'IRC', to: '/irc' },
    { icon: IconFileText, label: 'Logs', to: '/logs' },
    { icon: IconRoute, label: 'Routes', to: '/routes' },
    { icon: IconFilter, label: 'Rules', to: '/rules' },
    { icon: IconFolders, label: 'Sections', to: '/sections' },
    { icon: IconServer, label: 'Sites Manager', to: '/sites' },
    { icon: IconChartBar, label: 'Stats', to: '/stats' },
  ];

  const items = links.map((link) => (
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
  ));

  const handleLogout = () => {
    clearApiToken();
    navigate('/login');
  };

  return (
      <AppShell
        header={{ height: 60 }}
        navbar={{
        width: 240,
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
          <ActionIcon
            onClick={() => toggleColorScheme()}
            variant="default"
            size="lg"
            aria-label="Toggle color scheme"
          >
            {colorScheme === 'dark' ? <IconSun size="1.2rem" /> : <IconMoon size="1.2rem" />}
          </ActionIcon>
        </Group>
      </AppShell.Header>

      <AppShell.Navbar p="md">
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
