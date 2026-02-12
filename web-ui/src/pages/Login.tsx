import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { 
  TextInput, 
  Button, 
  Card, 
  Stack, 
  Title, 
  Text, 
  Alert,
  Box,
  ThemeIcon,
  Group,
} from '@mantine/core';
import { 
  IconLock, 
  IconLogin, 
  IconRocket, 
  IconAlertCircle,
  IconShieldLock,
} from '@tabler/icons-react';
import { setApiToken } from '../api/client';

const Login: React.FC = () => {
  const [apiKey, setApiKey] = useState('');
  const [error, setError] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  const navigate = useNavigate();

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setIsLoading(true);

    if (!apiKey.trim()) {
      setError('Please enter an API key');
      setIsLoading(false);
      return;
    }

    await new Promise(resolve => setTimeout(resolve, 500));
    setApiToken(apiKey);
    navigate('/');
  };

  return (
    <Box
      style={{
        minHeight: '100vh',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        background: 'linear-gradient(135deg, #0f1729 0%, #1e293b 50%, #1e3a8a 100%)',
        position: 'relative',
        overflow: 'hidden',
      }}
    >
      {/* Animated Background Elements */}
      <Box
        style={{
          position: 'absolute',
          top: '-20%',
          left: '-10%',
          width: '50%',
          height: '50%',
          background: 'radial-gradient(circle, rgba(67, 24, 255, 0.2) 0%, transparent 60%)',
          borderRadius: '50%',
          filter: 'blur(80px)',
          animation: 'float 12s ease-in-out infinite',
        }}
      />
      <Box
        style={{
          position: 'absolute',
          bottom: '-20%',
          right: '-10%',
          width: '50%',
          height: '50%',
          background: 'radial-gradient(circle, rgba(0, 212, 255, 0.15) 0%, transparent 60%)',
          borderRadius: '50%',
          filter: 'blur(80px)',
          animation: 'float 12s ease-in-out infinite reverse',
        }}
      />
      <Box
        style={{
          position: 'absolute',
          top: '50%',
          left: '50%',
          transform: 'translate(-50%, -50%)',
          width: '80%',
          height: '80%',
          background: 'radial-gradient(ellipse at center, rgba(5, 21, 63, 0.5) 0%, transparent 70%)',
          filter: 'blur(60px)',
        }}
      />
      
      {/* Grid Pattern Overlay */}
      <Box
        style={{
          position: 'absolute',
          inset: 0,
          backgroundImage: `
            linear-gradient(rgba(255, 255, 255, 0.02) 1px, transparent 1px),
            linear-gradient(90deg, rgba(255, 255, 255, 0.02) 1px, transparent 1px)
          `,
          backgroundSize: '60px 60px',
          pointerEvents: 'none',
        }}
      />

      <Card
        padding="xl"
        radius="xl"
        style={{
          width: '100%',
          maxWidth: '420px',
          background: 'linear-gradient(127.09deg, rgba(30, 41, 59, 0.95) 19.41%, rgba(40, 49, 71, 0.9) 76.65%)',
          backdropFilter: 'blur(20px)',
          border: '1px solid rgba(255, 255, 255, 0.1)',
          boxShadow: '0 24px 64px rgba(0, 0, 0, 0.5), 0 0 80px rgba(67, 24, 255, 0.15)',
          position: 'relative',
          zIndex: 1,
        }}
      >
        <form onSubmit={handleSubmit}>
          <Stack gap="lg">
            {/* Logo & Title */}
            <Stack align="center" gap="md">
              <ThemeIcon
                size={64}
                radius="xl"
                style={{
                  background: 'linear-gradient(135deg, #4318ff 0%, #868cff 100%)',
                  boxShadow: '0 8px 32px rgba(67, 24, 255, 0.5)',
                }}
              >
                <IconRocket size="2rem" stroke={2} color="white" />
              </ThemeIcon>
              
              <Box ta="center">
                <Title 
                  order={3}
                  style={{
                    background: 'linear-gradient(135deg, #fff 0%, #a0aec0 100%)',
                    WebkitBackgroundClip: 'text',
                    WebkitTextFillColor: 'transparent',
                    marginBottom: '4px',
                  }}
                >
                  Welcome Back
                </Title>
                <Text size="sm" c="dimmed">
                  Sign in to access your dashboard
                </Text>
              </Box>
            </Stack>

            {/* Error Alert */}
            {error && (
              <Alert
                icon={<IconAlertCircle size="1rem" />}
                color="red"
                radius="md"
                variant="light"
                styles={{
                  root: {
                    background: 'rgba(255, 77, 77, 0.1)',
                    border: '1px solid rgba(255, 77, 77, 0.3)',
                  },
                }}
              >
                {error}
              </Alert>
            )}

            {/* API Key Input */}
            <TextInput
              label="API Key"
              placeholder="Enter your API key"
              type="password"
              value={apiKey}
              onChange={(e) => {
                setApiKey(e.target.value);
                setError('');
              }}
              leftSection={<IconShieldLock size="1rem" color="#718096" />}
              size="md"
              radius="md"
              styles={{
                label: {
                  color: '#a0aec0',
                  fontWeight: 600,
                  marginBottom: '8px',
                },
                input: {
                  background: 'rgba(6, 11, 40, 0.6)',
                  border: '1px solid rgba(255, 255, 255, 0.1)',
                  color: '#fff',
                  '&:focus': {
                    borderColor: '#868cff',
                    boxShadow: '0 0 0 3px rgba(67, 24, 255, 0.15)',
                  },
                },
              }}
            />

            {/* Login Button */}
            <Button
              type="submit"
              size="md"
              radius="md"
              loading={isLoading}
              leftSection={<IconLogin size="1.2rem" />}
              fullWidth
              styles={{
                root: {
                  background: 'linear-gradient(135deg, #4318ff 0%, #868cff 100%)',
                  boxShadow: '0 8px 24px rgba(67, 24, 255, 0.5)',
                  transition: 'all 0.3s ease',
                  '&:hover': {
                    transform: 'translateY(-2px)',
                    boxShadow: '0 12px 32px rgba(67, 24, 255, 0.6)',
                  },
                  '&:active': {
                    transform: 'translateY(0)',
                  },
                },
              }}
            >
              Sign In
            </Button>

            {/* Info Box */}
            <Alert
              radius="md"
              styles={{
                root: {
                  background: 'rgba(0, 212, 255, 0.05)',
                  border: '1px solid rgba(0, 212, 255, 0.2)',
                },
              }}
            >
              <Group gap="xs">
                <ThemeIcon
                  size="sm"
                  radius="sm"
                  style={{
                    background: 'rgba(0, 212, 255, 0.2)',
                  }}
                >
                  <IconLock size="0.8rem" color="#00d4ff" />
                </ThemeIcon>
                <Text size="xs" c="dimmed" style={{ flex: 1 }}>
                  The API key can be found in your <Text span fw={600} c="#868cff">slftp.ini</Text> configuration file under the [api] section.
                </Text>
              </Group>
            </Alert>
          </Stack>
        </form>
      </Card>

      {/* CSS Animations */}
      <style>{`
        @keyframes float {
          0%, 100% {
            transform: translate(0, 0) scale(1);
          }
          33% {
            transform: translate(30px, -30px) scale(1.1);
          }
          66% {
            transform: translate(-20px, 20px) scale(0.9);
          }
        }
      `}</style>
    </Box>
  );
};

export default Login;
