import { Card, Group, Text, ThemeIcon, Stack, Badge } from '@mantine/core';
import { type ReactNode } from 'react';

interface StatCardProps {
  title: string;
  value: string | number;
  subtitle?: string;
  icon: ReactNode;
  iconColor?: string;
  iconGradient?: string;
  trend?: {
    value: number;
    isPositive: boolean;
  };
  variant?: 'default' | 'gradient' | 'outline';
}

export function StatCard({
  title,
  value,
  subtitle,
  icon,
  iconColor = 'brand',
  iconGradient = 'linear-gradient(135deg, #4318ff 0%, #868cff 100%)',
  trend,
  variant = 'default',
}: StatCardProps) {
  const getCardStyles = () => {
    switch (variant) {
      case 'gradient':
        return {
          background: 'linear-gradient(135deg, rgba(67, 24, 255, 0.15) 0%, rgba(134, 140, 255, 0.05) 100%)',
          border: '1px solid rgba(67, 24, 255, 0.2)',
        };
      case 'outline':
        return {
          background: 'transparent',
          border: '1px solid rgba(255, 255, 255, 0.1)',
        };
      default:
        return {
          background: 'rgba(17, 28, 68, 0.6)',
          border: '1px solid rgba(255, 255, 255, 0.08)',
        };
    }
  };

  const styles = getCardStyles();

  return (
    <Card
      padding="md"
      radius="lg"
      style={{
        ...styles,
        backdropFilter: 'blur(20px)',
        boxShadow: '0 4px 20px rgba(0, 0, 0, 0.25), 0 0 30px rgba(67, 24, 255, 0.03)',
        transition: 'all 0.3s ease',
      }}
      styles={{
        root: {
          '&:hover': {
            transform: 'translateY(-2px)',
            boxShadow: '0 8px 30px rgba(0, 0, 0, 0.35), 0 0 40px rgba(67, 24, 255, 0.08)',
            borderColor: 'rgba(255, 255, 255, 0.12)',
          },
        },
      }}
    >
      <Group justify="space-between" align="flex-start" wrap="nowrap">
        <Stack gap={4} style={{ flex: 1, minWidth: 0 }}>
          <Text 
            size="11px" 
            fw={700} 
            tt="uppercase"
            truncate
            style={{ 
              color: '#a3aed0',
              letterSpacing: '0.05em',
            }}
          >
            {title}
          </Text>
          
          <Group gap="xs" align="center" wrap="nowrap">
            <Text 
              fw={700} 
              size="xl"
              truncate
              style={{ 
                background: variant === 'gradient' ? 'linear-gradient(135deg, #fff 0%, #a3aed0 100%)' : 'none',
                WebkitBackgroundClip: variant === 'gradient' ? 'text' : 'unset',
                WebkitTextFillColor: variant === 'gradient' ? 'transparent' : 'white',
              }}
            >
              {value}
            </Text>
            
            {trend && (
              <Badge
                size="xs"
                variant="light"
                color={trend.isPositive ? 'success' : 'danger'}
                style={{
                  background: trend.isPositive 
                    ? 'rgba(0, 255, 136, 0.12)' 
                    : 'rgba(255, 77, 77, 0.12)',
                  border: trend.isPositive 
                    ? '1px solid rgba(0, 255, 136, 0.25)' 
                    : '1px solid rgba(255, 77, 77, 0.25)',
                  padding: '2px 6px',
                }}
              >
                {trend.isPositive ? '+' : ''}{trend.value}%
              </Badge>
            )}
          </Group>

          {subtitle && (
            <Text 
              size="xs" 
              truncate
              style={{ color: '#707eae' }}
            >
              {subtitle}
            </Text>
          )}
        </Stack>

        <ThemeIcon
          size={40}
          radius="lg"
          style={{
            background: iconGradient,
            boxShadow: `0 4px 16px ${iconColor === 'brand' ? 'rgba(67, 24, 255, 0.35)' : 'rgba(0, 0, 0, 0.25)'}`,
            flexShrink: 0,
          }}
        >
          {icon}
        </ThemeIcon>
      </Group>
    </Card>
  );
}

interface MiniStatCardProps {
  title: string;
  value: string | number;
  icon: ReactNode;
  color?: string;
  onClick?: () => void;
}

export function MiniStatCard({ title, value, icon, color = '#4318ff', onClick }: MiniStatCardProps) {
  return (
    <Card
      padding="sm"
      radius="md"
      onClick={onClick}
      style={{
        background: 'rgba(17, 28, 68, 0.4)',
        backdropFilter: 'blur(10px)',
        border: '1px solid rgba(255, 255, 255, 0.05)',
        cursor: onClick ? 'pointer' : 'default',
        transition: 'all 0.3s ease',
      }}
      styles={{
        root: {
          '&:hover': {
            background: 'rgba(17, 28, 68, 0.6)',
            borderColor: 'rgba(255, 255, 255, 0.1)',
            transform: onClick ? 'translateY(-2px)' : 'none',
          },
        },
      }}
    >
      <Group gap="sm" wrap="nowrap">
        <ThemeIcon
          size={32}
          radius="md"
          style={{
            background: `linear-gradient(135deg, ${color}30 0%, ${color}15 100%)`,
            border: `1px solid ${color}30`,
            flexShrink: 0,
          }}
        >
          {icon}
        </ThemeIcon>
        <Stack gap={0} style={{ minWidth: 0 }}>
          <Text size="11px" style={{ color: '#707eae' }} truncate>
            {title}
          </Text>
          <Text fw={700} size="md" truncate>
            {value}
          </Text>
        </Stack>
      </Group>
    </Card>
  );
}
