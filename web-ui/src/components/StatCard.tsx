import { Card, Group, Text, ThemeIcon, Stack, Badge } from '@mantine/core';
import { type ReactNode } from 'react';
import { useTheme } from '../context/ThemeContext';

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
    showSign?: boolean;
  };
  variant?: 'default' | 'gradient' | 'outline';
}

export function StatCard({
  title,
  value,
  subtitle,
  icon,
  iconColor,
  iconGradient,
  trend,
  variant = 'default',
}: StatCardProps) {
  const { currentTheme } = useTheme();
  const isMinimal = currentTheme === 'minimal';
  const isLight = currentTheme === 'light';

  // Use theme-aware colors if not explicitly provided
  const effectiveIconColor = iconColor || (isMinimal ? '#6b7280' : isLight ? '#2563eb' : '#4318ff');
  const effectiveIconGradient = iconGradient || (isMinimal 
    ? 'linear-gradient(135deg, #4b5563 0%, #6b7280 100%)' 
    : isLight
      ? 'linear-gradient(135deg, #3b82f6 0%, #2563eb 100%)'
      : 'linear-gradient(135deg, #4318ff 0%, #868cff 100%)');

  const getCardStyles = () => {
    switch (variant) {
      case 'gradient':
        return {
          background: isMinimal
            ? 'linear-gradient(135deg, rgba(75, 85, 99, 0.15) 0%, rgba(107, 114, 128, 0.05) 100%)'
            : isLight
              ? 'linear-gradient(135deg, rgba(59, 130, 246, 0.1) 0%, rgba(37, 99, 235, 0.05) 100%)'
              : 'linear-gradient(135deg, rgba(67, 24, 255, 0.15) 0%, rgba(134, 140, 255, 0.05) 100%)',
          border: isMinimal
            ? '1px solid rgba(75, 85, 99, 0.25)'
            : isLight
              ? '1px solid rgba(59, 130, 246, 0.2)'
              : '1px solid rgba(67, 24, 255, 0.2)',
        };
      case 'outline':
        return {
          background: 'transparent',
          border: '1px solid var(--border)',
        };
      default:
        return {
          background: 'var(--bg-card)',
          border: '1px solid var(--border)',
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
        boxShadow: 'var(--shadow)',
        transition: 'transform 0.2s ease, box-shadow 0.2s ease, border-color 0.2s ease',
      }}
      styles={{
        root: {
          '&:hover': {
            transform: 'translateY(-2px)',
            boxShadow: 'var(--shadow-lg)',
            borderColor: 'var(--border-hover)',
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
              color: 'var(--text-secondary)',
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
                background: variant === 'gradient' 
                  ? 'linear-gradient(135deg, var(--text-gradient-start) 0%, var(--text-gradient-end) 100%)' 
                  : 'none',
                WebkitBackgroundClip: variant === 'gradient' ? 'text' : 'unset',
                WebkitTextFillColor: variant === 'gradient' ? 'transparent' : 'var(--text-primary)',
                color: variant === 'gradient' ? 'transparent' : 'var(--text-primary)',
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
                {trend.showSign !== false && trend.isPositive ? '+' : ''}{trend.value}%
              </Badge>
            )}
          </Group>

          {subtitle && (
            <Text 
              size="xs" 
              truncate
              style={{ color: 'var(--text-muted)' }}
            >
              {subtitle}
            </Text>
          )}
        </Stack>

        <ThemeIcon
          size={40}
          radius="lg"
          style={{
            background: effectiveIconGradient,
            boxShadow: isMinimal 
              ? '0 2px 8px rgba(0, 0, 0, 0.2)' 
              : `0 3px 10px ${effectiveIconColor === 'brand' ? 'rgba(67, 24, 255, 0.28)' : 'rgba(0, 0, 0, 0.2)'}`,
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

export function MiniStatCard({ title, value, icon, color, onClick }: MiniStatCardProps) {
  const { currentTheme } = useTheme();
  const isMinimal = currentTheme === 'minimal';
  const isLight = currentTheme === 'light';

  const effectiveColor = color || (isMinimal ? '#6b7280' : isLight ? '#2563eb' : '#4318ff');

  return (
    <Card
      padding="sm"
      radius="md"
      onClick={onClick}
      style={{
        background: 'var(--bg-card-secondary)',
        border: '1px solid var(--border)',
        cursor: onClick ? 'pointer' : 'default',
        transition: 'transform 0.2s ease, background-color 0.2s ease, border-color 0.2s ease',
      }}
      styles={{
        root: {
          '&:hover': {
            background: 'var(--nav-hover-bg)',
            borderColor: 'var(--border-hover)',
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
            background: isLight 
              ? `${effectiveColor}15`
              : `linear-gradient(135deg, ${effectiveColor}30 0%, ${effectiveColor}15 100%)`,
            border: `1px solid ${effectiveColor}30`,
            color: effectiveColor,
            flexShrink: 0,
          }}
        >
          {icon}
        </ThemeIcon>
        <Stack gap={0} style={{ minWidth: 0 }}>
          <Text size="11px" style={{ color: 'var(--text-muted)' }} truncate>
            {title}
          </Text>
          <Text fw={700} size="md" truncate style={{ color: 'var(--text-primary)' }}>
            {value}
          </Text>
        </Stack>
      </Group>
    </Card>
  );
}
