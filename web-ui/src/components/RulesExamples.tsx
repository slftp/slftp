import { useState, useMemo } from 'react';
import { Select, Stack, Text, Paper, Code, Loader, Alert, Group, CopyButton, ActionIcon, Tooltip, Switch } from '@mantine/core';
import { useQuery } from '@tanstack/react-query';
import { fetchConfigContent } from '../api/client';
import { IconCopy, IconCheck, IconBulb, IconFilter } from '@tabler/icons-react';

interface RuleCondition {
  name: string;
  ops: string;
  description: string;
}

interface RulesExamplesProps {
  conditions: RuleCondition[];
}

export function RulesExamples({ conditions }: RulesExamplesProps) {
  const [selectedSection, setSelectedSection] = useState<string | null>(null);
  const [selectedCondition, setSelectedCondition] = useState<string | null>(null);
  const [useRegexGrouping, setUseRegexGrouping] = useState(false);

  // Fetch and parse slftp.precatcher
  const { data: precatcherSections, isLoading: loadingSections, error: sectionsError } = useQuery({
    queryKey: ['slftp-precatcher-sections'],
    queryFn: async () => {
      const content = await fetchConfigContent('slftp.precatcher');
      const lines = content.split('\n');
      const sections: string[] = [];
      
      for (const line of lines) {
        const trimmed = line.trim();
        if (trimmed.startsWith(';') && trimmed.split(';').length >= 3) {
          const parts = trimmed.split(';');
          if (parts[1] && parts[1].trim()) {
             sections.push(parts[1].trim());
          }
        }
      }
      return Array.from(new Set(sections));
    },
    refetchOnWindowFocus: false,
  });

  const sectionOptions = useMemo(() => {
    if (!precatcherSections) return [];
    return precatcherSections.map(s => ({ value: s, label: s }));
  }, [precatcherSections]);

  const conditionOptions = useMemo(() => {
    return conditions.map(c => ({
      value: c.name,
      label: c.name,
      description: c.description 
    }));
  }, [conditions]);

  const generateExample = (section: string, condition: string, regexGrouping: boolean, allSections: string[]) => {
    const condLower = condition.toLowerCase();
    const site = 'EXAMPLESITE';
    let s = section;
    let sectionFilter = '';

    if (regexGrouping) {
      s = '*';
      // Find similar existing sections or just pick another one
      const prefix = section.slice(0, 3);
      const others = (allSections || []).filter(sec => sec !== section);
      const similar = others.find(sec => sec.startsWith(prefix));
      
      let groupParts = [section + '.*'];
      if (similar) {
         groupParts.push(similar);
      } else if (others.length > 0) {
         groupParts.push(others[0]);
      } else {
         groupParts.push('OTHER');
      }
      
      const groupRegex = `/${groupParts.join('|')}/i`;
      sectionFilter = `section =~ ${groupRegex} && `; 
    }

    let finalAllow = '';
    if (regexGrouping) {
       // Remove the trailing " && " for the allow rule
       const filterOnly = sectionFilter.replace(/ && $/, '');
       finalAllow = `\n${site} ${s} if ${filterOnly} then ALLOW`;
    } else {
       finalAllow = `\n${site} ${s} if default then ALLOW`;
    }

    if (condLower.includes('releasename') || condLower.includes('release')) {
      return `${site} ${s} if ${sectionFilter}${condition} =~ /pattern/i then DROP\n` + 
             `${site} ${s} if ${sectionFilter}${condition} =~ /[\\._\\-](GERMAN|FRENCH)[\\._\\-]/i then DROP` + finalAllow;
    }
    
    if (condLower.includes('group')) {
      return `${site} ${s} if ${sectionFilter}${condition} in GROUP1, GROUP2, GROUP3 then DROP\n` + 
             `${site} ${s} if ${sectionFilter}${condition} notin MYFAVORITEGRP, ANOTHERGOODGRP then DROP` + finalAllow;
    }

    if (condLower === 'section') {
      const otherSection = (allSections || []).find(sec => sec !== section) || 'OTHER';
      // DROP specific subsets first, then ALLOW the broader group
      return `${site} * if section in ${section}-BAD, ${otherSection}-TRASH then DROP\n` +
             `${site} * if section =~ /^(${section}|${otherSection})/i then ALLOW`;
    }

    if (condLower.includes('language') || condLower.includes('lang')) {
      if (condLower.includes('mp3')) {
        return `${site} ${s} if ${sectionFilter}not ${condition} in EN, DE then DROP` + finalAllow;
      }
      if (condLower.includes('tv') || condLower.includes('imdb')) {
        return `${site} ${s} if ${sectionFilter}${condition} != English then DROP\n` + 
               `${site} ${s} if ${sectionFilter}${condition} notin English, German then DROP` + finalAllow;
      }
      return `${site} ${s} if ${sectionFilter}${condition} in 'French', 'Spanish' then DROP` + finalAllow;
    }

    if (condLower.includes('country')) {
      return `${site} ${s} if ${sectionFilter}${condition} notin USA, UK then DROP` + finalAllow;
    }

    if (condLower.includes('genre')) {
      return `${site} ${s} if ${sectionFilter}${condition} in Documentary, Sports then DROP\n` + 
             `${site} ${s} if ${sectionFilter}${condition} =~ /(Horror|Thriller)/i then ALLOW` + finalAllow;
    }
    
    if (condLower.includes('imdb')) {
      if (condLower.includes('rating') || condLower.includes('score')) {
        return `${site} ${s} if ${sectionFilter}${condition} < 65 then DROP\n` + 
               `${site} ${s} if ${sectionFilter}${condition} >= 80 then ALLOW`;
      }
      if (condLower.includes('votes')) {
        return `${site} ${s} if ${sectionFilter}${condition} < 500 then DROP` + finalAllow;
      }
      if (condLower.includes('year')) {
        return `${site} ${s} if ${sectionFilter}${condition} < 2020 then DROP` + finalAllow;
      }
    }

    if (condLower.includes('rating')) {
       return `${site} ${s} if ${sectionFilter}${condition} < 60 then DROP` + finalAllow;
    }

    if (condLower.includes('size') || condLower.includes('files') || condLower.includes('kb') || condLower.includes('disk')) {
      return `${site} ${s} if ${sectionFilter}${condition} < 500 then DROP\n` + 
             `${site} ${s} if ${sectionFilter}${condition} > 5000 then ALLOW` + finalAllow;
    }

    if (['internal', 'nuked', 'requested', 'imdblookupdone', 'pretimeok', 'default', 'fake', 'foreign', 'pred', 'autofollow', 'mp3live', 'mp3bootleg', 'mp3va', 'tvscripted', 'tvrunning', 'tvcurrent', 'tvdaily', 'imdblimited', 'imdbwide', 'imdbfestival', 'imdbstv'].some(x => condLower.includes(x))) {
      return `${site} ${s} if ${sectionFilter}${condition} then DROP\n` + 
             `${site} ${s} if ${sectionFilter}not ${condition} then ALLOW`;
    }

    return `${site} ${s} if ${sectionFilter}${condition} = "value" then DROP` + finalAllow;
  };

  const example = useMemo(() => {
    if (!selectedSection || !selectedCondition) return null;
    return generateExample(selectedSection, selectedCondition, useRegexGrouping, precatcherSections || []);
  }, [selectedSection, selectedCondition, useRegexGrouping, precatcherSections]);

  if (loadingSections) return <Loader size="sm" />;
  if (sectionsError) return <Alert color="red">Could not load sections from slftp.precatcher</Alert>;

  return (
    <Stack gap="md">
      <Group grow align="flex-start">
        <Select
          label="1. Select Section"
          placeholder="Choose a section"
          data={sectionOptions}
          value={selectedSection}
          onChange={setSelectedSection}
          searchable
          description="Main sections from slftp.precatcher"
        />
        
        <Select
          label="2. Select Condition"
          placeholder="Choose a condition"
          data={conditionOptions}
          value={selectedCondition}
          onChange={setSelectedCondition}
          searchable
          description="Available rule conditions"
        />
      </Group>

      <Paper p="sm" withBorder radius="md">
        <Group justify="space-between">
           <Group gap="xs">
              <IconFilter size="1.2rem" style={{ color: 'var(--mantine-color-blue-filled)' }} />
              <div>
                <Text size="sm" fw={500}>Regex Grouping</Text>
                <Text size="xs" c="dimmed">Group multiple sections using a regex condition (common in .rtpl files)</Text>
              </div>
           </Group>
           <Switch 
              checked={useRegexGrouping} 
              onChange={(event) => setUseRegexGrouping(event.currentTarget.checked)} 
           />
        </Group>
      </Paper>

      {example && (
        <Paper p="md" withBorder bg="var(--mantine-color-body)">
           <Stack gap="xs">
             <Group justify="space-between">
                <Group gap="xs">
                   <IconBulb size="1.2rem" style={{ color: 'var(--mantine-color-yellow-filled)' }} />
                   <Text fw={500}>Example Rule</Text>
                </Group>
                <CopyButton value={example} timeout={2000}>
                  {({ copied, copy }) => (
                    <Tooltip label={copied ? 'Copied' : 'Copy'} withArrow position="left">
                      <ActionIcon color={copied ? 'teal' : 'gray'} variant="subtle" onClick={copy}>
                        {copied ? <IconCheck size="1rem" /> : <IconCopy size="1rem" />}
                      </ActionIcon>
                    </Tooltip>
                  )}
                </CopyButton>
             </Group>
             <Code block style={{ fontSize: 14 }}>
               {example}
             </Code>
             <Text size="xs" c="dimmed">
               This is a generated example. You can copy this into your site rules (.rtpl) file and adjust the values.
             </Text>
           </Stack>
        </Paper>
      )}
      
      {!example && (
         <Text size="sm" c="dimmed" ta="center" py="md">
           Select a section and a condition to see an example.
         </Text>
      )}
    </Stack>
  );
}
