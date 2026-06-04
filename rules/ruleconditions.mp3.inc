{ Rule Descriptions for MP3 => TMP3Release }
const
  MP3GenreDescription: string = 'Returns with the mp3 genre.' + #13#10
    + 'Example: if mp3genre =~ *Metal* then ALLOW' + #13#10;

  MP3YearDescription: string = 'Returns with the mp3 year' + #13#10
    + 'Example: if mp3year < 2009 then DROP' + #13#10;

  MP3CurrentYearDescription: string = 'Returns true, if the mp3 year of the release name equals the current year.' + #13#10
    + 'Note: If no mp3 year is present, it will return false.' + #13#10
    + 'Example: !ruleadd DUMP MP3 ifnot mp3currentyear then DROP' + #13#10;

  MP3LanguageDescription: string = 'Returns with mp3 rip''s language tag. Language is EN by default.' + #13#10
    + 'Example: if mp3language != EN then DROP' + #13#10;

  MP3ForeignDescription: string = 'Returns true, if the mp3 rip''s language is not EN.' + #13#10
    + 'Example: if mp3foreign then DROP' + #13#10;

  MP3SourceDescription: string = 'Returns with the mp3 rip''s source.' + #13#10
    + 'Example: if not ( mp3source in CD, CDR, DVD, VINYL ) then DROP' + #13#10;

  MP3LiveDescription: string = 'Returns true, if the mp3 rip''s source is a live source.' + #13#10
    + 'Example: if mp3live then DROP' + #13#10;

  MP3TypeDescription: string = 'Returns with the mp3 rip''s types.' + #13#10
    + 'Example: if mp3type in Bootleg, Demo then DROP';

  MP3BootlegDescription: string = 'Returns true if the mp3 rip is bootleg.';

  MP3NumDisksDescription: string = 'Returns with the number of disks of the mp3 release.' + #13#10
    + 'Example: We drop rips with more than 2 DVD''s and everything else if more than 4 CD''s:' + #13#10
    + 'if mp3numdisks > 2 && mp3source = DVD then DROP' + #13#10
    + 'if mp3numdisks > 4 then DROP' + #13#10;

  MP3VADescription: string = 'Returns true if the mp3 rip is a compilation. (VA)';
