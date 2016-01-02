(*Rule Description's*)
const
  TagDescription: string = 'You can use tag condition to check if a specified tag exists in the releasename.' + #13#10 +
    'Basicly, tag is a shorthand to *.tag.*, *.tag-*, *-tag.*, *-tag-*' + #13#10 + '!ruleadd HQ PS2 if tag = NTSC then DROP' + #13#10 +
    '!ruleadd HQ TV if not tag in HDTV, PDTV then DROP';

  TVCurrentSeasonDescription: string = 'Returns true when the rip has the same season number as the database. Returns false when we have no value.';

