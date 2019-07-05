{ Rule Descriptions for Common => TRelease }
const
  ReleaseNameDescription: string = 'Returns with the name of the release.' + #13#10
    + 'Example: if releasename =~ *-keygen* then DROP' + #13#10;

  SectionDescription: string = 'Returns with the section name of the release.' + #13#10
    + 'Example: if section = TV then DROP' + #13#10;

  InternalDescription: string = 'Returns true, if the release is tagged as internal.' + #13#10
    + 'Example: !ruleadd * * if internal && destination notin DUMP1, DUMP2 then DROP' + #13#10;

  AgeDescription: string = 'This is useful for filtering old stuffs scanned by autodirlist in a not dated directory.' + #13#10
    + 'It expects the parameter in seconds.' + #13#10
    + 'Example: if age > 86400 then DROP' + #13#10;

  PretimeDescription: string = 'This is useful for anyone who needs to abide by certain pretime rules on a site. It expects the parameter in seconds.' + #13#10
    + 'Example: if pretime > 86400 && not pretimenotfound then DROP' + #13#10;

  PretimeFoundDescription: string = 'This is useful for anyone who needs to abide by certain pretime rules on a site. It expects the parameter as boolean.' + #13#10
    + 'Example: if pretime > 86400 && not pretimefound then DROP' + #13#10;

  CompleteDescription: string = 'Returns with the list of the sites, where the release is complete.' + #13#10
    + 'Example: if complete @ SITENAME && source != SITENAME then DROP' + #13#10;

  NotCompleteDescription: string = 'Returns with the list of the sites where the release is not yet complete.' + #13#10
    + 'This is the negated complete condition. It is true if the release is not notAllowed at the specified site and it is not pre or complete.' + #13#10
    + 'Example: you can add this rule on a slow 100mbit site: if notcomplete @ GBITSITE1 && source = GBITSITE2 then DROP' + #13#10
    + 'With this rule, 100mbit site will be started racing after gbitsite1 is finished.' + #13#10;

  PreDescription: string = 'Returns true, if the release is pred on the specified site.' + #13#10
    + 'Example: If you add this rule on a slow site, pres wont be sent there til gbitsite is complete:' + #13#10
    + 'if pre @ AFFILSITE && incomplete @ GBITSITE then DROP' + #13#10;

  AllowedDescription: string = 'Returns true, if the release is not notallowed on the specified site.' + #13#10
    + 'Example: if allowed @ SITENAME then DROP' + #13#10;

  NotAllowedDescription: string = 'Returns true, if the release is not allowed on the specified site.' + #13#10
    + 'Example: if notallowed @ SITENAME then DROP' + #13#10;

  GroupDescription: string = 'Returns the groupname.' + #13#10
    + 'Example: if group in GRP1, GRP2, GRP3 then ALLOW' + #13#10;

  FakeDescription: string = 'Returns true, if the release was recognized as fake by the fakechecker.' + #13#10;

  ForeignDescription: string = 'Returns true, if the releasename has a language tag (other than english).' + #13#10
    + 'Note: as mp3 rips have 2 letter codes, use mp3foreign and mp3language conditions.' + #13#10;

  LanguageDescription: string = 'Returns with the recognized language tags of the release.' + #13#10
    + 'Note: as mp3 rips have 2 letter codes, use mp3foreign and mp3language conditions.' + #13#10
    + 'Example: ifnot language in English, Slowenian, Chinese then DROP' + #13#10;

  YearDescription: string = 'Returns with the year tag of the release name. Returns with zero if year tag is not present.';

  CurrentYearDescription: string = 'Returns true, if the year tag of the release name equals the current year.' + #13#10
    + 'Note: If no year is present, it will return false.' + #13#10
    + 'Example: !ruleadd * X264-1080 ifnot currentyear then DROP' + #13#10;

  KnownGroupDescription: string = 'Returns true, if the groupname is known.' + #13#10
    + 'Note: If no known groups is set for a section in slftp.knowngroups, this condition wont match at all!' + #13#10
    + 'Example: !ruleadd * * ifnot knowngroup then DROP' + #13#10;

  UnKnownGroupDescription: string = 'Returns true, if the groupname is not known.' + #13#10
    + 'Note: If no known groups is set for a section in slftp.knowngroups, this condition wont match at all!' + #13#10
    + 'Example: !ruleadd * * if unknowngroup then DROP' + #13#10;

  SourceDescription: string = 'Returns with the name of the source site, which is currently fireing the ruleset.' + #13#10
    + 'You can use this function to setup static routing.' + #13#10
    + 'Example: we dont want to race SITE1' + chr(39) + 's MP3 section from SITE2 (but we want everything else):' + #13#10
    + '!ruleadd SITE1 MP3 if source = SITE2 then DROP' + #13#10;

  DestinationDescription: string = 'Returns with the name of the destination site.' + #13#10
    + 'You can use this function to make exceptions in generic rules.' + #13#10
    + 'Example: we want to drop stuffs detected as fake except on a few dumps:' + #13#10
    + '!ruleadd * * if fake && destination notin DUMP1, DUMP2 then DROP' + #13#10;

  CompleteSourceDescription: string = 'Returns true, if the source site is complete. You can use this rule to fill some sites from complete source only.';

  NewdirSourceDescription: string = 'Returns true, if source is newdir (not pre or complete).' + #13#10
    + 'Example: if newdirsource then DROP' + #13#10;

  NukedDescription: string = 'Returns true, if the release was nuked on the site.' + #13#10
    + 'Example: !ruleadd * * if nuked then DROP';

  TagDescription: string = 'You can use tag condition to check if a specified tag exists in the releasename.' + #13#10
    + 'Basicly, tag is a shorthand to *.tag.*, *.tag-*, *-tag.*, *-tag-*' + #13#10
    + 'Example: !ruleadd HQ PS2 if tag = NTSC then DROP' + #13#10
    + 'Example: !ruleadd HQ TV if not tag in HDTV, PDTV then DROP';

  DisksDescription: string = 'Returns with the number of disks (e.g.: 3 for Foobar.2008.NTSC.3DiSC.MDVDR-GRP' + #13#10
    + 'NOTE: this is NOT for MP3 rips.' + #13#10;

  AutofollowDescription: string = 'Returns true, if there was already an irc announce on the site (means: somebody started to send the release there).' + #13#10
    + 'Note: You have to add at least one NEWDIR irc catchevent to use this rule.' + #13#10
    + 'Example: !ruleadd * * if autofollow then ALLOW';

  PredDescription: string = 'Returns true, if there was a pre for this release on any of your sites.' + #13#10;

  DefaultDescription: string = 'This condition simple matches anything, you can use it for default policy.' + #13#10
    + 'If there is no matching rule then no action is taken which is same as DROP by default.' + #13#10
    + 'Example: if default then ALLOW' + #13#10;
