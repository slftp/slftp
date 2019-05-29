{ Rule Descriptions for NFO => TNFORelease }
const
  NfoGenreMaskDescription: string = 'Checks for genre parsed from the nfo file.' + #13#10
    + 'Genre string contains latin alphabet only, all other chars are replaced to spaces!' + #13#10
    + 'Note: nfogenre is for mdvdr/mv sections! For mp3 stuffs use mp3genre condition.' + #13#10
    + 'Example: if nfogenre =~ *Hip*Hop* then ALLOW' + #13#10;
