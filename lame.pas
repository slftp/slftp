unit lame;

interface

uses Classes;

type
  TLameInfo = record
    lame_present: Boolean;  
    lame_version: AnsiString;
    lame_tagversion: Integer;
    lame_lowpass: AnsiString;
    lame_stereomode: AnsiString;
    lame_bitrate: Integer;
    lame_source_frequency: AnsiString;
    lame_ath: Integer;
    lame_noiseshaping: Boolean;
    lame_unwise: Boolean;
    lame_vbrmethod: AnsiString;
    lame_preset: AnsiString;
    lame_gain: Integer;
    error: AnsiString;
  end;
  TID3v1Info = record
    id3_present: Boolean;
    id3_version: AnsiString;
    id3_title: AnsiString;
    id3_artist: AnsiString;
    id3_album: AnsiString;
    id3_track: Byte;
    id3_year: AnsiString;
    id3_genre: AnsiString;
    id3_comment: AnsiString;
  end;

function MP3Info(filename: AnsiString): AnsiString;
function Lame_Check(var buffer: array of Byte; offset: Integer; var dest_info: TLameInfo): Integer; overload;
function Lame_Check(stuff: TStream; offset: Integer; var dest_info: TLameInfo): Integer; overload;
function ID3_Check(var buffer: array of Byte; var dest_info: TID3v1Info): Integer;
function ID3v1InfoToString( const i: TID3v1Info): AnsiString;
function LameInfoToString(const l: TLameInfo): AnsiString;

const
  LAME_BUFFER_SIZE = 16384*2;
  ID3V1_BUFFER_SIZE = 128;

implementation

uses SysUtils, mystrings;


const
  genres : Array[0..147] of AnsiString = (
 'Blues',
 'Classic Rock',
 'Country',
 'Dance',
 'Disco',
 'Funk',
 'Grunge',
 'Hip-Hop',
 'Jazz',
 'Metal',
 'New Age',
 'Oldies',
 'Other',
 'Pop',
 'R&B',
 'Rap',
 'Reggae',
 'Rock',
 'Techno',
 'Industrial',
 'Alternative',
 'Ska',
 'Death Metal',
 'Pranks',
 'Soundtrack',
 'Euro-Techno',
 'Ambient',
 'Trip-Hop',
 'Vocal',
 'Jazz+Funk',
 'Fusion',
 'Trance',
 'Classical',
 'Instrumental',
 'Acid',
 'House',
 'Game',
 'Sound Clip',
 'Gospel',
 'Noise',
 'AlternRock',
 'Bass',
 'Soul',
 'Punk',
 'Space',
 'Meditative',
 'Instrumental Pop',
 'Instrumental Rock',
 'Ethnic',
 'Gothic',
 'Darkwave',
 'Techno-Industrial',
 'Electronic',
 'Pop-Folk',
 'Eurodance',
 'Dream',
 'Southern Rock',
 'Comedy',
 'Cult',
 'Gangsta',
 'Top 40',
 'Christian Rap',
 'Pop/Funk',
 'Jungle',
 'Native American',
 'Cabaret',
 'New Wave',
 'Psychadelic',
 'Rave',
 'Showtunes',
 'Trailer',
 'Lo-Fi',
 'Tribal',
 'Acid Punk',
 'Acid Jazz',
 'Polka',
 'Retro',
 'Musical',
 'Rock & Roll',
 'Hard Rock',
 'Folk',
 'Folk-Rock',
 'National Folk',
 'Swing',
 'Fast Fusion',
 'Bebob',
 'Latin',
 'Revival',
 'Celtic',
 'Bluegrass',
 'Avantgarde',
 'Gothic Rock',
 'Progressive Rock',
 'Psychedelic Rock',
 'Symphonic Rock',
 'Slow Rock',
 'Big Band',
 'Chorus',
 'Easy Listening',
 'Acoustic',
 'Humour',
 'Speech',
 'Chanson',
 'Opera',
 'Chamber Music',
 'Sonata',
 'Symphony',
 'Booty Bass',
 'Primus',
 'Porn Groove',
 'Satire',
 'Slow Jam',
 'Club',
 'Tango',
 'Samba',
 'Folklore',
 'Ballad',
 'Power Ballad',
 'Rhythmic Soul',
 'Freestyle',
 'Duet',
 'Punk Rock',
 'Drum Solo',
 'A capella',
 'Euro-House',
 'Dance Hall',
  'Goa',
                'Drum & Bass',
                'Club-House',
                'Hardcore',
                'Terror',
                'Indie',
                'BritPop',
                'Negerpunk',
                'Polsk Punk',
                'Beat',
                'Christian Gangsta Rap',
                'Heavy Metal',
                'Black Metal',
                'Crossover',
                'Contemporary Christian',
                'Christian Rock',
                'Merengue',
                'Salsa',
                'Thrash Metal',
                'Anime',
                'JPop',
                'Synthpop'

  );

type
  TIndexedString = record
    index: Integer;
    value: AnsiString;
  end;

const
  vbr_methods : array[1..4] of TIndexedString = (
    (index: 2; value: 'abr'),
    (index: 3; value: 'vbr old / vbr rh'),
    (index: 4; value: 'vbr mtrh'),
    (index: 5; value: 'vbr mt')
  );

  source_frequencies : array[1..4] of TIndexedString = (
    (index: 0; value: '32'),
    (index: 1; value: '44.1'),
    (index: 2; value: '48'),
    (index: 3; value: '48+')
  );
  stereo_modes : array[1..8] of TIndexedString = (
    (index: 0; value: 'Mono'),
    (index: 1; value: 'Stereo'),
    (index: 2; value: 'Dual'),
    (index: 3; value: 'Joint'),
    (index: 4; value: 'Force'),
    (index: 5; value: 'Auto'),
    (index: 6; value: 'Intensity'),
    (index: 7; value: 'Undefined')
  );

  lame_presets : array[1..20] of TIndexedString = (
    (index: 1000; value: 'R3MIX'),
    (index: 470; value: 'V3'),
    (index: 1002; value: 'EXTREME'),
    (index: 420; value: 'V8'),
    (index: 320; value: 'ABR_320'),
    (index: 1003; value: 'INSANE'),
    (index: 450; value: 'V5'),
    (index: 8; value: 'ABR_8'),
    (index: 1005; value: 'EXTREME_FAST'),
    (index: 490; value: 'V1'),
    (index: 440; value: 'V6'),
    (index: 1006; value: 'MEDIUM'),
    (index: 430; value: 'V7'),
    (index: 460; value: 'V4'),
    (index: 500; value: 'V0'),
    (index: 410; value: 'V9'),
    (index: 1004; value: 'STANDARD_FAST'),
    (index: 480; value: 'V2'),
    (index: 1001; value: 'STANDARD'),
    (index: 1007; value: 'MEDIUM_FAST')
  );

function SyncSafeIntToNormal(var buffer: array of byte): Cardinal;
var re, b1, b2, b3, b4: Cardinal;
begin
//memcpy(&re, buffer, 4);  ez nem jo endian miatt */
	re := (buffer[0] shl 24) or (buffer[1] shl 16) or (buffer[2] shl 8) or (buffer[3] shl 0);

	//most meg at kell konvertalni.*/
	b1 := (re and $FF000000) shr 3;
	b2 := (re and $00FF0000) shr 2;
	b3 := (re and $0000FF00) shr 1;
	b4 := (re and $000000FF) shr 0;

	Result:= b1 or b2 or b3 or b4;

end;

function QueryIndex(index: Integer; const iv: array of TIndexedString): AnsiString;
var i: Integer;
begin
  Result:= '';
  for i:= Low(iv) to High(iv) do
  begin
    if iv[i].index = index then
    begin
      Result:= iv[i].value;
      exit;
    end;
  end;
end;

function Lame_Check(stuff: TStream; offset: Integer; var dest_info: TLameInfo): Integer;
var buffer: array[1..LAME_BUFFER_SIZE] of byte;
begin
  stuff.Position:= offset;
  stuff.Read(buffer, LAME_BUFFER_SIZE);
  Result:= Lame_Check(buffer, offset, dest_info);
end;

(* returns:
 *  0: header does not violate any rules
 * -1: header violates the rules
 * >0: need to seek
 *
 * dest_info : if not null, text info about the lame info
 *)
function Lame_Check(var buffer: array of Byte; offset: Integer; var dest_info: TLameInfo): Integer;
var id3v2_size: Integer;
    i: Integer;
begin
  Result:= -1;

  dest_info.lame_present:= False;
  dest_info.error:= '';

	if((buffer[offset+0] = $49)and(buffer[offset+1] = $44) and (buffer[offset+2] = $33)) then
	begin
		id3v2_size := SyncSafeIntToNormal(buffer[offset+6]);

		if (buffer[offset+5] and 16 <> 0) then // if footer is present */
			inc(id3v2_size, 20)
		else
			inc(id3v2_size, 10);

		// lehet nem kell syncelni ha*/
		if (id3v2_size + 400 < LAME_BUFFER_SIZE) then
    begin
			Result:= Lame_Check(buffer, id3v2_size, dest_info);
      exit;
    end;


		Result:= id3v2_size;
    exit;
	end;


	if ( (buffer[offset+0] = $ff) and ((buffer[offset+1] = $fb ) or (buffer[offset+1] = $fa)) ) then
	begin
		//Mpeg1 Layer III header detected */
		if('Xing' <> MyCopy(buffer, offset+$24, 4)) then
    begin
      dest_info.error:= 'No Xing header found';
      exit;
		end;

		// checking LAME header
//    SetLength(lame_version, 15);
    dest_info.lame_present:= True; 
		dest_info.lame_version:= Trim(MyCopy(buffer, offset+$9c, 9));
		dest_info.lame_tagversion:= (buffer[offset+$A5] and $F0) shr 4;


    dest_info.lame_vbrmethod:= QueryIndex((buffer[offset+$A5] and $0F), vbr_methods);
		if (dest_info.lame_vbrmethod = '') then
		begin
		  dest_info.error:= 'Broken vbr_method in header';
      exit;
		end;

    dest_info.lame_lowpass:= Format('%.1f', [(buffer[offset+$A6]) / 10]);
    dest_info.lame_lowpass:= Csere(dest_info.lame_lowpass, ',', '.');
    dest_info.lame_ath:= (buffer[offset+$AF]) and $0f;
    dest_info.lame_bitrate:= (buffer[offset+$B0]);

    dest_info.lame_source_frequency:= QueryIndex(((buffer[offset+$B4]) and $c0) shr 6, source_frequencies);
    if dest_info.lame_source_frequency = '' then
    begin
		  dest_info.error:= 'Invalid source frequency';
      exit;
    end;
    dest_info.lame_unwise:= (buffer[offset+$B4] and $04) = 1;
    dest_info.lame_noiseshaping:= (buffer[offset+$B4] and $03) <> 0;
    dest_info.lame_stereomode:= QueryIndex(((buffer[offset+$B4] and 28) shr 2), stereo_modes);
    if dest_info.lame_stereomode = '' then
    begin
		  dest_info.error:= 'Invalid stereo mode';
      exit;
    end;

    dest_info.lame_gain:= (buffer[offset+$B5]);

    i:= (buffer[offset+$B6] and 7);
    i:= i shl 8;
    i:= i or buffer[offset+$B7];
    dest_info.lame_preset:= QueryIndex(i, lame_presets);
    if dest_info.lame_preset = '' then
    begin
		  dest_info.error:= 'Invalid lame preset';
      exit;
    end;

    Result:= 0;
    exit;

	end;


	dest_info.error:= 'No MPEG1 Layer III header detected';
	Result:= -1;
end;



(* returns:
 *  0: header does not violate any rules
 * -1: header violates the rules
 *
 * dest_info : if not null, text info about the lame info
 *)
function ID3_Check(var buffer: array of Byte; var dest_info: TID3v1Info): Integer;
begin
	//TAG*/
  dest_info.id3_present:= False;
  dest_info.id3_version:= '';
  dest_info.id3_title:= '';
  dest_info.id3_artist:= '';
  dest_info.id3_album:= '';
  dest_info.id3_track:= 0;
  dest_info.id3_year:= '';
  dest_info.id3_genre:= '';
  dest_info.id3_comment:= '';

	if((buffer[0] = $54)and(buffer[1] = $41)and(buffer[2] = $47)) then
	begin
    dest_info.id3_present:= True;

    //(buffer[127] >= 0) and
		if ( (buffer[127] <= 147)) then
			dest_info.id3_genre := genres[buffer[127]];

    dest_info.id3_title:= Trim(MyCopy( buffer, 3, 30));
    dest_info.id3_artist:= Trim(MyCopy( buffer, 33, 30));
    dest_info.id3_album:= Trim(MyCopy( buffer, 63, 30));
    dest_info.id3_year:= Trim(MyCopy( buffer, 93, 4));

		//ir3v1.1 detected*/
		if ((buffer[125] <> 0) or (buffer[126] = 0)) then
    begin
			dest_info.id3_version:= 'ID3v1.0';
      dest_info.id3_comment:= Trim(MyCopy( buffer, 97, 30));

    end
		else
    begin
      dest_info.id3_track:= buffer[126];
      dest_info.id3_version:= 'ID3v1.1';
      dest_info.id3_comment:= Trim(MyCopy( buffer, 97, 28));
    end;

		Result:= 0;
    exit;

	end;

	Result:= -1;
end;

function LameInfoToString(const l: TLameInfo): AnsiString;
begin
//Format('%s.%d %s %u %u %u %u %u %u',
//        [dest_info.lame_version, dest_info.lame_tagversion, dest_info.lame_vbrmethod,
//        buffer[offset+$9b], buffer[offset+$A6], ((buffer[offset+$AF] and $F0) shr 4), (buffer[offset+$AF] and $0F), buffer[offset+$B0],  buffer[offset+$B4]]);
  Result:= l.error;
  if l.lame_present then
  Result:= Format('%s.%d %s %s %s %d %s %d %d %d %d',
    [
      l.lame_version,
      l.lame_tagversion,
      l.lame_vbrmethod,
      l.lame_preset,
      l.lame_stereomode,
      l.lame_bitrate,
      l.lame_lowpass ,
      l.lame_ath,
      Integer(l.lame_noiseshaping),
      Integer(l.lame_unwise),
      l.lame_gain
    ]
  );
end;

function ID3v1InfoToString( const i: TID3v1Info): AnsiString;
begin
  Result:= '';
  if i.id3_present then
    Result:= i.id3_version+' '+i.id3_genre;
end;



function MP3Info(filename: AnsiString): AnsiString;
var i: Integer;
    buf1: array[0..LAME_BUFFER_SIZE-1] of Byte;
    buf2: array[0..ID3V1_BUFFER_SIZE-1] of Byte;
    s: Integer;
    re1: TLameInfo;
    re2: TID3v1Info;
    fd: TFileStream;
begin
  try
    fd:= TFileStream.Create(filename, fmOpenRead);
    try
  	 while(true) do
     begin
       s:= fd.Read(buf1, LAME_BUFFER_SIZE);
		   if (s < LAME_BUFFER_SIZE) then
		   begin
			   Result:= 'ERROR: IO error';
			   exit;
		   end;

       i := Lame_Check(buf1, 0, re1);
		   if (i > 0) then
			   fd.Seek(i, soFromBeginning)
		   else
			   break;
		 end;

     fd.Seek(-ID3V1_BUFFER_SIZE, soFromEnd);
     s:= fd.Read(buf2, ID3V1_BUFFER_SIZE);
 		 if (128 = s) then
  	 begin
	     ID3_Check(buf2, re2);
       Result:= LameInfoToString( re1 ) +' / '+ID3v1InfoToString( re2 );
     end else
       Result:= 'ERROR: IO error';
       
    finally
      fd.Free;
    end;
	except
		Result:= 'ERROR: Couldnt open '+ filename;
  end;
end;


end.
