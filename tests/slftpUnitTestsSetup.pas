unit slftpUnitTestsSetup;

interface

{* calls might depend on each other, so order might be important *}
procedure InitialConfigSetup;
procedure InitialDebugSetup;
procedure InitialKbSetup;

implementation

uses
  configunit, debugunit, encinifile, kb;

procedure InitialConfigSetup;
begin
  // works with the default values only
  config := TEncIniFile.Create('test', 'nopw');
end;

procedure InitialDebugSetup;
begin
  DebugInit;
end;

procedure InitialKbSetup;
begin
  kb_Init;
  tvtags.DelimitedText := 'AHDTV APDTV ADSR BDRip BluRay DSR DVDR DVDRip HDTV HDTVRip HR.PDTV PDTV WebRip WebHD SATRip dTV';
  kb_languages.DelimitedText := ReplaceText(ReplaceText('Baltic,HebSub,Abkhazian,Afar,Afrikaans,Akan,Albanian,Amharic,Angika,Arabic,Aragonese,Armenian,Assamese,Avaric,Avestan,Aymara,Azerbaijani,Bambara,Bashkir,Basque,Belarusian,Bengali,BHANGRA,Bihari,Bislama,Bosnian,Breton,Bulgarian,Burmese,Castilian,Catalan,CentralAKhmer,
Chamorro,Chechen,Chewa,Chichewa,Chinese,Chuang,Church-Slavic,Church-Slavonic,Chuvash,Cornish,Corsican,Cree,Croatian,Czech,Danish,Dhivehi,Divehi,DKDUB,Dutch,Dzongkha,Esperanto,Estonia,Estonian,Ewe,Faroese,Fijian,Finnish,Flemish,French,Fulah,Gaelic,Galician,Ganda,Georgian,German,Gikuyu,Greek,
Greenlandic,Guarani,Gujarati,Haitian,Haitian-Creole,Hausa,Hebrew,Hebrew,Herero,Hindi,Hiri-Motu,
Hungarian,Icelandic,Ido,Igbo,Indonesian,Indonesian,Interlingua,Interlingue,Inuktitut,Inupiaq,Irish,Italian,Japanese,Javanese,Kalaallisut,Kannada,Kanuri,Kashmiri,Kazakh,Kikuyu,Kinyarwanda,Kirghiz,Komi,Kongo,Korean,Kuanyama,Kurdish,Kwanyama,Kyrgyz,Lao,
Latin,Latvian,Letzeburgesch,Limburgan,Limburger,Limburgish,Lingala,Lithuanian,Luba-Katanga,Luxembourgish,Macedonian,Malagasy,Malay,Malayalam,Maldivian,Maltese,Manx,Maori,Marathi,
Marshallese,Moldavian,Mongolian,Nauru,Navaho,Navajo,Ndonga,Nepali,NORDIC,North-Ndebele,Northern-Frisian,Northern-Sami,Norwegian,Norwegian-Bokm-l,Norwegian-Nynorsk,Nuosu,Nyanja,
Occidental,Occitan,Ojibwa,Old-Bulgarian,Old-Church-Slavonic,Old-Slavonic,Oriya,Oromo,Ossetian,Ossetic,Pali,Panjabi,Pashto,Persian,PLDUB,Polish,Portuguese,Proven-al,Punjabi,Pushto,
Quechua,Romanian,Romansh,Rundi,Russian,Samoan,Sango,Sanskrit,Sardinian,Scottish-Gaelic,Serbian,Serbo-Croatian,Shona,Sichuan-Yi,Sindhi,Sinhala,Sinhalese,SLOSUBS,Slovak,Slovenian,Somali,South-Ndebele,Southern-Sotho,
Spanish,Sundanese,Swahili,Swati,Swedish,SWEDUB,Tagalog,Tahitian,Tajik,Tamil,Tatar,Telugu,Thai,Tibetan,Tigrinya,Tonga,Tsonga,Tswana,Turk,Turkish,Turkmen,Twi,Uighur,Ukrainian,Urdu,Uyghur,Uzbek,Valencian,Venda,Vietnamese,
Volap-k,Walloon,Welsh,Western-Frisian,Wolof,Xhosa,Yiddish,Yiddish,Yoruba,Zhuang,Zulu', #13, ''), #10, '');
end;

end.