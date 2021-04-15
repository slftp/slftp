{
  @abstract(Knowledge base classes for release information)
  The classes hold different information which are needed for all the different
  categories of releases
}
unit kb.releaseinfo;

interface

uses
  kb.releaseinfo.basic;

type
  { Different types of parsing language from releasename
    @definitionList(
      @itemLabel(lpm_default)
      @item(Means that the language parsing follows the normal naming scheme)

      @itemLabel(lpm_audio)
      @item(Means that the language is parsed by looking for short language codes)

      @itemLabel(lpm_musicvideo)
      @item(Means that the language is parsed by looking for short language codes)
    )
  }
  TLanguageParsingMode = (lpm_default, lpm_audio, lpm_musicvideo);

type
  { basic info parsed from the releasename }
  IBasicInfo = interface
  ['{7373295a-99e7-4b6b-82ac-def8ef80b397}']
    { Creates the class and extracts all the needed information from the releasename
      @param(aRefController reference to the outer reference count used)
      @param(aSection sectionname)
      @param(aRlsname releasename) }
    constructor Create(const aRefController: IUnknown; const aSection, aRlsname: String); // parses releasename

    { Get the instance of the used information store
      @returns(used information store instance) }
    function GetBasicInfo: TBasicInfoStore;

    property BasicInfo: TBasicInfoStore read GetBasicInfo; // --> allows to use rls.BasicInfo.<property_name>
  end;

  { pre information of the release }
  IPreInfo = interface
  ['{7373295a-99e7-4b6b-82ac-def8ef80b397}']
    { Creates the class
      @param(aRefController reference to the outer reference count used)
      @param(aSection sectionname)
      @param(aRlsname releasename) }
    constructor Create(const aRefController: IUnknown; const aSection, aRlsname: String);

    // TODO: Update function for the single pretime infos

    { Get the instance of the used information store
      @returns(used information store instance) }
    function GetPreInfo: TPreInfoStore;

    property PreInfo: TPreInfoStore read GetPreInfo;
  end;

  { 0-DAY information of the release }
  IZeroDayInfo = interface
  ['{7373295a-99e7-4b6b-82ac-def8ef80b397}']
    { Creates the class
      @param(aRefController reference to the outer reference count used)
      @param(aSection sectionname)
      @param(aRlsname releasename)
      @param(aZeroDaySource target operating system) }
    constructor Create(const aRefController: IUnknown; const aSection, aRlsname, aZeroDaySource: String);

    { Get the instance of the used information store
      @returns(used information store instance) }
    function GetZeroDayInfo: TZeroDayInfoStore;

    property ZeroDayInfo: TZeroDayInfoStore read GetZeroDayInfo;
  end;

// TRelease
// TTvRelease


implementation

uses
  SysUtils, debugunit, RegExpr;

const
  rsections = 'kb.releaseinfo';



end.

