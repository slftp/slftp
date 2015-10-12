{(*  TODO:   *)
# is it thread safe?
# need encrpytion support that we can use XML as configfile?
}
unit xmlwrapper;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  DOM, XMLWrite, XMLRead
{$ELSE}
  ActiveX, Variants, xmldom, XMLIntf, msxmldom, XMLDoc
{$ENDIF}
  , Classes, SysUtils, irc;

type
{$IFDEF FPC}
  TSLXMLNode = TDOMNode;
  TSLXMLDoc = TXMLDocument;
{$ELSE}
  TSLXMLNode = IXMLNode;
  TSLXMLDoc = IXMLDocument;
{$ENDIF}

  TSLXMLDocument = class
  private
    xmlFile: TSLXMLDoc;
    FFilename: string;
  public
    constructor Create; overload;
    constructor Create(Filename: string); overload;
    constructor Create(stream: TStream); overload;
    destructor Destroy; override;
    function FindNode(NodeName: string): TSLXMLNode;
    function NextSibling(InNode: TSLXMLNode): TSLXMLNode;
    function NextSiblingEx(InNode: TSLXMLNode; NodeName: string): TSLXMLNode;
    function GetDocumentElement: TSLXMLNode;
    function GetNodeName(InNode: TSLXMLNode): string;
    function GetFirstChild(InNode: TSLXMLNode): TSLXMLNode;
    function GetAttributeNode(InNode: TSLXMLNode; AttrName: string): TSLXMLNode;
    function GetNodeValue(InNode: TSLXMLNode): string;
    procedure SetNodeValue(InNode: TSLXMLNode; Value: string);
    function FindChildNode(InNode: TSLXMLNode; NodeName: string): TSLXMLNode;
    function GetChildNodeCount(InNode: TSLXMLNode): integer;
    function GetChildNodeItem(InNode: TSLXMLNode; Index: integer): TSLXMLNode;
    function GetAttributesCount(InNode: TSLXMLNode): integer;
    function GetAttributeNodeByIndex(InNode: TSLXMLNode;
      Index: integer): TSLXMLNode;
    function AddChild(InNode: TSLXMLNode; Name: string): TSLXMLNode;

    procedure LoadFromFile(XMLFileName: string);
    procedure LoadFromStream(XMLFile: TStream);
    procedure LoadFromWeb(url: string);
    //    procedure LoadFromEncrpytedFile(XMLFileName: String);
    //    procedure SaveToEncryptedFile(XMLFileName: String);
    procedure SaveToFile(XMLFileName: string);
  end;

procedure InitXMLWeapper;
procedure UninitXMLWeapper;

implementation

uses slhttp;

procedure InitXMLWeapper;
begin
{$IFDEF MSWINDOWS}
 // CoInitialize(nil);
{$ENDIF}

end;

procedure UninitXMLWeapper;
begin
{$IFDEF MSWINDOWS}
  CoUninitialize;
{$ENDIF}
end;

constructor TSLXMLDocument.Create;
begin
  inherited;
{$IFDEF FPC}
  xmlFile := TXMLDocument.Create;
{$ELSE}
  CoInitialize(nil);
  xmlFile := TXMLDocument.Create(nil);
  xmlFile.Options := [doNodeAutoCreate, doAttrNull, doAutoPrefix, doNamespaceDecl, doNodeAutoIndent];
  xmlfile.Active := True;
  xmlFile.Version := '1.0';
{$ENDIF}
end;

constructor TSLXMLDocument.Create(Filename: string);
begin
  Create;
  LoadFromFile(filename);
end;

constructor TSLXMLDocument.Create(stream: TStream);
begin
  Create;
  LoadFromStream(stream);
end;

procedure TSLXMLDocument.LoadFromFile(XMLFileName: string);
begin
  FFilename := XMLFileName;
{$IFDEF FPC}
  ReadXMLFile(xmlFile, XMLFileName);
{$ELSE}
  xmlFile.Active := False;
  xmlFile.LoadFromFile(XMLFileName);
  xmlFile.Active := True;
{$ENDIF}
end;

procedure TSLXMLDocument.SaveToFile(XMLFileName: string);
begin
{$IFDEF FPC}
  writeXMLFile(xmlFile, XMLFileName);
{$ELSE}
  xmlFile.SaveToFile(XMLFileName);
{$ENDIF}
end;

procedure TSLXMLDocument.LoadFromStream(XMLFile: TStream);
begin
  XMLFile.Position := 0;
{$IFDEF FPC}
  ReadXMLFile(self.xmlFile, xmlFile);
{$ELSE}
  self.xmlFile.LoadFromStream(xmlfile);
{$ENDIF}
end;

procedure TSLXMLDocument.LoadFromWeb(url: string);
var
  st: TStream;
  res: string;
begin
  res := slUrlGet(url);
  (*
  if res = '' then begin
  //Trow excption
  end;
  *)
  st := TStringStream.Create(res);
  try
    st.Position := 0;
{$IFDEF FPC}
    ReadXMLFile(xmlFile, st);
{$ELSE}
    xmlFile.LoadFromStream(st);
{$ENDIF}
  finally
    st.Free;
  end;
end;

destructor TSLXMLDocument.Destroy;
begin
{$IFDEF FPC}
  xmlFile.Free;
  xmlFile := nil;
{$ELSE}
    xmlFile := nil;
    //  CoUninitialize;
{$ENDIF}
  inherited;
end;

function TSLXMLDocument.GetDocumentElement: TSLXMLNode;
begin
  Result := xmlfile.DocumentElement;
end;

function TSLXMLDocument.FindNode(NodeName: string): TSLXMLNode;
begin
{$IFDEF FPC}
  Result := xmlFile.FindNode(NodeName);
{$ELSE}
  Result := xmlFile.Node;
  if (Assigned(Result)) then
    Result := xmlFile.Node.ChildNodes.FindNode(NodeName);
{$ENDIF}
end;

function TSLXMLDocument.NextSibling(InNode: TSLXMLNode): TSLXMLNode;
begin
{$IFDEF FPC}
  Result := InNode.NextSibling;
{$ELSE}
  Result := InNode.NextSibling;
{$ENDIF}
end;

function TSLXMLDocument.GetNodeName(InNode: TSLXMLNode): string;
begin
{$IFDEF FPC}
  Result := InNode.NodeName
{$ELSE}
  Result := InNode.GetNodeName;
{$ENDIF}
end;

function TSLXMLDocument.NextSiblingEx(InNode: TSLXMLNode;
  NodeName: string): TSLXMLNode;
var
  tmpStr: string;
begin
  Result := NextSibling(InNode);
  if (not Assigned(Result)) then
    exit;
  tmpStr := GetNodeName(Result);
  while (Assigned(Result) and not SameText(NodeName, tmpStr)) do
  begin
    Result := NextSibling(Result);
    if (Assigned(Result)) then
      tmpStr := GetNodeName(Result);
  end;
end;

function TSLXMLDocument.GetFirstChild(InNode: TSLXMLNode): TSLXMLNode;
begin
{$IFDEF FPC}
  Result := InNode.FirstChild;
{$ELSE}
  Result := InNode.GetChildNodes.First;
{$ENDIF}
end;

function TSLXMLDocument.GetAttributeNode(InNode: TSLXMLNode;
  AttrName: string): TSLXMLNode;
begin
{$IFDEF FPC}
  Result := InNode.Attributes.GetNamedItem(AttrName);
{$ELSE}
  Result := InNode.GetAttributeNodes.FindNode(AttrName);
{$ENDIF}
end;

function TSLXMLDocument.GetNodeValue(InNode: TSLXMLNode): string;
begin
{$IFDEF FPC}
  Result := InNode.TextContent;
{$ELSE}
  Result := InNode.Text;
{$ENDIF}
  Result := Trim(Result);
end;

procedure TSLXMLDocument.SetNodeValue(InNode: TSLXMLNode; Value: string);
begin
{$IFDEF FPC}
  InNode.TextContent := Value;
{$ELSE}
  InNode.SetNodeValue(Value);
{$ENDIF}
end;

function TSLXMLDocument.FindChildNode(InNode: TSLXMLNode;
  NodeName: string): TSLXMLNode;
begin
{$IFDEF FPC}
  Result := InNode.FindNode(NodeName);
{$ELSE}
  Result := InNode.GetChildNodes.FindNode(NodeName);
{$ENDIF}
end;

function TSLXMLDocument.GetChildNodeCount(InNode: TSLXMLNode): integer;
begin
  Result := InNode.ChildNodes.Count;
end;

function TSLXMLDocument.GetChildNodeItem(InNode: TSLXMLNode;
  Index: integer): TSLXMLNode;
begin
{$IFDEF FPC}
  Result := InNode.ChildNodes.Item[Index];
{$ELSE}
  Result := InNode.ChildNodes.Get(Index);
{$ENDIF}
end;

function TSLXMLDocument.GetAttributeNodeByIndex(InNode: TSLXMLNode;
  Index: integer): TSLXMLNode;
begin
{$IFDEF FPC}
  Result := InNode.Attributes.Item[Index];
{$ELSE}
  Result := InNode.AttributeNodes.Get(Index);
{$ENDIF}
end;

function TSLXMLDocument.GetAttributesCount(InNode: TSLXMLNode): integer;
begin
{$IFDEF FPC}
  Result := InNode.Attributes.Length;
{$ELSE}
  Result := InNode.AttributeNodes.Count;
{$ENDIF}
end;

function TSLXMLDocument.AddChild(InNode: TSLXMLNode; Name: string): TSLXMLNode;
begin
{$IFDEF FPC}
  Result := xmlFile.CreateElement(Name);
  if (not Assigned(InNode)) then
    xmlFile.AppendChild(Result)
  else
    InNode.AppendChild(Result);
{$ELSE}
  if (Assigned(InNode)) then
    Result := InNode.AddChild(Name)
  else
    Result := xmlFile.AddChild(Name);

{$ENDIF}
end;

end.

