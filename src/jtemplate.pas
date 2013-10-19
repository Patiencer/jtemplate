(*
  J-Template plugin.
  Copyright (C) 2012-2014 Silvio Clecio.

  Please see the LICENSE, README and AUTHORS files.
*)

unit JTemplate;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StrUtils, Classes, FPJSON;

type
  EJTemplate = class(Exception);

  { TJTemplate }

  TJTemplate = class
  private
    FContent: string;
    FFields: TJSONObject;
    FHTMLSupports: Boolean;
    FTagEscape: ShortString;
    FTagPrefix: ShortString;
    FTagSuffix: ShortString;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromFile(const AFileName: TFileName);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: TFileName);
    procedure Replace(const ARecursive: Boolean = False); overload;
    property Content: string read FContent write FContent;
    property Fields: TJSONObject read FFields;
    property HTMLSupports: Boolean read FHTMLSupports write FHTMLSupports;
    property TagPrefix: ShortString read FTagPrefix write FTagPrefix;
    property TagSuffix: ShortString read FTagSuffix write FTagSuffix;
    property TagEscape: ShortString read FTagEscape write FTagEscape;
  end;

resourcestring
  SNilParamError = '"%s" must not be nil.';

const
  LatinCharsCount = 74;
  LatinChars: array[0..LatinCharsCount] of string = (
    '"', '<', '>', '^', '~', '£', '§', '°', '²', '³', 'µ', '·', '¼', '½', '¿',
    'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î',
    'Ï', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'ß', 'á', 'à',
    'â', 'ã', 'ä', 'å', 'æ', 'ç', 'é', 'è', 'ê', 'ë', 'ì', 'í', 'î', 'ï', 'ñ',
    'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ù', 'ú', 'û', 'ü', 'ý', 'ÿ', '&', '´', '`');
  HtmlChars: array[0..LatinCharsCount] of string = (
    '&quot;', '&lt;', '&gt;', '&circ;', '&tilde;', '&pound;', '&sect;', '&deg;',
    '&sup2;', '&sup3;', '&micro;', '&middot;', '&frac14;', '&frac12;', '&iquest;',
    '&Agrave;', '&Aacute;', '&Acirc;', '&Atilde;', '&Auml;', '&Aring;', '&AElig;',
    '&Ccedil;', '&Egrave;', '&Eacute;', '&Ecirc;', '&Euml;', '&Igrave;', '&Iacute;',
    '&Icirc;', '&Iuml;', '&Ntilde;', '&Ograve;', '&Oacute;', '&Ocirc;', '&Otilde;',
    '&Ouml;', '&Ugrave;', '&Uacute;', '&Ucirc;', '&Uuml;', '&Yacute;', '&szlig;',
    '&aacute;', '&agrave;', '&acirc;', '&atilde;', '&auml;', '&aring;', '&aelig;',
    '&ccedil;', '&eacute;', '&egrave;', '&ecirc;', '&euml;', '&igrave;', '&iacute;',
    '&icirc;', '&iuml;', '&ntilde;', '&ograve;', '&oacute;', '&ocirc;', '&otilde;',
    '&ouml;', '&divide;', '&ugrave;', '&uacute;', '&ucirc;', '&uuml;', '&yacute;',
    '&yuml;', '&amp;', '&acute;', '&grave;');

function StrToHtml(const S: string): string;

implementation

function StrToHtml(const S: string): string;

  function _Found(const ABuf: PChar; const ALen: Integer): Integer; inline;
  var
    P: PString;
  begin
    for Result := Low(LatinChars) to High(LatinChars) do
    begin
      P := @LatinChars[Result];
      if Length(P^) <= ALen then
        if CompareByte(P^[1], ABuf^, Length(P^)) = 0 then
          // compare in blocks of 8(x64), 4, 2 and 1 byte
          Exit;
    end;
    Result := -1;
  end;

var
  I: Integer;
  VResStr: string;
  PComp, PLast: PChar;
begin
  VResStr := '';
  PComp := @S[1];
  PLast := PComp + Length(S);
  while PComp < PLast do
  begin
    I := _Found(PComp, PLast - PComp);
    if I > -1 then
    begin
      VResStr := VResStr + HtmlChars[I];
      Inc(PComp, Length(LatinChars[I]));
    end
    else
    begin
      VResStr := VResStr + PComp^; // it can be optimized decreasing the concatenations
      Inc(PComp);
    end;
  end;
  Result := VResStr;
end;

{ TJTemplate }

constructor TJTemplate.Create;
begin
  FFields := TJSONObject.Create;
  FTagPrefix := '@';
  FHTMLSupports := True;
end;

destructor TJTemplate.Destroy;
begin
  FreeAndNil(FFields);
  inherited Destroy;
end;

procedure TJTemplate.LoadFromStream(AStream: TStream);
begin
  if not Assigned(AStream) then
    raise EJTemplate.CreateFmt(SNilParamError, ['AStream']);
  AStream.Seek(0, 0);
  SetLength(FContent, AStream.Size);
  AStream.Read(Pointer(FContent)^, Length(FContent));
end;

procedure TJTemplate.LoadFromFile(const AFileName: TFileName);
var
  VFile: TFileStream;
begin
  VFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(VFile);
  finally
    VFile.Free;
  end;
end;

procedure TJTemplate.SaveToStream(AStream: TStream);
begin
  if not Assigned(AStream) then
    raise EJTemplate.CreateFmt(SNilParamError, ['AStream']);
  AStream.Seek(0, 0);
  AStream.Write(Pointer(FContent)^, Length(FContent));
end;

procedure TJTemplate.SaveToFile(const AFileName: TFileName);
var
  VFile: TFileStream;
begin
  VFile := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(VFile);
  finally
    VFile.Free;
  end;
end;

procedure TJTemplate.Replace(const ARecursive: Boolean);
var
  VName, VValue: string;
  I, P, VTagLen, VEscapLen: Integer;
begin
  VEscapLen := Length(FTagEscape);
  for I := 0 to Pred(FFields.Count) do
  begin
    VName := FTagPrefix + FFields.Names[I] + FTagSuffix;
    if FHTMLSupports then
      VValue := StrToHtml(FFields.Items[I].AsString)
    else
      VValue := FFields.Items[I].AsString;
    P := 1;
    VTagLen := Length(VName);
    repeat
      P := PosEx(VName, FContent, P);
      if P < 1 then
        Break;
      if (VEscapLen <> 0) and // no TagEscape defined
        (CompareChar(FContent[P - VEscapLen], FTagEscape[1], VEscapLen) = 0) then
      begin
        System.Delete(FContent, P - VEscapLen, VEscapLen);
        Inc(P, VTagLen - VEscapLen);
      end
      else
      begin
        System.Delete(FContent, P, VTagLen);
        Insert(VValue, FContent, P);
        Inc(P, Length(VValue));
        if not ARecursive then
          Break;
      end;
    until False;
  end;
end;

end.
