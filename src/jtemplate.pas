(*
  J-TEmplate plugin.
  Copyright (C) 2012-2014 Silvio Clecio.

  Please see the LICENSE file.
*)

unit JTemplate;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StrUtils, Classes, FPJSON;

type
  TJTemplate = class
  private
    FContent: string;
    FFields: TJSONObject;
    FHTMLSupports: Boolean;
    FTagEscape: ShortString;
    FTagPrefix: ShortString;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromFile(const AFileName: TFileName);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: TFileName);
    procedure Replace; overload;
    procedure Replace(const ARecursive: Boolean); overload;
    property Content: string read FContent write FContent;
    property Fields: TJSONObject read FFields;
    property HTMLSupports: Boolean read FHTMLSupports write FHTMLSupports;
    property TagPrefix: ShortString read FTagPrefix write FTagPrefix;
    property TagEscape: ShortString read FTagEscape write FTagEscape;
  end;

const
  LatinCharsCount = 75;
  LatinChars: array[1..LatinCharsCount] of string = (
    '"', '<', '>', '^', '~', '£', '§', '°', '²', '³', 'µ', '·', '¼', '½', '¿',
    'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î',
    'Ï', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'ß', 'á', 'à',
    'â', 'ã', 'ä', 'å', 'æ', 'ç', 'é', 'è', 'ê', 'ë', 'ì', 'í', 'î', 'ï', 'ñ',
    'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ù', 'ú', 'û', 'ü', 'ý', 'ÿ', '&', '´', '`');
  HtmlChars: array[1..LatinCharsCount] of string = (
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
var
  L, C: Integer;
  VFound: Boolean;
  PComp, PSrc, PLast: PChar;
  VResStr, VCompStr: string;
begin
  L := Length(LatinChars);
  if L <> Length(HtmlChars) then
    raise Exception.Create(SErrAmountStrings);
  Dec(L);
  VCompStr := S;
  VResStr := '';
  PSrc := @S[1];
  PComp := @VCompStr[1];
  PLast := PComp + Length(S);
  while PComp < PLast do
  begin
    VFound := False;
    for C := 0 to L do
    begin
      if (Length(LatinChars[C]) > 0) and (LatinChars[C][1] = PComp^) and
        (Length(LatinChars[C]) <= (PLast - PComp)) and
        (CompareByte(LatinChars[C][1], PComp^, Length(LatinChars[C])) = 0) then
      begin
        VResStr := VResStr + HtmlChars[C];
        PComp := PComp + Length(LatinChars[C]);
        PSrc := PSrc + Length(LatinChars[C]);
        VFound := True;
      end;
    end;
    if not VFound then
    begin
      VResStr := VResStr + PSrc^;
      Inc(PComp);
      Inc(PSrc);
    end;
  end;
  Result := VResStr;
end;

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
    Exit;
  AStream.Position := 0;
  SetLength(FContent, AStream.Size);
  AStream.Read(Pointer(FContent)^, AStream.Size);
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
    Exit;
  AStream.Position := 0;
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

procedure TJTemplate.Replace;
var
  VName, VValue: string;
  I, L, J, P, T: Integer;
begin
  if FTagEscape = '' then
    for I := 0 to Pred(FFields.Count) do
    begin
      VName := FTagPrefix + FFields.Names[I];
      if FHTMLSupports then
        VValue := StrToHtml(FFields.Items[I].AsString)
      else
        VValue := FFields.Items[I].AsString;
      for J := 1 to Length(FContent) do
      begin
        P := Pos(VName, FContent);
        if P <> 0 then
        begin
          L := Length(VName);
          System.Delete(FContent, P, L);
          Insert(VValue, FContent, P);
          Break;
        end;
      end;
    end
  else
    for I := 0 to Pred(FFields.Count) do
    begin
      VName := FTagPrefix + FFields.Names[I];
      if FHTMLSupports then
        VValue := StrToHtml(FFields.Items[I].AsString)
      else
        VValue := FFields.Items[I].AsString;
      for J := 1 to Length(FContent) do
      begin
        P := Pos(VName, FContent);
        if P <> 0 then
        begin
          T := Length(FTagEscape);
          if Copy(FContent, P - T, T) = FTagEscape then
          begin
            L := Length(VName);
            System.Delete(FContent, P - T, L + T);
            Insert(VName, FContent, P - T);
          end
          else
          begin
            L := Length(VName);
            System.Delete(FContent, P, L);
            Insert(VValue, FContent, P);
          end;
          Break;
        end;
      end;
    end;
end;

procedure TJTemplate.Replace(const ARecursive: Boolean);
var
  VName, VValue: string;
  E, I, L, J, P, T: Integer;
begin
  if ARecursive then
  begin
    if FTagEscape = '' then
      for I := 0 to Pred(FFields.Count) do
      begin
        E := 1;
        VName := FTagPrefix + FFields.Names[I];
        if FHTMLSupports then
          VValue := StrToHtml(FFields.Items[I].AsString)
        else
          VValue := FFields.Items[I].AsString;
        for J := 1 to Length(FContent) do
        begin
          P := PosEx(VName, FContent, E);
          if P > E then
            E := P;
          if P <> 0 then
          begin
            L := Length(VName);
            System.Delete(FContent, P, L);
            Insert(VValue, FContent, P);
          end;
        end;
      end
    else
      for I := 0 to Pred(FFields.Count) do
      begin
        E := 1;
        VName := FTagPrefix + FFields.Names[I];
        if FHTMLSupports then
          VValue := StrToHtml(FFields.Items[I].AsString)
        else
          VValue := FFields.Items[I].AsString;
        for J := 1 to Length(FContent) do
        begin
          P := PosEx(VName, FContent, E);
          if P > E then
            E := P;
          if P <> 0 then
          begin
            T := Length(FTagEscape);
            if Copy(FContent, P - T, T) = FTagEscape then
            begin
              L := Length(VName);
              System.Delete(FContent, P - T, L + T);
              Insert(VName, FContent, P - T);
            end
            else
            begin
              L := Length(VName);
              System.Delete(FContent, P, L);
              Insert(VValue, FContent, P);
            end;
          end;
        end;
      end;
  end
  else
    Replace;
end;

end.
