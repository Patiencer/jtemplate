program demo;

{$mode objfpc}{$H+}

uses
  JTemplate;

var
  VView: TJTemplate;
begin
  VView := TJTemplate.Create;
  try
    VView.LoadFromFile('test.html');
    VView.Fields.Add('value', 'a"b"c');
    VView.Replace;
    Write(VView.Content);
  finally
    VView.Free;
  end;
end.

