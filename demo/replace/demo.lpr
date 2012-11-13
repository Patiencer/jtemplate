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
    VView.Fields.Add('title', 'Demo');
    VView.Fields.Add('hr', '<hr />');
    VView.Fields.Add('test.hello', 'JTemplate demo');
    VView.Fields.Add('hr', '<hr />');
    VView.Replace;
    Write(VView.Content);
  finally
    VView.Free;
  end;
end.

