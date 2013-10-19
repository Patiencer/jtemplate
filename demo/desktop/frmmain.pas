unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Ipfilebroker, IpHtml, Forms, StdCtrls, JTemplate;

type

  { TfrMain }

  TfrMain = class(TForm)
    btReplace: TButton;
    dp: TIpFileDataProvider;
    edResult: TIpHtmlPanel;
    JTemplate1: TJTemplate;
    procedure btReplaceClick(Sender: TObject);
  end;

var
  frMain: TfrMain;

implementation

{$R *.lfm}

{ TfrMain }

procedure TfrMain.btReplaceClick(Sender: TObject);
begin
  JTemplate1.Fields.Add('title', 'My title');
  JTemplate1.Fields.Add('body', 'My body');
  JTemplate1.Replace;
  edResult.SetHtmlFromStr(JTemplate1.Content.Text);
  Caption := edResult.Title;
end;

end.

