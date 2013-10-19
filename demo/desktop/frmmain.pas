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
    procedure JTemplate1Replace(Sender: TObject);
  end;

var
  frMain: TfrMain;

implementation

{$R *.lfm}

{ TfrMain }

procedure TfrMain.btReplaceClick(Sender: TObject);
begin
  JTemplate1.Fields.Strings['title'] := 'My title';
  JTemplate1.Fields.Strings['body'] := 'My body';
  JTemplate1.Replace;
end;

procedure TfrMain.JTemplate1Replace(Sender: TObject);
begin
  edResult.SetHtmlFromStr(JTemplate1.Content.Text);
  Caption := edResult.Title;
end;

end.

