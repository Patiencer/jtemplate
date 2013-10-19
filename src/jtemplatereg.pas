(*
  J-Template plugin.
  Copyright (C) 2012-2014 Silvio Clecio.

  Please see the LICENSE, README and AUTHORS files.
*)

unit JTemplateReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, JTemplate;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('JTemplate', [TJTemplate]);
end;

end.
