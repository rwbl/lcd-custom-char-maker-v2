program lcdccm;
(*
  Project lcd-custom-char-maker-v2.
  20210428 rwbl
*)
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, uMisc, uImport, ucode;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='LCD Custom Char Maker';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormCharImport, FormCharImport);
  Application.CreateForm(TFormCode, FormCode);
  Application.Run;
end.

