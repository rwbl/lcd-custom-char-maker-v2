unit ucode;
(*
 Code viewer.
 20210428 rwbl
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ClipBrd, ActnList, Buttons, SynEdit, SynHighlighterCpp, SynHighlighterVB,
  SynHighlighterPython, SynHighlighterBat;

type

  { TFormCode }

  TFormCode = class(TForm)
    ActionCopyCode: TAction;
    ActionList: TActionList;
    PanelTop: TPanel;
    SpeedButton1: TSpeedButton;
    SynBatSyn: TSynBatSyn;
    SynEditCode: TSynEdit;
    SynCPPSyn: TSynCppSyn;
    SynB4RSyn: TSynVBSyn;
    SynPythonSyn: TSynPythonSyn;
    procedure ActionCopyCodeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormCode: TFormCode;

implementation

{$R *.lfm}

{ TFormCode }

procedure TFormCode.ActionCopyCodeExecute(Sender: TObject);
begin
  Clipboard.AsText := SynEditCode.Lines.Text;
end;

procedure TFormCode.FormCreate(Sender: TObject);
begin
  SynCPPSyn.AddSpecialAttribute('uint8_t','uint8_t');
end;

end.

