unit uImport;
(*
 Import a custom character definition.
 Supported are the formats BIN, HEX, DEC.
 ENSURE TO SET THE RIGHT FORMAT.
 Examples:
 BIN: 0b00100,0b00100,0b01010,0b01010,0b10001,0b10001,0b10001,0b01110
 BIN: B00100,B00100,B01010,B01010,B10001,B10001,B10001,B01110
 HEX: 0x0E,0x1B,0x11,0x11,0x11,0x11,0x11,0x1F
 DEC: 14,27,17,17,17,17,17,31
 20210428 rwbl
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ActnList, ClipBrd;

type
  { TFormCharImport }
  TFormCharImport = class(TForm)
    ActionPaste: TAction;
    ActionClear: TAction;
    ActionCancel: TAction;
    ActionImport: TAction;
    ActionListCharImport: TActionList;
    ButtonCancel: TButton;
    ButtonClear: TButton;
    ButtonPaste: TButton;
    ButtonImport: TButton;
    ComboBoxCustomCharFormat: TComboBox;
    MemoCustomCharPixels: TMemo;
    PanelBottom: TPanel;
    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionImportExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    //
  public
    // Custom character definition as comma delimieted string
    CustomCharPixels: String;
    // Selected format BIN,HEX or DEC
    CustomCharFormat: String;
  end;

var
  FormCharImport: TFormCharImport;

implementation

{$R *.lfm}

{ TFormCharImport }

procedure TFormCharImport.ActionImportExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFormCharImport.ActionCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormCharImport.ActionClearExecute(Sender: TObject);
begin
  MemoCustomCharPixels.Lines.Clear;
end;

procedure TFormCharImport.ActionPasteExecute(Sender: TObject);
begin
  MemoCustomCharPixels.Text := Clipboard.AsText;
end;

(*
  Handle the result.
  In case button Import, set the CustomCharCode to the Memo content.
  In the button events, the modalresult is set.
*)
procedure TFormCharImport.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := true;
  CustomCharFormat := ComboBoxCustomCharFormat.Text;
  if ModalResult = mrOK then
    CustomCharPixels := MemoCustomCharPixels.Lines.Text
  else
    CustomCharPixels := '';
end;

end.

