unit uMain;
(*
  Main unit for the lcd-custom-char-maker-v2 project.
  20210428 rwbl
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ActnList, Menus,
  StrUtils, ClipBrd, Buttons,
  uMisc,
  uImport,
  uCode;

const
  APPVERSION          = 'v1.0.0 (Build 20210428)';
  APPTITLE            = 'LCD Custom Char Maker';
  APPTITLEVERSION     = APPTITLE + ' ' + APPVERSION;
  APPDESCRIPTION      = APPTITLEVERSION + LineEnding + LineEnding +
    'Open Source Project for creating Custom Characters for LCD displays connected to Arduino, Raspberry Pi, Tinkerforge or other.' + LineEnding +
    '(c) 2021 by Robert W.B. Linn, Pinneberg, Germany'+LineEnding+LineEnding+
    'Developed with Lazarus v2.0.12, FPC 3.2.0, SVN 64642.'+LineEnding+LineEnding+
    'Application for personal use only under the GNU GENERAL PUBLIC LICENSE Version 3.';

  // The char matrix has 8 rows x 5 cols pixels
  CHARMATRIX_ROWS              = 8;
  CHARMATRIX_COLS              = 5;
  CHARMATRIX_CHARWIDTH         = 30;
  CHARMATRIX_CHARHEIGHT        = 40;
  CHARMATRIX_CHARLOW_TAG       = 0;
  CHARMATRIX_CHARHIGH_TAG      = 1;
  CHARMATRIX_CHARLOW_COLOR     = clWhite;
  CHARMATRIX_CHARHIGH_COLOR    = clBlue;

  // Formats - all is used for the import dialog to select the format
  FORMAT_DEC          = 'DEC';
  FORMAT_HEX          = 'HEX';
  FORMAT_BIN          = 'BIN';
  FORMAT_ALL          : Array of String = (FORMAT_BIN,FORMAT_HEX,FORMAT_DEC);
  FORMAT_DEC_PREFIX   = '';
  FORMAT_HEX_PREFIX   = '0x';
  FORMAT_BIN_PREFIX   = '0b';
  FORMAT_BIN_BITS     = 5;    // 5 bits used to define a pixel row
  // Special characters using in the format function
  CHAR_APOSTROPHE     = #39;

  // Code Types & Code Templates
  CODE_CPP                 = 'CPP';
  CODE_TEMPLATE_CPP        = 'lcdccmex.pde';
  CODE_B4R                 = 'B4R';
  CODE_TEMPLATE_B4R        = 'lcdccmex.b4r';
  CODE_PYTHON_TF           = 'Python Tinkerforge';
  CODE_TEMPLATE_PYTHON_TF  = 'lcdccmex.py';
  CODE_MQTT_TF             = 'MQTT Tinkerforge';
  CODE_TEMPLATE_MQTT_TF    = 'lcdccmex.sh';
  CODE_TYPES_ALL           : Array of String = (CODE_CPP, CODE_B4R, CODE_PYTHON_TF, CODE_MQTT_TF);

type
  { TFormMain }
  TFormMain = class(TForm)
    ActionCodeExample: TAction;
    ActionCodeMQTTTF: TAction;
    ActionEditCopyCode: TAction;
    ActionEditCopyBIN: TAction;
    ActionEditCopyHEX: TAction;
    ActionFileImport: TAction;
    ActionCodeCPP: TAction;
    ActionCodePythonTF: TAction;
    ActionCodeB4R: TAction;
    ActionHelpAbout: TAction;
    ActionEditPixelsLow: TAction;
    ActionEditPixelsHigh: TAction;
    ActionEditCopyDEC: TAction;
    ActionFileExit: TAction;
    ActionFileSave: TAction;
    ActionFileOpen: TAction;
    ActionCreateCharMatrix: TAction;
    ActionListMain: TActionList;
    EditCustomCharName: TEdit;
    GroupBoxCode: TGroupBox;
    GroupBoxCustomCharValues: TGroupBox;
    ImageListMain: TImageList;
    LEditDECPixels: TLabeledEdit;
    LEditHEXPixels: TLabeledEdit;
    LEditBINPixels: TLabeledEdit;
    MainMenuMain: TMainMenu;
    MemoCode: TMemo;
    MenuFile: TMenuItem;
    MenuCode: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    N6: TMenuItem;
    N5: TMenuItem;
    MenuItemCodeTinkerforge: TMenuItem;
    N4: TMenuItem;
    N3: TMenuItem;
    MenuItemImport: TMenuItem;
    MenuItemCodeArduino: TMenuItem;
    MenuItemCodePython: TMenuItem;
    MenuItemCodeB4X: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemFileSave: TMenuItem;
    MenuItemFileExit: TMenuItem;
    MenuItemEditCopy: TMenuItem;
    MenuItemEditBitsHigh: TMenuItem;
    MenuItemEditBitsLow: TMenuItem;
    MenuItemHelpAbout: TMenuItem;
    MenuHelp: TMenuItem;
    N2: TMenuItem;
    MenuEdit: TMenuItem;
    N1: TMenuItem;
    OpenDialogCustomChar: TOpenDialog;
    Panel1: TPanel;
    PanelCustomCharEdit: TPanel;
    PanelConversion: TPanel;
    PanelSetCharBits: TPanel;
    PanelCharMatrix: TPanel;
    PanelCharacter: TPanel;
    SaveDialogCustomChar: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButtonPixelsLow: TSpeedButton;
    SpeedButtonPixelsHigh: TSpeedButton;
    StatusBarMain: TStatusBar;
    ToolBarMain: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure ActionCodeCPPExecute(Sender: TObject);
    procedure ActionCodeB4RExecute(Sender: TObject);
    procedure ActionCodeExampleExecute(Sender: TObject);
    procedure ActionCodePythonTFExecute(Sender: TObject);
    procedure ActionCodeMQTTTFExecute(Sender: TObject);
    procedure ActionCreateCharMatrixExecute(Sender: TObject);
    procedure ActionEditCopyBINExecute(Sender: TObject);
    procedure ActionEditCopyCodeExecute(Sender: TObject);
    procedure ActionEditCopyHEXExecute(Sender: TObject);
    procedure ActionEditPixelsHighExecute(Sender: TObject);
    procedure ActionEditPixelsLowExecute(Sender: TObject);
    procedure ActionEditCopyDECExecute(Sender: TObject);
    procedure ActionFileExitExecute(Sender: TObject);
    procedure ActionFileImportExecute(Sender: TObject);
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    // Fields
    CodeType: String;
    // Functions
    procedure CreateCharMatrix(Sender: TObject);
    procedure LoadCharMatrix(Sender: TObject; AList: TStringList);
    Function  SaveCharMatrix(Sender: TObject):TStringList;
    Procedure SetCharPixels(AState: Integer);
    Function  GetCharPixel(ACharID: String): Integer;
    Procedure SetCharPixel(ACharID: String; AState: Integer);
    procedure PanelCharPixelClick(Sender: TObject);
    //
    Function  SetValueFormat(AFormat:String): String;
    Procedure SetLCDCharValues(Sender: TObject);
    //
    procedure ImportCharFormat(AFormat: String; ACode: String);
    procedure ShowCodeExample(ACodeType: String);
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

//
// FORM
//

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FormMain.Caption := APPTITLEVERSION;
  ActionCreateCharMatrixExecute(Sender);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  EditCustomCharName.SetFocus;
end;

//
// ACTIONS
//

(*
  Create the character matrix with a panel for each char.
  The matrix has 8 rows with 5 cols.
  Each char bit panel is identified by its id row:col assigned to the hint.
  The panel tag sets the state high (1, blue) or low (0, white).
*)
procedure TFormMain.ActionCreateCharMatrixExecute(Sender: TObject);
begin
  CreateCharMatrix(Sender);
end;

procedure TFormMain.ActionFileOpenExecute(Sender: TObject);
var sl: TStringList;
  function GetFilename(f: String):String;
  begin
   GetFilename := ExtractFileName(f.Substring(1, pos('.',f) - 2));
  end;

begin
  sl := TStringList.Create;
  MemoCode.Lines.Clear;
  if OpenDialogCustomChar.Execute then begin
    if fileExists(OpenDialogCustomChar.Filename) then begin
      sl.LoadFromFile(OpenDialogCustomChar.Filename);
      LoadCharMatrix(Sender, sl);
      sl.Free;
      SetLCDCharValues(Sender);
      EditCustomCharName.Text := GetFileName(OpenDialogCustomChar.Filename);
      SetStatusBarSimpleText(StatusBarMain, Format(
        'Custom character opened: %s',
        [OpenDialogCustomChar.Filename]));
    end;
  end;
end;

procedure TFormMain.ActionFileSaveExecute(Sender: TObject);
begin
  // Check if name entered
  if Length(EditCustomCharName.Text) = 0 then begin
    ShowError(
      'Save Custom Character',
      Format('%s%s%s',[
        'Can not save the custom character.',
        LineEnding,
        'No name defined.']));
    SetStatusBarSimpleText(StatusBarMain, Format(
      'Custom character not saved',
      []));
  end
  else begin
    SaveDialogCustomChar.FileName := EditCustomCharName.Text;
    if SaveDialogCustomChar.Execute then begin
      SaveCharMatrix(Sender).SaveToFile(SaveDialogCustomChar.FileName);
    end;
  end;
end;

procedure TFormMain.ActionHelpAboutExecute(Sender: TObject);
begin
  ShowInfo('About ...', APPDESCRIPTION);
end;

(*
  Import the custom char code.
  showmessage(Format( 'Import: %s%s%s',
  [FormCharImport.CustomCharFormat, LineEnding,FormCharImport.CustomCharCode]));
*)
procedure TFormMain.ActionFileImportExecute(Sender: TObject);
begin
  FormCharImport.ComboBoxCustomCharFormat.Items.Clear;
  FormCharImport.ComboBoxCustomCharFormat.Items.AddStrings(FORMAT_ALL);
  // Clear the custom char code memo
  { TODO 3 -cImport : Import dialog: option to preset the custom char code via paste. }
  FormCharImport.MemoCustomCharPixels.Lines.Clear;
  // The dialog is closed with ok, lets import
  if FormCharImport.showmodal = mrOk then begin
    ImportCharFormat(
      FormCharImport.CustomCharFormat,
      FormCharImport.CustomCharPixels);
    SetLCDCharValues(Sender);
  end;
end;

procedure TFormMain.ActionFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ActionEditCopyDECExecute(Sender: TObject);
begin
  Clipboard.AsText := LEditDECPixels.Text;
  SetStatusBarSimpleText(StatusBarMain, 'DEC value copied to the clipboard.');
end;

procedure TFormMain.ActionEditCopyHEXExecute(Sender: TObject);
begin
  Clipboard.AsText := LEditHEXPixels.Text;
  SetStatusBarSimpleText(StatusBarMain, 'HEX value copied to the clipboard.');
end;

procedure TFormMain.ActionEditCopyBINExecute(Sender: TObject);
begin
  Clipboard.AsText := LEditBINPixels.Text;
  SetStatusBarSimpleText(StatusBarMain, 'BIN value copied to the clipboard.');
end;

procedure TFormMain.ActionEditCopyCodeExecute(Sender: TObject);
begin
  Clipboard.AsText := MemoCode.Lines.Text;
  SetStatusBarSimpleText(StatusBarMain, 'Code copied to the clipboard.');
end;

procedure TFormMain.ActionEditPixelsHighExecute(Sender: TObject);
begin
  SetCharPixels(CHARMATRIX_CHARHIGH_TAG);
  SetLCDCharValues(Sender);
end;

procedure TFormMain.ActionEditPixelsLowExecute(Sender: TObject);
begin
  SetCharPixels(CHARMATRIX_CHARLOW_TAG);
  SetLCDCharValues(Sender);
end;

(*
  Arduino C++ Code
  // Arduino LCD Custom Character
  byte battery[8] = {0b01110,0b11011,0b10001,0b10001,0b10001,0b10001,0b10001,0b11111};
*)
procedure TFormMain.ActionCodeCPPExecute(Sender: TObject);
var
  CharName: String;
  CharPixels: String;
begin
  CodeType := CODE_CPP;
  CharName := EditCustomCharName.Text;
  CharPixels := LEditBINPixels.Text;
  MemoCode.Lines.Clear;
  MemoCode.Lines.Add('// Arduino LCD Custom Character');
  MemoCode.Lines.Add(Format('byte %s[8] = {%s};',
    [Charname, CharPixels]));
  MemoCode.SelStart := 0;
end;

(*
  B4R hex array with example Battery:
  ' B4R LCD Custom Character
  Dim battery(8) As Byte
  battery = Array As Byte (0x0E,0x1B,0x11,0x11,0x11,0x11,0x11,0x1F)
*)
procedure TFormMain.ActionCodeB4RExecute(Sender: TObject);
var
  CharName: String;
  CharPixels: String;
begin
  CodeType := CODE_B4R;
  CharName := EditCustomCharName.Text;
  CharPixels := LEditHEXPixels.Text;
  MemoCode.Lines.Clear;
  MemoCode.Lines.Add(Format('%s %s',
    [CHAR_APOSTROPHE, 'B4R LCD Custom Character']));
	MemoCode.Lines.Add(Format('Dim %s(8) As Byte',
    [CharName]));
  MemoCode.Lines.Add(Format('%s = Array As Byte (%s)',
    [CharName, CharPixels]));
  MemoCode.SelStart := 0;
end;

(*
  Python int array with Tinkerforge example for battery character
  battery = [14, 27, 17, 17, 17, 17, 17, 31]
  lcd.set_custom_character(0, battery)
  lcd.write_line(0, 0, "Battery: " + "\x08")
*)
procedure TFormMain.ActionCodePythonTFExecute(Sender: TObject);
var
  CharName: String;
  CharPixels: String;
begin
  CodeType := CODE_PYTHON_TF;
  CharName := EditCustomCharName.Text;
  CharPixels := LEditDECPixels.Text;
  MemoCode.Lines.Clear;
  MemoCode.Lines.Add('# Python Custom Character Int');
  MemoCode.Lines.Add(Format('%s = [%s]',
    [CharName, CharPixels]));
  MemoCode.Lines.Add('');

  MemoCode.Lines.Add('# Python Custom Character Byte');
  CharPixels := LEditBINPixels.Text;
  MemoCode.Lines.Add(Format('%s = (%s)',
    [CharName, CharPixels]));
  MemoCode.SelStart := 0;
end;

(*

*)
procedure TFormMain.ActionCodeMQTTTFExecute(Sender: TObject);
var
  CharName: String;
  CharPixels: String;
begin
  CodeType := CODE_MQTT_TF;
  CharName := EditCustomCharName.Text;
  CharPixels := LEditDECPixels.Text;
  MemoCode.Lines.Clear;
  MemoCode.Lines.Add('# Tinkerforge MQTT2 API @ Index 0');
  //MemoCode.Lines.Add(Format('{"index":0, "%s":[%s]}',[CharName, CharPixels]));
  MemoCode.Lines.Add('# Set custom character');
  MemoCode.Lines.Add(Format('mosquitto_pub -t tinkerforge/request/lcd_20x4_bricklet/BHN/set_custom_character -m %s{"index":0, "character":[%s]}%s',
    [CHAR_APOSTROPHE, CharPixels, CHAR_APOSTROPHE]));
  MemoCode.Lines.Add('# Write custom character with text');
  MemoCode.Lines.Add(Format('mosquitto_pub -t tinkerforge/request/lcd_20x4_bricklet/BHN/write_line -m %s{"line": 0, "position": 0, "text": "%s: \u0008"}%s',
    [CHAR_APOSTROPHE,CharName,CHAR_APOSTROPHE]));
  MemoCode.SelStart := 0;
end;

procedure TFormMain.ActionCodeExampleExecute(Sender: TObject);
begin
  ShowCodeExample(CodeType);
end;


//
// CHAR MATRIX
//

(*
  Create the character matrix with a panel for each char.
  The matrix has 8 rows with 5 cols.
  Each char bit panel is identified by its id row:col assigned to the hint.
  The panel tag sets the state high (1, blue) or low (0, white).
*)
procedure TFormMain.CreateCharMatrix(Sender: TObject);
var
  row,col: Integer;

  procedure CreateCharPanel(row, col: Integer; l: Integer; t: Integer);
  Var
    PnlChar: TPanel;
  begin
    PnlChar := TPanel.Create(self);
    With PnlChar do begin
      Left        := l;
      Top         := t;
      Width       := CHARMATRIX_CHARWIDTH;
      Height      := CHARMATRIX_CHARHEIGHT;
      BevelOuter  := bvNone;
      Caption     := '';
      Color       := CHARMATRIX_CHARLOW_COLOR;
      Hint        := IntToStr(row)+':'+IntTostr(col);
      Tag         := CHARMATRIX_CHARLOW_TAG;
      Parent      := PanelCharMatrix;
      BorderStyle := bsSingle;
      BorderWidth := 1;
      Onclick     := @PanelCharPixelClick;
   end;
  end;

begin
  for row := 1 to CHARMATRIX_ROWS do begin       // 8
    for col := 1 to CHARMATRIX_COLS do begin     // 5
      // row, col, left, top
      CreateCharPanel(row, col,
        10 + (col - 1) * (CHARMATRIX_CHARWIDTH + 10),
        10 + (row - 1) * (CHARMATRIX_CHARHEIGHT +  10) );
    end;
  end;
  PanelCharacter.Width := 260;
end;

(*
  Set the char matrix bits from character definition.
  Each list item contains row:col:state.
*)
Procedure TFormMain.LoadCharMatrix(Sender: TObject; AList: TStringList);
var
  i: Integer;
  Items: TStringArray;
  CharID: String;
begin
  // MemoLog.Lines.Clear;
  // MemoLog.Lines.Add('%d', [AList.Count]);
  for i := 0 to AList.Count - 1 do begin
    // Split list entry into 3 array items row,col,state
    Items := AList[i].Split(':');
    CharID := Format('%s:%s',[Items[0],items[1]]);
    // MemoLog.Lines.Add('%s#%d', [CharID, StrToInt(Items[2])]);
    SetCharPixel(CharID, StrToInt(Items[2]));
  end;
end;

(*
  Save the char matrix bits to a character definition list.
  Each list item contains row:col:state.
*)
Function TFormMain.SaveCharMatrix(Sender: TObject):TStringList;
var
  row, col: Integer;
  CharPixelID: String;
  CharPixelState: String;
begin
  SaveCharMatrix := TStringList.Create;
  for row := 1 to CHARMATRIX_ROWS do begin
    for col := 1 to CHARMATRIX_COLS do begin
      CharPixelID := Format('%d:%d', [row,col]);
      CharPixelState := Format('%d', [GetCharPixel(CharPixelID)]);
      SaveCharMatrix.Add(Format('%s:%s', [CharPixelID, CharPixelState]));
    end;
  end;
end;

(*
  Set char bit to high (1) or low (0) - assigned to the property Tag
*)
procedure TFormMain.PanelCharPixelClick(Sender: TObject);
var
  PanelChar: TPanel;
begin
  MemoCode.Lines.Clear;
  PanelChar := Sender as TPanel;
  if PanelChar.Tag = 0 then begin
    PanelChar.Tag := CHARMATRIX_CHARHIGH_TAG;
    PanelChar.Color := CHARMATRIX_CHARHIGH_COLOR;
  end
  else begin
    PanelChar.Tag := CHARMATRIX_CHARLOW_TAG;
    PanelChar.Color := CHARMATRIX_CHARLOW_COLOR;
  end;
  SetLCDCharValues(Sender);
end;

(*
  Set the state of the char matrix bits to low (0) or high (1).
*)
Procedure TFormMain.SetCharPixels(AState: Integer);
var
 i : Integer;
 ParentControl: TWinControl;
begin
  ParentControl := PanelCharMatrix;
  for i := 0 to ParentControl.ControlCount - 1 do begin
    if ParentControl.Controls[i] is TPanel then begin
      ParentControl.Controls[i].Tag := AState;
      if AState = 0 then
        ParentControl.Controls[i].Color := CHARMATRIX_CHARLOW_COLOR
      else
        ParentControl.Controls[i].Color := CHARMATRIX_CHARHIGH_COLOR;
    end;
  end;
end;

(*
  Get the state 0 or 1 of a char.
  A char is represented by a panel.
  Each char bit panel is identified by its id row:col assigned to the hint.
  The panel tag sets the state high (1, blue) or low (0, white).
  ACharID: ID as string row:col
  Returns State 0 or 1
  Example: ShowMessage(IntToStr(GetCharPixelState('1:3')));
*)
Function TFormMain.GetCharPixel(ACharID: String): Integer;
var
 i : Integer;
 ParentControl: TWinControl;
begin
  GetCharPixel := -1;
  ParentControl := PanelCharMatrix;
  for i := 0 to ParentControl.ControlCount - 1 do begin
    if ParentControl.Controls[i] is TPanel then begin
      if ParentControl.Controls[i].Hint = ACharID then begin
        GetCharPixel := ParentControl.Controls[i].Tag;
        Break;
      end;
    end;
  end;
end;

(*
  Set the state of a char bit to 0 or 1.
  ACharID - ID defined as string row:col
  AState - State 0 (lw) or 1 (hight)
  Example: SetCharPixel('1:3', 1);
*)
Procedure TFormMain.SetCharPixel(ACharID: String; AState: Integer);
var
 i : Integer;
 ParentControl: TWinControl;
begin
  ParentControl := PanelCharMatrix;
  for i := 0 to ParentControl.ControlCount - 1 do begin
    if ParentControl.Controls[i] is TPanel then begin
      if ParentControl.Controls[i].Hint = ACharID then begin
        ParentControl.Controls[i].Tag := AState;
        if AState = 0 then
          ParentControl.Controls[i].Color := CHARMATRIX_CHARLOW_COLOR
        else
          ParentControl.Controls[i].Color := CHARMATRIX_CHARHIGH_COLOR;
        Break;
      end;
    end;
  end;
end;

//
// VALUES
//

(*
  Set the Format for the types DEC,HEX,BIN.
  DEC = NN, HEX = 0xNN, BIN = BNNNNN & 0bNNNNN
*)
Function TFormMain.SetValueFormat(AFormat:String): String;
var
 RowNr,ColNr: Integer;
 ResultValue: String = '';
 RowByteStr: String;      // 000 01110
 RowDECStr: String;       // 14
 RowHEXStr: String;       // 0E
begin
  // MemoLog.Lines.Clear;
  AFormat := UpperCase(AFormat);
  // Loop over the rows & cols to create the byte string, i.e. 00001110
  // The byte string (length 8) is used to create the DEC and HEX values
  for RowNr := 1 To CHARMATRIX_ROWS do begin
    // BIN - set bin prefix of 3 bits to get to the 8 bits (each row has 5 pixel = bits)
    RowByteStr := '000';
		for ColNr := 1 To CHARMATRIX_COLS do begin
      RowByteStr := RowByteStr + Format(
        '%d', [GetCharPixel(Format('%d:%d', [RowNr,ColNr]))]);
		end;
    // DEC - convert byte string to dec
    RowDECStr := Format('%d', [Numb2Dec(RowByteStr, 2)]);
    // HEX - convert byte string to hex via dec
    RowHEXStr := Format('%s', [IntToHex(Numb2Dec(RowByteStr, 2),2)]);
    // Set the ResultValue
    if AFormat = FORMAT_BIN then  ResultValue := ResultValue + FORMAT_BIN_PREFIX + RowByteStr.Substring(3);
    if AFormat = FORMAT_DEC then  ResultValue := ResultValue + FORMAT_DEC_PREFIX + RowDECStr;
    if AFormat = FORMAT_HEX then  ResultValue := ResultValue + FORMAT_HEX_PREFIX + RowHEXStr;
    // Add comma , if rownr less max rows
    if RowNr < CHARMATRIX_ROWS then ResultValue := ResultValue + ',';
  end;
  SetValueFormat := ResultValue;
End;

(*
  Set the LCD char values, build the payload string and the customcharmap
  CustomCharMap in range \u0009 (=Index 0) to \u000F (=Index 7)
  Byte arroweq[8] = {B00000,B00000,B11111,B00000,B11111,B00000,B00000,B00000};
*)
Procedure TFormMain.SetLCDCharValues(Sender: TObject);
begin
	// Create the string arrays for Decimal,Hex,Bin with example battery character
  // 14, 27, 17, 17, 17, 17, 17, 31
  LEditDECPixels.Text := SetValueFormat(FORMAT_DEC);
	// 0x0E, 0x1B, 0x11, 0x11, 0x11, 0x11, 0x11, 0x1F
  LEditHEXPixels.Text := SetValueFormat(FORMAT_HEX);
	//0b01110, 0b11011, 0b10001, 0b10001, 0b10001, 0b10001, 0b10001, 0b11111
  LEditBINPixels.Text := SetValueFormat(FORMAT_BIN);
end;

//
//IMPORT
//
(*
  Import a pixel array defined as example:
  BIN: 0b00100,0b00100,0b01010,0b01010,0b10001,0b10001,0b10001,0b01110
  HEX: 0x0E,0x1B,0x11,0x11,0x11,0x11,0x11,0x1F
  DEC: 14,27,17,17,17,17,17,31
  The pixel array must not end with a comma.
  The prefix are stripped (0b, B, 0x) to read bit for bit (pixel-by-pixel).
  The HEX and DEC values are converted to BIN with 5 bits.
*)
procedure TFormMain.ImportCharFormat(AFormat: String; ACode: String);
var
  sl: TStringList;
  RowNr,ColNr: Integer;
  Pixels: String;
  DecValue: Integer;
begin
  try
    // Clean the char code definition
    case AFormat of
      FORMAT_BIN: Begin
        ACode := ACode.Replace(FORMAT_BIN_PREFIX,'').Replace(','+LineEnding, ',');
      end;
      FORMAT_HEX: Begin
        ACode := ACode.Replace(FORMAT_HEX_PREFIX,'').Replace(','+LineEnding, ',');
      end;
    end;
    // Creat the stringlist
    sl := TStringList.Create;
    // Load the code as comma delimited text
    sl.Delimiter:=',';
    sl.DelimitedText := ACode;
    // Check if the stringlist length is equal number of rows of the char maxtrix
    if sl.count < CHARMATRIX_ROWS then begin
      ShowError(
        'Import Error',
        Format('Pixels are missing in the character definition:%s%s',[LineEnding, sl.Text]));
    end
    else begin
      // Loop over the stringlist with 8 items
      for RowNr := 0 to CHARMATRIX_ROWS - 1 do begin
        // Get the line with the pixel value
        Pixels := sl[RowNr].Trim;
        // Convert the line value depending format to get a bit string
        case AFormat of
          FORMAT_HEX: begin
            // Convert the hex value to dec
            DecValue := HexToInt(Pixels);
            // Convert the dec value to bin with 5 bits (pixels)
            Pixels := Dec2Numb(DecValue, FORMAT_BIN_BITS, 2);
          end;
          FORMAT_DEC: begin
            // Convert the dec value to bin with 5 bits (pixels)
            DecValue := strToInt(Pixels);
            Pixels := Dec2Numb(DecValue, FORMAT_BIN_BITS, 2);
          end;
        end;
        // Loop over the 5 bits and set the char matrix pixel bit
        for ColNr := 1 to CHARMATRIX_COLS do begin
          SetCharPixel(Format(
            '%d:%d',[RowNr + 1,ColNr]),
            StrToInt(Pixels[ColNr]));
        end;
      end;
      sl.Free;
    end;
  except on E: Exception do
    ShowError(
      'Import Error',
      Format('%s',[E.Message]));
  end;
end;

(*

*)
procedure TFormMain.ShowCodeExample(ACodeType: String);
var
  CharName: String;
  CharPixels: String;
  Code: String;
  sl: TStringList;
begin
  // Check on codetype
  if ACodeType = '' then begin
    ShowWarning('Code Example', 'No code type selected.');
    Exit;
  end;
  // Get the character name
  CharName := EditCustomCharName.Text;
  // Set the caption of the form
  FormCode.Caption := Format('%s - %s', [FormCode.Caption, ACodeType]);
  // Depending CodeType load code example template, set the synedit highlighter
  sl := TStringList.Create;
  case ACodeType of
    CODE_CPP : begin
      CharPixels := LEditBINPixels.Text;
      sl.LoadFromFile(CODE_TEMPLATE_CPP);
      FormCode.SynEditCode.Highlighter := FormCode.SynCPPSyn;
    end;
    CODE_B4R : begin
      CharPixels := LEditHEXPixels.Text;
      sl.LoadFromFile(CODE_TEMPLATE_B4R);
      FormCode.SynEditCode.Highlighter := FormCode.SynB4RSyn;
    end;
    CODE_PYTHON_TF: begin
      CharPixels := LEditDECPixels.Text;
      sl.LoadFromFile(CODE_TEMPLATE_PYTHON_TF);
      FormCode.SynEditCode.Highlighter := FormCode.SynPythonSyn;
    end;
    CODE_MQTT_TF: begin
      CharPixels := LEditDECPixels.Text;
      sl.LoadFromFile(CODE_TEMPLATE_MQTT_TF);
      FormCode.SynEditCode.Highlighter := FormCode.SynBatSyn;
    end;
  end;
  // Replace placeholder in the code example
  sl.Text := sl.Text.Replace('#CUSTOMCHARNAME#', CharName);
  sl.Text := sl.Text.Replace('#CUSTOMCHARPIXELS#', CharPixels);
  sl.Text := sl.Text.Replace(Chr(9), '    ');
  Code := sl.Text;
  sl.Free;
  // Assign the code example to synedit and show the form
  FormCode.SynEditCode.Lines.Text := Code;
  FormCode.ShowModal;
end;

end.

