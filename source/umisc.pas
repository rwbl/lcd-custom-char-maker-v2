unit uMisc;
(*
 Various helper functions.
 20210428 rwbl
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  ComCtrls,         // Statusbar
  Forms,            // Application.MessageBox
  LCLType,          // MB_nnnnn
  FMTBcd,
  Dialogs
  ;

Procedure SetStatusBarSimpleText(AStatusBar: TStatusBar; AText: String);

procedure ShowInfo(ACaption, AMessage: String);
procedure ShowWarning(ACaption, AMessage: String);
procedure ShowError(ACaption, AMessage: String);

function ByteStringToHex(ABin:String):String;
function ByteStringToChar(ABin:String):String;
function HexToInt(AHex:String): integer;

implementation

//
// STATUSBAR
//
Procedure SetStatusBarSimpleText(AStatusBar: TStatusBar; AText: String);
begin
  AStatusBar.SimpleText := AText;
end;

//
// MESSAGES
//

procedure ShowInfo(ACaption, AMessage: String);
begin
  Application.MessageBox(PChar(AMessage), PChar(ACaption), MB_ICONINFORMATION)
end;

procedure ShowWarning(ACaption, AMessage: String);
begin
  Application.MessageBox(PChar(AMessage), PChar(ACaption), MB_ICONWARNING)
end;

procedure ShowError(ACaption, AMessage: String);
begin
  Application.MessageBox(PChar(AMessage), PChar(ACaption), MB_ICONERROR)
end;

//
// CONVERSION
//

(*
  Convert a byte string with 8 bits into a hex string
  Example: 11011111 converted to DF
*)
function ByteStringToHex(ABin:String):String;
var
  Hex: PChar;
begin
  GetMem(Hex,16);
  BinToHex(PChar(ABin), Hex, length(ABin));
  ByteStringToHex := Hex;
end;

(*
  Convert a byte string with 8 bits into a char.
  Hint: LCD Char table has HIGH + LOW 4-bit.
  Char A has HIGH 0100 and LOW 0001 = 01000001
  Example: ByteStringToChar('01000001') result A
*)
function ByteStringToChar(ABin:String):String;
var
  Dec: Integer;
begin
  Dec := Numb2Dec(ABin, 2);
  ByteStringToChar := Chr(Dec);
end;


(*
  Convert HEX string to Integer using radix 16.
  Example: "A" converted to 10
*)
function HexToInt(AHex:String): integer;
begin
  HexToInt := Hex2Dec(AHex);
end;

end.

