unit dex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDexItem = class(TObject)
  private
    FParent: TDexItem;
  public
    constructor Create;
    destructor Destroy; override;
    property Parent: TDexItem read FParent;
  end;

  THeaderItem = class(TDexItem)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TDexFile = class(TDexItem)
  public
    constructor Create;
    destructor Destroy; override;
  end;


function ReadMagic(S: TStream): string;
function ReadUint(S: TStream): int64;
function ReadUshort(S: TStream): int64;
function ReadBytes(S: TStream; Size: integer): string;
function ReadULEB128(S: TStream): qword;
function ReadMUTF8(S: TStream): string;

implementation


constructor TDexItem.Create;
begin
  inherited Create;
end;

destructor TDexItem.Destroy;
begin
  inherited Destroy;
end;

constructor THeaderItem.Create;
begin
  inherited Create;
end;

destructor THeaderItem.Destroy;
begin
  inherited Destroy;
end;

constructor TDexFile.Create;
begin
  inherited Create;
end;

destructor TDexFile.Destroy;
begin
  inherited Destroy;
end;

function ReadMagic(S: TStream): string;
var
  i: integer;
  b: byte;
begin
  Result := '';
  for i := 1 to 8 do
  begin
    b := S.ReadByte;
    if b = 10 then
      Result += '\n'
    else if b = 0 then
      Result += '\0'
    else
      Result += char(b);
  end;
end;

function ReadUint(S: TStream): int64;
begin
  Result := S.ReadByte + 256 * S.ReadByte + 256 * 256 * S.ReadByte +
    256 * 256 * 256 * S.ReadByte;
end;

function ReadUshort(S: TStream): int64;
begin
  Result := S.ReadByte + 256 * S.ReadByte;
end;

function ReadBytes(S: TStream; Size: integer): string;
var
  i: integer;
  b: byte;
begin
  Result := '';
  if Size > 0 then
  begin
    for i := 1 to Size do
    begin
      b := S.readbyte;
      Result += IntToHex(b, 2);
    end;
  end;
end;

function ReadULEB128(S: TStream): qword;
var
  I, lShift: integer;
  lByte: byte;
begin
  Result := 0;
  lShift := 0;
  for I := 0 to 7 do
  begin
    lByte := S.ReadByte;
    Result := Result or (UInt64(lByte and $7F) shl lShift);
    if (lByte and $80) = 0 then
      Break;
    Inc(lShift, 7);
  end;
end;

function ReadMUTF8(S: TStream): string;
var
  b: byte;
begin
  Result := '';
  repeat
    b := S.ReadByte;
    if b > 0 then
      Result += Chr(b);
  until b = 0;
end;


end.
