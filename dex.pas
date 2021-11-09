unit dex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  TYPE_HEADER_ITEM = $0000;
  TYPE_STRING_ID_ITEM = $0001;
  TYPE_TYPE_ID_ITEM = $0002;
  TYPE_PROTO_ID_ITEM = $0003;
  TYPE_FIELD_ID_ITEM = $0004;
  TYPE_METHOD_ID_ITEM = $0005;
  TYPE_CLASS_DEF_ITEM = $0006;
  TYPE_CALL_SITE_ID_ITEM = $0007;
  TYPE_METHOD_HANDLE_ITEM = $0008;
  TYPE_MAP_LIST = $1000;
  TYPE_TYPE_LIST = $1001;
  TYPE_ANNOTATION_SET_REF_LIST = $1002;
  TYPE_ANNOTATION_SET_ITEM = $1003;
  TYPE_CLASS_DATA_ITEM = $2000;
  TYPE_CODE_ITEM = $2001;
  TYPE_STRING_DATA_ITEM = $2002;
  TYPE_DEBUG_INFO_ITEM = $2003;
  TYPE_ANNOTATION_ITEM = $2004;
  TYPE_ENCODED_ARRAY_ITEM = $2005;
  TYPE_ANNOTATIONS_DIRECTORY_ITEM = $2006;
  TYPE_HIDDENAPI_CLASS_DATA_ITEM = $F000;

type
  DexHeader = packed record
    magic: array[0..7] of byte;
    checksum: longword;
    signature: array[0..19] of byte;
    file_size: longword;
    header_size: longword;
    endian_tag: longword;
    link_size: longword;
    link_off: longword;
    map_off: longword;
    string_ids_size: longword;
    string_ids_off: longword;
    type_ids_size: longword;
    type_ids_off: longword;
    proto_ids_size: longword;
    proto_ids_off: longword;
    field_ids_size: longword;
    field_ids_off: longword;
    method_ids_size: longword;
    method_ids_off: longword;
    class_defs_size: longword;
    class_defs_off: longword;
    data_size: longword;
    data_off: longword;
  end;

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
