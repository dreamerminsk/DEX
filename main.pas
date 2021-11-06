unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, LResources,
  bufstream, dex;

type

  { TMainForm }

  TMainForm = class(TForm)
    CoolBar: TCoolBar;
    DexTreeSheet: TTabSheet;
    DexViewSheet: TTabSheet;
    DexListView: TListView;
    DexTreeView: TTreeView;
    OpenButton: TButton;
    Memo: TMemo;
    DexOpenDialog: TOpenDialog;
    PageControl: TPageControl;
    StatusBar: TStatusBar;
    DebugSheet: TTabSheet;
    procedure OpenButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure Clear();
  public

  end;

var
  MainForm: TMainForm;
  DexRoot: TTreeNode;
  DexHeaderNode: TTreeNode;
  buf: TBufferedFileStream;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.OpenButtonClick(Sender: TObject);
var
  i: int64;
  StringIdsSize, StringIdsOff: int64;
  TypeIdsSize, TypeIdsOff: int64;
  StringIds: array of int64;
begin
  if DexOpenDialog.Execute then
  begin
    Clear;
    MainForm.Text := DexOpenDialog.FileName;
    DexRoot := DexTreeView.Items.AddChild(nil, DexOpenDialog.FileName);
    DexHeaderNode := DexTreeView.Items.AddChildFirst(DexRoot, 'header');
    buf := TBufferedFileStream.Create(DexOpenDialog.FileName, fmOpenRead);
    DexTreeView.Items.AddChildFirst(DexHeaderNode, 'magic: ' + ReadMagic(buf));
    Memo.Lines.Add('magic: ' + ReadMagic(buf));
    Memo.Lines.Add('checksum: ' + IntToHex(ReadUint(buf), 8));
    Memo.Lines.Add('signature: ' + ReadBytes(buf, 20));
    Memo.Lines.Add('file_size: ' + IntToStr(ReadUint(buf)));
    Memo.Lines.Add('header_size: ' + IntToStr(ReadUint(buf)));
    Memo.Lines.Add('endian_tag: ' + IntToHex(ReadUint(buf), 8));

    Memo.Lines.Add('link_size: ' + IntToStr(ReadUint(buf)));
    Memo.Lines.Add('link_off: ' + IntToStr(ReadUint(buf)));

    Memo.Lines.Add('map_off: ' + IntToStr(ReadUint(buf)));

    StringIdsSize := ReadUint(buf);
    Memo.Lines.Add('string_ids_size: ' + IntToStr(StringIdsSize));
    StringIdsOff := ReadUint(buf);
    Memo.Lines.Add('string_ids_off: ' + IntToStr(StringIdsOff));

    TypeIdsSize := ReadUint(buf);
    Memo.Lines.Add('type_ids_size: ' + IntToStr(TypeIdsSize));
    TypeIdsOff := ReadUint(buf);
    Memo.Lines.Add('type_ids_off: ' + IntToStr(TypeIdsOff));

    Memo.Lines.Add('proto_ids_size: ' + IntToStr(ReadUint(buf)));
    Memo.Lines.Add('proto_ids_off: ' + IntToStr(ReadUint(buf)));

    Memo.Lines.Add('field_ids_size: ' + IntToStr(ReadUint(buf)));
    Memo.Lines.Add('field_ids_off: ' + IntToStr(ReadUint(buf)));

    Memo.Lines.Add('method_ids_size: ' + IntToStr(ReadUint(buf)));
    Memo.Lines.Add('method_ids_off: ' + IntToStr(ReadUint(buf)));

    Memo.Lines.Add('class_defs_size: ' + IntToStr(ReadUint(buf)));
    Memo.Lines.Add('class_defs_off: ' + IntToStr(ReadUint(buf)));

    Memo.Lines.Add('data_size: ' + IntToStr(ReadUint(buf)));
    Memo.Lines.Add('data_off: ' + IntToStr(ReadUint(buf)));

    buf.seek(StringIdsOff, TSeekOrigin.soBeginning);
    SetLength(StringIds, StringIdsSize);
    for i := 1 to StringIdsSize do
    begin
      StringIds[i - 1] := ReadUint(buf);
    end;
    for i := 1 to StringIdsSize do
    begin
      buf.seek(StringIds[i - 1], TSeekOrigin.soBeginning);
      Memo.Lines.Add('string_data_item[' + IntToStr(i - 1) + ']: ' +
        IntToStr(ReadULEB128(buf)) + ', "' + ReadMUTF8(buf) + '"');
      Application.ProcessMessages;
    end;
  end;
end;



procedure TMainForm.Clear();
begin
  FreeAndNil(buf);
  DexTreeView.Items.Clear;
  DexListView.Items.Clear;
  Memo.Clear;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Memo.Clear;
end;



end.
