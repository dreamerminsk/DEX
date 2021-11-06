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
  StringIdsSize, StringIdsOff, TypeIdsSize, TypeIdsOff, ProtoIdsSize,
  ProtoIdsOff, FieldIdsSize, FieldIdsOff, MethodIdsSize, MethodIdsOff,
  ClassDefsSize, ClassDefsOff, DataSize, DataOff: int64;
  StringIds: array of int64;
begin
  if DexOpenDialog.Execute then
  begin
    Clear;
    MainForm.Text := DexOpenDialog.FileName;
    DexRoot := DexTreeView.Items.AddChild(nil, DexOpenDialog.FileName);
    DexHeaderNode := DexTreeView.Items.AddChildFirst(DexRoot, 'header');

    buf := TBufferedFileStream.Create(DexOpenDialog.FileName, fmOpenRead);
    DexTreeView.Items.AddChild(DexHeaderNode, 'magic: ' + ReadMagic(buf));
    DexTreeView.Items.AddChild(DexHeaderNode, 'checksum: ' +
      IntToHex(ReadUint(buf), 8));
    DexTreeView.Items.AddChild(DexHeaderNode, 'signature: ' + ReadBytes(buf, 20));
    DexTreeView.Items.AddChild(DexHeaderNode, 'file_size: ' +
      IntToStr(ReadUint(buf)));
    DexTreeView.Items.AddChild(DexHeaderNode, 'header_size: ' +
      IntToStr(ReadUint(buf)));
    DexTreeView.Items.AddChild(DexHeaderNode, 'endian_tag: ' +
      IntToHex(ReadUint(buf), 8));

    DexTreeView.Items.AddChild(DexHeaderNode, 'link_size: ' +
      IntToStr(ReadUint(buf)));
    DexTreeView.Items.AddChild(DexHeaderNode, 'link_off: ' + IntToStr(ReadUint(buf)));

    DexTreeView.Items.AddChild(DexHeaderNode, 'map_off: ' + IntToStr(ReadUint(buf)));

    StringIdsSize := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'string_ids_size: ' +
      IntToStr(StringIdsSize));
    StringIdsOff := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'string_ids_off: ' +
      IntToStr(StringIdsOff));

    TypeIdsSize := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'type_ids_size: ' +
      IntToStr(TypeIdsSize));
    TypeIdsOff := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'type_ids_off: ' +
      IntToStr(TypeIdsOff));

    ProtoIdsSize := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'proto_ids_size: ' +
      IntToStr(ProtoIdsSize));
    ProtoIdsOff := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'proto_ids_off: ' +
      IntToStr(ProtoIdsOff));

    FieldIdsSize := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'field_ids_size: ' +
      IntToStr(FieldIdsSize));
    FieldIdsOff := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'field_ids_off: ' +
      IntToStr(FieldIdsOff));

    MethodIdsSize := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'method_ids_size: ' +
      IntToStr(methodIdsSize));
    MethodIdsOff := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'method_ids_off: ' +
      IntToStr(methodIdsOff));

    ClassDefsSize := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'class_defs_size: ' +
      IntToStr(ClassDefsSize));
    ClassDefsOff := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'class_defs_off: ' +
      IntToStr(ClassDefsOff));

    DataSize := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'data_size: ' + IntToStr(DataSize));
    DataOff := ReadUint(buf);
    DexTreeView.Items.AddChild(DexHeaderNode, 'data_off: ' + IntToStr(DataOff));

    DexTreeView.Items.AddChild(DexRoot, 'string_ids');
    DexTreeView.Items.AddChild(DexRoot, 'type_ids');
    DexTreeView.Items.AddChild(DexRoot, 'proto_ids');
    DexTreeView.Items.AddChild(DexRoot, 'field_ids');
    DexTreeView.Items.AddChild(DexRoot, 'method_ids');
    DexTreeView.Items.AddChild(DexRoot, 'class_defs');
    DexTreeView.Items.AddChild(DexRoot, 'call_site_ids');
    DexTreeView.Items.AddChild(DexRoot, 'method_handles');
    DexTreeView.Items.AddChild(DexRoot, 'data');
    DexTreeView.Items.AddChild(DexRoot, 'link_data');

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
