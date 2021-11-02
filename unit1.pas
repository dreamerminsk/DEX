unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ShellCtrls, bufstream, dex;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    DexOpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    ShellListView1: TShellListView;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure Clear();
  public

  end;

var
  Form1: TForm1;
  buf: TBufferedFileStream;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if DexOpenDialog.Execute then
  begin
    Clear;
    Form1.Text := DexOpenDialog.FileName;
    buf := TBufferedFileStream.Create(DexOpenDialog.FileName, fmOpenRead);
    Memo1.Lines.Add('magic: ' + ReadMagic(buf));
    Memo1.Lines.Add('checksum: ' + IntToHex(ReadUint(buf), 8));
    Memo1.Lines.Add('signature: ' + ReadBytes(buf, 20));
    Memo1.Lines.Add('file_size: ' + IntToStr(ReadUint(buf)));
    Memo1.Lines.Add('header_size: ' + IntToStr(ReadUint(buf)));
    Memo1.Lines.Add('endian_tag: ' + IntToHex(ReadUint(buf), 8));

    Memo1.Lines.Add('link_size: ' + IntToStr(ReadUint(buf)));
    Memo1.Lines.Add('link_off: ' + IntToStr(ReadUint(buf)));

    Memo1.Lines.Add('map_off: ' + IntToStr(ReadUint(buf)));

    Memo1.Lines.Add('string_ids_size: ' + IntToStr(ReadUint(buf)));
    Memo1.Lines.Add('string_ids_off: ' + IntToStr(ReadUint(buf)));

    Memo1.Lines.Add('type_ids_size: ' + IntToStr(ReadUint(buf)));
    Memo1.Lines.Add('type_ids_off: ' + IntToStr(ReadUint(buf)));

    Memo1.Lines.Add('proto_ids_size: ' + IntToStr(ReadUint(buf)));
    Memo1.Lines.Add('proto_ids_off: ' + IntToStr(ReadUint(buf)));

    Memo1.Lines.Add('field_ids_size: ' + IntToStr(ReadUint(buf)));
    Memo1.Lines.Add('field_ids_off: ' + IntToStr(ReadUint(buf)));

    Memo1.Lines.Add('method_ids_size: ' + IntToStr(ReadUint(buf)));
    Memo1.Lines.Add('method_ids_off: ' + IntToStr(ReadUint(buf)));

    Memo1.Lines.Add('class_defs_size: ' + IntToStr(ReadUint(buf)));
    Memo1.Lines.Add('class_defs_off: ' + IntToStr(ReadUint(buf)));

    Memo1.Lines.Add('data_size: ' + IntToStr(ReadUint(buf)));
    Memo1.Lines.Add('data_off: ' + IntToStr(ReadUint(buf)));
  end;
end;



procedure TForm1.Clear();
begin
  FreeAndNil(buf);
  Memo1.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
end;



end.
