{*******************************************************}
{                                                       }
{                       EhLib 9.4                       }
{             DBGridEh import/export routines           }
{                      Build 9.4.18                     }
{                                                       }
{   Copyright (c) 2015-2019 by Dmitry V. Bolshakov      }
{                                                       }
{*******************************************************}

unit DBVertGridEhImpExp;

{$I EhLib.Inc}

interface

uses
  SysUtils, Classes, Graphics, Dialogs, GridsEh, Controls, Variants,
{$IFDEF EH_LIB_16} System.Zip, {$ENDIF}
{$IFDEF FPC}
  XMLRead, DOM, EhLibXmlConsts,
{$ELSE}
  xmldom, XMLIntf, XMLDoc, EhLibXmlConsts,
{$ENDIF}

{$IFDEF EH_LIB_17} System.UITypes, System.Contnrs, {$ENDIF}
{$IFDEF CIL}
  EhLibVCLNET,
  System.Runtime.InteropServices, System.Reflection,
{$ELSE}
  {$IFDEF FPC}
    EhLibLCL, LCLType,
  {$ELSE}
    EhLibVCL, Messages, SqlTimSt,
  {$ENDIF}
{$ENDIF}
  XlsMemFilesEh,
  DBVertGridsEh,
  Db, Clipbrd, StdCtrls;

//type

//  TDBVertGridExportToXlsxEh = class(TXlsxFileWriterEh)
//  private
//    FDBVertGrid: TCustomDBVertGridEh;
//  protected
//    function GetColWidth(ACol: Integer): Integer; override;
//    function InitTableCheckEof: Boolean; override;
//    function GetColCount: Integer; override;
//    function InitRowCheckEof(ARow: Integer): Boolean; override;
//
//    procedure GetCellData(ACol, ARow: Integer; CellData: TXlsxCellDataEh); override;
//  public
//    property DBVertGrid: TCustomDBVertGridEh read FDBVertGrid write FDBVertGrid;
//  end;

  procedure ExportDBVertGridEhToXlsx(DBVertGridEh: TCustomDBVertGridEh;
    const FileName: String);

implementation

//type TDBVertGridEhCrack = class(TCustomDBVertGridEh);

procedure ExportDBVertGridEhToXlsx(DBVertGridEh: TCustomDBVertGridEh;
  const FileName: String);
var
  cr: IXlsFileCellsRangeEh;
  xlsFile: TXlsMemFileEh;
  Sheet: TXlsWorksheetEh;
  i: Integer;
  AFont: TFont;
begin
  xlsFile := TXlsMemFileEh.Create;
  xlsFile.Workbook.Worksheets[0].Name := 'Sheet1';
  Sheet := xlsFile.Workbook.Worksheets[0];

  Sheet.Columns[0].Width := Sheet.Columns.ScreenToXlsWidth(TDBVertGridEh(DBVertGridEh).ColWidths[0]);
  Sheet.Columns[1].Width := Sheet.Columns.ScreenToXlsWidth(TDBVertGridEh(DBVertGridEh).ColWidths[1]);

  for i := 0 to DBVertGridEh.VisibleFieldRowCount - 1 do
  begin
    //Label Cell
    Sheet.Cells[0, i].Value := DBVertGridEh.VisibleFieldRow[i].RowLabel.Caption;
    cr := Sheet.GetCellsRange(0, i, 0, i);
    cr.Fill.Color := DBVertGridEh.VisibleFieldRow[i].RowLabel.Color;

    AFont := DBVertGridEh.VisibleFieldRow[i].RowLabel.Font;
    cr.Font.Name := AFont.Name;
    cr.Font.Size := AFont.Size;
    cr.Font.Color := AFont.Color;
    cr.Font.IsBold := fsBold in AFont.Style;
    cr.Font.IsItalic := fsItalic in AFont.Style;
    cr.Font.IsUnderline := fsUnderline in AFont.Style;

    cr.ApplyChages;

    //Data Cell
    Sheet.Cells[1, i].Value := DBVertGridEh.VisibleFieldRow[i].Field.Value;
    cr := Sheet.GetCellsRange(1, i, 1, i);
    cr.Fill.Color := DBVertGridEh.VisibleFieldRow[i].Color;

    AFont := DBVertGridEh.VisibleFieldRow[i].Font;
    cr.Font.Name := AFont.Name;
    cr.Font.Size := AFont.Size;
    cr.Font.Color := AFont.Color;
    cr.Font.IsBold := fsBold in AFont.Style;
    cr.Font.IsItalic := fsItalic in AFont.Style;
    cr.Font.IsUnderline := fsUnderline in AFont.Style;

    cr.HorzAlign := AlignmentToXlsFileCellHorzAlign(DBVertGridEh.VisibleFieldRow[i].Alignment);

    cr.ApplyChages;
  end;

  cr := Sheet.GetCellsRange(0,0, 1,DBVertGridEh.VisibleFieldRowCount - 1);
  cr.Border.Top.Style := clsMediumEh;
  cr.Border.Bottom.Style := clsMediumEh;
  cr.Border.Left.Style := clsMediumEh;
  cr.Border.Right.Style := clsMediumEh;

  cr.InsideBorder.Top.Style := clsThinEh;
  cr.InsideBorder.Bottom.Style := clsThinEh;
  cr.InsideBorder.Left.Style := clsThinEh;
  cr.InsideBorder.Right.Style := clsThinEh;
  cr.ApplyChages;

  xlsFile.SaveToFile(FileName);
  xlsFile.Free;
end;

end.

