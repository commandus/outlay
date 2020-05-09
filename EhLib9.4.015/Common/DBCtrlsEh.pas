{*******************************************************}
{                                                       }
{                       EhLib 9.4                       }
{     TDBEditEh, TDBDateTimeEditEh, TDBComboBoxEh,      }
{      TDBNumberEditEh, TDBCheckBoxEh components        }
{                     (Build 9.4.48)                    }
{                                                       }
{     Copyright (c) 2001-2019 by Dmitry V. Bolshakov    }
{                                                       }
{*******************************************************}

{$I EhLib.Inc}

unit DBCtrlsEh;

interface

uses
 Messages, Contnrs, Variants, StrUtils,
  {$IFDEF EH_LIB_17} System.Generics.Collections, System.UITypes, {$ENDIF}
  {$IFDEF FPC}
    EhLibLCL, LMessages, LCLType, MaskEdit, LCLIntf,
    {$IFDEF FPC_CROSSP}
    {$ELSE}
      ComCtrls, Windows,
    {$ENDIF}
  {$ELSE}
    EhLibVCL, DBConsts, Mask, RichEdit, ComCtrls, Windows,
  {$ENDIF}
  SysUtils, Classes, Controls, Forms, Graphics, Menus,
  StdCtrls, ExtCtrls, Buttons, Db, DBCtrls, Imglist, GridsEh,
  ToolCtrlsEh, ActnList, Math, DynVarsEh, DropDownFormEh;

const
  CM_EDITIMAGECHANGEDEH = WM_USER + 101;

type

{ IInplaceEditHolderEh }

  IInplaceEditHolderEh = interface
    ['{4BE708F1-4EA2-4AC7-BA64-89D7D2B83E09}']
    function InplaceEditCanModify(Control: TWinControl): Boolean;

    procedure GetMouseDownInfo(var Pos: TPoint; var Time: LongInt);
    procedure InplaceEditWndProc(Control: TWinControl; var Message: TMessage);
    procedure InplaceEditKeyDown(Control: TWinControl; var Key: Word; Shift: TShiftState);
    procedure InplaceEditKeyPress(Control: TWinControl; var Key: Char);
    procedure InplaceEditKeyUp(Control: TWinControl; var Key: Word; Shift: TShiftState);
    procedure InternalSetFocusedControl(Control: TWinControl);
  end;

{ IInplaceEditEh }

  IInplaceEditEh = interface
    ['{81F0C558-B001-4477-BAA6-2DC373FCDF88}']
    function GetFont: TFont;
    procedure SetInplaceEditHolder(AInplaceEditHolder: TWinControl);

    procedure SetBorderStyle(ABorderStyle: TBorderStyle);
    procedure SetFont(AFont: TFont);
    procedure SetColor(AColor: TColor);
    procedure SetOnKeyPress(AKeyPressEvent: TKeyPressEvent);
    procedure SetOnExit(AKeyPressEvent: TNotifyEvent);
  end;

{ TEditImageEh }

  TEditImageEh = class(TPersistent)
  private
    FEditControl: TWinControl;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FUseImageHeight: Boolean;
    FVisible: Boolean;
    FWidth: Integer;

    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetUseImageHeight(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Integer);

  public
    constructor Create(EditControl: TWinControl);
    destructor Destroy; override;
    function Showing: Boolean;
    function DrawWidth: Integer;
    procedure Assign(Source: TPersistent); override;

  published
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property UseImageHeight: Boolean read FUseImageHeight write SetUseImageHeight default True;
    property Visible: Boolean read FVisible write SetVisible default False;
    property Width: Integer read FWidth write SetWidth default 0;
  end;

{ TFieldDataLinkEh }

  TFieldDataLinkEh = class(TDataLink)
  private
    FFields: TFieldsArrEh;
    FFieldName: string;
    FControl: TComponent;
    FOnDataChange: TNotifyEvent;
    FOnEditingChange: TNotifyEvent;
    FOnUpdateData: TNotifyEvent;
    FOnActiveChange: TNotifyEvent;
    FMultiFields: Boolean;
    FDataIndepended: Boolean;
    FEditing: Boolean;
    FModified: Boolean;

    function GetActive: Boolean;
    function GetCanModify: Boolean;
    function GetDataSetActive: Boolean;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldsCount: Integer;
    function GetFieldsField(Index: Integer): TField;

    procedure SetDataSource(const Value: TDataSource);
    procedure SetEditing(Value: Boolean);
    procedure SetField(Value: TObjectList);
    procedure SetFieldName(const Value: string);
    procedure SetMultiFields(const Value: Boolean);
    procedure UpdateRightToLeft;

  protected
    function FieldFound(Value: TField): Boolean;
    procedure ActiveChanged; override;
    procedure DataEvent(Event: TDataEvent; Info: TDataEventInfoTypeEh); override;
    procedure EditingChanged; override;
{$IFDEF CIL}
    procedure FocusControl(const Field: TField); override;
{$ELSE}
    procedure FocusControl(Field: TFieldRef); override;
{$ENDIF}
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
    procedure UpdateDataIndepended;
    procedure UpdateField; virtual;
  public
    DataIndependentValue: Variant;

    constructor Create;
    function Edit: Boolean;
    function IsDataIndepended: Boolean; virtual;
    procedure Modified;
    procedure SetModified(Value: Boolean);
    procedure SetText(const Text: String);
    procedure SetValue(Value: Variant);
    procedure Reset;

    property Active: Boolean read GetActive;
    property CanModify: Boolean read GetCanModify;
    property Control: TComponent read FControl write FControl;
    property DataIndepended: Boolean read FDataIndepended;
    property DataSetActive: Boolean read GetDataSetActive;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Editing: Boolean read FEditing;
    property Field: TField read GetField;
    property FieldName: string read FFieldName write SetFieldName;
    property Fields[Index: Integer]: TField read GetFieldsField;
    property FieldsCount: Integer read GetFieldsCount;
    property MultiFields: Boolean read FMultiFields write SetMultiFields;
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnEditingChange: TNotifyEvent read FOnEditingChange write FOnEditingChange;
    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;
  end;

  TCustomDBEditEh = class;

{ TCustomControlEmptyDataInfoEh }

  TCustomControlEmptyDataInfoEh = class(TPersistent)
  private
    FAlignment: TAlignment;
    FAlignmentIsStored: Boolean;
    FColor: TColor;
    FFont: TFont;
    FParentFont: Boolean;
    FText: String;
    function DefaultFont: TFont;
    function GetAlignment: TAlignment;
    function IsAlignmentStored: Boolean;
    function IsFontStored: Boolean;
    procedure FontChanged(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAlignmentIsStored(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetParentFont(const Value: Boolean);
    procedure SetText(const Value: String);
  protected
    FControl: TWinControl;

    function GetControlAlignment: TAlignment; virtual;
    function GetControlFont: TFont;  virtual;
    function GetControlColor: TColor; virtual;
    function ControlIsEmpty: Boolean; virtual;
    function GetPaintColor: TColor; virtual;

    property Color: TColor read FColor write FColor default clDefault;
    property Text: String read FText write SetText;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property ParentFont: Boolean read FParentFont write SetParentFont default True;
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property AlignmentIsStored: Boolean read IsAlignmentStored write SetAlignmentIsStored stored False;
  public
    constructor Create(AControl: TWinControl);
    destructor Destroy; override;

    procedure PaintEmptyDataInfo; virtual;
    procedure RefreshDefaultFont;
    function Showing: Boolean;
  end;

{ TControlEmptyDataInfoEh }

  TControlEmptyDataInfoEh = class(TCustomControlEmptyDataInfoEh)
  protected
    function GetControlAlignment: TAlignment; override;
    function GetControlFont: TFont;  override;
    function ControlIsEmpty: Boolean; override;
    function GetControl: TCustomDBEditEh; virtual;

  public
    constructor Create(AControl: TCustomDBEditEh);
    destructor Destroy; override;

    procedure PaintEmptyDataInfo; override;

  published
    property Color;
    property Text;
    property Font;
    property ParentFont;
    property Alignment;
    property AlignmentIsStored;
  end;

{ TEditButtonsBoxEh  }

  TEditButtonsBoxEh = class(TWinControl)
  private
    FLayoutCount: Integer;
    FBtnCtlList: TEditButtonControlList;
    FOnDown: TButtonDownEventEh;
    FOnClick: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FFlat: Boolean;
    FButtonsWidth: Integer;
    FButtonHeight: Integer;
    FMaxButtonHeight: Integer;
    FBorderActive: Boolean;
    FOnCreateEditButtonControl: TCreateEditButtonControlEvent;

    function GetButtonsCount: Integer;

    procedure SetBorderActive(const Value: Boolean);
    procedure SetButtonsCount(const Value: Integer);

    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    {$IFDEF FPC}
  protected
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginLayout;
    procedure EndLayout;
    procedure LayoutChanged;
    procedure UpdateEditButtonControlList;
    procedure UpdateEditButtonControlsState;

    property BorderActive: Boolean read FBorderActive write SetBorderActive;
    property BtnCtlList: TEditButtonControlList read FBtnCtlList;
    property ButtonHeight: Integer read FButtonHeight;
    property ButtonsCount: Integer read GetButtonsCount write SetButtonsCount;
    property ButtonsWidth: Integer read FButtonsWidth;
    property Flat: Boolean read FFlat write FFlat;
    property MaxButtonHeight: Integer read FMaxButtonHeight write FMaxButtonHeight;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCreateEditButtonControl: TCreateEditButtonControlEvent read FOnCreateEditButtonControl write FOnCreateEditButtonControl;
    property OnDown: TButtonDownEventEh read FOnDown write FOnDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  end;

{ IControlLabelOwnerEh }

  IControlLabelOwnerEh = interface
  ['{5EE8C2C7-BD36-4131-9617-FF023104A331}']
    function GetControlLabelCaption: String;
    function GetControlTextBaseLine: Integer;
    procedure AdjustLabelBounds;
    procedure LabelSpacingChanged;
  end;

{ TControlLabelEh }

  TControlLabelEh = class(TCustomLabel)
  private
    FCaptionStored: Boolean;
    FVisible: Boolean;

    function GetCaption: TCaption;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    function IsCaptionStored: Boolean;

    procedure SetCaption(const Value: TCaption);
    procedure SetHeight(const Value: Integer);
    procedure SetVisible(const Value: Boolean); {$IFDEF FPC} reintroduce; {$ENDIF}
    procedure SetWidth(const Value: Integer);
    function IsHeightStored: Boolean;
    function IsWidthStored: Boolean;

  protected
    {$IFDEF FPC}
    {$ELSE}
    procedure AdjustBounds; override;
    {$ENDIF}
    procedure Loaded; override;
    procedure UpdateVisibility; virtual;
    procedure UpdateCaption; virtual;

  public
    constructor Create(AOwner: TComponent); override;

    {$IFDEF FPC}
    procedure AdjustSize; override;
    {$ENDIF}
    procedure UpdateParent;

  published
    property BiDiMode;
    property Caption: TCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property Height: Integer read GetHeight write SetHeight stored IsHeightStored;
    property Layout;
    property Left: Integer read GetLeft;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
{$IFDEF EH_LIB_17}
    property StyleElements;
{$ENDIF}
    property Top: Integer read GetTop;
    {$IFDEF EH_LIB_13}
    property Touch;
    {$ENDIF}
    property Transparent;
    property Visible: Boolean read GetVisible write SetVisible default False;
    property Width: Integer read GetWidth write SetWidth stored IsWidthStored;
    property WordWrap;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF EH_LIB_13}
    property OnGesture;
    {$ENDIF}
    {$IFDEF EH_LIB_9}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TSpacingBoundEh = (sbNearBoundEh, sbFarBoundEh);
  TLabelPositionEh = (lpAboveLeftEh, lpAboveCenterEh, lpAboveRightEh,
                      lpBelowLeftEh, lpBelowCenterEh, lpBelowRightEh,
                      lpLeftTopEh, lpLeftTextBaselineEh,  lpLeftCenterEh, lpLeftBottomEh,
                      lpRightTopEh, lpRightTextBaselineEh, lpRightCenterEh, lpRightBottomEh);


{ TControlLabelLocationEh }

  TControlLabelLocationEh = class(TPersistent)
  private
    FSpacing: Integer;
    FOffset: Integer;
    FPosition: TLabelPositionEh;
    FEditControl: TControl;
    FLabelSpacingBound: TSpacingBoundEh;
    procedure SetOffset(const Value: Integer);
    procedure SetPosition(const Value: TLabelPositionEh);
    procedure SetSpacing(const Value: Integer);
    procedure SetLabelSpacingBound(const Value: TSpacingBoundEh);

  public
    constructor Create(AEditControl: TControl);
    destructor Destroy; override;
    procedure CalcLabelPosForControl(LabelWidth, LabelHeight: Integer; var LabelPos: TPoint);

  published
    property LabelSpacingBound: TSpacingBoundEh read FLabelSpacingBound write SetLabelSpacingBound default sbNearBoundEh;
    property Spacing: Integer read FSpacing write SetSpacing default 3;
    property Offset: Integer read FOffset write SetOffset default 0;
    property Position: TLabelPositionEh read FPosition write SetPosition default lpAboveLeftEh;
  end;

{ TCustomDBEditEh }

  TGetImageIndexEventEh = procedure(Sender: TObject; var ImageIndex: Integer) of object;
  TOnCheckDrawRequiredStateEventEh = procedure(Sender: TObject; var DrawState: Boolean) of object;
  TEditButtonDefaultActionProc = procedure(EditControl: TControl;
    EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh;
    IsMouseDown: Boolean; var Handled: Boolean);


  TDBEditEhValue = (evAlignmentEh, evEditMaskEh);
  TDBEditEhValues = set of TDBEditEhValue;

  TCustomDBEditEh = class(TCustomMaskEdit, IInplaceEditEh, IComboEditEh,
    IControlLabelOwnerEh, IUnknown)
  private
    FAlwaysShowBorder: Boolean;
    FAssignedValues: TDBEditEhValues;
    FCanvas: TControlCanvas;
    FCompleteKeyPress: String;
    FControlLabel: TControlLabelEh;
    FControlLabelLocation: TControlLabelLocationEh;
    FDynProps: TDynVarsEh;
    FEditButton: TEditButtonEh;
    FEditButtons: TEditButtonsEh;
    FEditImage: TEditImageEh;
    FEmptyDataInfo: TControlEmptyDataInfoEh;
    FFlat: Boolean;
    FHighlightRequired: Boolean;
    FMRUList: TMRUListEh;
    FMRUListControl: TWinControl;
    FReadOnly: Boolean;
    FShowHint: Boolean;
    FTooltips: Boolean;
    FWantReturns: Boolean;
    FWantTabs: Boolean;
    FWordWrap: Boolean;

    FOnButtonClick: TButtonClickEventEh;
    FOnButtonDown: TButtonDownEventEh;
    FOnCheckDrawRequiredState: TOnCheckDrawRequiredStateEventEh;
    FOnCloseDropDownForm: TEditControlCloseDropDownFormEventEh;
    FOnGetFieldData: TGetFieldDataEventEh;
    FOnGetImageIndex: TGetImageIndexEventEh;
    FOnOpenDropDownForm: TEditControlShowDropDownFormEventEh;
    FOnUpdateData: TUpdateDataEventEh;

    function CheckHintTextRect(var TextWidth, TextHeight: Integer): Boolean;
    function GetAlignment: TAlignment;
    function GetCanvas: TCanvas;
    function GetEditMask: String;
    function GetField: TField;
    function GetImages: TCustomImageList;
    function GetMRUListControl: TWinControl;
    function GetPasswordChar: Char;
    function GetReadOnly: Boolean; {$IFDEF FPC} reintroduce; {$ENDIF}
    function GetShowHint: Boolean;
    function GetText: String;
    function GetTextMargins: TPoint;
    function GetValue: Variant;
    function GetVisible: Boolean;
    function ImageRect: TRect;
    function IsAlignmentStored: Boolean;
    function IsEditMaskStored: Boolean;
    function IsTextStored: Boolean;
    function IsValueStored: Boolean;

    procedure ActiveChange(Sender: TObject);
    procedure CheckCursor;
    {$IFDEF FPC}
    {$ELSE}
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    {$ENDIF}
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMEditImageChangedEh(var Message: TMessage); message CM_EDITIMAGECHANGEDEH;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message: TMessage); message CM_MOUSEWHEEL;
    procedure CMParentShowHintChanged(var Message: TMessage); message CM_PARENTSHOWHINTCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;

    procedure DataChange(Sender: TObject);
    procedure DrawBorder(DC: HDC; ActiveBorder: Boolean);
    procedure DrawEditImage(DC: HDC);
    procedure EditButtonChanged(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure InternalMove(const Loc: TRect; Redraw: Boolean);
    procedure InternalUpdateData(Sender: TObject);
    procedure ReadEditMask(Reader: TReader);
    procedure RecreateWndHandle;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAlwaysShowBorder(const Value: Boolean);
    procedure SetControlLabelParams(const Value: TControlLabelLocationEh);
    procedure SetDynProps(const Value: TDynVarsEh);
    procedure SetEditButton(const Value: TEditButtonEh);
    procedure SetEditButtons(const Value: TEditButtonsEh);
    procedure SetEditImage(const Value: TEditImageEh);
    procedure SetEditMask(const Value: String);
    procedure SetEditRect;
    procedure SetEmptyDataInfo(const Value: TControlEmptyDataInfoEh);
    procedure SetFlat(const Value: Boolean);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetMRUList(const Value: TMRUListEh);
    procedure SetOnGetImageIndex(const Value: TGetImageIndexEventEh);
    procedure SetPasswordChar(const Value: Char);
    procedure SetReadOnly(Value: Boolean); {$IFDEF FPC} reintroduce; {$ENDIF}
    procedure SetShowHint(const Value: Boolean);
    procedure SetText(const Value: String); {$IFDEF CIL} reintroduce; {$ENDIF}
    procedure SetTooltips(const Value: Boolean);
    procedure SetValue(const Value: Variant);
    procedure SetVisible(const Value: Boolean); {$IFDEF FPC} reintroduce; {$ENDIF}
    procedure SetWordWrap(const Value: Boolean);
    procedure UpdateDrawBorder;
    procedure WriteEditMask(Writer: TWriter);

    {$IFDEF FPC_CROSSP}
    {$ELSE}
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMUndo(var Message: TWMUndo); message WM_UNDO;
    {$ENDIF}
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMClear(var Message: TWMCut); message WM_CLEAR;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

  protected
    FAlignment: TAlignment;
    FBorderActive: Boolean;
    FButtonHeight: Integer;
    FButtonsBox: TEditButtonsBoxEh;
    FDataLink: TFieldDataLinkEh;
    FDataPosting: Boolean;
    FDownButton: Integer;
    FDroppedDown: Boolean;
    FDroppedDownButton: TEditButtonEh;
    FDroppedDownButtonControl: TEditButtonControlEh;
    FFocused: Boolean;
    FImageWidth: Integer;
    FInplaceEditHolder: TWinControl;
    FInplaceMode: Boolean;
    FInternalDataSourceRef: TDataSource;
    FInternalTextSetting: Boolean;
    FIntfInplaceEditHolder: IInplaceEditHolderEh;
    FMouseAboveControl: Boolean;
    FNoClickCloseUp: Boolean;
    FPressed: Boolean;
    FPressedRect: TRect;
    FUserTextChanged: Boolean;

    {$IFDEF FPC}
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
    procedure DoAutoSize; override;
    procedure Resize; override;
    {$ENDIF}

    function EditCanModify: Boolean; override;

    function ButtonEnabled: Boolean; virtual;
    function ButtonRect: TRect;
    function CreateDataLink: TFieldDataLinkEh; virtual;
    function CreateEditButton: TEditButtonEh; virtual;
    function CreateEditButtons: TEditButtonsEh; virtual;
    function CreateEditImage: TEditImageEh; virtual;
    function CreateMRUListControl: TWinControl; virtual;
    function DataIndepended: Boolean; virtual;
    function DefaultAlignment: TAlignment; virtual;
    function DefaultEditMask: String; virtual;
    function DefaultImageIndex: Integer; virtual;
    function EditButtonDefaultAction(AEditButton: TEditButtonEh): Boolean; virtual;
    function EditRect: TRect;
    function FixClipboardText(const Text: String): String; virtual;
    function GetControlLabelCaption: String; virtual;
    function GetControlTextBaseLine: Integer; virtual;
    function GetDataField: string; virtual;
    function GetDataSource: TDataSource; virtual;
    function GetDisplayTextForPaintCopy: String; virtual;
    function GetEditButtonByShortCut(ShortCut: TShortCut): TEditButtonEh;
    function GetFillColor: TColor; virtual;
    function GetFont: TFont;
    function GetFontColor: TColor; virtual;
    function GetVariantValue: Variant; virtual;
    function IsValidChar(InputChar: Char): Boolean; virtual;
    function IsWindowVisibleState: Boolean;
    function PostDataEvent: Boolean;

    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintWindow(DC: HDC); override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure WndProc(var Message: TMessage); override;

    procedure ActiveChanged; virtual;
    procedure AdjustHeight; virtual;
    procedure AdjustLabelBounds; virtual;
    procedure BeforeShowDefaulEditDropDownForm(EditControl: TControl; Button: TEditButtonEh; var DropDownForm: TCustomForm; DynParams: TDynVarsEh); virtual;
    procedure CalcEditRect(var ARect: TRect); virtual;
    procedure CheckEditButtonsRemoveNotification(AComponent: TComponent);
    procedure CheckInplaceEditHolderKeyDown(var Key: Word; Shift: TShiftState);
    procedure CheckInplaceEditHolderKeyPress(var Key: Char);
    procedure CheckInplaceEditHolderKeyUp(var Key: Word; Shift: TShiftState);
    procedure CloseUp(Accept: Boolean); virtual;
    procedure CreateEditButtonControl(var EditButtonControl: TEditButtonControlEh); virtual;
    procedure DataChanged; virtual;
    procedure DropDownAction(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; var Handled: Boolean); virtual;
    procedure DropDownFormCallbackProc(DropDownForm: TCustomForm; Accept: Boolean; DynParams: TDynVarsEh; SysParams: TDropDownFormSysParams);
    procedure DropDownFormCloseProc(EditControl: TControl; Button: TEditButtonEh; Accept: Boolean; DropDownForm: TCustomForm; DynParams: TDynVarsEh);
    procedure EditButtonClick(Sender: TObject); virtual;
    procedure EditButtonClickDefaultAction(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; TopButton: Boolean; var Handled: Boolean); virtual;
    procedure EditButtonDown(Sender: TObject; TopButton: Boolean; var AutoRepeat: Boolean; var Handled: Boolean); virtual;
    procedure EditButtonDownDefaultAction(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; TopButton: Boolean; var AutoRepeat: Boolean; var Handled: Boolean); virtual;
    procedure EditButtonImagesRefComponentNotifyEvent(Sender: TObject; RefComponent: TComponent);
    procedure EditButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
    procedure EditButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure EditingChanged; virtual;
    procedure FilterMRUItem(const AText: String; var Accept: Boolean); virtual;
    procedure GetDefaultDropDownForm(var DropDownForm: TCustomForm; var FreeFormOnClose: Boolean); virtual;
    procedure GetVarValue(var VarValue: Variant); virtual;
    procedure InternalSetText(const AText: String); virtual;
    procedure InternalSetValue(AValue: Variant); virtual;
    procedure InternalUpdatePostData; virtual;
    procedure LabelSpacingChanged; virtual;
    procedure MRUListCloseUp(Sender: TObject; Accept: Boolean);
    procedure MRUListControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MRUListDropDown(Sender: TObject);
    procedure MRUListFillAutogenItems(Sender: TMRUListEh; AutogenItems: TStrings); virtual;
    procedure PaintRequiredState(ACanvas: TCanvas); virtual;
    procedure ResetMaxLength; virtual;
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetBorderStyle(ABorderStyle: TBorderStyle); {$IFDEF FPC} reintroduce; {$ENDIF}
    procedure SetColor(AColor: TColor); {$IFDEF FPC} reintroduce; {$ENDIF}
    procedure SetControlEditMask(const Value: string);
    procedure SetControlReadOnly(Value: Boolean);
    procedure SetDataField(const Value: string); virtual;
    procedure SetDataSource(Value: TDataSource); virtual;
    procedure SetEditButtonClosedUp; virtual;
    procedure SetEditButtonDroppedDown(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh); virtual;
    procedure SetFocused(Value: Boolean); virtual;
    procedure SetFont(AFont: TFont);
    procedure SetInplaceEditHolder(AInplaceEditHolder: TWinControl);
    procedure SetOnExit(AKeyPressEvent: TNotifyEvent);
    procedure SetOnKeyPress(AKeyPressEvent: TKeyPressEvent);
    procedure SetVariantValue(const VariantValue: Variant); virtual;
    procedure SetVarValue(const VarValue: Variant); virtual;
    procedure UpdateControlReadOnly; virtual;
    procedure UpdateEditButtonControlList;
    procedure UpdateEditButtonControlsState;
    procedure UpdateHeight; virtual;
    procedure UpdateHintProcessing; virtual;
    procedure UpdateImageIndex; virtual;
    procedure UserChange; virtual;

    property AssignedValues: TDBEditEhValues read FAssignedValues;
    property Canvas: TCanvas read GetCanvas;
    property EditButton: TEditButtonEh read FEditButton write SetEditButton;
    property EditButtons: TEditButtonsEh read FEditButtons write SetEditButtons;
    property EditImage: TEditImageEh read FEditImage write SetEditImage;
    property HighlightRequired: Boolean read FHighlightRequired write FHighlightRequired default False;
    property Images: TCustomImageList read GetImages write SetImages;
    property MRUList: TMRUListEh read FMRUList write SetMRUList;
    property MRUListControl: TWinControl read GetMRUListControl;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar default #0;
    property WantReturns: Boolean read FWantReturns write FWantReturns default False;
    property WantTabs: Boolean read FWantTabs write FWantTabs default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;

    property OnButtonClick: TButtonClickEventEh read FOnButtonClick write FOnButtonClick;
    property OnButtonDown: TButtonDownEventEh read FOnButtonDown write FOnButtonDown;
    property OnCheckDrawRequiredState: TOnCheckDrawRequiredStateEventEh read FOnCheckDrawRequiredState write FOnCheckDrawRequiredState;
    property OnGetImageIndex: TGetImageIndexEventEh read FOnGetImageIndex write SetOnGetImageIndex;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;

    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function GetCompleteKeyPress: String;
    function GetEditButtonControlByEditButton(AEditButton: TEditButtonEh): TEditButtonControlEh;
    function GetFirstDefaultActionEditButton: TEditButtonEh;
    function IsEmpty: Boolean; virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;

    {$IFDEF FPC}
    procedure Clear; virtual;
    {$ELSE}
    procedure Clear; override;
    {$ENDIF}
    procedure DefaultHandler(var Message); override;
    procedure Deselect;
    procedure Hide;
    procedure Move(const Loc: TRect);
    procedure Reset; override;
    procedure SetFocus; override;
    procedure Undo; {$IFDEF FPC} reintroduce; {$ENDIF}
    procedure UpdateData; virtual;
    procedure UpdateLoc(const Loc: TRect);
    {$IFDEF FPC}
    function Ctl3D: Boolean;
    {$ENDIF}

    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property AlwaysShowBorder: Boolean read FAlwaysShowBorder write SetAlwaysShowBorder default False;
    property ControlLabel: TControlLabelEh read FControlLabel;
    property ControlLabelLocation: TControlLabelLocationEh read FControlLabelLocation write SetControlLabelParams;
    property DataField: String read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DisplayTextForPaintCopy: String read GetDisplayTextForPaintCopy;
    property DynProps: TDynVarsEh read FDynProps write SetDynProps;
    property EditMask: String read GetEditMask write SetEditMask stored False;
    property EmptyDataInfo: TControlEmptyDataInfoEh read FEmptyDataInfo write SetEmptyDataInfo;
    property Field: TField read GetField;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Font;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint: Boolean read GetShowHint write SetShowHint default False;
    property Text: String read GetText write SetText stored IsTextStored;
    property Tooltips: Boolean read FTooltips write SetTooltips default False;
    property Value: Variant read GetValue write SetValue stored IsValueStored;
    property Visible: Boolean read GetVisible write SetVisible;

    property OnCloseDropDownForm: TEditControlCloseDropDownFormEventEh read FOnCloseDropDownForm write FOnCloseDropDownForm;
    property OnGetFieldData: TGetFieldDataEventEh read FOnGetFieldData write FOnGetFieldData;
    property OnOpenDropDownForm: TEditControlShowDropDownFormEventEh read FOnOpenDropDownForm write FOnOpenDropDownForm;
    property OnUpdateData: TUpdateDataEventEh read FOnUpdateData write FOnUpdateData;
  end;

  TDBEditEh = class(TCustomDBEditEh)
  published
    property Align;
    property Alignment;
    property AlwaysShowBorder;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    {$IFDEF FPC}
    {$ELSE}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    {$ENDIF}
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property ControlLabel;
    property ControlLabelLocation;
    property Constraints;
    {$IFDEF FPC}
    {$ELSE}
    property Ctl3D;
    {$ENDIF}
    property DataField;
    property DataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DynProps;
    property EditButtons;
    property EmptyDataInfo;
    property Enabled;
    property EditMask;
    property Font;
    property Flat;
    property HighlightRequired;
    property Images;
    {$IFDEF FPC}
    {$ELSE}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property MaxLength;
    property MRUList;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF FPC}
    {$ELSE}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
{$IFDEF EH_LIB_17}
    property StyleElements;
{$ENDIF}
    property TabOrder;
    property TabStop;
    property Text;
    property Tooltips;
{$IFDEF EH_LIB_13}
    property Touch;
{$ENDIF}
    property Visible;
    property WantTabs;
    property WantReturns;
    property WordWrap;

    property OnChange;
    property OnCheckDrawRequiredState;
    property OnClick;
    property OnCloseDropDownForm;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
{$IFDEF EH_LIB_13}
    property OnGesture;
{$ENDIF}
    property OnGetFieldData;
    property OnGetImageIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnOpenDropDownForm;
    property OnStartDock;
    property OnStartDrag;
    property OnUpdateData;
  end;

var
  DBEditEhEditButtonDefaultActionProc: TEditButtonDefaultActionProc;
  DefaultDBEditEhDropDownFormClass: TCustomDropDownFormClassEh;

procedure DefaultDBEditEhEditButtonDefaultAction(EditControl: TControl;
    EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh;
    IsMouseDown: Boolean; var Handled: Boolean);

type

{ TCustomDBDateTimeEditEh }

  TDateTimeKindEh = (dtkDateEh, dtkTimeEh, dtkDateTimeEh, dtkCustomEh);

  TElementMaskPosEh = record
    Pos, Length: Integer;
    Present: Boolean;
  end;

  TDateTimeElementsMaskPosEh = record
    Year: TElementMaskPosEh;
    Month: TElementMaskPosEh;
    Day: TElementMaskPosEh;
    Hour: TElementMaskPosEh;
    Min: TElementMaskPosEh;
    Sec: TElementMaskPosEh;
  end;

  TDateTimeStampEh = packed record
    Year : Integer;
    Month : Integer;
    Day : Integer;
    Hour : Integer;
    Minute : Integer;
    Second : Integer;
  end;

  function DateTimeStampToVarValue(DateTimeStamp: TDateTimeStampEh;
    var DateTimeMaskPos: TDateTimeElementsMaskPosEh; var DateTimeVal: Variant;
    AutoCorrect, RaiseError: Boolean): Boolean;
  function DateTimeStrToDate(const DateTimeStr: String;
    var DateTimeMaskPos: TDateTimeElementsMaskPosEh; var DateTimeStamp: TDateTimeStampEh): Boolean;
  function RemoveNonFormatDateTimeText(const EditFormat: String): String;

type
  TCustomDBDateTimeEditEh = class(TCustomDBEditEh)
  private
    FCalendarVisible: Boolean;
    FDateTimeFormat: String;
    FDropDownCalendar: TWinControl;
    FEditFormat: String;
    FEditValidating: Boolean;
    FInternalTextSetting: Boolean;
    FKind: TDateTimeKindEh;
    FIgnoreMouseScreenRect: TRect;
    FIgnoreNextDropDown: Boolean;

    FOnCloseUp: TCloseUpEventEh;
    FOnDropDown: TNotifyEvent;

    function GetDropDownCalendar: TWinControl;
    function IsEditFormatStored: Boolean;
    function IsKindStored: Boolean;
    {$IFDEF FPC}
    {$ELSE}
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    {$ENDIF}
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMMouseWheel(var Message: TMessage); message CM_MOUSEWHEEL;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;

    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;

    procedure CloseWinCallbackProc(Control: TWinControl; Accept: Boolean);
    procedure ReadEditFormat(Reader: TReader);
    procedure SetEditFormat(const Value: String);
    procedure SetKind(const Value: TDateTimeKindEh);
    procedure UpdateValueFromText;
    procedure WriteEditFormat(Writer: TWriter);
  protected
    FDateTimeMaskPos: TDateTimeElementsMaskPosEh;
    FFourDigitYear: Boolean;
    FValue: Variant;

    function CreateEditButton: TEditButtonEh; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function GetDisplayTextForPaintCopy: String; override;
    function GetVariantValue: Variant; override;
    function TextDateTimeToVarValue(const AText: string; var AValue: Variant; const AAutoCorrect, ARaiseError: Boolean): Boolean; virtual;

    procedure Change; override;
    procedure DataChanged; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DropDownAction(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; var Handled: Boolean); override;
    procedure EditButtonClickDefaultAction(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; TopButton: Boolean; var Handled: Boolean); override;
    procedure EditButtonDownDefaultAction(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; TopButton: Boolean; var AutoRepeat: Boolean; var Handled: Boolean); override;
    procedure EditButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure FilterMRUItem(const AText: String; var Accept: Boolean); override;
    procedure IncrementItemAtCurPos(IsIncrease: Boolean); virtual;
    procedure InternalSetControlText(const AText: String); virtual;
    procedure InternalSetText(const AText: String); override;
    procedure InternalSetValue(AValue: Variant); override;
    procedure InternalUpdatePostData; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure UpdateFourDigitYear; virtual;
    procedure WndProc(var Message: TMessage); override;

    property DropDownCalendar: TWinControl read GetDropDownCalendar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function DateTimeFormat: String;
    function IsEmpty: Boolean; override;

    procedure CloseUp(Accept: Boolean); override;
    procedure DropDown; virtual;
    procedure UpdateMask; virtual;
    procedure ValidateEdit; override;

    property CalendarVisible: Boolean read FCalendarVisible;
    property EditFormat: String read FEditFormat write SetEditFormat stored False;
    property Kind: TDateTimeKindEh read FKind write SetKind stored IsKindStored;
    property OnCloseUp: TCloseUpEventEh read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
  end;

{ TDBDateTimeEditEh }

  TDBDateTimeEditEh = class(TCustomDBDateTimeEditEh)
  published
    property ControlLabel;
    property ControlLabelLocation;

    property Align;
    property Alignment;
    property AlwaysShowBorder;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    {$IFDEF FPC}
    {$ELSE}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    {$ENDIF}
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    {$IFDEF FPC}
    {$ELSE}
    property Ctl3D;
    {$ENDIF}
    property DataField;
    property DataSource;
    property DynProps;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EditButton;
    property EditButtons;
    property EditFormat;
    property EmptyDataInfo;
    property Font;
    property Flat;
    property HighlightRequired;
    property Images;
    {$IFDEF FPC}
    {$ELSE}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property Kind;
    property MRUList;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF FPC}
    {$ELSE}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
{$IFDEF EH_LIB_17}
    property StyleElements;
{$ENDIF}
    property TabOrder;
    property TabStop;
    property Tooltips;
{$IFDEF EH_LIB_13}
    property Touch;
{$ENDIF}
    property Value;
    property Visible;

    property OnButtonClick;
    property OnButtonDown;
    property OnChange;
    property OnCheckDrawRequiredState;
    property OnClick;
    property OnCloseDropDownForm;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
{$IFDEF EH_LIB_13}
    property OnGesture;
{$ENDIF}
    property OnGetImageIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnOpenDropDownForm;
    property OnStartDock;
    property OnStartDrag;
    property OnUpdateData;
  end;

{ TDropDownBoxEh }

  TDropDownBoxEh = class(TPersistent)
  private
    FAlign: TDropDownAlign;
    FAutoDrop: Boolean;
    FRows: Integer;
    FSizable: Boolean;
    FWidth: Integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Align: TDropDownAlign read FAlign write FAlign default daLeft;
    property AutoDrop: Boolean read FAutoDrop write FAutoDrop default False;
    property Rows: Integer read FRows write FRows default 7;
    property Sizable: Boolean read FSizable write FSizable default False;
    property Width: Integer read FWidth write FWidth default 0;
  end;

{ TCustomDBComboBoxEh }

  TCustomDBComboBoxEh = class(TCustomDBEditEh)
  private
    FCaseInsensitiveTextSearch: Boolean;
    FDropDownBox: TDropDownBoxEh;
    FItems: TStrings;
    FKeyItems: TStrings;
    FLimitTextToListValues: Boolean;
    FLimitTextToListValuesStored: Boolean;
    FListVisible: Boolean;
    FPopupListbox: TWinControl;
    FPopupListboxClass: TWinControlClass;
    FWheelEventInListbox: Boolean;

    FOnCloseUp: TCloseUpEventEh;
    FOnClosingUp: TAcceptEventEh;
    FOnDropDown: TNotifyEvent;
    FOnGetItemImageIndex: TListGetImageIndexEventEh;
    FOnGetItemsList: TNotifyEvent;
    FOnNotInList: TNotInListEventEh;

    function DefaultLimitTextToListValues: Boolean;
    function GetImages: TCustomImageList;
    function GetLimitTextToListValues: Boolean;
    function IsLimitTextToListValuesStored: Boolean;

    {$IFDEF FPC}
    {$ELSE}
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    {$ENDIF}
    procedure CMMouseWheel(var Message: TMessage); message CM_MOUSEWHEEL;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;

    procedure WMClear(var Message: TWMCut); message WM_CLEAR;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;

    procedure ItemsChanged(Sender: TObject);
    procedure KeyItemsChanged(Sender: TObject);
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetCaseInsensitiveTextSearch(const Value: Boolean);
    procedure SetDropDownBox(const Value: TDropDownBoxEh);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetItemIndex(const Value: Integer);
    procedure SetItems(const Value: TStrings);
    procedure SetKeyItems(const Value: TStrings);
    procedure SetLimitTextToListValues(const Value: Boolean);
    procedure SetLimitTextToListValuesStored(const Value: Boolean);

  protected
    FDefaultItemIndex: Integer;
    FItemIndex: Integer;
    FItemsCount: Integer;
    FKeyBased: Boolean;
    FVarValue: Variant;

    function ConvertDataText(const Value: String): String;
    function CreateDropDownBox: TDropDownBoxEh; virtual;
    function CreateEditButton: TEditButtonEh; override;
    function DefaultAlignment: TAlignment; override;
    function DefaultImageIndex: Integer; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function GetDisplayTextForPaintCopy: String; override;
    function GetPopupListbox: TWinControl;
    function GetPopupListboxColor: TColor; virtual;
    function GetVariantValue: Variant; override;
    function IsValidChar(InputChar: Char): Boolean; override;
    function LocateStr(const Str: String; PartialKey: Boolean): Boolean; virtual;
    function ProcessSearchStr(const Str: String): Boolean; virtual;
    function SelfPopupListboxFont: TFont; virtual;
    function TextListIndepended: Boolean;
    function TraceMouseMoveForPopupListbox(Sender: TObject; Shift: TShiftState; X, Y: Integer): Boolean;

    procedure Change; override;
    procedure Click; override;
    procedure DataChanged; override;
    procedure DropDownAction(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; var Handled: Boolean); override;
    procedure EditButtonClick(Sender: TObject); override;
    procedure EditButtonClickDefaultAction(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; TopButton: Boolean; var Handled: Boolean); override;
    procedure EditButtonDownDefaultAction(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; TopButton: Boolean; var AutoRepeat: Boolean; var Handled: Boolean); override;
    procedure EditButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure GetItemsList; virtual;
    procedure InternalSetItemIndex(const Value: Integer); virtual;
    procedure InternalSetText(const AText: String); override;
    procedure InternalSetValue(AValue: Variant); override;
    procedure InternalUpdatePostData; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PopupListboxGetImageIndex(Sender: TObject; ItemIndex: Integer; var ImageIndex: Integer);
    procedure ResetMaxLength; override;
    procedure SetVariantValue(const VariantValue: Variant); override;
    procedure UpdateControlReadOnly; override;
    procedure UpdateImageIndex; override;
    procedure UpdateItemIndex; virtual;
    procedure UpdateItems;
    procedure UpdatePopupListboxItemIndex; virtual;
    procedure WndProc(var Message: TMessage); override;

    property PopupListbox: TWinControl read GetPopupListbox;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure CloseUp(Accept: Boolean); override;
    procedure DefaultHandler(var Message); override;
    procedure DropDown(AEditButton: TEditButtonEh = nil); virtual;
    procedure SelectNextValue(IsPrior: Boolean); virtual;
    procedure UpdateData; override;

    property CaseInsensitiveTextSearch: Boolean read FCaseInsensitiveTextSearch write SetCaseInsensitiveTextSearch default True;
    property DropDownBox: TDropDownBoxEh read FDropDownBox write SetDropDownBox;
    property HighlightRequired;
    property Images: TCustomImageList read GetImages write SetImages;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Items: TStrings read FItems write SetItems;
    property KeyItems: TStrings read FKeyItems write SetKeyItems;
    property LimitTextToListValues: Boolean read GetLimitTextToListValues write SetLimitTextToListValues stored IsLimitTextToListValuesStored;
    property LimitTextToListValuesStored: Boolean read IsLimitTextToListValuesStored write SetLimitTextToListValuesStored stored False;
    property ListVisible: Boolean read FListVisible;
    property PopupListboxClass: TWinControlClass read FPopupListboxClass write FPopupListboxClass;

    property OnCloseUp: TCloseUpEventEh read FOnCloseUp write FOnCloseUp;
    property OnClosingUp: TAcceptEventEh read FOnClosingUp write FOnClosingUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnGetItemImageIndex: TListGetImageIndexEventEh read FOnGetItemImageIndex write FOnGetItemImageIndex;
    property OnGetItemsList: TNotifyEvent read FOnGetItemsList write FOnGetItemsList;
    property OnNotInList: TNotInListEventEh read FOnNotInList write FOnNotInList;
  end;

{ TDBComboBoxEh }

  TDBComboBoxEh = class(TCustomDBComboBoxEh)
  published
    property ControlLabel;
    property ControlLabelLocation;

    property Align;
    property Alignment;
    property AlwaysShowBorder;
    property Anchors;
    property AutoSelect;
    property AutoSize;

    {$IFDEF FPC}
    {$ELSE}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    {$ENDIF}

    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    {$IFDEF FPC}
    {$ELSE}
    property Ctl3D;
    {$ENDIF}
    property DataField;
    property DataSource;
    property DynProps;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownBox;
    property EmptyDataInfo;
    property Enabled;
    property EditButton;
    property EditButtons;
    property EditMask;
    property Font;
    property Flat;
    property HighlightRequired;
    property Images;
    {$IFDEF FPC}
    {$ELSE}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property Items;
    property KeyItems;
    property LimitTextToListValues;
    property LimitTextToListValuesStored;
    property MaxLength;
    property MRUList;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF FPC}
    {$ELSE}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
{$IFDEF EH_LIB_17}
    property StyleElements;
{$ENDIF}
    property TabOrder;
    property TabStop;
    property Text;
    property CaseInsensitiveTextSearch;
    property Tooltips;
{$IFDEF EH_LIB_13}
    property Touch;
{$ENDIF}
    property Visible;
    property WordWrap;

    property OnButtonClick;
    property OnButtonDown;
    property OnChange;
    property OnCheckDrawRequiredState;
    property OnClick;
    property OnCloseDropDownForm;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
{$IFDEF EH_LIB_13}
    property OnGesture;
{$ENDIF}
    property OnGetImageIndex;
    property OnGetItemImageIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNotInList;
    property OnOpenDropDownForm;
    property OnStartDock;
    property OnStartDrag;
    property OnUpdateData;
  end;

{ TCustomDBNumberEdit }

  TDBNumberValue = (evDisplayFormatEh, evCurrencyEh, evMaxValueEh, evMinValueEh);
  TDBNumberValues = set of TDBNumberValue;

  TCustomDBNumberEditEh = class(TCustomDBEditEh)
  private
    FAssignedValues: TDBNumberValues;
    FCalculatorVisible: Boolean;
    FCurrency: Boolean;
    FDecimalPlaces: Cardinal;
    FDisplayFormat: String;
    FDropDownCalculator: TWinControl;
    FEditFormat: String;
    FIncrement: Extended;
    FInternalTextSetting: Boolean;
    FMinValue, FMaxValue: Extended;
    FValue: Variant;

    FOnCloseUp: TCloseUpEventEh;
    FOnDropDown: TNotifyEvent;

    function CheckValue(NewValue: Extended): Extended;
    function DisplayFormatToEditFormat(const AFormat: string): string;
    function GetCurrency: Boolean;
    function GetDisplayFormat: string;
    function GetMaxValue: Extended;
    function GetMinValue: Extended;
    function IsCurrencyStored: Boolean;
    function IsDisplayFormatStored: Boolean;
    function IsIncrementStored: Boolean;
    function IsMaxValueStored: Boolean;
    function IsMinValueStored: Boolean;
    function TextToValText(const AValue: string): string;

    {$IFDEF FPC}
    {$ELSE}
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    {$ENDIF}
    procedure CMMouseWheel(var Message: TMessage); message CM_MOUSEWHEEL;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;

    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;

    procedure SetCurrency(const Value: Boolean);
    procedure SetDecimalPlaces(Value: Cardinal);
    procedure SetDisplayFormat(const Value: string);
    procedure SetMaxValue(AValue: Extended);
    procedure SetMinValue(AValue: Extended);

  protected
    function CreateEditButton: TEditButtonEh; override;
    function DefaultAlignment: TAlignment; override;
    function DefaultCurrency: Boolean;
    function DefaultDisplayFormat: String;
    function DefaultMaxValue: Extended;
    function DefaultMinValue: Extended;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function FormatDisplayText(Value: Extended): string;
    function GetDisplayText: string; virtual;
    function GetDropDownCalculator: TWinControl; virtual;
    function GetVariantValue: Variant; override;
    function IntDigitsInText: Integer;
    function IsValidChar(Key: Char): Boolean; override;

    procedure CloseWinCallbackProc(Control: TWinControl; Accept: Boolean);
    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DataChanged; override;
    procedure DropDownAction(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; var Handled: Boolean); override;
    procedure EditButtonClickDefaultAction(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; TopButton: Boolean; var Handled: Boolean); override;
    procedure EditButtonDownDefaultAction(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; TopButton: Boolean; var AutoRepeat: Boolean; var Handled: Boolean); override;
    procedure InternalSetControlText(const AText: String);
    procedure InternalSetText(const AText: String); override;
    procedure InternalSetValue(AValue: Variant); override;
    procedure InternalUpdatePostData; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure ReformatEditText(const NewText: String); dynamic;
    procedure UpdateValueFromText;
    procedure WndProc(var Message: TMessage); override;

    property AssignedValues: TDBNumberValues read FAssignedValues;
    property currency: Boolean read GetCurrency write SetCurrency stored IsCurrencyStored;
    property DecimalPlaces: Cardinal read FDecimalPlaces write SetDecimalPlaces default 2;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat stored IsDisplayFormatStored;
    property DropDownCalculator: TWinControl read GetDropDownCalculator;
    property Increment: Extended read FIncrement write FIncrement stored IsIncrementStored;
    property MaxLength default 0;
    property MaxValue: Extended read GetMaxValue write SetMaxValue stored IsMaxValueStored;
    property MinValue: Extended read GetMinValue write SetMinValue stored IsMinValueStored;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CloseUp(Accept: Boolean); override;
    procedure DropDown; virtual;
    procedure IncrementValue(IsIncrease: Boolean);

    property CalculatorVisible: Boolean read FCalculatorVisible;
    property HighlightRequired;
    property DisplayText: string read GetDisplayText;

    property OnCloseUp: TCloseUpEventEh read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
  end;

{ TNumberEdit }

  TDBNumberEditEh = class(TCustomDBNumberEditEh)
  published
    property ControlLabel;
    property ControlLabelLocation;

    property Align;
    property Alignment;
    property AlwaysShowBorder;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    {$IFDEF FPC}
    {$ELSE}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
     {$ENDIF}
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    {$IFDEF FPC}
    {$ELSE}
    property Ctl3D;
    {$ENDIF}
    property currency;
    property DataField;
    property DataSource;
    property DecimalPlaces;
    property DisplayFormat;
    property DynProps;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EmptyDataInfo;
    property Enabled;
    property EditButton;
    property EditButtons;
    property Font;
    property Flat;
    property HighlightRequired;
    property Images;
    {$IFDEF FPC}
    {$ELSE}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property Increment;
    property MaxValue;
    property MinValue;
    property MRUList;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF FPC}
    {$ELSE}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
{$IFDEF EH_LIB_17}
    property StyleElements;
{$ENDIF}
    property TabOrder;
    property TabStop;
    property Tooltips;
{$IFDEF EH_LIB_13}
    property Touch;
{$ENDIF}
    property Value;
    property Visible;

    property OnButtonClick;
    property OnButtonDown;
    property OnChange;
    property OnCheckDrawRequiredState;
    property OnClick;
    property OnCloseDropDownForm;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
{$IFDEF EH_LIB_13}
    property OnGesture;
{$ENDIF}
    property OnGetImageIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnOpenDropDownForm;
    property OnStartDock;
    property OnStartDrag;
    property OnUpdateData;
  end;

{ TCustomDBCheckBoxEh }

  TCustomDBCheckBoxEh = class(TCustomCheckBox)
  private
    FAlignment: TLeftRight;
    FAllowGrayed: Boolean;
    FAlwaysShowBorder: Boolean;
    FCanvas: TCanvas;
    FClicksDisabled: Boolean;
    FDataLink: TFieldDataLinkEh;
    FDynProps: TDynVarsEh;
    FFlat: Boolean;
    FModified: Boolean;
    FMouseAboveControl: Boolean;
    FState: TCheckBoxState;
    FValueCheck: string;
    FValueUncheck: string;

    FOnUpdateData: TUpdateDataEventEh;

    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldState: TCheckBoxState;
    function GetModified: Boolean;
    function GetReadOnly: Boolean;
    function IsStateStored: Boolean;
    function IsValueCheckedStored: Boolean;
    function IsValueUncheckedStored: Boolean;
    function ValueMatch(const ValueList, Value: string): Boolean;

    {$IFDEF FPC}
    {$ELSE}
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    {$ENDIF}
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;

    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;

    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    procedure DataChange(Sender: TObject);
    procedure InternalUpdateData(Sender: TObject);
    procedure ReadValueChecked(Reader: TReader);
    procedure ReadValueUnchecked(Reader: TReader);
    procedure SetAlignment(const Value: TLeftRight);
    procedure SetAlwaysShowBorder(const Value: Boolean);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDynProps(const Value: TDynVarsEh);
    procedure SetFlat(const Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetState(const Value: TCheckBoxState);
    procedure SetValueCheck(const Value: string);
    procedure SetValueUncheck(const Value: string);
    procedure WriteValueChecked(Writer: TWriter);
    procedure WriteValueUnchecked(Writer: TWriter);

  protected
    FDataPosting: Boolean;
    FToggleKeyDown: Boolean;

    function DataIndepended: Boolean; virtual;
    function GetChecked: Boolean; override;
    function PostDataEvent: Boolean;

    procedure Click; override;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DrawCaptionRect(ARect: TRect; AFocused, AMouseAboveControl, ADown: Boolean); virtual;
    procedure DrawCheckBoxRect(ARect: TRect; AState: TCheckBoxState; AFocused, AMouseAboveControl, ADown: Boolean); virtual;
    procedure DrawState(AState: TCheckBoxState; AFocused, AMouseAboveControl, ADown: Boolean); virtual;
    procedure InternalSetState(Value: TCheckBoxState); virtual;
    procedure InternalUpdatePostData; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure RecreateWndHandle;
    procedure SetChecked(Value: Boolean); override;
    procedure Toggle; override;
    procedure WndProc(var Message: TMessage); override;

    property Canvas: TCanvas read FCanvas;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ExecuteAction(Action: TBasicAction): Boolean; override;
    {$IFDEF FPC}
    function GetControlsAlignment: TAlignment;
    {$ELSE}
    function GetControlsAlignment: TAlignment; override;
    {$ENDIF}
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;

    procedure UpdateData; virtual;

    property Alignment: TLeftRight read FAlignment write SetAlignment default taRightJustify;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property AlwaysShowBorder: Boolean read FAlwaysShowBorder write SetAlwaysShowBorder default False;
    property Checked;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DynProps: TDynVarsEh read FDynProps write SetDynProps;
    property Field: TField read GetField;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Modified: Boolean read GetModified;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property State: TCheckBoxState read FState write SetState stored IsStateStored;
    property TabStop default True;
    property ValueChecked: String read FValueCheck write SetValueCheck stored False;
    property ValueUnchecked: String read FValueUncheck write SetValueUncheck stored False;
    {$IFDEF FPC}
    {$ELSE}
    property WordWrap;
    {$ENDIF}

    property OnUpdateData: TUpdateDataEventEh read FOnUpdateData write FOnUpdateData;
  end;

{ TDBCheckBoxEh }

  TDBCheckBoxEh = class(TCustomDBCheckBoxEh)
  published
    property Align;
    property Action;
    property Alignment;
    property AllowGrayed;
    property AlwaysShowBorder;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    {$IFDEF FPC}
    {$ELSE}
    property Ctl3D;
    {$ENDIF}
    property DataField;
    property DataSource;
    property DynProps;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Flat;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF FPC}
    {$ELSE}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property State;
{$IFDEF EH_LIB_17}
    property StyleElements;
{$ENDIF}
    property TabOrder;
    property TabStop;
{$IFDEF EH_LIB_13}
    property Touch;
{$ENDIF}
    property ValueChecked;
    property ValueUnchecked;
    property Visible;
    {$IFDEF FPC}
    {$ELSE}
    property WordWrap;
    {$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
{$IFDEF EH_LIB_13}
    property OnGesture;
{$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUpdateData;
  end;

{ TCustomDBMemoEh }

  TCustomDBMemoEh = class(TCustomDBEditEh)
  private
    FLines: TStrings;
    FScrollBars: TScrollStyle;
    FWantTabs: Boolean;

    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;

    function GetWordWrap: Boolean;
    function IsLinesStored: Boolean;

  protected
    function GetCaretPos: TPoint; {$IFDEF FPC} reintroduce; {$ELSE} virtual; {$ENDIF}

    procedure SetCaretPos(const Value: TPoint); {$IFDEF FPC} reintroduce; {$ELSE} virtual; {$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure SetLines(Value: TStrings);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetWordWrap(Value: Boolean);
    procedure SetEditMode;
    procedure PutToFieldAfterChange;

    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssNone;
    property WantTabs: Boolean read FWantTabs write FWantTabs default False;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CaretPos: TPoint read GetCaretPos write SetCaretPos;
    property Lines: TStrings read FLines write SetLines stored IsLinesStored;
  end;

{ TDBMemoEh }

  TDBMemoEh = class(TCustomDBMemoEh)
  published
    property ControlLabel;
    property ControlLabelLocation;
    property Lines;
    property ScrollBars;

    property Align;
    property Alignment;
    property AlwaysShowBorder;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    {$IFDEF FPC}
    {$ELSE}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
     {$ENDIF}
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    {$IFDEF FPC}
    {$ELSE}
    property Ctl3D;
    {$ENDIF}
    property DataField;
    property DataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DynProps;
    property EditButtons;
    property EmptyDataInfo;
    property Enabled;
    property EditMask;
    property Font;
    property Flat;
    property HighlightRequired;
    property Images;
    {$IFDEF FPC}
    {$ELSE}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property MaxLength;
    property MRUList;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF FPC}
    {$ELSE}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
{$IFDEF EH_LIB_17}
    property StyleElements;
{$ENDIF}
    property TabOrder;
    property TabStop;
    property Tooltips;
{$IFDEF EH_LIB_13}
    property Touch;
{$ENDIF}
    property Visible;
    property WantTabs;
    property WantReturns;
    property WordWrap;

    property OnChange;
    property OnCheckDrawRequiredState;
    property OnClick;
    property OnCloseDropDownForm;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
{$IFDEF EH_LIB_13}
    property OnGesture;
{$ENDIF}
    property OnGetFieldData;
    property OnGetImageIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnOpenDropDownForm;
    property OnStartDock;
    property OnStartDrag;
    property OnUpdateData;
  end;

  TSelectionDrawStyleEh = (sdsDefaultEh, sdsClassicEh, sdsFramedEh, sdsThemedEh);

{ TDBImageEmptyDataInfoEh }

  TCustomDBImageEh = class;

  TDBImageEmptyDataInfoEh = class(TCustomControlEmptyDataInfoEh)
  protected
    function ControlIsEmpty: Boolean; override;
    function GetControl: TCustomDBImageEh; virtual;
    function GetControlAlignment: TAlignment; override;
    function GetControlColor: TColor; override;
    function GetControlFont: TFont;  override;

    procedure GetDrawRect(var ADrawRect: TRect); virtual;

  public
    constructor Create(AControl: TCustomDBImageEh);
    destructor Destroy; override;

    procedure PaintEmptyDataInfo; override;

  published
    property Color;
    property Text;
    property Font;
    property ParentFont;
    property Alignment;
    property AlignmentIsStored;
  end;


{ TCustomDBImageEh }

  TCustomDBImageEh = class(TCustomControlEh, IControlLabelOwnerEh)
  private
    FAutoDisplay: Boolean;
    FBorderStyle: TBorderStyle;
    FControlLabel: TControlLabelEh;
    FControlLabelLocation: TControlLabelLocationEh;
    FDataLink: TFieldDataLinkEh;
    FDynProps: TDynVarsEh;
    FEditButton: TEditButtonEh;
    FEmptyDataInfo: TDBImageEmptyDataInfoEh;
    FPicture: TPictureEh;
    FPictureLoaded: Boolean;
    FPicturePlacement: TImagePlacementEh;
    FSelectionDrawStyle: TSelectionDrawStyleEh;
    FSystemPopupMenu: TPopupMenu;
    FZoom: Integer;
    FZoomAllowed: Boolean;

    FOnButtonClick: TButtonClickEventEh;
    FOnButtonDown: TButtonDownEventEh;
    FOnCloseDropDownForm: TEditControlCloseDropDownFormEventEh;
    FOnOpenDropDownForm: TEditControlShowDropDownFormEventEh;

    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    function IsPictureStored: Boolean;

    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;

    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMSize(var Message: TMessage); message WM_SIZE;

    procedure CheckEditButtonsRemoveNotification(AComponent: TComponent);
    procedure DataChange(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle); {$IFDEF FPC} reintroduce; {$ENDIF}
    procedure SetControlLabelParams(const Value: TControlLabelLocationEh);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDynProps(const Value: TDynVarsEh);
    procedure SetEditButton(const Value: TEditButtonEh);
    procedure SetEmptyDataInfo(const Value: TDBImageEmptyDataInfoEh);
    procedure SetPicture(Value: TPictureEh);
    procedure SetPicturePlacement(const Value: TImagePlacementEh);
    procedure SetReadOnly(Value: Boolean);
    procedure SetSelectionDrawStyle(const Value: TSelectionDrawStyleEh);
    procedure SetZoom(const Value: Integer);
    procedure UpdateData(Sender: TObject);

  protected
    EditButtonControlLineRec: TEditButtonControlLineRec;
    FDroppedDown: Boolean;
    FEditButtonControl: TEditButtonControlEh;
    FImageMouseDownPos: TPoint;
    FImagePos: TPoint;
    FMouseDownPos: TPoint;
    FNoClickCloseUp: Boolean;
    FZoomIsTemporary: Boolean;

    function ButtonEnabled: Boolean; virtual;
    function CreateEditButton: TEditButtonEh; virtual;
    function CreateEditButtonControl: TEditButtonControlEh; virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function GetControlLabelCaption: String;
    function GetControlTextBaseLine: Integer; virtual;
    function GetPalette: HPALETTE; override;
    function GetPopupMenu: TPopupMenu; override;
    function GetSystemPopupMenu: TPopupMenu; virtual;

    procedure AdjustLabelBounds; virtual;
    procedure ButtonDown(IsDownButton: Boolean); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DropDown; virtual;
    procedure DropDownFormCallbackProc(DropDownForm: TCustomForm; Accept: Boolean; DynParams: TDynVarsEh; SysParams: TDropDownFormSysParams);
    procedure EditButtonChanged(Sender: TObject);
    procedure EditButtonClick(Sender: TObject); virtual;
    procedure EditButtonDown(Sender: TObject; TopButton: Boolean; var AutoRepeat: Boolean; var Handled: Boolean); virtual;
    procedure EditButtonImagesRefComponentNotifyEvent(Sender: TObject; RefComponent: TComponent);
    procedure EditButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
    procedure EditButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure LabelSpacingChanged; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateEditButtonControlList;
    procedure UpdateEditButtonControlsState;

    property Font;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ActualSelectionDrawStyle: TSelectionDrawStyleEh; virtual;
    function CanModify: Boolean;
    function IsEmpty: Boolean; virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function ScaleRect(const ARect: TRect; ZoomPercent: Integer): TRect; virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    {$IFDEF FPC}
    function Ctl3D: Boolean;
    {$ENDIF}

    procedure FormPopupMenu(APopupMenu: TPopupMenu); virtual;
    procedure CloseUp(Accept: Boolean); virtual;
    procedure CopyToClipboard; virtual;
    procedure CutToClipboard; virtual;
    procedure LoadPicture; virtual;
    procedure MenuItemCopy(Sender: TObject); virtual;
    procedure MenuItemCut(Sender: TObject); virtual;
    procedure MenuItemDefaultZoom(Sender: TObject); virtual;
    procedure MenuItemDelete(Sender: TObject); virtual;
    procedure MenuItemLoad(Sender: TObject); virtual;
    procedure MenuItemPaste(Sender: TObject); virtual;
    procedure MenuItemSave(Sender: TObject); virtual;
    procedure PasteFromClipboard; virtual;
    procedure ResetPos;
    procedure ResetZoom;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure TemporaryMoveImageTo(AImagePos: TPoint); virtual;
    procedure TemporaryZoomTo(ZoomPercent: Integer); virtual;

    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ControlLabel: TControlLabelEh read FControlLabel;
    property ControlLabelLocation: TControlLabelLocationEh read FControlLabelLocation write SetControlLabelParams;
    property DataField: string read GetDataField write SetDataField;
    property DataLink: TFieldDataLinkEh read FDataLink;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DynProps: TDynVarsEh read FDynProps write SetDynProps;
    property EditButton: TEditButtonEh read FEditButton write SetEditButton;
    property EmptyDataInfo: TDBImageEmptyDataInfoEh read FEmptyDataInfo write SetEmptyDataInfo;
    property Field: TField read GetField;
    property Picture: TPictureEh read FPicture write SetPicture stored IsPictureStored;
    property PicturePlacement: TImagePlacementEh read FPicturePlacement write SetPicturePlacement default ipReduceFitEh;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SelectionDrawStyle: TSelectionDrawStyleEh read FSelectionDrawStyle write SetSelectionDrawStyle default sdsDefaultEh;
    property Zoom: Integer read FZoom write SetZoom default 100;
    property ZoomAllowed: Boolean read FZoomAllowed write FZoomAllowed default True;
    property ZoomIsTemporary: Boolean read FZoomIsTemporary;

    property OnButtonClick: TButtonClickEventEh read FOnButtonClick write FOnButtonClick;
    property OnButtonDown: TButtonDownEventEh read FOnButtonDown write FOnButtonDown;
    property OnCloseDropDownForm: TEditControlCloseDropDownFormEventEh read FOnCloseDropDownForm write FOnCloseDropDownForm;
    property OnOpenDropDownForm: TEditControlShowDropDownFormEventEh read FOnOpenDropDownForm write FOnOpenDropDownForm;
  end;

  TDBImageEhPopupMenuProc = procedure (DBImage: TCustomDBImageEh; PopupMenu: TPopupMenu);

{ TDBImageEh }

  TDBImageEh = class(TCustomDBImageEh)
  published
    property ControlLabel;
    property ControlLabelLocation;

    property Align;
    property Anchors;
    property AutoDisplay;
    property BorderStyle;
    property Color;
    property Constraints;
    {$IFDEF FPC}
    {$ELSE}
    property Ctl3D;
    {$ENDIF}
    property DataField;
    property DataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DynProps;
    property EditButton;
    property Enabled;
    property Font;
    property ParentColor default False;
    {$IFDEF FPC}
    {$ELSE}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property Picture;
    property PicturePlacement;
    property PopupMenu;
    property ReadOnly;
    property SelectionDrawStyle;
    property ShowHint;
{$IFDEF EH_LIB_17}
    property StyleElements;
{$ENDIF}
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnButtonClick;
    property OnButtonDown;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

var
  DBImageEhEditButtonDefaultActionProc: TEditButtonDefaultActionProc;
  DefaultDBImageEhDropDownFormClass: TCustomDropDownFormClassEh;
  DBImageEhFormPopupMenuProc: TDBImageEhPopupMenuProc;

procedure DefaultDBImageEhEditButtonDefaultAction(EditControl: TControl;
  EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh;
  IsMouseDown: Boolean; var Handled: Boolean);

procedure DefaultFormDBImageEhPopupMenu(DBImage: TCustomDBImageEh;
  PopupMenu: TPopupMenu);

type

{ TCustomDBRadioGroupEh }

  TCustomDBRadioGroupEh = class(TCustomRadioGroup)
  private
    FDataLink: TFieldDataLinkEh;
    FDynProps: TDynVarsEh;
    FValue: string;
    FValues: TStrings;
    FInSetValue: Boolean;

    FOnChange: TNotifyEvent;
    FOnGetFieldData: TGetFieldDataEventEh;
    FOnUpdateData: TUpdateDataEventEh;

    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    function GetButtonValue(Index: Integer): string;

    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;

    procedure DataChange(Sender: TObject);
    procedure InternalUpdateData(Sender: TObject);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDynProps(const Value: TDynVarsEh);
    procedure SetItems(Value: TStrings);
    procedure SetReadOnly(Value: Boolean);
    procedure SetValue(const Value: string);
    procedure SetValues(Value: TStrings);
    procedure UpdateData;
  protected
    FDataPosting: Boolean;

    {$IFDEF FPC}
  public
    function CanModify: Boolean; override;
  protected
    {$ELSE}
    function CanModify: Boolean; override;
    {$ENDIF}
    function CreateDataLink: TFieldDataLinkEh; virtual;
    function DataIndepended: Boolean; virtual;
    function PostDataEvent: Boolean;

    procedure InternalUpdatePostData; virtual;
    procedure Change; virtual;
    procedure Click; override;
    procedure DataChanged; virtual;
    procedure InternalSetValue(const Value: string); virtual;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property DataLink: TFieldDataLinkEh read FDataLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;

    property DynProps: TDynVarsEh read FDynProps write SetDynProps;
    property Field: TField read GetField;
    property ItemIndex;
    property Value: string read FValue write SetValue;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Items write SetItems;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Values: TStrings read FValues write SetValues;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetFieldData: TGetFieldDataEventEh read FOnGetFieldData write FOnGetFieldData;
    property OnUpdateData: TUpdateDataEventEh read FOnUpdateData write FOnUpdateData;
  end;

{ TDBRadioGroupEh }

  TDBRadioGroupEh = class(TCustomDBRadioGroupEh)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Columns;
    property Constraints;
    {$IFDEF FPC}
    {$ELSE}
    property Ctl3D;
    {$ENDIF}
    property DataField;
    property DataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Items;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF FPC}
    {$ELSE}
    property ParentBackground;
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
{$IFDEF EH_LIB_17}
    property StyleElements;
{$ENDIF}
    property TabOrder;
    property TabStop;
{$IFDEF EH_LIB_13}
    property Touch;
{$ENDIF}
    property Values;
    property Visible;

    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
{$IFDEF EH_LIB_13}
    property OnGesture;
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnGetFieldData;
    property OnStartDock;
    property OnStartDrag;
    property OnUpdateData;
  end;

{$IFDEF FPC}
  
{$ELSE}
{ TCustomDBRichEditEh }

  TCustomDBRichEditEh = class(TCustomRichEdit, IControlLabelOwnerEh)
  private
    FDataLink: TFieldDataLinkEh;
    FAutoDisplay: Boolean;
    FFocused: Boolean;
    FMemoLoaded: Boolean;
    FDataSave: string;
    FCreatingWnd: Integer;
    FDynProps: TDynVarsEh;
    FOnOpenDropDownForm: TEditControlShowDropDownFormEventEh;
    FOnCloseDropDownForm: TEditControlCloseDropDownFormEventEh;
    FControlLabel: TControlLabelEh;
    FControlLabelLocation: TControlLabelLocationEh;

    function BeginEditing: Boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    function GetRtfText: String;

    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetControlLabelParams(const Value: TControlLabelLocationEh);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDynProps(const Value: TDynVarsEh);
    procedure SetEditButtons(const Value: TEditButtonsEh);
    procedure SetEditRect;
    procedure SetFocused(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetRtfText(const Value: String);
    procedure UpdateData(Sender: TObject);

    procedure EMSetCharFormat(var Message: TMessage); message EM_SETCHARFORMAT;
    procedure EMSetParaFormat(var Message: TMessage); message EM_SETPARAFORMAT;

    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;

    procedure WMClear(var Message: TMessage); message WM_CLEAR;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;

  protected
    FButtonsBox: TEditButtonsBoxEh;
    FDataPosting: Boolean;
    FDroppedDown: Boolean;
    FEditButtons: TEditButtonsEh;
    FInternalRtfText: String;
    FNoClickCloseUp: Boolean;

    function CreateEditButtons: TEditButtonsEh; virtual;
    function ButtonRect: TRect; virtual;
    function GetControlLabelCaption: String; virtual;
    function GetControlTextBaseLine: Integer; virtual;

    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DestroyWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure WndProc(var Message: TMessage); override;

    procedure AdjustLabelBounds; virtual;
    procedure CalcEditRect(var ARect: TRect); virtual;
    procedure CheckShowDropDownForm(EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh; var Handled: Boolean); virtual;
    procedure CloseUp(Accept: Boolean); virtual;
    procedure CreateEditButtonControl(var EditButtonControl: TEditButtonControlEh); virtual;
    procedure DropDownFormCallbackProc(DropDownForm: TCustomForm; Accept: Boolean; DynParams: TDynVarsEh; SysParams: TDropDownFormSysParams);
    procedure DropDownFormCloseProc(EditControl: TControl; Button: TEditButtonEh; Accept: Boolean; DropDownForm: TCustomForm; DynParams: TDynVarsEh);
    procedure EditButtonChanged(Sender: TObject);
    procedure EditButtonClick(Sender: TObject); virtual;
    procedure EditButtonDown(Sender: TObject; TopButton: Boolean; var AutoRepeat: Boolean; var Handled: Boolean); virtual;
    procedure EditButtonImagesRefComponentNotifyEvent(Sender: TObject; RefComponent: TComponent);
    procedure EditButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
    procedure EditButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure GetDefaultDropDownForm(var DropDownForm: TCustomForm; var FreeFormOnClose: Boolean); virtual;
    procedure GetVarValue(var VarValue: Variant);
    procedure LabelSpacingChanged; virtual;
    procedure LoadMemoFromString(const Data: string);
    procedure ReadLines_Data(Stream: TStream);
    procedure ReadRtfText(Reader: TReader);
    procedure SetEnableChangeNotification(const Value: Boolean); virtual;
    procedure SetVarValue(const VarValue: Variant);
    procedure UpdateEditButtonControlList;
    procedure UpdateEditButtonControlsState;
    procedure WriteLines_Data(Stream: TStream);
    procedure WriteRtfText(Writer: TWriter);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    function DataIndepended: Boolean; virtual;

    procedure LoadMemo; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;


    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property ControlLabel: TControlLabelEh read FControlLabel;
    property ControlLabelLocation: TControlLabelLocationEh read FControlLabelLocation write SetControlLabelParams;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DynProps: TDynVarsEh read FDynProps write SetDynProps;
    property EditButtons: TEditButtonsEh read FEditButtons write SetEditButtons;
    property Field: TField read GetField;
    property Lines stored False;
    property ParentFont default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property RtfText: String read GetRtfText write SetRtfText;

    property OnCloseDropDownForm: TEditControlCloseDropDownFormEventEh read FOnCloseDropDownForm write FOnCloseDropDownForm;
    property OnOpenDropDownForm: TEditControlShowDropDownFormEventEh read FOnOpenDropDownForm write FOnOpenDropDownForm;
  end;

{ TDBRichEditEh }

  TDBRichEditEh = class(TCustomDBRichEditEh)
  published
    property ControlLabel;
    property ControlLabelLocation;

    property Align;
    property Alignment;
    property Anchors;
    property AutoDisplay;
    {$IFDEF FPC}
    {$ELSE}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
     {$ENDIF}
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    {$IFDEF FPC}
    {$ELSE}
    property Ctl3D;
    {$ENDIF}
    property DataField;
    property DataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DynProps;
    property EditButtons;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    {$IFDEF FPC}
    {$ELSE}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF FPC}
    {$ELSE}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
{$IFDEF EH_LIB_17}
    property StyleElements;
{$ENDIF}
    property TabOrder;
    property TabStop;
{$IFDEF EH_LIB_13}
    property Touch;
{$ENDIF}
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;

    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
{$IFDEF EH_LIB_13}
    property OnGesture;
{$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
{$IFDEF EH_LIB_9}
    property OnMouseActivate;
{$ENDIF}
    property OnMouseDown;
{$IFDEF EH_LIB_13}
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnResizeRequest;
    property OnSelectionChange;
    property OnProtectChange;
    property OnSaveClipboard;
    property OnStartDock;
    property OnStartDrag;

    property BorderWidth;
    property Lines;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

var
  DBRichEditEhEditButtonDefaultActionProc: TEditButtonDefaultActionProc;
  DefaultDBRichEditEhDropDownFormClass: TCustomDropDownFormClassEh;

procedure DefaultDBRichEditEhEditButtonDefaultAction(EditControl: TControl;
    EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh;
    IsMouseDown: Boolean; var Handled: Boolean);
{$ENDIF}

var
  OldStyleFlatBorder: Boolean = False;

const
  SLoadPictureTitle = 'Load Picture';
  SSavePictureTitle = 'Save Picture As';

implementation

uses Clipbrd,
  Types, MaskUtils, DateUtils, Themes,
  PictureEditFormsEh, MemoEditFormsEh,
  CalculatorEh, EhLibLangConsts,
{$IFDEF FPC}
{$ELSE}
  RichEditFormsEh, Consts, Commctrl, UxTheme,
{$ENDIF}
  Dialogs, ExtDlgs, DBUtilsEh;

type
  TEditButtonEhCracker = class(TEditButtonEh);
  TDropDownFormCallParamsEhCracker = class(TDropDownFormCallParamsEh);

function VarToStr(const V: Variant): string;
begin
  Result := '';
  if VarIsArray(V) then Exit;
  try
    Result := Variants.VarToStr(V);
  except
  end;
end;

procedure CheckEditButtonDownForDropDownForm(EditControl: TWinControl;
  ADataLink: TDataLink; AField: TField; ACurValue: Variant;
  EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh;
  AOnOpenDropDownFormProc: TEditControlShowDropDownFormEventEh;
  ADropDownFormCallbackProc: TDropDownFormCallbackProcEh;
  var Handled: Boolean);
var
  DDParams: TDynVarsEh;
  SysParams: TEditControlDropDownFormSysParams;
  IntDropDownForm: IDropDownFormEh;
  ADropDownForm: TCustomForm;
  ADropDownFormClass: TCustomDropDownFormClassEh;
  TheMsg: TMsg;
  ADataSet: TDataSet;
  i: Integer;
  Fields: TFieldListEh;
  DDFormCallParams: TDropDownFormCallParamsEh;
  AFreeFormOnClose: Boolean;
  AFieldName: String;
begin
  ADropDownForm := nil;
  if EditButtonControl.AlwaysDown then Exit;
  if PeekMessage(TheMsg, EditControl.Handle, WM_USER, WM_USER, PM_NOREMOVE) then
  begin
    if (TheMsg.wParam = WPARAM(EditControl.Handle)) and
       (TheMsg.lParam = LPARAM(EditControl)) and
       (EditButton = nil) then
    begin
      Handled := True;
      Exit;
    end;
    if (TheMsg.wParam = WPARAM(EditControl.Handle)) and (TheMsg.lParam = LPARAM(EditButton)) then
    begin
      Handled := True;
      Exit;
    end;
  end;
  if EditButton <> nil then
    DDFormCallParams := EditButton.DropDownFormParams
  else
    DDFormCallParams := nil;

  AFreeFormOnClose := False;
  if DDFormCallParams <> nil then
    if DDFormCallParams.DropDownForm <> nil then
      ADropDownForm := DDFormCallParams.DropDownForm
    else if DDFormCallParams.DropDownFormClassName <> '' then
    begin
      ADropDownFormClass := TCustomDropDownFormClassEh(GetClass(DDFormCallParams.DropDownFormClassName));
      if ADropDownFormClass <> nil then
      begin
        ADropDownForm := ADropDownFormClass.GetGlobalRef;
        if ADropDownForm = nil then
        begin
          ADropDownForm := ADropDownFormClass.Create(EditControl);
          if ADropDownFormClass.GetGlobalRef = nil then
            AFreeFormOnClose := True;
        end;
      end else
        raise Exception.Create('Class ''' + DDFormCallParams.DropDownFormClassName + ''' is not registered');
    end;

  DDParams := TDynVarsEh.Create(EditControl);
  SysParams := TEditControlDropDownFormSysParams.Create;
  ADataSet := ADataLink.DataSet;

  SysParams.FreeFormOnClose := AFreeFormOnClose;
  SysParams.FEditControl := EditControl;
  SysParams.FEditButton := EditButton;

  if DDFormCallParams.PassFieldNames <> '' then
  begin
    Fields := TFieldListEh.Create;
    try
      ADataSet.GetFieldList(Fields, DDFormCallParams.AssignBackFieldNames);
      for I := 0 to Fields.Count - 1 do
        DDParams.CreateDynVar(TField(Fields[i]).FieldName, TField(Fields[i]).Value)
    finally
      Fields.Free;
    end;
  end else if DDFormCallParams.PassParams = pspFieldValueEh then
  begin
    if AField <> nil
      then AFieldName := AField.FieldName
      else AFieldName := '';
    DDParams.CreateDynVar(AFieldName, ACurValue);
  end else if DDFormCallParams.PassParams = pspRecordValuesEh then
  begin
    ADataSet := ADataLink.DataSet;
    for i := 0 to ADataSet.Fields.Count-1 do
      DDParams.CreateDynVar(ADataSet.Fields[i].FieldName, ADataSet.Fields[i].Value);
  end;

  if Supports(ADropDownForm, IDropDownFormEh, IntDropDownForm) then
    IntDropDownForm.ReadOnly := False;

  if ADropDownForm <> nil then
    if (AField <> nil) and AField.ReadOnly and (IntDropDownForm <> nil) then
      IntDropDownForm.ReadOnly := True;

  if Assigned(AOnOpenDropDownFormProc) then
    AOnOpenDropDownFormProc(EditControl, EditButton, ADropDownForm, DDParams);

  if Supports(ADropDownForm, IDropDownFormEh, IntDropDownForm) then
  begin

    if DDFormCallParams.SaveFormSize then
    begin
      DDFormCallParams.OldFormWidth := ADropDownForm.Width;
      if DDFormCallParams.FormWidth > 0 then
      begin
        ADropDownForm.Width := DDFormCallParams.FormWidth;
      end;
      DDFormCallParams.OldFormHeight := ADropDownForm.Height;
      if DDFormCallParams.FormHeight > 0 then
      begin
        ADropDownForm.Height := DDFormCallParams.FormHeight;
      end;
    end;

    EditButtonControl.AlwaysDown := True;

    IntDropDownForm.ExecuteNomodal(ClientToScreenRect(EditControl), nil,
      DDFormCallParams.Align, DDParams, SysParams, ADropDownFormCallbackProc);
    Handled := True;
  end else
  begin
    DDParams.Free;
    SysParams.Free;
  end;
end;

procedure DefaultDropDownFormCallbackProc(EditControl: TWinControl;
  ADataLink: TDataLink;
  DropDownForm: TCustomForm;
  Accept: Boolean; DynParams: TDynVarsEh; SysParams: TDropDownFormSysParams;
  SetVarValueProc: TSetVarValueProcEh;
  AOnCloseDropDownForm: TEditControlCloseDropDownFormEventEh);
var
  ADataSet: TDataSet;
  Fields: TFieldListEh;
  I: Integer;
  DataSetWasInEditState: Boolean;
  ASysParams: TEditControlDropDownFormSysParams;
  DDFormCallParams: TDropDownFormCallParamsEh;
begin

  ASysParams := TEditControlDropDownFormSysParams(SysParams);
  if ASysParams.FEditButton <> nil then
    DDFormCallParams := ASysParams.FEditButton.DropDownFormParams
  else
    DDFormCallParams := nil;

  try
  try

  if Accept then
  begin
    if (DDFormCallParams.PassParams in [pspFieldValueEh, pspRecordValuesEh]) or
       (DDFormCallParams.AssignBackFieldNames <> '') then
    begin
      ADataSet := ADataLink.DataSet;
      DataSetWasInEditState := False;
      if ADataSet <> nil then
      begin
        DataSetWasInEditState := (ADataSet.State in [dsEdit, dsInsert]);
        if not DataSetWasInEditState then
          ADataSet.Edit;
      end;
      if DDFormCallParams.AssignBackFieldNames <> '' then
      begin
        Fields := TFieldListEh.Create;
        try
          ADataSet.GetFieldList(Fields, DDFormCallParams.AssignBackFieldNames);
          for I := 0 to Fields.Count - 1 do
            TField(Fields[I]).Value := DynParams[TField(Fields[I]).FieldName].Value;
        finally
          Fields.Free;
        end;
      end else
        SetVarValueProc(DynParams.Items[0].Value);

      if (ADataSet <> nil) and not DataSetWasInEditState then
        ADataSet.Post;
    end;

  end;

  DropDownForm.Hide;
  if DDFormCallParams.SaveFormSize then
  begin
    DDFormCallParams.FormWidth := DropDownForm.Width;
    if DDFormCallParams.OldFormWidth > 0 then
      DropDownForm.Width := DDFormCallParams.OldFormWidth;
    DDFormCallParams.FormHeight := DropDownForm.Height;
    if DDFormCallParams.OldFormHeight > 0 then
      DropDownForm.Height := DDFormCallParams.OldFormHeight;
  end;

  if Assigned(AOnCloseDropDownForm) then
    AOnCloseDropDownForm(EditControl, nil, Accept, DDFormCallParams.DropDownForm, DynParams);

  if ASysParams.FEditButton <> nil
    then PostMessage(EditControl.Handle, WM_USER, WPARAM(EditControl.Handle), LPARAM(ASysParams.FEditButton))
    else PostMessage(EditControl.Handle, WM_USER, WPARAM(EditControl.Handle), LPARAM(EditControl));

  finally
    DynParams.Free;
    SysParams.Free;
  end;
  except
    TCustomDropDownFormEh(DropDownForm).KeepFormVisible := True;
    Application.HandleException(EditControl);
    TCustomDropDownFormEh(DropDownForm).KeepFormVisible := False;
  end;
end;

{ TEditImageEh }

constructor TEditImageEh.Create(EditControl: TWinControl);
begin
  inherited Create;
  FEditControl := EditControl;
  FUseImageHeight := True;
  FImageIndex := -1;
end;

destructor TEditImageEh.Destroy;
begin
  inherited Destroy;
end;

procedure TEditImageEh.Assign(Source: TPersistent);
begin
  if Source is TEditImageEh then
  begin
    Images := TEditImageEh(Source).Images;
    ImageIndex := TEditImageEh(Source).ImageIndex;
    Visible := TEditImageEh(Source).Visible;
    Width := TEditImageEh(Source).Width;
  end else
    inherited Assign(Source);
end;

procedure TEditImageEh.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if FEditControl <> nil then FEditControl.Invalidate;
  end;
end;

procedure TEditImageEh.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    if FEditControl <> nil then
    begin
      FEditControl.Perform(CM_EDITIMAGECHANGEDEH, ObjectToIntPtr(Self), 0);
      if Value <> nil then Value.FreeNotification(FEditControl);
    end;
  end;
end;

procedure TEditImageEh.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if FEditControl <> nil then FEditControl.Perform(CM_EDITIMAGECHANGEDEH, ObjectToIntPtr(Self), 0);
  end;
end;

procedure TEditImageEh.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    if FEditControl <> nil then FEditControl.Perform(CM_EDITIMAGECHANGEDEH, ObjectToIntPtr(Self), 0);
  end;
end;

procedure TEditImageEh.SetUseImageHeight(const Value: Boolean);
begin
  if FUseImageHeight <> Value then
  begin
    FUseImageHeight := Value;
    if FEditControl <> nil then FEditControl.Perform(CM_EDITIMAGECHANGEDEH, ObjectToIntPtr(Self), 0);
  end;
end;

function TEditImageEh.Showing: Boolean;
begin
  Result := Visible and (Images <> nil) and (ImageIndex >= 0);
end;

function TEditImageEh.DrawWidth: Integer;
begin
  if not Visible or (Images = nil) then
    Result := 0
  else if (Width > 0) and (Images <> nil) then
    Result := Width + 4 
  else if Images <> nil then
    Result := Images.Width + 4
  else
    Result := 0;
end;

{ TFieldDataLinkEh }

constructor TFieldDataLinkEh.Create;
begin
  inherited Create;
  VisualControl := True;
  FDataIndepended := True;
  DataIndependentValue := Null;
end;

function TFieldDataLinkEh.Edit: Boolean;
begin
  if DataIndepended then
  begin
    if not Editing and not ReadOnly then
    begin
      FEditing := True;
      FModified := False;
      if Assigned(OnEditingChange) then OnEditingChange(Self);
    end;
  end else if CanModify then
    inherited Edit;
  Result := FEditing;
end;

function TFieldDataLinkEh.GetActive: Boolean;
begin
  if DataIndepended then Result := True
  else Result := inherited Active and (Field <> nil);
end;

function TFieldDataLinkEh.GetDataSetActive: Boolean;
begin
  Result := (DataSource <> nil) and (DataSource.DataSet <> nil) and DataSource.DataSet.Active;
end;

function TFieldDataLinkEh.GetCanModify: Boolean;
begin
  Result := ((Field <> nil) and Field.CanModify) or DataIndepended;
end;

function TFieldDataLinkEh.GetDataSource: TDataSource;
begin
  Result := inherited DataSource;
end;

procedure TFieldDataLinkEh.Modified;
begin
  FModified := True;
end;

procedure TFieldDataLinkEh.RecordChanged(Field: TField);
begin
  if (Field = nil) or FieldFound(Field) then
  begin
    if Assigned(FOnDataChange) then FOnDataChange(Self);
    FModified := False;
  end;
end;

procedure TFieldDataLinkEh.SetDataSource(const Value: TDataSource);
begin
  if Value <> inherited DataSource then
  begin
    inherited DataSource := Value;
    UpdateDataIndepended;
  end;
end;

procedure TFieldDataLinkEh.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    UpdateField;
    UpdateDataIndepended;
  end;
end;

procedure TFieldDataLinkEh.SetText(const Text: String);
begin
  if DataIndepended then
  begin
    DataIndependentValue := Text;
    RecordChanged(nil);
  end else if Field.IsBlob then
    Field.AsString := Text
  else
    Field.Text := Text;
end;

procedure TFieldDataLinkEh.SetValue(Value: Variant);
var i: Integer;
begin
  if DataIndepended then
  begin
    DataIndependentValue := Value;
    RecordChanged(nil);
  end else if FieldsCount > 1 then
  begin
    if VarEquals(Value, Null)
      then for i := 0 to FieldsCount - 1 do Fields[i].AsVariant := Null
      else for i := 0 to FieldsCount - 1 do Fields[i].AsVariant := Value[i]
  end else if Field <> nil then
{$IFDEF EH_LIB_8}
    Field.AsVariant := Value;
{$ELSE}
   if (Field.DataType = ftLargeint) and (Value <> Null)
     then Field.AsFloat := Value
     else Field.AsVariant := Value;
{$ENDIF}
end;

procedure TFieldDataLinkEh.UpdateData;
begin
  if DataIndepended then
  begin
    if FModified then
      if Assigned(OnUpdateData) then OnUpdateData(Self);
    FEditing := False;
    FModified := False;
  end else if FModified then
  begin
    if (Field <> nil) and Assigned(FOnUpdateData) then FOnUpdateData(Self);
    FModified := False;
  end;
end;

procedure TFieldDataLinkEh.UpdateDataIndepended;
var
  OldDataIndepended: Boolean;
begin
  if FDataIndepended <> IsDataIndepended then
  begin
    OldDataIndepended := FDataIndepended;
    FDataIndepended := IsDataIndepended;
    DataIndependentValue := Null;
    LayoutChanged;
    if not OldDataIndepended and FDataIndepended then
      RecordChanged(nil);
  end;
end;

function TFieldDataLinkEh.IsDataIndepended: Boolean;
begin
  Result := (DataSource = nil) and (FieldName = '');
end;

procedure TFieldDataLinkEh.ActiveChanged;
begin
  UpdateField;
  if Assigned(FOnActiveChange) then FOnActiveChange(Self);
end;

procedure TFieldDataLinkEh.EditingChanged;
begin
  SetEditing(inherited Editing and CanModify);
end;

function TFieldDataLinkEh.FieldFound(Value: TField): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 0 to Length(FFields) - 1 do
    if FFields[i] = Value then
    begin
      Result := True;
      Exit;
    end;
end;

{$IFDEF CIL}
procedure TFieldDataLinkEh.FocusControl(const Field: TField);
begin
  if (Field <> nil) and FieldFound(Field) and (FControl is TWinControl) then
    if TWinControl(FControl).CanFocus then
    begin
      TWinControl(FControl).SetFocus;
    end;
end;
{$ELSE}
procedure TFieldDataLinkEh.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and FieldFound(Field^) and (FControl is TWinControl) then
    if TWinControl(FControl).CanFocus then
    begin
      Field^ := nil;
      TWinControl(FControl).SetFocus;
    end;
end;
{$ENDIF}

function TFieldDataLinkEh.GetField: TField;
begin
  if Length(FFields) = 0
    then Result := nil
    else Result := FFields[0];
end;

function TFieldDataLinkEh.GetFieldsCount: Integer;
begin
  Result := Length(FFields);
end;

function TFieldDataLinkEh.GetFieldsField(Index: Integer): TField;
begin
  if Length(FFields) = 0
    then Result := nil
    else Result := FFields[Index];
end;

procedure TFieldDataLinkEh.LayoutChanged;
begin
  UpdateField;
end;

procedure TFieldDataLinkEh.Reset;
begin
  RecordChanged(nil);
end;

procedure TFieldDataLinkEh.SetMultiFields(const Value: Boolean);
begin
  if FMultiFields <> Value then
  begin
    FMultiFields := Value;
    UpdateField;
  end;
end;

procedure TFieldDataLinkEh.UpdateField;
var
{$IFDEF EH_LIB_17}
  ListOfFields: TList<TField>;
  I: Integer;
{$ENDIF}
  FieldList: TObjectListEh;
begin
  FieldList := TObjectListEh.Create;
  try
  if inherited Active and (FFieldName <> '') then
  begin
    if MultiFields then
      if Assigned(FControl) then
        GetFieldsProperty(FieldList, DataSource.DataSet, FControl, FFieldName)
      else
      begin
{$IFDEF EH_LIB_17}
        ListOfFields := TList<TField>.Create;
        DataSet.GetFieldList(ListOfFields, FFieldName);
        for I := 0 to ListOfFields.Count-1 do
          FieldList.Add(ListOfFields[i]);
        FreeAndNil(ListOfFields);
{$ELSE}
        DataSet.GetFieldList(FieldList, FFieldName);
{$ENDIF}
      end
    else
      if Assigned(FControl)
        then FieldList.Add(GetFieldProperty(DataSource.DataSet, FControl, FFieldName))
        else FieldList.Add(DataSource.DataSet.FieldByName(FFieldName));
  end;
  SetField(FieldList);
  finally
    FreeAndNil(FieldList);
  end;
end;

procedure TFieldDataLinkEh.UpdateRightToLeft;
{$IFDEF FPC}
{$ELSE}
var
  IsRightAligned: Boolean;
  AUseRightToLeftAlignment: Boolean;
  wc: TWinControl;
{$ENDIF}
begin
  {$IFDEF FPC}
  {$ELSE}
  if Assigned(FControl) and
    (FControl is TWinControl) and
    not (csDestroying in FControl.ComponentState)
  then
  begin
    wc := FControl as TWinControl;
    if wc.IsRightToLeft then
    begin
      IsRightAligned :=
        (GetWindowLong(wc.Handle, GWL_EXSTYLE) and WS_EX_RIGHT) = WS_EX_RIGHT;
      AUseRightToLeftAlignment :=
        DBUseRightToLeftAlignment(wc, Field);
      if (IsRightAligned and (not AUseRightToLeftAlignment)) or
        ((not IsRightAligned) and AUseRightToLeftAlignment)
      then
        wc.Perform(CM_RECREATEWND, 0, 0);
    end;
  end;
  {$ENDIF}
end;

procedure TFieldDataLinkEh.SetEditing(Value: Boolean);
begin
  if FEditing <> Value then
  begin
    FEditing := Value;
    FModified := False;
    if Assigned(FOnEditingChange) then
      FOnEditingChange(Self);
  end;
end;

procedure TFieldDataLinkEh.SetField(Value: TObjectList);

  function CompareFieldsAndList(Value: TObjectList): Boolean;
  begin
    Result := True;
  end;

var
  i: Integer;
begin
  if CompareFieldsAndList(Value) then
  begin
    SetLength(FFields, Value.Count);
    for i := 0 to Value.Count - 1 do
      FFields[i] := TField(Value[i]);
    EditingChanged;
    RecordChanged(nil);
    UpdateRightToLeft;
  end;
end;

procedure TFieldDataLinkEh.SetModified(Value: Boolean);
begin
  FModified := Value;
end;

procedure TFieldDataLinkEh.DataEvent(Event: TDataEvent; Info: TDataEventInfoTypeEh);
begin
  inherited DataEvent(Event, Info);
  if Event = deDisabledStateChange then
  begin
    if Boolean(Info)
      then UpdateField
      else SetLength(FFields, 0);
  end;
end;

{ TControlEmptyDataInfoEh }

constructor TControlEmptyDataInfoEh.Create(AControl: TCustomDBEditEh);
begin
  inherited Create(AControl);
end;

destructor TControlEmptyDataInfoEh.Destroy;
begin
  inherited Destroy;
end;

function TControlEmptyDataInfoEh.GetControl: TCustomDBEditEh;
begin
  Result := TCustomDBEditEh(FControl);
end;

function TControlEmptyDataInfoEh.GetControlAlignment: TAlignment;
begin
  Result := GetControl.Alignment;
end;

function TControlEmptyDataInfoEh.ControlIsEmpty: Boolean;
begin
  Result := GetControl.IsEmpty;
end;

function TControlEmptyDataInfoEh.GetControlFont: TFont;
begin
  if Assigned(FControl) and (FControl is TControl)
{$IFDEF CIL}
    then Result := IControl(FControl).GetFont
{$ELSE}
    then Result := GetControl.Font
{$ENDIF}
    else Result := FFont;
end;

procedure TControlEmptyDataInfoEh.PaintEmptyDataInfo;
const
  RTLAlignments: array[TAlignment] of TAlignment =
    (taRightJustify, taLeftJustify, taCenter);
var
  ARect: TRect;
  Margins: TPoint;
  Left: Integer;
  DrawAlignment: TAlignment;
begin
  GetControl.Canvas.Font := FFont;
  ARect := FControl.ClientRect;
  {$IFNDEF MSWINDOWS}
  if (GetControl.BorderStyle = bsSingle) then
    InflateRect(ARect, -2, -2);
  {$ENDIF}
  ARect.Left := ARect.Left + GetControl.FImageWidth;
  ARect.Right := ARect.Right - GetControl.FButtonsBox.ButtonsWidth;
  SelectClipRgn(GetControl.Canvas.Handle, 0);

  Margins := GetControl.GetTextMargins;

  if FControl.UseRightToLeftAlignment
    then DrawAlignment := RTLAlignments[Alignment]
    else DrawAlignment := Alignment;

  case DrawAlignment of
    taLeftJustify: Left := Margins.X + GetControl.FImageWidth;
    taRightJustify: Left := ARect.Right - GetControl.Canvas.TextWidth(Text) - Margins.X;
  else
    Left := (ARect.Right + ARect.Left - GetControl.Canvas.TextWidth(Text)) div 2;
  end;
  if Color <> clDefault
    then GetControl.Canvas.Brush.Color := Color
    else GetControl.Canvas.Brush.Color := GetControl.GetFillColor;

  {$IFNDEF MSWINDOWS}
  GetControl.Canvas.Brush.Style := bsClear;
  {$ENDIF}
  GetControl.Canvas.TextRect(ARect, Left, Margins.Y, Text);
end;

{ TCustomControlEmptyDataInfoEh }

constructor TCustomControlEmptyDataInfoEh.Create(AControl: TWinControl);
begin
  inherited Create;
  FControl := AControl;
  FFont := TFont.Create;
  FFont.Assign(DefaultFont);
  FFont.Color := clSilver;
  FFont.Style := FFont.Style + [fsItalic];
  FFont.OnChange := FontChanged;
  FAlignment := GetControlAlignment;
  FParentFont := True;
  FColor := clDefault;
end;

destructor TCustomControlEmptyDataInfoEh.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

function TCustomControlEmptyDataInfoEh.DefaultFont: TFont;
begin
  Result := GetControlFont;
end;

procedure TCustomControlEmptyDataInfoEh.FontChanged(Sender: TObject);
begin
  FParentFont := False;
  FControl.Invalidate;
end;

procedure TCustomControlEmptyDataInfoEh.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    FControl.Invalidate;
  end;
end;

procedure TCustomControlEmptyDataInfoEh.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TCustomControlEmptyDataInfoEh.RefreshDefaultFont;
var
  Save: TNotifyEvent;
begin
  if not FParentFont then Exit;

  Save := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Size := DefaultFont.Size;
    FFont.Name := DefaultFont.Name;
    FFont.Charset := DefaultFont.Charset;
  finally
    FFont.OnChange := Save;
  end;
end;

function TCustomControlEmptyDataInfoEh.Showing: Boolean;
begin
  Result :=
    ((FText <> '') or (Color <> clDefault)) and
    (ControlIsEmpty);
end;

procedure TCustomControlEmptyDataInfoEh.SetParentFont(const Value: Boolean);
begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    RefreshDefaultFont;
    FControl.Invalidate;
  end;
end;

function TCustomControlEmptyDataInfoEh.IsFontStored: Boolean;
begin
  Result := not ParentFont;
end;

function TCustomControlEmptyDataInfoEh.GetAlignment: TAlignment;
begin
  if AlignmentIsStored
    then Result := FAlignment
    else Result := GetControlAlignment;
end;

procedure TCustomControlEmptyDataInfoEh.SetAlignment(const Value: TAlignment);
begin
  AlignmentIsStored := True;
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    FControl.Invalidate;
  end;
end;

function TCustomControlEmptyDataInfoEh.IsAlignmentStored: Boolean;
begin
  Result := FAlignmentIsStored;
end;

procedure TCustomControlEmptyDataInfoEh.SetAlignmentIsStored(
  const Value: Boolean);
begin
  if (Value = True) and (IsAlignmentStored = False) then
  begin
    FAlignmentIsStored := True;
    FAlignment := GetControlAlignment;
    FControl.Invalidate;
  end else if (Value = False) and (IsAlignmentStored = True) then
  begin
    FAlignmentIsStored := False;
    FControl.Invalidate;
  end;
end;

procedure TCustomControlEmptyDataInfoEh.PaintEmptyDataInfo;
begin

end;

function TCustomControlEmptyDataInfoEh.GetControlAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TCustomControlEmptyDataInfoEh.GetControlFont: TFont;
begin
  Result := nil;
end;

function TCustomControlEmptyDataInfoEh.GetControlColor: TColor;
begin
  Result := clWindow;
end;

function TCustomControlEmptyDataInfoEh.ControlIsEmpty: Boolean;
begin
  Result := False;
end;

function TCustomControlEmptyDataInfoEh.GetPaintColor: TColor;
begin
  if Color <> clDefault
    then Result := Color
    else Result := GetControlColor;
end;


procedure DefaultDBEditEhEditButtonDefaultAction(EditControl: TControl;
    EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh;
    IsMouseDown: Boolean; var Handled: Boolean);
var
  DBEditControl: TCustomDBEditEh;
  ADropDownFormParams: TDropDownFormCallParamsEhCracker;

  AForm: TCustomForm;
  FDynParamsInteractorItfs: IDynParamsInteractableEh;
  DDParams: TDynVarsEh;
  OutDDParams: TDynVarsEh;
begin
  DBEditControl := (EditControl as TCustomDBEditEh);

  if (EditButton.Style in [ebsDropDownEh, ebsAltDropDownEh]) and IsMouseDown then
  begin
    ADropDownFormParams := TDropDownFormCallParamsEhCracker(EditButton.DropDownFormParams);

    ADropDownFormParams.FEditButton := EditButton;
    ADropDownFormParams.FEditButtonControl := EditButtonControl;
    ADropDownFormParams.FEditControl := DBEditControl;
    ADropDownFormParams.FOnOpenDropDownFormProc := DBEditControl.BeforeShowDefaulEditDropDownForm;
    ADropDownFormParams.FOnCloseDropDownFormProc := DBEditControl.DropDownFormCloseProc;
    ADropDownFormParams.FDataLink := DBEditControl.FDataLink;
    ADropDownFormParams.FField := DBEditControl.Field;
    ADropDownFormParams.FOnSetVarValueProc := DBEditControl.SetVarValue;
    ADropDownFormParams.FOnGetVarValueProc := DBEditControl.GetVarValue;
    ADropDownFormParams.FOnGetActualDropDownFormProc := DBEditControl.GetDefaultDropDownForm;

    ADropDownFormParams.CheckShowDropDownForm(Handled);
  end else if (EditButton.Style in [ebsEllipsisEh, ebsGlyphEh]) and not IsMouseDown then
  begin
    AForm := TMemoEditWinEh.GetGlobalRef;

    if Supports(AForm, IDynParamsInteractableEh, FDynParamsInteractorItfs) then
    begin
      DDParams := TDynVarsEh.Create(nil);
      try
      DDParams.CreateDynVar('', DBEditControl.Text);
      FDynParamsInteractorItfs.SetInDynParams(DDParams);

      if TMemoEditWinEh.GetGlobalRef.ShowModal = mrOk then
      begin
        FDynParamsInteractorItfs.GetOutDynParams(OutDDParams);
        DBEditControl.Text := VarToStr(OutDDParams.Items[0].Value);
      end;
      finally
        DDParams.Free;
      end;
      Handled := True;
    end;
  end;
end;

{ TCustomDBEditEh }

constructor TCustomDBEditEh.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csCaptureMouse];
  FDataLink := CreateDataLink;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := InternalUpdateData;
  FDataLink.OnActiveChange := ActiveChange;

  FEditButton := CreateEditButton;
  FEditButton.OnChanged := EditButtonChanged;
  FEditButton.OnRefComponentChanged := EditButtonImagesRefComponentNotifyEvent;
  FEditButtons := CreateEditButtons;
  FEditButtons.OnChanged := EditButtonChanged;
  FEditButtons.OnRefComponentChanged := EditButtonImagesRefComponentNotifyEvent;
  FEditImage := CreateEditImage;

  FMRUList := TMRUListEh.Create(Self);
  FMRUList.OnSetDropDown := MRUListDropDown;
  FMRUList.OnSetCloseUp := MRUListCloseUp;
  FMRUList.OnFillAutogenItems := MRUListFillAutogenItems;

  FDynProps := TDynVarsEh.Create(Self);
  FEmptyDataInfo := TControlEmptyDataInfoEh.Create(Self);
  UpdateControlReadOnly;
  UpdateImageIndex;

  FButtonsBox := TEditButtonsBoxEh.Create(Self);
  FButtonsBox.SetBounds(0,0,0,0);
  FButtonsBox.Visible := False;
  FButtonsBox.Parent := Self;
  FButtonsBox.OnDown := EditButtonDown;
  FButtonsBox.OnClick := EditButtonClick;
  FButtonsBox.OnMouseMove := EditButtonMouseMove;
  FButtonsBox.OnMouseUp := EditButtonMouseUp;
  FButtonsBox.OnCreateEditButtonControl := CreateEditButtonControl;

  FControlLabel := TControlLabelEh.Create(Self);
  FControlLabel.FreeNotification(Self);
  FControlLabel.FocusControl := Self;
  FControlLabelLocation := TControlLabelLocationEh.Create(Self);
end;

destructor TCustomDBEditEh.Destroy;
begin
  Destroying();
  DataSource := nil;
  FreeAndNil(FEditImage);
  FreeAndNil(FEditButton);
  FreeAndNil(FEditButtons);
  FreeAndNil(FDataLink);
  FreeAndNil(FCanvas);
  FreeAndNil(FMRUList);
  FreeAndNil(FEmptyDataInfo);
  FreeAndNil(FButtonsBox);
  FreeAndNil(FDynProps);
  FreeAndNil(FControlLabel);
  FreeAndNil(FControlLabelLocation);
  inherited Destroy;
end;

procedure TCustomDBEditEh.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) then
  begin
    if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    begin
      F := DataSource.DataSet.FindField(DataField);
      if Assigned(F) and (F.DataType in [ftString, ftWideString]) and (F.Size = MaxLength)
        then MaxLength := 0;
    end;
  end;
end;

procedure TCustomDBEditEh.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  {$WARNINGS OFF}
  SaveFont := SelectObject(DC, Font.Handle);
  {$WARNINGS ON}
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);

  if NewStyleControls then
  begin
    if Ctl3D then I := 8 else I := 6;
    if Flat then Dec(I, 2);
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  if (EditImage <> nil) and
     (EditImage.Images <> nil) and
     EditImage.UseImageHeight and
     (EditImage.Images.Height > Metrics.tmHeight)
    then Height := EditImage.Images.Height + I
    else Height := Metrics.tmHeight + I;
end;

function TCustomDBEditEh.ButtonRect: TRect;
begin
  if NewStyleControls and not Ctl3D and (BorderStyle = bsSingle)
    then Result := Rect(ClientWidth - FButtonsBox.ButtonsWidth - 1, 1, ClientWidth - 1, ClientHeight - 1)
    else Result := Rect(ClientWidth - FButtonsBox.ButtonsWidth, 0, ClientWidth, ClientHeight);

  {$IFDEF MSWINDOWS}
  {$ELSE}
  
  if (BorderStyle = bsSingle) then
    Result := Rect(ClientWidth - FButtonsBox.ButtonsWidth - 2, 2, ClientWidth - 2, ClientHeight - 2);
  {$ENDIF}

  if inherited UseRightToLeftAlignment then
    OffsetRect(Result, FButtonsBox.ButtonsWidth - ClientWidth, 0);
end;

function TCustomDBEditEh.ButtonEnabled: Boolean;
begin
  Result := Enabled and Assigned(FDataLink) and FDataLink.Active;
end;

procedure TCustomDBEditEh.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
  if (csDesigning in ComponentState) then DataChange(Self);
  UpdateDrawBorder;
end;

procedure TCustomDBEditEh.Notification(AComponent: TComponent; Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = FInternalDataSourceRef)
    then
      DataSource := nil
    else if (EditImage <> nil) and (EditImage.Images <> nil) and (AComponent = EditImage.Images)
    then
      EditImage.Images := nil
    else if (AComponent is TPopupMenu) then
    begin
      if Assigned(EditButton) and (AComponent = EditButton.DropdownMenu) then
        EditButton.DropdownMenu := nil;
      for i := 0 to EditButtons.Count - 1 do
        if EditButtons[i].DropdownMenu = AComponent then
          EditButtons[i].DropdownMenu := nil;
    end else if AComponent = FControlLabel then
      FControlLabel := nil;
    CheckEditButtonsRemoveNotification(AComponent);
  end;
end;

function TCustomDBEditEh.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TCustomDBEditEh.KeyDown(var Key: Word; Shift: TShiftState);
var AutoRepeat: Boolean;
  eb: TEditButtonEh;
begin
  CheckInplaceEditHolderKeyDown(Key, Shift);
  if Key = 0 then Exit;
  inherited KeyDown(Key, Shift);
  if Key = 0 then Exit;
  eb := GetEditButtonByShortCut(ShortCut(Key, Shift));
  if (eb <> nil) and eb.Enabled then
    if (eb = FEditButton) and ButtonEnabled then
    begin
      FButtonsBox.BtnCtlList[0].EditButtonControl.EditButtonDown(0, AutoRepeat);
      FButtonsBox.BtnCtlList[0].EditButtonControl.Click;
      Key := 0;
    end else
    begin
      FButtonsBox.BtnCtlList[eb.Index + 1].EditButtonControl.EditButtonDown(0, AutoRepeat);
      FButtonsBox.BtnCtlList[eb.Index + 1].EditButtonControl.Click; 
      Key := 0;
    end;
  if (Key = Word('A')) and (Shift = [ssCtrl]) then
    SelectAll;
  if ((Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift))) and not ReadOnly then
  begin
    if not FDataLink.Edit then
      Key := 0;
  end;
  if (WantReturns and (Key = VK_RETURN)) and not ReadOnly then
    FDataLink.Edit;
  if (WantTabs and (Key = VK_TAB)) and not ReadOnly then
    FDataLink.Edit;
end;

procedure TCustomDBEditEh.KeyUp(var Key: Word; Shift: TShiftState);
begin
  CheckInplaceEditHolderKeyUp(Key, Shift);
  if Key = 0 then Exit;
  inherited KeyUp(Key, Shift);
end;

procedure TCustomDBEditEh.KeyPress(var Key: Char);
begin
  CheckInplaceEditHolderKeyPress(Key);
  if Key = #0 then Exit;
  if IsMasked and (Key = ^A) then Exit; 
  inherited KeyPress(Key);
  if not DataIndepended then
    if (Key >= #32) and (FDataLink.Field <> nil) and not IsValidChar(Key) then
    begin
      {$IFDEF FPC_CROSSP}
      {$ELSE}
      MessageBeep(0);
      {$ENDIF}
      Key := #0;
    end;
  case Key of
    ^H, ^V, ^X, #32..High(Char):
      if not ReadOnly then
      begin
        if not FDataLink.Edit then
          Key := #0;
      end;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
  if (Integer(Key) = VK_BACK) and MRUList.Active and Showing and not FDroppedDown and (Text = '') then
    MRUList.DropDown;
end;

procedure TCustomDBEditEh.WMChar(var Message: TWMChar);
var
  CharMsg: TMsg;
  DBC: Boolean;
begin
  FCompleteKeyPress := Char(Message.CharCode);
  try
    DBC := False;
    if (IsLeadCharEh(Char(Message.CharCode))) then
      if PeekMessage(CharMsg, Handle, WM_CHAR, WM_CHAR, PM_NOREMOVE) then
        if CharMsg.Message <> WM_Quit then
        begin
{$IFDEF CIL}
{$ELSE}
          FCompleteKeyPress := FCompleteKeyPress + Char(CharMsg.wParam);
{$ENDIF}
          DBC := True;
        end;

    inherited;

    if DBC and (Char(Message.CharCode) = #0) then
      PeekMessage(CharMsg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE);
  finally
    FCompleteKeyPress := '';
  end;
end;

function TCustomDBEditEh.GetCompleteKeyPress: String;
begin
  Result := FCompleteKeyPress;
end;

function TCustomDBEditEh.EditCanModify: Boolean;
begin
  Result := not ReadOnly and FDataLink.Edit;
end;

function TCustomDBEditEh.EditRect: TRect;
begin
  if NewStyleControls and not Ctl3D and (BorderStyle = bsSingle) then
    Result := Rect(1 + FImageWidth, 1, ClientWidth - FButtonsBox.ButtonsWidth - 2, ClientHeight - 1)
  else
    Result := Rect(FImageWidth, 0, ClientWidth - FButtonsBox.ButtonsWidth - 1, ClientHeight);
  if inherited UseRightToLeftAlignment then
    OffsetRect(Result, FButtonsBox.ButtonsWidth - FImageWidth + 1, 0);
end;

procedure TCustomDBEditEh.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TCustomDBEditEh.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    RecreateWndHandle;
  end;
end;

procedure TCustomDBEditEh.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) and not IsMasked then Invalidate;
    FDataLink.Reset;
  end;
end;

function TCustomDBEditEh.CreateEditButton: TEditButtonEh;
begin
  Result := TEditButtonEh.Create(Self);
end;

function TCustomDBEditEh.CreateEditButtons: TEditButtonsEh;
begin
  Result := TEditButtonsEh.Create(Self, TVisibleEditButtonEh);
end;

procedure TCustomDBEditEh.CreateEditButtonControl(
  var EditButtonControl: TEditButtonControlEh);
begin
  EditButtonControl := TEditButtonControlEh.Create(Self);
  EditButtonControl.ControlStyle := EditButtonControl.ControlStyle + [csReplicatable];
  EditButtonControl.Width := 10;
  EditButtonControl.Height := 17;
  EditButtonControl.Visible := True;
  EditButtonControl.Transparent := False;
end;

function TCustomDBEditEh.CreateEditImage: TEditImageEh;
begin
  Result := TEditImageEh.Create(Self);
end;

function TCustomDBEditEh.CreateDataLink: TFieldDataLinkEh;
begin
  Result := TFieldDataLinkEh.Create;
end;

procedure TCustomDBEditEh.Change;
begin
  if csDestroying in ComponentState then
    Exit;
  FDataLink.Modified;
  Modified := True;
  UpdateImageIndex();
  inherited Change;
end;

procedure TCustomDBEditEh.CreateParams(var Params: TCreateParams);
const
  Alignments: array[Boolean, Boolean, TAlignment] of DWORD =
  (((ES_LEFT, ES_LEFT, ES_LEFT), (ES_RIGHT, ES_RIGHT, ES_RIGHT)),
    ((ES_LEFT, ES_RIGHT, ES_CENTER), (ES_RIGHT, ES_LEFT, ES_CENTER)));
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
  PasswordChars: array[Boolean] of DWORD = (ES_MULTILINE, 0);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WordWraps[FWordWrap] or
    PasswordChars[PasswordChar <> #0] or
    Alignments[FWordWrap, UseRightToLeftAlignment, Alignment];
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TCustomDBEditEh.CreateWnd;
begin
  inherited CreateWnd;
  UpdateHeight;
  UpdateDrawBorder;

  UpdateEditButtonControlList;
  UpdateEditButtonControlsState;

  FImageWidth := EditImage.DrawWidth;
  UpdateImageIndex;
  SetEditRect;
  UpdateControlReadOnly;
end;

function TCustomDBEditEh.DataIndepended: Boolean;
begin
  Result := FDataLink.DataIndepended;
end;

function TCustomDBEditEh.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCustomDBEditEh.GetDefaultDropDownForm(var DropDownForm: TCustomForm;
  var FreeFormOnClose: Boolean);
begin
  DropDownForm := DefaultDBEditEhDropDownFormClass.GetGlobalRef;
  if DropDownForm <> nil then
    FreeFormOnClose := False;
end;

procedure TCustomDBEditEh.SetDataSource(Value: TDataSource);
begin
  if Value <> DataSource then
  begin
    if (not FDataLink.DataSourceFixed) then
      FDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
    DataChange(nil);
    Invalidate;
    FInternalDataSourceRef := Value;
    FDataLink.UpdateDataIndepended;
  end;
end;

procedure TCustomDBEditEh.CalcEditRect(var ARect: TRect);
{$IFDEF FPC_CROSSP}
{$ELSE}
var
  smRes: LRESULT;
{$ENDIF}
begin
  if inherited UseRightToLeftAlignment
    then SetRect(ARect, FButtonsBox.ButtonsWidth, 0, ClientWidth, ClientHeight)
    else SetRect(ARect, 0, 0, ClientWidth - FButtonsBox.ButtonsWidth, ClientHeight);
  if EditImage.Visible and (EditImage.Images <> nil) then
    if inherited UseRightToLeftAlignment
      then Dec(ARect.Right, FImageWidth)
      else Inc(ARect.Left, FImageWidth);

{$IFDEF FPC_CROSSP}
{$ELSE}
  if ThemesEnabled and not Ctl3D and (BorderStyle = bsSingle) then
  begin
    smRes := SendMessage(Handle, EM_GETMARGINS, 0, 0);
    ARect.Left := ARect.Left + LoWord(smRes) - 1;
    ARect.Right := ARect.Right - HiWord(smRes) + 1;
  end;
{$ENDIF}
end;

procedure TCustomDBEditEh.SetEditRect;
{$IFDEF FPC_CROSSP}
begin
end;
{$ELSE}
var
  Loc: TRect;
begin
  if not HandleAllocated then Exit;
  SetRect(Loc, 0, 0, ClientWidth, ClientHeight);
  CalcEditRect(Loc);
  SendStructMessage(Handle, EM_SETRECTNP, 0, Loc);
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;
{$ENDIF}

function TCustomDBEditEh.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TCustomDBEditEh.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
  UpdateEditButtonControlsState;
  Invalidate;
end;

function TCustomDBEditEh.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TCustomDBEditEh.SetEditButton(const Value: TEditButtonEh);
begin
  FEditButton.Assign(Value);
end;

procedure TCustomDBEditEh.SetEditImage(const Value: TEditImageEh);
begin
  FEditImage.Assign(Value);
end;

procedure TCustomDBEditEh.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    EditingChanged;
  end;
end;

procedure TCustomDBEditEh.UpdateControlReadOnly;
var
  IsReadOnly: Boolean;
begin
  if ReadOnly then
    IsReadOnly := True
  else
  begin
    IsReadOnly := not FDataLink.Editing;
    if IsReadOnly and Assigned(OnUpdateData) and
      (FDataLink.DataSet <> nil) and (FDataLink.DataSet.State = dsEdit)
    then
      IsReadOnly := False;
  end;
  if IsReadOnly and FInplaceMode then
    IsReadOnly := not FIntfInplaceEditHolder.InplaceEditCanModify(Self);
  SetControlReadOnly(IsReadOnly);
end;

function TCustomDBEditEh.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCustomDBEditEh.ActiveChange(Sender: TObject);
begin
  ActiveChanged;
end;

procedure TCustomDBEditEh.DataChange(Sender: TObject);
begin
  DataChanged;
  UpdateEditButtonControlsState;
  if ControlLabel <> nil then
    ControlLabel.UpdateCaption;
end;

procedure TCustomDBEditEh.DrawBorder(DC: HDC; ActiveBorder: Boolean);
{$IFDEF FPC_CROSSP}
begin
end;
{$ELSE}
var
  R: TRect;
  BtnFaceBrush: HBRUSH;
  NeedReleaseDC: Boolean;
begin
  if not (NewStyleControls and Ctl3D and (BorderStyle = bsSingle))
    or not HandleAllocated then Exit;

  NeedReleaseDC := False;
  if DC = 0 then
  begin
    DC := GetWindowDC(Handle);
    NeedReleaseDC := True;
  end;
  BtnFaceBrush := GetSysColorBrush(COLOR_BTNFACE);

  GetWindowRect(Handle, R);
  OffsetRect(R, -R.Left, -R.Top);

  if ActiveBorder
    then DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT)
    else FrameRect(DC, R, BtnFaceBrush);

  OffsetRect(R, -R.Left, -R.Top);
  InflateRect(R, -1, -1);
  FrameRect(DC, R, BtnFaceBrush);

  if NeedReleaseDC then
    ReleaseDC(Handle, DC);
end;
{$ENDIF} 

procedure TCustomDBEditEh.DrawEditImage(DC: HDC);
var
  ImRect: TRect;
  ei: TEditImageEh;
begin
  ei := EditImage;
  if Assigned(ei) then
  begin
    if not ei.Visible or (ei.Images = nil) or (ei.ImageIndex < 0) then
      Exit;
    ImRect := ImageRect;
    InflateRect(ImRect, -2, -1);
    DrawImage(DC, ImRect, ei.Images, ei.ImageIndex, False);
  end;
end;

procedure TCustomDBEditEh.EditingChange(Sender: TObject);
begin
  EditingChanged;
end;

function TCustomDBEditEh.PostDataEvent: Boolean;
begin
  Result := False;
  FDataPosting := True;
  try
    if Assigned(FOnUpdateData) then FOnUpdateData(Self, Result);
  finally
    FDataPosting := False;
  end;
end;

procedure TCustomDBEditEh.ReadEditMask(Reader: TReader);
begin
  EditMask := Reader.ReadString;
end;

procedure TCustomDBEditEh.WriteEditMask(Writer: TWriter);
begin
  Writer.WriteString(EditMask);
end;

procedure TCustomDBEditEh.InternalUpdateData(Sender: TObject);
begin
  if FDataPosting
    then Exit
    else UpdateData;
end;

procedure TCustomDBEditEh.UpdateDrawBorder;
var NewBorderActive: Boolean;
begin
  if (csLoading in ComponentState) then Exit;
  NewBorderActive := (csDesigning in ComponentState) or (Focused)
    or FMouseAboveControl or AlwaysShowBorder;
  if NewBorderActive <> FBorderActive then
  begin
    FBorderActive := NewBorderActive;
    if NewStyleControls and Ctl3D and (BorderStyle = bsSingle) and Flat and
      (not ThemesEnabled or OldStyleFlatBorder)
    then
      DrawBorder(0, FBorderActive);
    UpdateEditButtonControlsState;
  end;
end;

{$IFDEF FPC_CROSSP}
{$ELSE}
procedure TCustomDBEditEh.WMNCPaint(var Message: TWMNCPaint);
begin
  if NewStyleControls and Ctl3D and (BorderStyle = bsSingle) and Flat and
    (not ThemesEnabled or OldStyleFlatBorder)
  then
  begin
    DrawBorder(0, FBorderActive);
    Message.Result := 1;
  end else
    inherited;
end;

procedure TCustomDBEditEh.WMUndo(var Message: TWMUndo);
begin
  if EditCanModify then
    inherited;
end;
{$ENDIF} 

procedure TCustomDBEditEh.WMSetCursor(var Message: TWMSetCursor);
{$IFDEF FPC_CROSSP}
begin
  inherited;
end;
{$ELSE}
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if PtInRect(ButtonRect, Point(P.X, P.Y)) or PtInRect(ImageRect, Point(P.X, P.Y))
    then Windows.SetCursor(LoadCursorEh(0, idc_Arrow))
    else inherited;
end;
{$ENDIF}

procedure TCustomDBEditEh.WMPaste(var Message: TWMPaste);
var
  AText, ClipboardText: String;
  NewPos: Integer;
begin
  if EditCanModify then
  begin
    if IsMasked then
    begin
      inherited;
    end
    else if AutoSize and
            ( Clipboard.HasFormat(CF_TEXT)
            {$IFDEF FPC_CROSSP}
            {$ELSE}
            or Clipboard.HasFormat(CF_OEMTEXT)
            or Clipboard.HasFormat(CF_UNICODETEXT)
            {$ENDIF}
            )
    then
    begin
      ClipboardText := Clipboard.AsText;
      AText := FixClipboardText(ClipboardText);
      if AText <> ClipboardText then
      begin
        if (MaxLength > 0) and (Length(Text) + Length(AText) - SelLength > MaxLength) then
          AText := Copy(AText, 1, MaxLength - Length(Text) + SelLength);
        NewPos := Length(Copy(Text, 1, SelStart) + AText);
        Text := Copy(Text, 1, SelStart) + AText + Copy(Text, SelStart + SelLength + 1, MAXINT);
        SelStart := NewPos;
      end else
        inherited;
   end else
      inherited;
  end;
end;

procedure TCustomDBEditEh.WMCut(var Message: TWMCut);
begin
  if EditCanModify then
    inherited;
end;

procedure TCustomDBEditEh.WMClear(var Message: TWMCut);
begin
  if EditCanModify then
    inherited;
end;

procedure TCustomDBEditEh.WMGetDlgCode(var Message: TWMGetDlgCode);
var HolderMessage: Longint;
begin
{$IFDEF FPC_CROSSP}
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
{$ELSE}
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_HASSETSEL;
{$ENDIF}
  if FWantTabs then
    Message.Result := Message.Result or DLGC_WANTTAB;
  if FWantReturns then
    Message.Result := Message.Result or DLGC_WANTALLKEYS;
  if FInplaceMode then
  begin
    HolderMessage := FInplaceEditHolder.Perform(WM_GETDLGCODE,0,0);
    if HolderMessage and DLGC_WANTTAB > 0 then
      Message.Result := Message.Result or DLGC_WANTTAB;
  end
end;

procedure TCustomDBEditEh.WMCancelMode(var Message: TMessage);
begin
  inherited;
end;

procedure TCustomDBEditEh.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

procedure TCustomDBEditEh.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateEditButtonControlList;
  SetEditRect;
end;

procedure TCustomDBEditEh.WMKillFocus(var Message: TWMKillFocus);
var i: Integer;
begin
  if csDestroying in ComponentState then
    inherited
  else
  begin
    if MRUList.DroppedDown and not (Message.FocusedWnd = MRUListControl.Handle) then
      MRUList.CloseUp(False);
    inherited;
    UpdateDrawBorder;
    Invalidate;
    for i := 0 to ControlCount - 1 do
      if GetCaptureControl = Controls[i] then
      begin
        Controls[i].Perform(WM_CANCELMODE, 0, 0);
        Break;
      end;
  end;
end;

procedure TCustomDBEditEh.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if MouseCapture then
  begin
    if GetFocus <> Handle then
      MouseCapture := False;
  end;
end;

procedure TCustomDBEditEh.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  UpdateDrawBorder;
  Invalidate;
end;

procedure TCustomDBEditEh.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  if (Message.CharCode in [VK_RETURN, VK_ESCAPE]) and MRUList.DroppedDown and
     (TPopupListboxFormEh(MRUListControl).ItemIndex >= 0) then
  begin
    Message.Result := 1;
    Exit;
  end;
  inherited;
  if (Message.CharCode = VK_ESCAPE) and Modified then
    Message.Result := 1;
  if (Message.CharCode = VK_RETURN) and FInplaceMode then
    Message.Result := 1;
end;

procedure TCustomDBEditEh.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  if SysLocale.FarEast and FDataLink.CanModify then
    SetControlReadOnly(FReadOnly);
end;

procedure TCustomDBEditEh.CMExit(var Message: TCMExit);
begin
  if IsMasked and not (csDesigning in ComponentState) then
  begin
    ValidateEdit;
    CheckCursor;
  end;
  DoExit;
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
end;

procedure TCustomDBEditEh.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if (csFixedHeight in ControlStyle) and not ((csDesigning in
    ComponentState) and (csLoading in ComponentState)) then AdjustHeight;
  SetEditRect;
  EmptyDataInfo.RefreshDefaultFont;
  AdjustLabelBounds;
end;

procedure TCustomDBEditEh.CMColorChanged(var Message: TMessage);
begin
  inherited;
  UpdateEditButtonControlsState;
end;

procedure TCustomDBEditEh.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseAboveControl := True;
  UpdateDrawBorder;
end;

procedure TCustomDBEditEh.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseAboveControl := False;
  UpdateDrawBorder;
end;

function TCustomDBEditEh.CheckHintTextRect(var TextWidth, TextHeight: Integer): Boolean;
var
  NewRect, r: TRect;
  uFormat: Integer;
begin
  CalcEditRect(r);
  Result := False;
  {$IFDEF FPC}
  uFormat := DT_CALCRECT or DT_LEFT or DT_NOPREFIX;
  {$ELSE}
  uFormat := DT_CALCRECT or DT_LEFT or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly;
  {$ENDIF}
  if WordWrap then uFormat := uFormat or DT_WORDBREAK;

  NewRect := Rect(0, 0, r.Right - r.Left, 0);
  if NewRect.Right <= 0 then NewRect.Right := 1;
  Canvas.Font := Font;
  DrawTextEh(Canvas.Handle, Text, Length(Text), NewRect, uFormat);
  TextWidth := NewRect.Right - NewRect.Left;
  TextHeight := NewRect.Bottom - NewRect.Top;
  if (NewRect.Right - NewRect.Left > r.Right - r.Left - 2) or
    (NewRect.Bottom - NewRect.Top > r.Bottom - r.Top) then
    Result := True;
end;

procedure TCustomDBEditEh.CMParentShowHintChanged(var Message: TMessage);
begin
  inherited;
  if ParentShowHint then
    FShowHint := Parent.ShowHint;
  UpdateHintProcessing;
end;

{ TToolTipsWindow }

{$IFDEF CIL}
type
  TToolTipsWindow = class(THintWindow)
  public
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: TObject): TRect; override;
  end;

function TToolTipsWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: TObject): TRect;
begin
  Canvas.Font.Assign(TFont(AData));
  Result := inherited CalcHintRect(MaxWidth, AHint, AData);
end;

{$ELSE}

type
  TToolTipsWindow = class(THintWindow)
  public
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
  end;

function TToolTipsWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
begin
  Canvas.Font.Assign(TFont(AData));
  Result := inherited CalcHintRect(MaxWidth, AHint, AData);
end;

{$ENDIF}

procedure TCustomDBEditEh.CMHintShow(var Message: TCMHintShow);
var
  TextWidth, TextHeight: Integer;
{$IFDEF CIL}
  AHintInfo: THintInfo;
{$ENDIF}
begin
  if Tooltips then
  begin
{$IFDEF CIL}
    if Message.OriginalMessage.LParam = 0 then Exit;
    AHintInfo := Message.HintInfo;
{$ENDIF}
    if CheckHintTextRect(TextWidth, TextHeight) then
    begin
{$IFDEF CIL}
      AHintInfo.HintStr := Text;
      AHintInfo.HintPos := ClientToScreen(Point(0, Height));
      AHintInfo.HintWindowClass := TToolTipsWindow;
      AHintInfo.HintData := Font;
      Message.HintInfo := AHintInfo;
{$ELSE}
      Message.HintInfo^.HintStr := Text;
      Message.HintInfo^.HintPos := ClientToScreen(Point(0, Height));
      Message.HintInfo^.HintWindowClass := TToolTipsWindow;
      Message.HintInfo^.HintData := Font;
{$ENDIF}
    end else if not ShowHint then
      Message.Result := 1
  end else if not ShowHint then
    Message.Result := 1;
end;

procedure TCustomDBEditEh.CheckCursor;
var
  SelStart, SelStop: Integer;
begin
  if not HandleAllocated then Exit;
  if (IsMasked) then
  begin
    GetSel(SelStart, SelStop);
    if SelStart = SelStop then
      if SelStart - 2 < 0
        then SetCursor(0)
        else SetCursor(SelStart - 2);
  end;
end;

procedure TCustomDBEditEh.PaintWindow(DC: HDC);
const
  AlignStyle: array[Boolean, TAlignment] of DWORD =
  ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
var
{$IFDEF MSWINDOWS}
  Left: Integer;
  Margins: TPoint;
  R: TRect;
  S: string;
{$ENDIF}

  PS: TPaintStruct;
  AAlignment: TAlignment;
  ExStyle: DWORD;
  PaintControlName: Boolean;
begin
  DrawEditImage(DC);
  AAlignment := Alignment;
  if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
  PaintControlName := (csDesigning in ComponentState) and not (FDataLink.Active);
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  if ((AAlignment = taLeftJustify) or FFocused or FWordWrap) and
    not (csPaintCopy in ControlState) and not PaintControlName then
  begin
    if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
    begin { This keeps the right aligned text, right aligned }
      ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
        (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
      if UseRightToLeftReading then ExStyle := ExStyle or WS_EX_RTLREADING;
      if UseRightToLeftScrollbar then ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
      ExStyle := ExStyle or
        AlignStyle[UseRightToLeftAlignment, AAlignment];
      if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
        SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
    end;

    if DC = 0 then DC := BeginPaint(Handle, PS);
    FCanvas.Handle := DC;
    try
      PaintRequiredState(FCanvas);
    finally
      FCanvas.Handle := 0;
      if DC = 0 then EndPaint(Handle, PS);
    end;

    inherited PaintWindow(DC);

    if FEmptyDataInfo.Showing {and not Focused} then
    begin
      if DC = 0 then
        DC := BeginPaint(Handle, PS);
      FCanvas.Handle := DC;
      try
        FEmptyDataInfo.PaintEmptyDataInfo;
      finally
        FCanvas.Handle := 0;
        if DC = 0 then
          EndPaint(Handle, PS);
      end;
    end;

    Exit;
  end;

{$IFDEF MSWINDOWS}
{ Since edit controls do not handle justification unless multi-line (and
  then only poorly) we will draw right and center justify manually unless
  the edit has the focus. }
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    FCanvas.Font.Color := GetFontColor;
    R := ClientRect;
    if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
    begin
      FCanvas.Brush.Color := StyleServices.GetSystemColor(clWindowFrame);
      FCanvas.FrameRect(R);
      InflateRect(R, -1, -1);
    end;
    R := EditRect;
    FCanvas.Brush.Color := GetFillColor;
    S := GetDisplayTextForPaintCopy;

    if PasswordChar <> #0 then
      S := StringOfChar(PasswordChar, Length(S));
    Margins := GetTextMargins;
    case AAlignment of
      taLeftJustify: Left := Margins.X;
      taRightJustify: Left := EditRect.Right - FCanvas.TextWidth(S) - Margins.X;
    else
      Left := (EditRect.Right - FCanvas.TextWidth(S)) div 2;
    end;
    {$IFDEF FPC}
    {$ELSE}
    if SysLocale.MiddleEast then FCanvas.UpdateTextFlags;
    {$ENDIF}
    FCanvas.TextRect(R, Left, Margins.Y, S);
    PaintRequiredState(FCanvas);

    if FEmptyDataInfo.Showing then
      FEmptyDataInfo.PaintEmptyDataInfo;

  finally
    FCanvas.Handle := 0;
    if DC = 0 then EndPaint(Handle, PS);
  end;
{$ENDIF}
end;

procedure TCustomDBEditEh.PaintRequiredState(ACanvas: TCanvas);
var
  r: TRect;
  DrawState: Boolean;
begin
  if DataIndepended then
    DrawState := HighlightRequired and FDataLink.Active and
     ( VarIsNull(Value) or (Text = '') )
  else
    DrawState := HighlightRequired and FDataLink.Active and
      (FDataLink.DataSet.State in [dsInsert, dsEdit] ) and (Field <> nil) and
      Field.Required and Field.IsNull;
  if Assigned(FOnCheckDrawRequiredState) then
    FOnCheckDrawRequiredState(Self, DrawState);
  if DrawState then
  begin
    ACanvas.Pen.Color := clRed;
    ACanvas.Pen.Style := psDot;
    CalcEditRect(r);
    ACanvas.MoveTo(r.Left+2, ClientHeight-1);
    ACanvas.LineTo(r.Right-3, ClientHeight-1);
  end;
end;

procedure TCustomDBEditEh.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := ObjectToIntPtr(FDataLink);
end;

function TCustomDBEditEh.GetVariantValue: Variant;
begin
  if DataIndepended then
    Result := Variant(Text)
  else
    Result := Variant(Text);
end;

function TCustomDBEditEh.IsValidChar(InputChar: Char): Boolean;
begin
  if (FDataLink.Field <> nil) then
    Result := FDataLink.Field.IsValidChar(InputChar)
  else
    Result := True;
end;

procedure TCustomDBEditEh.CMEditImageChangedEh(var Message: TMessage);
begin
  RecreateWndHandle;
end;

procedure TCustomDBEditEh.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  UpdateEditButtonControlsState;
end;

function TCustomDBEditEh.GetTextMargins: TPoint;
{$IFDEF FPC_CROSSP}
begin
  Result := Point(3, 2);
end;
{$ELSE}
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
  AEditRect: TRect;
begin
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then I := 0 else
      if Ctl3D then I := 1 else I := 2;
    SendStructMessage(Self.Handle, EM_GETRECT, 0, AEditRect);
    if inherited UseRightToLeftAlignment
      then Result.X := ClientWidth - AEditRect.Right - FImageWidth
      else Result.X := AEditRect.Left - FImageWidth;
    Result.Y := I;
  end else
  begin
    if BorderStyle = bsNone then I := 0 else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      {$WARNINGS OFF}
      SaveFont := SelectObject(DC, Font.Handle);
      {$WARNINGS ON}
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;
{$ENDIF} 

function TCustomDBEditEh.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TCustomDBEditEh.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TCustomDBEditEh.UpdateHeight;
begin
  if AutoSize and (BorderStyle = bsSingle) then
  begin
    ControlStyle := ControlStyle + [csFixedHeight];
    AdjustHeight;
  end else
    ControlStyle := ControlStyle - [csFixedHeight];
end;

function TCustomDBEditEh.GetText: String;
begin
  Result := inherited Text;
end;

procedure TCustomDBEditEh.SetText(const Value: String);
begin
  if (csDesigning in ComponentState) and not DataIndepended then Exit;
  if not DataIndepended then DataSource.DataSet.Edit;
  InternalSetText(Value);
  if FDataPosting then Exit;
  try
    UpdateData;
  except
    FDataLink.Reset;
    raise;
  end;
end;

function TCustomDBEditEh.GetValue: Variant;
begin
  Result := GetVariantValue;
end;

procedure TCustomDBEditEh.SetValue(const Value: Variant);
begin
  SetVariantValue(Value);
end;

procedure TCustomDBEditEh.SetVariantValue(const VariantValue: Variant);
begin
  if (csDesigning in ComponentState) and not DataIndepended then Exit;
  if not DataIndepended then DataSource.DataSet.Edit;
  InternalSetValue(VariantValue);
  if FDataPosting then Exit;
  try
    UpdateData;
  except
    FDataLink.Reset;
    raise;
  end;
end;

procedure TCustomDBEditEh.SetControlEditMask(const Value: string);
begin
  inherited EditMask := Value;
end;

procedure TCustomDBEditEh.SetControlReadOnly(Value: Boolean);
begin
  inherited ReadOnly := Value;
end;

function TCustomDBEditEh.ImageRect: TRect;
begin
  Result := Rect(0, 0, FImageWidth, ClientHeight);
  if inherited UseRightToLeftAlignment then
    OffsetRect(Result, ClientWidth - FImageWidth, 0);
end;

procedure TCustomDBEditEh.InternalUpdatePostData;
begin
  if DataIndepended
    then FDataLink.SetText(EditText)
    else FDataLink.SetText(Text);
end;

procedure TCustomDBEditEh.UpdateData;
begin
  if FFocused then ValidateEdit;
  if not PostDataEvent then
    InternalUpdatePostData;
  Modified := False;
  if MRUList.AutoAdd and MRUList.Active and Showing then
    MRUList.Add(Text);
end;

procedure TCustomDBEditEh.WndProc(var Message: TMessage);
var
  ShiftState: TShiftState;
  MousePos: TPoint;
  ClickTime: LongInt;
  KeyMsg: TWMKey;
begin
  if FInplaceMode then
  begin
    Message.Result := 0;
    FIntfInplaceEditHolder.InplaceEditWndProc(Self, Message);
    if Message.Result <> 0 then Exit;
  end;
  if FInplaceMode then
    case Message.Msg of
      WM_SETFOCUS:
        begin
          if (GetParentForm(Self) = nil) or GetParentForm(Self).SetFocusedControl(FInplaceEditHolder) then
            Dispatch(Message);
          Exit;
        end;
      WM_LBUTTONDOWN:
        begin
          FIntfInplaceEditHolder.GetMouseDownInfo(MousePos, ClickTime);
          if IsDoubleClickMessage(MousePos,
{$IFDEF CIL}
               ClientToScreen(SmallPointToPoint(TWMMouse.Create(Message).Pos)), GetMessageTime - ClickTime)
{$ELSE}
               ClientToScreen(SmallPointToPoint(TWMMouse(Message).Pos)), GetMessageTimeEh - ClickTime)
{$ENDIF}
          then
            Message.Msg := WM_LBUTTONDBLCLK;
        end;
    end;

  if (MRUList <> nil) and MRUList.DroppedDown then
  begin
    case Message.Msg of
      {$IFDEF FPC}
      CN_KEYDOWN, CN_SYSKEYDOWN, CN_CHAR,
      {$ELSE}
      {$ENDIF}
      WM_KEYDOWN, WM_SYSKEYDOWN, WM_CHAR:
{$IFDEF CIL}
{$ELSE}
{$ENDIF}
        begin
          KeyMsg := TWMKey(Message);
          ShiftState := KeyDataToShiftState(KeyMsg.KeyData);
          if ((KeyMsg.CharCode in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT]) and not (ssAlt in ShiftState))
            or ((KeyMsg.CharCode in [VK_HOME, VK_END]) and (ssCtrl in KeyDataToShiftState(KeyMsg.KeyData)))
          then
          begin
            SendMessage(MRUListControl.Handle, Message.Msg, Message.WParam, Message.LParam);
            Exit;
          end;
          if KeyMsg.CharCode in [VK_RETURN, VK_ESCAPE] then
          begin
            MRUList.CloseUp(KeyMsg.CharCode = VK_RETURN);
            Exit;
          end;
        end;
    end;
  end;

  inherited WndProc(Message);

  if FUserTextChanged then
  begin
    FUserTextChanged := False;
    UserChange;
  end;
end;

procedure TCustomDBEditEh.DefaultHandler(var Message);
var
  Msg: TMessage;
begin
  VarToMessage(Message, Msg);
  case Msg.Msg of
    WM_LBUTTONDBLCLK, WM_LBUTTONDOWN, WM_LBUTTONUP,
      WM_MBUTTONDBLCLK, WM_MBUTTONDOWN, WM_MBUTTONUP,
      WM_RBUTTONDBLCLK, WM_RBUTTONDOWN, WM_RBUTTONUP:
{$IFDEF CIL}
{$ELSE}
{$ENDIF}
        if (PtInRect(ButtonRect, Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos)) or
            PtInRect(ImageRect, Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos))) and
           not MouseCapture
        then
          Exit;
    WM_CHAR:
{$IFDEF CIL}
{$ELSE}
{$ENDIF}
      begin
        if (not WantReturns and (TWMKey(Message).CharCode = VK_RETURN)) or
           (not WantTabs and (TWMKey(Message).CharCode = VK_TAB)) or
           (Char(TWMKey(Message).CharCode) = #10)
        then
        begin
          Exit;
        end;
      end;
  end;
  inherited DefaultHandler(Message);
end;

procedure TCustomDBEditEh.ActiveChanged;
begin
  ResetMaxLength;
  UpdateEditButtonControlsState;
end;

procedure TCustomDBEditEh.DataChanged;
var
  AValue: Variant;
  Handled: Boolean;
  AEditText: String;
begin
  if FDataLink.Field <> nil then
  begin

    Handled := False;
    if Assigned(OnGetFieldData) then
    begin
      AValue := Unassigned;
      OnGetFieldData(Self, AValue, Handled);
    end;

    if not Handled then
    begin
      if FFocused and FDataLink.CanModify then
        if FDataLink.Field.IsBlob
          then AValue := FDataLink.Field.AsString
          else AValue := FDataLink.Field.Text
      else
      begin
        if FDataLink.Field.IsBlob
          then AValue := FDataLink.Field.AsString
          else AValue := FDataLink.Field.DisplayText;
      end;

      if FAlignment <> FDataLink.Field.Alignment then Invalidate;
      if not (evEditMaskEh in FAssignedValues) then
        SetControlEditMask(FDataLink.Field.EditMask);
      if not (csDesigning in ComponentState) then
      begin
        if (FDataLink.Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) then
          MaxLength := FDataLink.Field.Size;
      end;
    end;

    if FFocused and FDataLink.CanModify then
    begin
      InternalSetText(VarToStr(AValue));
    end else
    begin
      AEditText := VarToStr(AValue);
      if EditMask <> '' then
        EditText := FormatMaskTextEh(EditMask, AEditText)
      else
        EditText := AEditText;
    end;
  end
  else if DataIndepended then
  begin
    if not (evEditMaskEh in FAssignedValues) then
      SetControlEditMask('');
    EditText := VarToStr(FDataLink.DataIndependentValue);
  end else
  begin
    if not (evEditMaskEh in FAssignedValues) then
      SetControlEditMask('');
    EditText := '';
  end;
  UpdateControlReadOnly;
  Modified := False;
end;

procedure TCustomDBEditEh.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('EditMask', ReadEditMask, WriteEditMask, IsEditMaskStored);
end;

procedure TCustomDBEditEh.EditingChanged;
begin
  UpdateControlReadOnly;
end;

procedure TCustomDBEditEh.InternalSetText(const AText: String);
begin
  FInternalTextSetting := True;
  try
    inherited Text := AText;
  finally
    FInternalTextSetting := False;
  end;
end;

procedure TCustomDBEditEh.InternalSetValue(AValue: Variant);
begin
  InternalSetText(VarToStr(AValue));
end;

function TCustomDBEditEh.GetAlignment: TAlignment;
begin
  if evAlignmentEh in FAssignedValues
    then Result := FAlignment
    else Result := DefaultAlignment;
end;

procedure TCustomDBEditEh.SetAlignment(const Value: TAlignment);
begin
  if (evAlignmentEh in FAssignedValues) and (Value = FAlignment) then Exit;
  FAlignment := Value;
  Include(FAssignedValues, evAlignmentEh);
  if not (csLoading in ComponentState) then
    RecreateWndHandle;
end;

function TCustomDBEditEh.IsAlignmentStored: Boolean;
begin
  Result := (evAlignmentEh in FAssignedValues);
end;

function TCustomDBEditEh.DefaultAlignment: TAlignment;
begin
  if Assigned(Field)
    then Result := Field.Alignment
    else Result := taLeftJustify;
end;

function TCustomDBEditEh.GetEditMask: String;
begin
  Result := inherited EditMask;
end;

procedure TCustomDBEditEh.SetEditMask(const Value: String);
var OldText: String;
begin
  OldText := '';
  if (evEditMaskEh in FAssignedValues) and (Value = inherited EditMask) then Exit;
  if (csLoading in ComponentState) and (Text <> '') and DataIndepended then
    OldText := Text;
  inherited EditMask := Value;
  Include(FAssignedValues, evEditMaskEh);
  if (csLoading in ComponentState) and (OldText <> '') and DataIndepended then
    InternalSetText(OldText);
  if DataIndepended then
    InternalUpdatePostData
  else
    DataChange(nil);
end;

function TCustomDBEditEh.IsEditMaskStored: Boolean;
begin
  Result := (evEditMaskEh in FAssignedValues);
end;

function TCustomDBEditEh.DefaultEditMask: String;
begin
  if Assigned(Field)
    then Result := Field.EditMask
    else Result := '';
end;

function TCustomDBEditEh.IsTextStored: Boolean;
begin
  Result := (Text <> '') and DataIndepended;
end;

function TCustomDBEditEh.IsValueStored: Boolean;
begin
  Result := (Value <> Null) and DataIndepended;
end;

procedure TCustomDBEditEh.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    inherited SetAutoSize(Value);
    UpdateHeight;
  end;
end;

procedure TCustomDBEditEh.SetAlwaysShowBorder(const Value: Boolean);
begin
  if FAlwaysShowBorder <> Value then
  begin
    FAlwaysShowBorder := Value;
    UpdateDrawBorder;
  end;
end;

procedure TCustomDBEditEh.SetWordWrap(const Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    RecreateWndHandle;
  end;
end;

procedure TCustomDBEditEh.EditButtonClick(Sender: TObject);
var
  Handled: Boolean;
  i: Integer;
begin
  Handled := False;
  if (Sender = FButtonsBox.BtnCtlList[0].EditButtonControl) then
  begin
    EditButton.Click(Sender, Handled);
    if not Handled and Assigned(FOnButtonClick) then
      FOnButtonClick(Sender, Handled);

    if not Handled and
       EditButton.DefaultAction and
       Assigned(DBEditEhEditButtonDefaultActionProc)
    then
      EditButtonClickDefaultAction(EditButton,
        FButtonsBox.BtnCtlList[0].EditButtonControl, False, Handled);
  end else if (Sender is TEditButtonControlEh) then
  begin
    for i := 1 to Length(FButtonsBox.BtnCtlList) - 1 do
    begin
      if (Sender = FButtonsBox.BtnCtlList[i].EditButtonControl) then
      begin
        EditButtons[i - 1].Click(Sender, Handled);

        if not Handled and
           EditButtons[i - 1].DefaultAction and
           Assigned(DBEditEhEditButtonDefaultActionProc)
        then
          EditButtonClickDefaultAction(EditButtons[i - 1],
            FButtonsBox.BtnCtlList[i].EditButtonControl, False, Handled);
      end
    end
  end;
  if not Handled and
         FDroppedDown and
     not FNoClickCloseUp and
        (Sender = FDroppedDownButtonControl)
  then
    CloseUp(False);
  FNoClickCloseUp := False;
end;

procedure TCustomDBEditEh.EditButtonClickDefaultAction(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh; TopButton: Boolean; var Handled: Boolean);
begin
  DBEditEhEditButtonDefaultActionProc(Self, EditButton, EditButtonControl, False, Handled);
end;

procedure TCustomDBEditEh.EditButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  DoClick := (X >= 0) and (X < TControl(Sender).ClientWidth) and
    (Y >= 0) and (Y <= TControl(Sender).ClientHeight);
  if not DoClick then
    FNoClickCloseUp := False;
end;

procedure TCustomDBEditEh.EditButtonDown(Sender: TObject; TopButton: Boolean;
  var AutoRepeat: Boolean; var Handled: Boolean);
var
  i: Integer;
  p: TPoint;
begin
  Handled := False;
  SetFocus;
  if (not Focused) then Exit;

  if (Sender = FButtonsBox.BtnCtlList[0].EditButtonControl) then
  begin
    if not FButtonsBox.BtnCtlList[0].EditButtonControl.Enabled then Exit;
    if Assigned(FOnButtonDown) then
      FOnButtonDown(Sender, TopButton, AutoRepeat, Handled);
    if not Handled then
      CheckEditButtonDownForDropDownForm(Self, FDataLink, Field, Text,
        EditButton, FButtonsBox.BtnCtlList[0].EditButtonControl,
        OnOpenDropDownForm, DropDownFormCallbackProc,
        Handled);

    if not Handled then
      if Assigned(EditButton.DropdownMenu) then
      begin
        P := TControl(Sender).ClientToScreen(Point(0, TControl(Sender).Height));
        if EditButton.DropdownMenu.Alignment = paRight then
          Inc(P.X, TControl(Sender).Width);
        EditButton.DropdownMenu.Popup(p.X, p.y);
        KillMouseUp(TControl(Sender));
        TControl(Sender).Perform(WM_LBUTTONUP, 0, 0);
        Handled := True;
      end else if (EditButton.Action = nil) and EditButton.DefaultAction then
        EditButtonDownDefaultAction(EditButton,
          FButtonsBox.BtnCtlList[0].EditButtonControl, TopButton, AutoRepeat, Handled);
  end
  else if (Sender is TEditButtonControlEh) then
    for i := 1 to Length(FButtonsBox.BtnCtlList) - 1 do
    begin
      if (Sender = FButtonsBox.BtnCtlList[i].EditButtonControl) then
      begin
        if Assigned(EditButtons[i - 1].OnDown) then
          EditButtons[i - 1].OnDown(Sender, TopButton, AutoRepeat, Handled);
        if not Handled then
          CheckEditButtonDownForDropDownForm(Self, FDataLink, Field, Text,
            EditButtons[i - 1], FButtonsBox.BtnCtlList[i].EditButtonControl,
            OnOpenDropDownForm, DropDownFormCallbackProc, Handled);

        if not Handled then
          if Assigned(EditButtons[i - 1].DropdownMenu) then
          begin
            P := TControl(Sender).ClientToScreen(Point(0, TControl(Sender).Height));
            if EditButtons[i - 1].DropdownMenu.Alignment = paRight then
              Inc(P.X, TControl(Sender).Width);
            EditButtons[i - 1].DropdownMenu.Popup(p.X, p.y);
            KillMouseUp(TControl(Sender));
            TControl(Sender).Perform(WM_LBUTTONUP, 0, 0);
            Handled := True;
          end;

        if not Handled and EditButtons[i - 1].DefaultAction then
          EditButtonDownDefaultAction(EditButtons[i - 1],
            FButtonsBox.BtnCtlList[i].EditButtonControl, TopButton, AutoRepeat, Handled);
      end;
    end;
end;

procedure TCustomDBEditEh.EditButtonDownDefaultAction(
  EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh;
  TopButton: Boolean; var AutoRepeat: Boolean; var Handled: Boolean);
begin
  DBEditEhEditButtonDefaultActionProc(Self, EditButton, EditButtonControl, True, Handled);
end;

procedure TCustomDBEditEh.BeforeShowDefaulEditDropDownForm(EditControl: TControl;
  Button: TEditButtonEh; var DropDownForm: TCustomForm; DynParams: TDynVarsEh);
begin
  if DropDownForm is TCustomDropDownFormEh then
    TCustomDropDownFormEh(DropDownForm).ReadOnly := ReadOnly or not FDataLink.CanModify;
  if Assigned(OnOpenDropDownForm) then
    OnOpenDropDownForm(Self, Button, DropDownForm, DynParams);
end;

procedure TCustomDBEditEh.SetEditButtonDroppedDown(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh);
begin
  FDroppedDown := True;
  FDroppedDownButton := EditButton;
  FDroppedDownButtonControl := EditButtonControl;
  FDroppedDownButtonControl.AlwaysDown := True;
end;

procedure TCustomDBEditEh.SetEditButtonClosedUp;
begin
  if FDroppedDownButtonControl <> nil then
    FDroppedDownButtonControl.AlwaysDown := False;
  FDroppedDown := False;
  FDroppedDownButton := nil;
  FDroppedDownButtonControl := nil;
end;

procedure TCustomDBEditEh.DropDownAction(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh; var Handled: Boolean);
begin
  if MRUList.DroppedDown then
    MRUList.CloseUp(False);
end;

procedure TCustomDBEditEh.CloseUp(Accept: Boolean);
begin
end;

procedure TCustomDBEditEh.EditButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TCustomDBEditEh.DropDownFormCallbackProc(DropDownForm: TCustomForm;
  Accept: Boolean; DynParams: TDynVarsEh; SysParams: TDropDownFormSysParams);
var
  I: Integer;
begin
  for i := 0 to Length(FButtonsBox.BtnCtlList) - 1 do
    FButtonsBox.BtnCtlList[i].EditButtonControl.AlwaysDown := False;

  DefaultDropDownFormCallbackProc(Self, FDataLink, DropDownForm,
    Accept, DynParams, SysParams,
    SetValue, OnCloseDropDownForm);
end;

{$IFDEF FPC}
{$ELSE}
procedure TCustomDBEditEh.CMCancelMode(var Message: TCMCancelMode);

  function CheckDataListChilds: Boolean;
  var i: Integer;
  begin
    Result := False;
    if FMRUListControl <> nil then
      for i := 0 to MRUListControl.ControlCount - 1 do
        if MRUListControl.Controls[I] = Message.Sender then
        begin
          Result := True;
          Exit;
        end;
  end;

var
  i: Integer;
begin
  inherited;
  for i := 0 to ControlCount - 1 do
    if GetCaptureControl = Controls[i] then
    begin
      Controls[i].Perform(WM_CANCELMODE, 0, 0);
      Break;
    end;
  if (Message.Sender <> Self) and not ContainsControl(Message.Sender) and
    (Message.Sender <> FMRUListControl) and not CheckDataListChilds
  then
    MRUList.CloseUp(False);
end;

procedure TCustomDBEditEh.CMRecreateWnd(var Message: TMessage);
var
  WasFocused: Boolean;
begin
  WasFocused := Focused;
  inherited;
  if WasFocused then
    UpdateDrawBorder;
end;


procedure TCustomDBEditEh.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  ClearButtonsBitmapCache;
end;

procedure TCustomDBEditEh.CMDialogKey(var Message: TCMDialogKey);
begin
  inherited;
end;

{$ENDIF}

procedure TCustomDBEditEh.SetEditButtons(const Value: TEditButtonsEh);
begin
  FEditButtons.Assign(Value);
end;

procedure ResetEditButtonControl(ControlRec: TEditButtonControlLineRec;
  Intex: Integer; Flat: Boolean; MaxButtonHeight: Integer;
  var MinButtonHeight: Integer);
var
  AButtonHeight: Integer;
begin
  ControlRec.EditButtonControl.Visible := ControlRec.EditButton.Visible;
  ControlRec.EditButtonControl.Enabled := ControlRec.EditButton.Enabled;
  ControlRec.EditButtonControl.Style := ControlRec.EditButton.Style;
  ControlRec.EditButtonControl.Glyph := ControlRec.EditButton.Glyph;
  ControlRec.EditButtonControl.NumGlyphs := ControlRec.EditButton.NumGlyphs;
  ControlRec.EditButtonControl.Hint := ControlRec.EditButton.Hint;
  ControlRec.EditButtonControl.DrawBackTime := ControlRec.EditButton.DrawBackTime;
  ControlRec.EditButtonControl.Flat := Flat;
  ControlRec.EditButtonControl.FExternalEditButtonImages := ControlRec.EditButton.Images;

  if not ControlRec.EditButton.Visible then
    ControlRec.EditButtonControl.Width := 0
  else if ControlRec.EditButton.Width > 0 then
    ControlRec.EditButtonControl.Width := ControlRec.EditButton.Width
  else if Flat then
    ControlRec.EditButtonControl.Width := FlatButtonWidth
  else
    ControlRec.EditButtonControl.Width := GetSystemMetrics(SM_CXVSCROLL);

  if ControlRec.EditButton.Visible then
  begin
    if MaxButtonHeight > Round(ControlRec.EditButtonControl.Width * 3 / 2)
      then AButtonHeight := DefaultEditButtonHeight(ControlRec.EditButtonControl.Width, Flat)
      else AButtonHeight := MaxButtonHeight;

    if AButtonHeight < MinButtonHeight then
      MinButtonHeight := AButtonHeight;
  end;

  if ControlRec.ButtonLine <> nil then
  begin
    ControlRec.ButtonLine.Visible := Flat and ControlRec.EditButton.Visible and not ThemesEnabled;
    if Flat and ControlRec.EditButton.Visible and not ThemesEnabled
      then ControlRec.ButtonLine.Width := 1
      else ControlRec.ButtonLine.Width := 0;
  end;
end;

procedure TCustomDBEditEh.UpdateEditButtonControlList;
var
  i: Integer;
  AButtonRect: TRect;
begin
  FButtonsBox.BeginLayout;

  FButtonsBox.ButtonsCount := EditButtons.Count + 1;
  FButtonsBox.Flat := Flat;
  FButtonsBox.MaxButtonHeight := ButtonRect.Bottom - ButtonRect.Top;

  FButtonsBox.BtnCtlList[0].EditButton := EditButton;
  for i := 1 to EditButtons.Count do
    FButtonsBox.BtnCtlList[i].EditButton := EditButtons[i - 1];
  FButtonsBox.EndLayout;

  AButtonRect := ButtonRect;

  if FButtonsBox.ButtonsWidth > 0 then
  begin
    FButtonsBox.SetBounds(AButtonRect.Left, AButtonRect.Top, AButtonRect.Right-AButtonRect.Left, AButtonRect.Bottom-AButtonRect.Top);
    FButtonsBox.Visible := True;
    ShowWindow(FButtonsBox.Handle, SW_SHOWNORMAL);
  end else
  begin
    FButtonsBox.Visible := False;
    ShowWindow(FButtonsBox.Handle, SW_HIDE);
  end;
end;

procedure TCustomDBEditEh.UpdateEditButtonControlsState;
var
  i: Integer;
begin
  FButtonsBox.BorderActive := FBorderActive;
  FButtonsBox.UpdateEditButtonControlsState;

  TEditButtonEhCracker(EditButton).FParentDefinedDefaultAction :=
    EditButtonDefaultAction(EditButton);

  for i := 0 to EditButtons.Count-1 do
  begin
    TEditButtonEhCracker(EditButtons[i]).FParentDefinedDefaultAction :=
      EditButtonDefaultAction(EditButtons[i]);
  end;
end;

function TCustomDBEditEh.EditButtonDefaultAction(AEditButton: TEditButtonEh): Boolean;
begin
  if AEditButton = EditButton then
    Result := EditButton.Visible
  else
    Result := 
              (@AEditButton.OnClick = nil) and
              (@AEditButton.OnDown = nil) and
              (AEditButton.DropDownFormParams.DropDownForm = nil) and
              (AEditButton.DropDownFormParams.DropDownFormClassName = '') and
              (AEditButton.DropdownMenu = nil);
end;

procedure TCustomDBEditEh.CheckEditButtonsRemoveNotification(AComponent: TComponent);

  procedure CheckButtonRemoveNotification(EditButton: TEditButtonEh);
  begin
    if EditButton.Images.NormalImages = AComponent then
      EditButton.Images.NormalImages := nil;
    if EditButton.Images.HotImages = AComponent then
      EditButton.Images.HotImages := nil;
    if EditButton.Images.PressedImages = AComponent then
      EditButton.Images.PressedImages := nil;
    if EditButton.Images.DisabledImages = AComponent then
      EditButton.Images.DisabledImages := nil;
  end;

var
  i: Integer;
begin
  if csDestroying in ComponentState then Exit;
  CheckButtonRemoveNotification(EditButton);
  for i := 0 to EditButtons.Count-1 do
    CheckButtonRemoveNotification(EditButtons[i]);
end;

procedure TCustomDBEditEh.EditButtonImagesRefComponentNotifyEvent(
  Sender: TObject; RefComponent: TComponent);

  procedure UpdateButtonFreeNotifications(EditButton: TEditButtonEh);
  begin
    if EditButton.Images.NormalImages = RefComponent then
      RefComponent.FreeNotification(Self);
    if EditButton.Images.HotImages = RefComponent then
      RefComponent.FreeNotification(Self);
    if EditButton.Images.PressedImages = RefComponent then
      RefComponent.FreeNotification(Self);
    if EditButton.Images.DisabledImages = RefComponent then
      RefComponent.FreeNotification(Self);
  end;

var
  i: Integer;
begin
  Invalidate;
  if RefComponent = nil then Exit;
  UpdateButtonFreeNotifications(EditButton);
  for i := 0 to EditButtons.Count-1 do
    UpdateButtonFreeNotifications(EditButtons[i]);
end;

procedure TCustomDBEditEh.EditButtonChanged(Sender: TObject);
begin
  if not HandleAllocated then Exit;
  UpdateEditButtonControlList;
  UpdateEditButtonControlsState;
  SetEditRect;
  Invalidate;
end;

function TCustomDBEditEh.GetEditButtonByShortCut(ShortCut: TShortCut): TEditButtonEh;
var i: Integer;
begin
  Result := nil;
  if (ShortCut = FEditButton.ShortCut) then
    Result := FEditButton
  else
    for i := 0 to EditButtons.Count - 1 do
      if (ShortCut = EditButtons[i].ShortCut) then
      begin
        Result := EditButtons[i];
        Exit;
      end;
end;

function TCustomDBEditEh.GetPasswordChar: Char;
begin
  Result := inherited PasswordChar;
end;

procedure TCustomDBEditEh.SetPasswordChar(const Value: Char);
begin
  if inherited PasswordChar <> Value then
  begin
    inherited PasswordChar := Value;
    RecreateWndHandle;
  end;
end;

procedure TCustomDBEditEh.SetInplaceEditHolder(AInplaceEditHolder: TWinControl);
begin
  if AInplaceEditHolder = FInplaceEditHolder then Exit;
  FInplaceEditHolder := AInplaceEditHolder;
  if FInplaceEditHolder = nil then
    FIntfInplaceEditHolder := nil
  else if not Supports(FInplaceEditHolder, IInplaceEditHolderEh, FIntfInplaceEditHolder) then
    raise Exception.Create('InplaceEditHolder have to support IInplaceEditHolderEh interface');
  FInplaceMode := (FInplaceEditHolder <> nil);
end;

procedure TCustomDBEditEh.CheckInplaceEditHolderKeyDown(var Key: Word; Shift: TShiftState);
begin
  if FInplaceMode then
    FIntfInplaceEditHolder.InplaceEditKeyDown(Self, Key, Shift);
end;

procedure TCustomDBEditEh.CheckInplaceEditHolderKeyPress(var Key: Char);
begin
  if FInplaceMode then
    FIntfInplaceEditHolder.InplaceEditKeyPress(Self, Key);
end;

procedure TCustomDBEditEh.CheckInplaceEditHolderKeyUp(var Key: Word; Shift: TShiftState);
begin
  if FInplaceMode then
    FIntfInplaceEditHolder.InplaceEditKeyUp(Self, Key, Shift);
end;

procedure TCustomDBEditEh.InternalMove(const Loc: TRect; Redraw: Boolean);
{$IFDEF CIL}
var
  Msg: TCMCancelMode;
{$ENDIF}
begin
  if IsRectEmpty(Loc) then Hide
  else
  begin
    CreateHandle;
    Redraw := Redraw or not IsWindowVisible(Handle);
    Invalidate;
{$IFDEF CIL}
    Msg := TCMCancelMode.Create;
    Msg.Sender := FInplaceEditHolder;
    Perform(CM_CANCELMODE, 0, Msg.OriginalMessage.LParam);
{$ELSE}
  {$IFDEF FPC}
    
  {$ELSE}
     Perform(CM_CANCELMODE, 0, ObjectToIntPtr(FInplaceEditHolder));
  {$ENDIF}
{$ENDIF}

    {$IFDEF FPC}
    SetBounds(Loc.Left, Loc.Top, Loc.Right - Loc.Left, Loc.Bottom - Loc.Top);
    Show;
    BoundsChanged;
    if FInplaceEditHolder.Focused then
      FIntfInplaceEditHolder.InternalSetFocusedControl(Self);
    {$ELSE}
    SetWindowPos(Handle, HWND_TOP,
      Loc.Left, Loc.Top, Loc.Right - Loc.Left, Loc.Bottom - Loc.Top,
      SWP_SHOWWINDOW {or SWP_NOREDRAW});
    
    if Redraw then Invalidate;

    if FInplaceEditHolder.Focused then
      Windows.SetFocus(Handle);
    {$ENDIF} 
  end;
end;

procedure TCustomDBEditEh.CMShowingChanged(var Message: TMessage);
begin
  {$IFDEF FPC}
  inherited;
  {$ELSE}
  if not FInplaceMode then { Ignore showing using the Visible property when InplaceMode}
    inherited;
  {$ENDIF}
end;

procedure TCustomDBEditEh.Hide;
begin
  if not FInplaceMode then
    Visible := False
  else if HandleAllocated and IsWindowVisibleState then
  begin
    Invalidate;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOZORDER {or SWP_NOREDRAW});
    {$IFDEF FPC_CROSSP}
    {$ELSE}
    if Focused then
      Windows.SetFocus(FInplaceEditHolder.Handle);
    {$ENDIF}
  end;
end;

procedure TCustomDBEditEh.Move(const Loc: TRect);
begin
  if FInplaceMode
    then InternalMove(Loc, True)
    else BoundsRect := Loc;
end;

procedure TCustomDBEditEh.SetFocus;
begin
  if not FInplaceMode then
  begin
    inherited SetFocus;
    {$IFDEF EH_LIB_12}
    {$ELSE}
    WinServiceSetFocus(Handle);
    {$ENDIF}
//  {$IFDEF FPC_CROSSP}
//  {$ELSE}
  end else if IsWindowVisible(Handle) then
    WinServiceSetFocus(Handle);
//    Windows.SetFocus(Handle);
//  {$ENDIF}
end;

procedure TCustomDBEditEh.UpdateLoc(const Loc: TRect);
begin
  if FInplaceMode
    then InternalMove(Loc, False)
    else BoundsRect := Loc;
end;

function TCustomDBEditEh.IsWindowVisibleState: Boolean;
begin
  Result := (GetWindowLong(Handle, GWL_STYLE) and WS_VISIBLE) <> 0;
end;

function TCustomDBEditEh.GetVisible: Boolean;
begin
  {$IFDEF FPC}
  Result := inherited Visible;
  {$ELSE}
  if FInplaceMode
    then Result := HandleAllocated and IsWindowVisibleState
    else Result := inherited Visible;
  {$ENDIF}
end;

procedure TCustomDBEditEh.SetVisible(const Value: Boolean);
begin
  {$IFDEF FPC}
  inherited SetVisible(Value);
  {$ELSE}
  if FInplaceMode and not Value
    then Hide
    else inherited Visible := Value;
  {$ENDIF}
end;

function TCustomDBEditEh.GetDisplayTextForPaintCopy: String;
begin
  if (csDesigning in ComponentState) and not (FDataLink.Active) then
    Result := Name
  else if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
  begin
    Result := FDataLink.Field.DisplayText;
    case CharCase of
      ecUpperCase: Result := NlsUpperCase(Result);
      ecLowerCase: Result := NlsLowerCase(Result);
    end;
  end else
    Result := EditText;
end;

procedure TCustomDBEditEh.SetMRUList(const Value: TMRUListEh);
begin
  FMRUList.Assign(Value);
end;

function TCustomDBEditEh.GetMRUListControl: TWinControl;
begin
  if not Assigned(FMRUListControl) then
    FMRUListControl := CreateMRUListControl;
  Result := FMRUListControl;
end;

function TCustomDBEditEh.CreateMRUListControl: TWinControl;
var
  mrc: TMRUListboxEh;
begin
  Result := TMRUListboxEh.Create(Self);
  mrc := TMRUListboxEh(Result);
  mrc.InternalResizing := True;
  Result.Visible := False;
  {$IFDEF FPC}
  {$ELSE}
  mrc.Ctl3D := False;
  mrc.ParentCtl3D := False;
  {$ENDIF}
  mrc.Sorted := True;
  mrc.OnMouseUp := MRUListControlMouseUp;
  mrc.InternalResizing := False;
end;

procedure TCustomDBEditEh.MRUListFillAutogenItems(Sender: TMRUListEh; AutogenItems: TStrings);
var
  DatasetFeatures: TDatasetFeaturesEh;
begin
  if (Field <> nil) and (Field.DataSet <> nil) and Field.DataSet.Active then
  begin
    DatasetFeatures := GetDatasetFeaturesForDataSet(Field.DataSet);
    if DatasetFeatures <> nil then
      DatasetFeatures.FillFieldUniqueValues(Field, AutogenItems);
  end;
end;

procedure TCustomDBEditEh.MRUListControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    MRUList.CloseUp(PtInRect(MRUListControl.ClientRect, Point(X, Y)));
end;

procedure TCustomDBEditEh.MRUListCloseUp(Sender: TObject; Accept: Boolean);
var
  pls: TPopupListboxFormEh;
begin
  if MRUList.DroppedDown then
  begin
    pls := TPopupListboxFormEh(MRUListControl);
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    pls.Hide;
    if TPopupListboxFormEh(MRUListControl).SizeGripResized then
    begin
      MRUList.Rows := TPopupListboxFormEh(MRUListControl).ResizedRowCount;
      MRUList.Width := TPopupListboxFormEh(MRUListControl).ResizedWidth;
    end;
    MRUListControl.Visible := False;
    if (GetFocus = MRUListControl.Handle) then
      SetFocus;
    MRUList.DroppedDown := False;
    if Accept and not ReadOnly and FDataLink.Edit then
    begin
       if pls.ItemIndex >= 0 then
         InternalSetText(pls.Items[pls.ItemIndex]);
      if FFocused then SelectAll;
    end;
  end;
end;

procedure TCustomDBEditEh.MRUListDropDown(Sender: TObject);
var
  P: TPoint;
  OldSizeGripResized: Boolean;
  AEditRect: TRect;
  mlc: TPopupListboxFormEh;
begin
  mlc := TPopupListboxFormEh(MRUListControl);
  begin
    OldSizeGripResized := mlc.SizeGripResized;
    mlc.InternalResizing := True;
    if not MRUList.FilterItemsTo(mlc.Items, Text) then
      MRUList.CloseUp(False);
    if mlc.Items.Count < MRUList.Rows
      then mlc.RowCount := mlc.Items.Count
      else mlc.RowCount := MRUList.Rows;

    if MRUList.DroppedDown then
    begin
      AEditRect := ClientRect;
      CalcEditRect(AEditRect);
      AEditRect.TopLeft := Self.ClientToScreen(AEditRect.TopLeft);
      AEditRect.BottomRight := Self.ClientToScreen(AEditRect.BottomRight);
      P := AlignDropDownWindowRect(AEditRect, MRUListControl, daLeft);
      mlc.SetBounds(P.x, P.y, mlc.Width, mlc.Height);
      mlc.Show;
      mlc.SizeGripResized := OldSizeGripResized;
    end;

    if (mlc.Items.Count <= 0) and MRUList.DroppedDown then
      MRUList.CloseUp(False)
    else if not MRUList.DroppedDown and (mlc.Items.Count > 0) then
    begin
      mlc.Color := Self.Color;
      mlc.Font := Self.Font;
      mlc.ItemHeight := mlc.GetTextHeight;
      mlc.ItemIndex := -1;
      if mlc.Items.Count < mlc.RowCount then
        mlc.RowCount := mlc.Items.Count;
      AEditRect := ClientRect;
      CalcEditRect(AEditRect);
      AEditRect.TopLeft := Self.ClientToScreen(AEditRect.TopLeft);
      AEditRect.BottomRight := Self.ClientToScreen(AEditRect.BottomRight);
      if MRUList.Width > 0
        then mlc.Width := MRUList.Width
        else mlc.Width := AEditRect.Right-AEditRect.Left;
      P := AlignDropDownWindowRect(AEditRect, MRUListControl, daLeft);
      mlc.SetBounds(P.x, P.y, mlc.Width, mlc.Height);
      mlc.Show;
      MRUList.DroppedDown := True;
      mlc.SizeGripResized := False;
    end;
    mlc.InternalResizing := False;
  end;
end;

procedure TCustomDBEditEh.CMTextChanged(var Message: TMessage);
begin
  inherited;
  {$IFDEF FPC_CROSSP}
  if not FInternalTextSetting then
    FUserTextChanged := True;
  {$ELSE}
  {$ENDIF}
end;

procedure TCustomDBEditEh.CNCommand(var Message: TWMCommand);
begin
  inherited;
  {$IFDEF FPC_CROSSP}
  {$ELSE}
  if (Message.NotifyCode = EN_CHANGE) then
    FUserTextChanged := True;
  {$ENDIF}
end;

procedure TCustomDBEditEh.UserChange;
var
  BlankText: String;
begin
  if IsMasked
    then BlankText := FormatMaskText(EditMask, '')
    else BlankText := '';

  if MRUList.DroppedDown and (Text = BlankText) then
  begin
    MRUList.CloseUp(False);
  end
  else if MRUList.Active and
          Showing and
          not FDroppedDown and
          (Text <> BlankText) and
          FFocused
  then
  begin
    MRUList.DropDown;
  end;
end;

procedure TCustomDBEditEh.CMMouseWheel(var Message: TMessage);
begin
  if MRUList.DroppedDown then
  begin
{$IFDEF CIL}
{$ELSE}
{$ENDIF}
    if SendMessage(MRUListControl.Handle, CM_MOUSEWHEEL, Message.WParam, Message.LParam) <> 0 then
    begin
      Message.Result := 1;
      Exit;
    end;
  end;
  inherited;
end;

procedure TCustomDBEditEh.FilterMRUItem(const AText: String; var Accept: Boolean);
begin
  if MRUList.CaseSensitive
    then Accept := (NlsCompareStr(Copy(AText, 1, Length(Text)), Text) = 0)
    else Accept := (NlsCompareText(Copy(AText, 1, Length(Text)), Text) = 0);
  if Assigned(MRUList.OnFilterItem) then
    MRUList.OnFilterItem(Self, Accept);
end;

procedure TCustomDBEditEh.Deselect;
begin
  {$IFDEF FPC_CROSSP}
  {$ELSE}
  SendMessage(Handle, EM_SETSEL, $7FFFFFFF, Longint($FFFFFFFF));
  {$ENDIF}
end;

function TCustomDBEditEh.GetImages: TCustomImageList;
begin
  Result := EditImage.Images;
end;

procedure TCustomDBEditEh.SetImages(const Value: TCustomImageList);
begin
  EditImage.Images := Value;
  EditImage.Visible := True;
end;

function TCustomDBEditEh.DefaultImageIndex: Integer;
begin
  Result := -1;
end;

procedure TCustomDBEditEh.UpdateImageIndex;
var
  ImageIndex: Longint;
begin
  if EditImage.Visible and (EditImage.Images <> nil) then
  begin
    ImageIndex := DefaultImageIndex;
    if VarType(Value) in [varDouble, varSmallint, varInteger, varSingle, varCurrency] then
      ImageIndex := Integer(Round(Double(Value)));
    if Assigned(OnGetImageIndex) then
      OnGetImageIndex(Self, ImageIndex);
    EditImage.ImageIndex := ImageIndex;
  end;
end;

procedure TCustomDBEditEh.SetBorderStyle(ABorderStyle: TBorderStyle);
begin
  BorderStyle := ABorderStyle;
end;

procedure TCustomDBEditEh.SetColor(AColor: TColor);
begin
  Color := AColor;
end;

procedure TCustomDBEditEh.SetFont(AFont: TFont);
begin
  Font := AFont;
end;

procedure TCustomDBEditEh.SetOnExit(AKeyPressEvent: TNotifyEvent);
begin
  OnExit := AKeyPressEvent;
end;

procedure TCustomDBEditEh.SetOnKeyPress(AKeyPressEvent: TKeyPressEvent);
begin
  OnKeyPress := AKeyPressEvent;
end;

function TCustomDBEditEh.GetFont: TFont;
begin
  Result := Font;
end;

procedure TCustomDBEditEh.SetOnGetImageIndex(const Value: TGetImageIndexEventEh);
begin
  FOnGetImageIndex := Value;
  UpdateImageIndex;
end;

procedure TCustomDBEditEh.SetTooltips(const Value: Boolean);
begin
  if FTooltips <> Value then
  begin
    FTooltips := Value;
    UpdateHintProcessing;
  end;
end;

procedure TCustomDBEditEh.UpdateHintProcessing;
begin
  inherited ShowHint := FTooltips or FShowHint;
end;

function TCustomDBEditEh.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  Result := FCanvas;
end;

function TCustomDBEditEh.GetShowHint: Boolean;
begin
  Result := FShowHint;
end;

procedure TCustomDBEditEh.SetShowHint(const Value: Boolean);
begin
  if FShowHint <> Value then
  begin
    FShowHint := Value;
    UpdateHintProcessing;
  end;
end;

procedure TCustomDBEditEh.Undo;
begin
  if not DataIndepended then DataSource.DataSet.Edit;
  inherited Undo;
  if FDataPosting or FFocused then Exit;
  try
    UpdateData;
  except
    FDataLink.Reset;
    raise;
  end;
end;

procedure TCustomDBEditEh.Clear;
begin
  Text := '';
end;

procedure TCustomDBEditEh.SetEmptyDataInfo(const Value: TControlEmptyDataInfoEh);
begin
  FEmptyDataInfo.Assign(Value);
end;

procedure TCustomDBEditEh.SetDynProps(const Value: TDynVarsEh);
begin
  FDynProps.Assign(Value);
end;

function TCustomDBEditEh.IsEmpty: Boolean;
var
  EmptyString: String;
begin
  if EditMask <> '' then
  begin
    EmptyString := FormatMaskTextEh(EditMask, '');
    Result := EditText = EmptyString;
  end else
  begin
    Result := (Text = '');
  end;
end;

procedure TCustomDBEditEh.SetVarValue(const VarValue: Variant);
begin
  SetVariantValue(VarValue);
end;

procedure TCustomDBEditEh.GetVarValue(var VarValue: Variant);
begin
  VarValue := GetVariantValue;
end;

procedure TCustomDBEditEh.DropDownFormCloseProc(EditControl: TControl;
  Button: TEditButtonEh; Accept: Boolean; DropDownForm: TCustomForm;
  DynParams: TDynVarsEh);
var
  I: Integer;
begin
  for i := 0 to Length(FButtonsBox.BtnCtlList) - 1 do
    FButtonsBox.BtnCtlList[i].EditButtonControl.AlwaysDown := False;
  if Assigned(OnCloseDropDownForm) then
    OnCloseDropDownForm(EditControl, Button, Accept, DropDownForm, DynParams);
end;

procedure TCustomDBEditEh.RecreateWndHandle;
begin
  {$IFDEF FPC}
  RecreateWnd(Self);
  {$ELSE}
  RecreateWnd;
  {$ENDIF}
end;

{$IFDEF FPC}
function TCustomDBEditEh.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result := True;
end;

procedure TCustomDBEditEh.DoAutoSize;
begin
end;

procedure TCustomDBEditEh.Resize;
begin
  inherited Resize;
  if not HandleAllocated then Exit;
  UpdateEditButtonControlList;
  SetEditRect;
end;

function TCustomDBEditEh.Ctl3D: Boolean;
begin
  Result := True;
end;
{$ENDIF}

function TCustomDBEditEh.GetEditButtonControlByEditButton(
  AEditButton: TEditButtonEh): TEditButtonControlEh;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Length(FButtonsBox.BtnCtlList)-1 do
    if FButtonsBox.BtnCtlList[i].EditButton = AEditButton then
    begin
      Result := FButtonsBox.BtnCtlList[i].EditButtonControl;
      Exit;
    end;
end;

function TCustomDBEditEh.GetFirstDefaultActionEditButton: TEditButtonEh;
var
  i: Integer;
begin
  Result := nil;
  if EditButton.DefaultAction and EditButton.Visible then
    Result := EditButton
  else if True then
    for i := 0 to EditButtons.Count-1 do
      if EditButtons[i].DefaultAction and EditButton.Visible then
      begin
        Result := EditButtons[i];
        Exit;
      end;
end;

procedure TCustomDBEditEh.SetBounds(ALeft: Integer; ATop: Integer;
  AWidth: Integer; AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  AdjustLabelBounds;
end;

procedure TCustomDBEditEh.AdjustLabelBounds;
var
  NewPos: TPoint;
begin
  if FControlLabel = nil then Exit;
  FControlLabelLocation.CalcLabelPosForControl(FControlLabel.Width, FControlLabel.Height, NewPos);
  FControlLabel.SetBounds(NewPos.X, NewPos.Y, FControlLabel.Width, FControlLabel.Height);
end;

function TCustomDBEditEh.GetControlLabelCaption: String;
begin
  if Field <> nil
    then Result := Field.DisplayName
    else Result := Name;
end;

function TCustomDBEditEh.GetControlTextBaseLine: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  {$WARNINGS OFF}
  SaveFont := SelectObject(DC, Font.Handle);
  {$WARNINGS ON}
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
    if Ctl3D and not Flat then I := 1 else I := 0;
    I := GetSystemMetrics(SM_CYBORDER) * 2 + I;
  end else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Result := Metrics.tmHeight + I;
end;

procedure TCustomDBEditEh.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FControlLabel = nil then Exit;
  FControlLabel.UpdateParent;
  FControlLabel.UpdateVisibility;
  FControlLabel.UpdateCaption;
end;

procedure TCustomDBEditEh.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  AdjustLabelBounds;
end;

procedure TCustomDBEditEh.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FControlLabel <> nil then
    FControlLabel.UpdateVisibility;
end;

procedure TCustomDBEditEh.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FControlLabel <> nil then
    FControlLabel.BiDiMode := BiDiMode;
end;

procedure TCustomDBEditEh.LabelSpacingChanged;
begin
  if not (csLoading in ComponentState) then
    AdjustLabelBounds;
end;

procedure TCustomDBEditEh.SetControlLabelParams(
  const Value: TControlLabelLocationEh);
begin
  FControlLabelLocation.Assign(Value);
end;

function TCustomDBEditEh.GetFillColor: TColor;
{$IFDEF EH_LIB_16}
const
  StyleColor: array[Boolean] of TStyleColor = (scEditDisabled, scEdit);
{$ENDIF}
begin
{$IFDEF EH_LIB_16}
  if CustomStyleActive then
    Result := StyleServices.GetStyleColor(StyleColor[Enabled])
  else
{$ENDIF}
    Result := StyleServices.GetSystemColor(Color);
end;

function TCustomDBEditEh.GetFontColor: TColor;
{$IFDEF EH_LIB_16}
const
  StyleFontColor: array[Boolean] of TStyleFont = (sfEditBoxTextDisabled, sfEditBoxTextNormal);
{$ENDIF}
begin
{$IFDEF EH_LIB_16}
  if CustomStyleActive then
    Result := StyleServices.GetStyleFontColor(StyleFontColor[Enabled])
  else
{$ENDIF}
    if not Enabled then
      Result := StyleServices.GetSystemColor(clGrayText)
    else
      Result := StyleServices.GetSystemColor(Font.Color);
end;

function TCustomDBEditEh.FixClipboardText(const Text: String): String;
var
  i, k: Integer;
begin
  k := 0;
  for i := Length(Text) downto 1 do
  begin
    if (Text[i] = #13) or (Text[i] = #10) then
      k := i
    else
      Break;
  end;
  if k > 0 then
    Result := Copy(Text, 1, k-1)
  else
    Result := Text;

  Result := StringReplace(Result, #13#10, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #10#13, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #13, ' ', [rfReplaceAll]);
end;

{ TCustomDBDateTimeEditEh }

type TDateOrder = (doMDY, doDMY, doYMD);

const
  DefaultDateOrder = doDMY;
  CenturyOffset: Byte = 60;

function CurrentYear: Word;
begin
  Result := DateUtils.YearOf(Now)
end;

function ExpandYear(Year: Integer): Integer;
var
  N: Longint;
begin
  Result := Year;
  if Result < 100 then
  begin
    N := CurrentYear - CenturyOffset;
    Inc(Result, N div 100 * 100);
    if (CenturyOffset > 0) and (Result < N) then
      Inc(Result, 100);
  end;
end;

function DaysPerMonth(AYear, AMonth: Integer): Integer;
const
  DaysInMonth: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then Inc(Result); { leap-year Feb is special }
end;

function CorrectDate(var Year, Month, Day: Integer): Boolean;
var
  CurYear, CurMonth, CurDay: Word;
begin
  Result := False;
  DecodeDate(Date, CurYear, CurMonth, CurDay);
  if Day <= 0 then Day := CurDay;
  if Month <= 0 then Month := CurMonth;
  if Year <= 0 then Year := CurYear;
  if Month > 12 then Month := 12;
  if Year > 9999 then Year := 9999;
  if Year >= 0 then Year := ExpandYear(Year);
  if DaysPerMonth(Year, Month) < Day then
    Day := DaysPerMonth(Year, Month);
  if (Day <> CurDay) or (Month <> CurMonth) or (Year <> CurYear) then
    Result := True;
end;

function CorrectTime(var Hor, Min, Sec: Integer): Boolean;
begin
  Result := False;
  if (Hor < 0) or (Min < 0) or (Sec < 0) or
    (Hor > 23) or (Min > 59) or (Sec > 59) then
  begin
    Result := True;
    if (Hor < 0) then Hor := 0;
    if Min < 0 then Min := 0;
    if Sec < 0 then Sec := 0;
    if Hor > 23 then Hor := 23;
    if Min > 59 then Min := 59;
    if Sec > 59 then Sec := 59;
  end;
end;

function GetDateOrder(const DateFormat: string): TDateOrder;
var
  I: Integer;
begin
  Result := DefaultDateOrder;
  I := 1;
  while I <= Length(DateFormat) do
  begin
    case Chr(Ord(DateFormat[I]) and $DF) of
{$IFDEF RX_D3}
      'E': Result := doYMD;
{$ENDIF}
      'Y': Result := doYMD;
      'M': Result := doMDY;
      'D': Result := doDMY;
    else
      Inc(I);
      Continue;
    end;
    Break;
  end;
end;

function DefDateFormat(FourDigitYear: Boolean): string;
begin
  if FourDigitYear then
  begin
    case GetDateOrder(FormatSettings.ShortDateFormat) of
      doMDY: Result := 'MM/DD/YYYY';
      doDMY: Result := 'DD/MM/YYYY';
      doYMD: Result := 'YYYY/MM/DD';
    end;
  end else
  begin
    case GetDateOrder(FormatSettings.ShortDateFormat) of
      doMDY: Result := 'MM/DD/YY';
      doDMY: Result := 'DD/MM/YY';
      doYMD: Result := 'YY/MM/DD';
    end;
  end;
end;

function DefTimeFormat: string;
begin
  Result := 'HH:NN:SS';
end;

function DoEncodeDate(Year, Month, Day: Integer; var Date: TDateTime): Boolean;
var
  I: Integer;
  Lp: Boolean;
begin
  Result := False;
  if not ((Year >= 1) and (Year <= 9999)) then Exit;
  Lp := IsLeapYear(Year);
  if (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= MonthDays[Lp, Month]) then
  begin
    for I := 1 to Month - 1 do Inc(Day, MonthDays[Lp, I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Result := True;
  end;
end;

function DoEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;
begin
  Result := False;
  if (Hour < 24) and (Min < 60) and (Sec < 60) and (MSec < 1000) then
  begin
    {$HINTS OFF}
    Time := (Hour * 3600000 + Min * 60000 + Sec * 1000 + MSec) / MSecsPerDay;
    {$HINTS ON}
    Result := True;
  end;
end;

procedure GetFormatElementAtPos(const Text: String; var Pos, Len: Integer; FourDigitYear: Boolean);
var FormatChar: Char;
  DateFormat: String;
  i, fp, l: Integer;
begin
  DateFormat := DefDateFormat(FourDigitYear);
  if Pos > Length(DateFormat) then Pos := Length(DateFormat);
  FormatChar := DateFormat[Pos];
  if (FormatChar = FormatSettings.DateSeparator) or (FormatChar = FormatSettings.TimeSeparator) then
  begin
    Inc(Pos);
    if Pos > Length(DateFormat) then Exit;
    FormatChar := DateFormat[Pos];
  end;
  if not CharInSetEh(NlsUpperCase(FormatChar)[1], ['D', 'M', 'Y', 'H', 'N', 'S']) then Exit;
  fp := 0;
  for i := 1 to Length(DateFormat) do
    if DateFormat[i] = FormatChar then
    begin
      fp := i;
      Break;
    end;
  l := Length(DateFormat) - fp + 1;
  for i := fp to Length(DateFormat) do
    if DateFormat[i] <> FormatChar then
    begin
      l := i - fp;
      Break;
    end;
  Pos := fp; Len := l;
end;

function IncrementStrDateAtPos(const Text, DateTimeMask: String; IsIncrease: Boolean; var Pos, Len: Integer): String;
var FormatChar: Char;
  DFormat: String;
  i, fp, l, n: Integer;
begin
  Result := Text;
  if Pos > Length(DateTimeMask) then Pos := Length(DateTimeMask);
  FormatChar := DateTimeMask[Pos];
  if (FormatChar = FormatSettings.DateSeparator) or ( FormatChar = FormatSettings.TimeSeparator) then
  begin
    Inc(Pos);
    if Pos > Length(DateTimeMask) then Exit;
    FormatChar := DateTimeMask[Pos];
  end;
  if not CharInSetEh(NlsUpperCase(FormatChar)[1], ['D', 'M', 'Y', 'H', 'N', 'S']) then Exit;
  fp := 0;
  for i := 1 to Length(DateTimeMask) do
    if DateTimeMask[i] = FormatChar then
    begin
      fp := i;
      Break;
    end;
  l := Length(DateTimeMask) - fp + 1;
  DFormat := '';
  for i := fp to Length(DateTimeMask) do
    if DateTimeMask[i] <> FormatChar then
    begin
      l := i - fp;
      Break;
    end else
      DFormat := DFormat + '0';
  n := StrToIntDef(Copy(Text, fp, l), 0);
  case NlsUpperCase(FormatChar)[1] of
    'D': if IsIncrease then if n >= 31 then n := 1 else Inc(n)
         else if n <= 1 then n := 31 else Dec(n);
    'M': if IsIncrease then if n >= 12 then n := 1 else Inc(n)
         else if n <= 1 then n := 12 else Dec(n);
    'Y': if IsIncrease then if n >= 9999 then n := 1 else Inc(n)
         else if n <= 1 then n := 9999 else Dec(n);
    'H': if IsIncrease then if n >= 23 then n := 0 else Inc(n)
         else if n <= 0 then n := 23 else Dec(n);
    'N', 'S': if IsIncrease then if n >= 59 then n := 0 else Inc(n)
              else if n <= 0 then n := 59 else Dec(n);
  end;
  DFormat := FormatFloat(DFormat, n);
  Pos := fp; Len := l;
  Result := Copy(Text, 1, fp - 1) + DFormat + Copy(Text, fp + l, 255);
end;

procedure ClearElementsMask(var ElementMask: TElementMaskPosEh);
begin
  ElementMask.Pos := -1;
  ElementMask.Length := -1;
  ElementMask.Present := False;
end;

procedure ClearDateTimeElementsMask(var DateTimeMaskPos: TDateTimeElementsMaskPosEh);
begin
  ClearElementsMask(DateTimeMaskPos.Year);
  ClearElementsMask(DateTimeMaskPos.Month);
  ClearElementsMask(DateTimeMaskPos.Day);
  ClearElementsMask(DateTimeMaskPos.Hour);
  ClearElementsMask(DateTimeMaskPos.Min);
  ClearElementsMask(DateTimeMaskPos.Sec);
end;

function EditFormatToEditMask(const EditFormatStr: String; var DateTimeMaskPos: TDateTimeElementsMaskPosEh): String;
var
  i, EmPos: Integer;
  CurElement, C: Char;
  CurElementLength: Integer;
  EscChar: Boolean;
  ADateTimeMaskPos: TDateTimeElementsMaskPosEh;

  procedure AddToMask(var ElementMask: TElementMaskPosEh);
  begin
    if ((CurElement = 'Y') and not (CurElementLength in [4,2])) or
       ((CurElement <> 'Y') and (CurElementLength <> 2))  then
      raise Exception.Create('Invalid datetime format element length: "' + CurElement +'"');
    if ElementMask.Present then
      raise Exception.Create('Duplicating datetime format element: "' + CurElement +'"');
    ElementMask.Pos := EmPos - CurElementLength;
    ElementMask.Length := CurElementLength;
    ElementMask.Present := True;
    Result := Result + DupeString('9', CurElementLength);
  end;

  procedure PromoteDateTimeChar(DateTimeChar: Char);
  begin
    if CurElement = DateTimeChar then
      Inc(CurElementLength)
    else
    begin
      if CurElement <> #0 then
      begin
        case CurElement of
          'Y': AddToMask(ADateTimeMaskPos.Year);
          'M': AddToMask(ADateTimeMaskPos.Month);
          'D': AddToMask(ADateTimeMaskPos.Day);
          'H': AddToMask(ADateTimeMaskPos.Hour);
          'N': AddToMask(ADateTimeMaskPos.Min);
          'S': AddToMask(ADateTimeMaskPos.Sec);
        end;
      end;
      CurElementLength := 1;
    end;
    if ((CurElement = 'Y') and (CurElementLength > 4)) or
       ((CurElement <> 'Y') and (CurElementLength > 2))
    then
      raise Exception.Create('Element in EditFormat: "' + EditFormatStr + '" is too long. Pos: ' + IntToStr(i));
    CurElement := DateTimeChar;
    EscChar := False;
  end;

begin
  Result := '';
  ClearDateTimeElementsMask(ADateTimeMaskPos);
  CurElement := #0;
  CurElementLength := 0;
  EmPos := 1;
  EscChar := False;
  for i := 1 to Length(EditFormatStr) do
  begin
    C := NlsUpperCase(EditFormatStr[i])[1];
    if CharInSetEh(C, ['D', 'M', 'Y', 'H', 'N', 'S']) and not EscChar then
      PromoteDateTimeChar(C)
    else
    begin
      if CurElement <> #0 then
        PromoteDateTimeChar(#0);
      if (NlsUpperCase(EditFormatStr[i])[1] = '\') and not EscChar then
      begin
        Dec(EmPos);
        EscChar := True;
      end
      else if CharInSetEh(C, ['!','>','<','L','l','A','a','C','c','0','9','#',';']) and
        ( (i = 1) or ((NlsUpperCase(EditFormatStr[i])[1] <> '\')) )
      then
      begin
        Result := Result + '\' + EditFormatStr[i];
        EscChar := False;
      end else
      begin
        Result := Result + EditFormatStr[i];
        EscChar := False;
      end;
      CurElement := #0;
    end;
    Inc(EmPos);
  end;
  if CurElement <> #0 then
    PromoteDateTimeChar(#0);
  Result := '!' + Result + ';1; ';
  DateTimeMaskPos := ADateTimeMaskPos;
end;

function StrToWordCheck(const Str: String): Integer;
var
  i, p: Integer;
  s: String;
begin
  s := '';
  Result := -1;
  p := Length(Str) + 1;
  for i := 1 to Length(Str) do
    if Str[i] <> ' ' then
    begin
      p := i;
      Break;
    end;

  if p = Length(Str) + 1 then
    Exit;

  for i := p to Length(Str) do
    if not CharInSetEh(Str[i], ['0','1','2','3','4','5','6','7','8','9',' ']) then
      Exit
    else if Str[i] <> ' ' then
      s := s + Str[i];

  try
    Result := StrToInt(s);
  except
    Result := -1;
  end;
end;

function DateTimeToDateTimeStamp(ADateTime: TDateTime): TDateTimeStampEh;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDateTime(ADateTime, Year, Month, Day, Hour, Min, Sec, MSec);
  Result.Year := Year;
  Result.Month := Month;
  Result.Day := Day;
  Result.Hour := Hour;
  Result.Minute := Min;
  Result.Second := Sec;
end;

function VarToDateTimeStamp(DateTimeVal: Variant): TDateTimeStampEh;
begin
  if VarIsNull(DateTimeVal)then
  begin
    Result.Year := -1;
    Result.Month := -1;
    Result.Day := -1;
    Result.Hour := -1;
    Result.Minute := -1;
    Result.Second := -1;
  end else
    Result := DateTimeToDateTimeStamp(DateTimeVal);
end;

function DateTimeStrToDate(const DateTimeStr: String;
  var DateTimeMaskPos: TDateTimeElementsMaskPosEh; var DateTimeStamp: TDateTimeStampEh): Boolean;
var
  ms: TDateTimeElementsMaskPosEh;
begin
  Result := True;
  ms := DateTimeMaskPos;
  if ms.Year.Present
    then DateTimeStamp.Year := StrToWordCheck(Copy(DateTimeStr, ms.Year.Pos, ms.Year.Length))
    else DateTimeStamp.Year := -1;
  if ms.Month.Present
    then DateTimeStamp.Month := StrToWordCheck(Copy(DateTimeStr, ms.Month.Pos, ms.Month.Length))
    else DateTimeStamp.Month := -1;
  if ms.Day.Present
    then DateTimeStamp.Day := StrToWordCheck(Copy(DateTimeStr, ms.Day.Pos, ms.Day.Length))
    else DateTimeStamp.Day := -1;
  if ms.Hour.Present
    then DateTimeStamp.Hour := StrToWordCheck(Copy(DateTimeStr, ms.Hour.Pos, ms.Hour.Length))
    else DateTimeStamp.Hour := -1;
  if ms.Min.Present
    then DateTimeStamp.Minute := StrToWordCheck(Copy(DateTimeStr, ms.Min.Pos, ms.Min.Length))
    else DateTimeStamp.Minute := -1;
  if ms.Sec.Present
    then DateTimeStamp.Second := StrToWordCheck(Copy(DateTimeStr, ms.Sec.Pos, ms.Sec.Length))
    else DateTimeStamp.Second := -1;

  if DateTimeStamp.Year > -1 then
    DateTimeStamp.Year := ExpandYear(DateTimeStamp.Year);

  if ms.Year.Present and (DateTimeStamp.Year = -1) then
    Result := False;
  if ms.Month.Present and (DateTimeStamp.Month = -1) then
    Result := False;
  if ms.Day.Present and (DateTimeStamp.Day = -1) then
    Result := False;
  if ms.Hour.Present and (DateTimeStamp.Hour = -1) then
    Result := False;
  if ms.Min.Present and (DateTimeStamp.Minute = -1) then
    Result := False;
  if ms.Sec.Present and (DateTimeStamp.Second = -1) then
    Result := False;
end;

function DoEncodeDateTime(Year, Month, Day, Hour, Min, Sec: Integer; var Date: TDateTime): Boolean; overload;
var
  ADate, ATime: TDateTime;
  MSec: Word;
begin
  MSec := 0;
  Result := False;
  if (Year < 0) and (Month < 0) and (Day < 0) then
    ADate := 0
  else if (Year <= 0) or (Month <= 0) or (Day <= 0)  then
    Exit
  else if not DoEncodeDate(Year, Month, Day, ADate) then
    Exit;

  if (Hour < 0 ) or (Min < 0) or (Sec < 0) then
    Exit
  else if DoEncodeTime(Hour, Min, Sec, MSec, ATime) then
  begin
    Date := ADate + ATime;
    Result := True;
  end;
end;

function DoEncodeDateTime(DateTimeStamp: TDateTimeStampEh; var Date: TDateTime): Boolean; overload;
var
  s: TDateTimeStampEh;
begin
  s := DateTimeStamp;
  Result := DoEncodeDateTime(s.Year, s.Month, s.Day, s.Hour, s.Minute, s.Second, Date);
end;

function EncodeDateTimeEh(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word): TDateTime;
begin
  if (AYear = 0) and (AMonth = 0) and (ADay = 0) then
    Result := EncodeTime(AHour, AMinute, ASecond, AMilliSecond)
  else
    Result := EncodeDateTime(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
end;

function DateTimeStampToVarValue(DateTimeStamp: TDateTimeStampEh;
  var DateTimeMaskPos: TDateTimeElementsMaskPosEh; var DateTimeVal: Variant;
  AutoCorrect, RaiseError: Boolean): Boolean;
var
  Y,M,D,H,N,S,MS: Word;
  ADateTime: TDateTime;
  dts: TDateTimeStampEh;
  dtmp: TDateTimeElementsMaskPosEh;
begin
  Y := 1900; M := 1; D := 1;
  H := 0; N := 0; S := 0; MS := 0;
  dts := DateTimeStamp;
  dtmp := DateTimeMaskPos;
  if (dts.Year = -1) and
     (dts.Month = -1) and
     (dts.Day = -1) and
     (dts.Hour = -1) and
     (dts.Minute = -1) and
     (dts.Second = -1)
  then
  begin
    Result := True;
    DateTimeVal := Null;
    Exit;
  end;

  if AutoCorrect then
  begin
    CorrectDate(dts.Year, dts.Month, dts.Day);
    CorrectTime(dts.Hour, dts.Minute, dts.Second);
  end else
  begin
    if not dtmp.Year.Present and (dts.Year <= 0) then
      dts.Year := 1;
    if not dtmp.Month.Present and (dts.Month <= 0) then
      dts.Month := 1;
    if not dtmp.Day.Present and (dts.Day <= 0) then
      dts.Day := 1;
    if not dtmp.Hour.Present and (dts.Hour < 0) then
      dts.Hour := 0;
    if not dtmp.Min.Present and (dts.Minute < 0) then
      dts.Minute := 0;
    if not dtmp.Sec.Present and (dts.Second < 0) then
      dts.Second := 0;
  end;

  if DoEncodeDateTime(dts, ADateTime) then
  begin
    if not VarIsNull(DateTimeVal) then
      DecodeDateTime(DateTimeVal, Y, M, D, H, N, S, MS);
    if dtmp.Year.Present and (dts.Year > 0) then
      Y := dts.Year;
    if dtmp.Month.Present and (dts.Month > 0) then
      M := dts.Month;
    if dtmp.Day.Present and (dts.Day > 0) then
      D := dts.Day;
    if dtmp.Hour.Present and (dts.Hour >= 0) then
      H := dts.Hour;
    if dtmp.Min.Present and (dts.Minute >= 0) then
      N := dts.Minute;
    if dtmp.Sec.Present and (dts.Second >= 0) then
      S := dts.Second;
    DateTimeVal := EncodeDateTimeEh(Y, M, D, H, N, S, MS);
    Result := True;
  end else
  begin
    Result := False;
    DateTimeVal := Null;
  end;
end;

function DateTimeEditFormatToDisplayFormat(const EditFormat: String): String;
var
  i: Integer;
  EscChar, InQuote: Boolean;
  C: Char;
begin
  Result := '';
  EscChar := False;
  InQuote := False;
  for i := 1 to Length(EditFormat) do
  begin
    C := NlsUpperCase(EditFormat[i])[1];
    if CharInSetEh(C, ['D', 'M', 'Y', 'H', 'N', 'S', '/', ':']) and not EscChar then
    begin
      if InQuote then
      begin
        Result := Result + '''';
        InQuote := False;
      end;
      Result := Result + C;
      EscChar := False;
    end else if (C = '\') and not EscChar then
      EscChar := True
    else
    begin
      if not InQuote then
      begin
        Result := Result + '''';
        InQuote := True;
      end;
      if C = '''' then
        Result := Result + ''''''
      else
        Result := Result + EditFormat[i];
      EscChar := False;
    end;
  end;
  if InQuote then
    Result := Result + '''';
end;

function RemoveNonFormatDateTimeText(const EditFormat: String): String;
var
  i: Integer;
  EscChar: Boolean;
  C: Char;
begin
  Result := '';
  EscChar := False;
  for i := 1 to Length(EditFormat) do
  begin
    C := NlsUpperCase(EditFormat[i])[1];
    if CharInSetEh(C, ['D', 'M', 'Y', 'H', 'N', 'S', '/', ':']) and not EscChar then
    begin
      Result := Result + C;
      EscChar := False;
    end else if (C = '\') and not EscChar then
      EscChar := True
    else
    begin
      Result := Result + ' ';
      EscChar := False;
    end;
  end;
end;

constructor TCustomDBDateTimeEditEh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  UpdateFourDigitYear;
  UpdateMask;
end;

destructor TCustomDBDateTimeEditEh.Destroy;
begin
  CloseUp(False);
  FreeAndNil(FDropDownCalendar);
  inherited Destroy;
end;


procedure TCustomDBDateTimeEditEh.Change;
begin
  if not FInternalTextSetting then
    UpdateValueFromText;
  inherited Change;
end;

procedure TCustomDBDateTimeEditEh.DropDownAction(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh; var Handled: Boolean);
begin
  EditButtonControl.AlwaysDown := True;
  inherited DropDownAction(EditButton, EditButtonControl, Handled);
  FIgnoreMouseScreenRect.TopLeft := EditButtonControl.ClientToScreen(Point(0,0));
  FIgnoreMouseScreenRect.Right := FIgnoreMouseScreenRect.Left + EditButtonControl.Width;
  FIgnoreMouseScreenRect.Bottom := FIgnoreMouseScreenRect.Top + EditButtonControl.Height;
  DropDown;
  if FCalendarVisible then
    SetEditButtonDroppedDown(EditButton, EditButtonControl);
  Handled := True;
end;

procedure TCustomDBDateTimeEditEh.EditButtonDownDefaultAction(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh; TopButton: Boolean;
  var AutoRepeat: Boolean; var Handled: Boolean);
begin
  if (EditButton.Style in [ebsUpDownEh, ebsAltUpDownEh]) then
  begin
    if IsMasked and not ReadOnly and FDataLink.Edit then
      IncrementItemAtCurPos(TopButton);
    Handled := True;
  end else
  begin
    if not FDroppedDown then
    begin
      DropDownAction(EditButton, EditButtonControl, Handled);
      FNoClickCloseUp := True;
    end;
  end;
end;

procedure TCustomDBDateTimeEditEh.EditButtonClickDefaultAction(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh; TopButton: Boolean; var Handled: Boolean);
begin
  
end;

procedure TCustomDBDateTimeEditEh.DropDown;
var
  P: TPoint;
  AAlignment: TDropDownAlign;
  ADateTime: TDateTime;
  TimeUnits: TCalendarDateTimeUnitsEh;
  PopupDateTimePickerItfs: IPopupDateTimePickerEh;
begin
  if not FCalendarVisible then
  begin
    if (FIgnoreNextDropDown) then
    begin
      FIgnoreNextDropDown := False;
      Exit;
    end;

    if Assigned(FOnDropDown) then FOnDropDown(Self);
    {$IFDEF FPC}
    if Value = Null
      then ADateTime := TDate(Date)
      else ADateTime := TDateTime(Value);
    {$ELSE}
    if Value = Null
      then ADateTime := TDate(Date)
      else ADateTime := VarToDateTime(Value);
    {$ENDIF}
    if inherited UseRightToLeftAlignment
      then AAlignment := daRight
      else AAlignment := daLeft;
    ParseDateTimeFormatForTimeUnits(EditFormat, TimeUnits);
    PopupDateTimePickerItfs := DropDownCalendar as IPopupDateTimePickerEh;
    DropDownCalendar.HandleNeeded; 
    PopupDateTimePickerItfs.SetTimeUnits(TimeUnits);
    PopupDateTimePickerItfs.SetFontOptions(Font, True);
    PopupDateTimePickerItfs.UpdateSize;
    P := AlignDropDownWindow(Self, DropDownCalendar, AAlignment);
    PopupDateTimePickerItfs.ShowPicker(ADateTime, P, CloseWinCallbackProc);
    FCalendarVisible := True;
    FIgnoreNextDropDown := False;
  end;
end;

procedure TCustomDBDateTimeEditEh.CloseWinCallbackProc(Control: TWinControl;
  Accept: Boolean);
var
  AMousePressed: Boolean;
  AMousePos: TPoint;
begin
  AMousePressed := IsMouseButtonPressedEh(mbLeft);
  if (AMousePressed) then
  begin
    AMousePos := SafeGetMouseCursorPos();
    if PtInRect(FIgnoreMouseScreenRect, AMousePos) then
      FIgnoreNextDropDown := True;
  end;

  CloseUp(Accept);
end;

procedure TCustomDBDateTimeEditEh.CloseUp(Accept: Boolean);
var
  DateTimeStamp: TDateTimeStampEh;
  ADate: TDateTime;
  AValue: Variant;
begin
  if FCalendarVisible then
  begin
    FCalendarVisible := False;
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if HandleAllocated and
       ((GetFocus = DropDownCalendar.Handle) or
        (GetParent(GetFocus) = DropDownCalendar.Handle))
    then
    begin
      SetFocus;
    end;

    (DropDownCalendar as IPopupDateTimePickerEh).HidePicker;

    FCalendarVisible := False;
    if not HandleAllocated then Exit;

    PostMessage(Handle, CM_IGNOREEDITDOWN, Integer(FButtonsBox.BtnCtlList[0].EditButtonControl.Tag), 0);
    if Accept and not ReadOnly and FDataLink.Edit {and (Kind = dtkDateEh)} then
    begin
      AValue := FValue;
      ADate := (DropDownCalendar as IPopupDateTimePickerEh).GetDateTime;
      DateTimeStamp := DateTimeToDateTimeStamp(ADate);
      DateTimeStampToVarValue(DateTimeStamp, FDateTimeMaskPos, AValue, True, False);
      InternalSetValue(AValue);
      if FFocused then SelectAll;
    end;
    SetEditButtonClosedUp;
    if Assigned(FOnCloseUp) then FOnCloseUp(Self, Accept);
  end;
end;

procedure TCustomDBDateTimeEditEh.ValidateEdit;
var S: String;
  V: Variant;
begin
  if FEditValidating then Exit;
  FEditValidating := True;
  try
    inherited ValidateEdit;
    if not IsMasked then Exit;
    S := Text;
    V := FValue;
    TextDateTimeToVarValue(S, V, True, False);
    FValue := V;
    if V = Null
      then InternalSetControlText('')
      else InternalSetControlText(FormatDateTime(DateTimeFormat, V));
  finally
    FEditValidating := False;
  end;
end;

function TCustomDBDateTimeEditEh.GetVariantValue: Variant;
begin
  Result := FValue;
end;

function TCustomDBDateTimeEditEh.GetDropDownCalendar: TWinControl;
begin
  if (FDropDownCalendar <> nil) and
     (FDropDownCalendar.ClassType <> EhLibManager.PopupDateTimePickerClass)
  then
    FreeAndNil(FDropDownCalendar);

  if FDropDownCalendar = nil then
    FDropDownCalendar := EhLibManager.PopupDateTimePickerClass.Create(Self);
  Result := FDropDownCalendar;
end;

procedure TCustomDBDateTimeEditEh.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if IsMasked then
    if (Key in [VK_DOWN, VK_UP]) and (Shift = []) and not ReadOnly then
    begin
      if Assigned(OnKeyDown) then OnKeyDown(Self, Key, Shift);
      if Key = 0 then Exit;
      CheckInplaceEditHolderKeyDown(Key, Shift);
      if Key = 0 then Exit;
      if FDataLink.Edit then IncrementItemAtCurPos(Key = VK_UP);
    end else if (Key in [VK_LEFT, VK_RIGHT]) and (Shift = []) and (SelLength > 1) then
    begin
      if Assigned(OnKeyDown) then OnKeyDown(Self, Key, Shift);
      if Key = 0 then Exit;
      CheckInplaceEditHolderKeyDown(Key, Shift);
      if Key = 0 then Exit;
      if Key = VK_LEFT then SetCursor(SelStart)
      else SetCursor(SelStart + SelLength - 1);
    end;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomDBDateTimeEditEh.KeyPress(var Key: Char);
var SStart, SLen, NewPos: Integer;
begin
  if FCalendarVisible and CharInSetEh(Key, [#13, #27]) then
  begin
    CloseUp(Key = #13);
    Key := #0;
  end;
  inherited KeyPress(Key);
  if IsMasked and ((Key = FormatSettings.DateSeparator) or (Key = FormatSettings.TimeSeparator)) then
  begin
    SStart := SelStart + 1;
    NewPos := Pos(Key, Copy(Text, SStart, 255));
    if NewPos = 0 then NewPos := 1
    else Inc(NewPos, SStart + 1);
    GetFormatElementAtPos(Text, NewPos, SLen, FFourDigitYear);
    SetSel(NewPos - 1, NewPos + SLen - 1);
    Key := #0;
  end;
end;

procedure TCustomDBDateTimeEditEh.UpdateFourDigitYear;
var AFourDigitYear: Boolean;
begin
  AFourDigitYear := (Pos('YYYY', NlsUpperCase(FormatSettings.ShortDateFormat)) > 0) or
    (Pos('YYY', NlsUpperCase(FormatSettings.ShortDateFormat)) > 0);
  if AFourDigitYear <> FFourDigitYear then
  begin
    FFourDigitYear := AFourDigitYear;
    UpdateMask;
  end;
end;

{$IFDEF FPC}
{$ELSE}
procedure TCustomDBDateTimeEditEh.CMCancelMode(var Message: TCMCancelMode);
  function CheckActiveListChilds: Boolean;
  var i: Integer;
  begin
    Result := False;
    if FDropDownCalendar <> nil then
      for i := 0 to DropDownCalendar.ControlCount - 1 do
        if DropDownCalendar.Controls[I] = Message.Sender then
        begin
          Result := True;
          Exit;
        end;
  end;
begin
  inherited;
  if (Message.Sender <> Self) and (Message.Sender <> FDropDownCalendar) and
    not ContainsControl(Message.Sender) and not CheckActiveListChilds then
    CloseUp(False);
end;
{$ENDIF}

procedure TCustomDBDateTimeEditEh.CMEnter(var Message: TCMEnter);
begin
  UpdateFourDigitYear;
  inherited;
end;

procedure TCustomDBDateTimeEditEh.WMCancelMode(var Message: TMessage);
begin
  inherited;
end;

procedure TCustomDBDateTimeEditEh.WMKillFocus(var Message: TWMKillFocus);
begin
  if FCalendarVisible and not
    ((Message.FocusedWnd = DropDownCalendar.Handle) or
    (GetParent(Message.FocusedWnd) = DropDownCalendar.Handle)
    ) then
    CloseUp(False);
  inherited;
end;

procedure TCustomDBDateTimeEditEh.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
end;

procedure TCustomDBDateTimeEditEh.WMPaste(var Message: TWMPaste);
begin
  if (GetSelStart = 0) and
     (GetSelLength = 1) and
     ( Clipboard.HasFormat(CF_TEXT)
     {$IFDEF FPC_CROSSP}
     {$ELSE}
        or Clipboard.HasFormat(CF_OEMTEXT)
        or Clipboard.HasFormat(CF_UNICODETEXT)
     {$ENDIF} 
     )
  then
    SelectAll;
  inherited;
end;

procedure TCustomDBDateTimeEditEh.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  if (Message.CharCode in [VK_RETURN, VK_ESCAPE]) and FCalendarVisible then
  begin
    Message.Result := 1;
  end else
    inherited;
end;

procedure WMKeyMessageToKeyState(var Message: TWMKey; out Key: Word;
  out ShiftState: TShiftState);
begin
  ShiftState := KeyDataToShiftState(Message.KeyData);
  Key := Message.CharCode;
end;

procedure TCustomDBDateTimeEditEh.WndProc(var Message: TMessage);
begin
  if FCalendarVisible then
  begin
    case Message.Msg of
      wm_KeyDown, {wm_SysKeyDown, }wm_Char:
{$IFDEF CIL}
{$ELSE}
{$ENDIF}
        begin
          SendMessage(DropDownCalendar.Handle, Message.Msg, Message.WParam, Message.LParam);
          Exit;
        end;
    end;
  end;
  inherited WndProc(Message);
end;

procedure TCustomDBDateTimeEditEh.DataChanged;
var
  AValue: Variant;
  Handled: Boolean;
begin
  if FDataLink.Field <> nil then
  begin

    Handled := False;
    if Assigned(OnGetFieldData) then
    begin
      AValue := Unassigned;
      OnGetFieldData(Self, AValue, Handled);
    end;

    if not Handled then
    begin
      if FAlignment <> FDataLink.Field.Alignment then Invalidate;
      InternalSetValue(FDataLink.Field.Value);
    end else
      InternalSetValue(AValue);

  end
  else if DataIndepended then
  begin
    InternalSetValue(FDataLink.DataIndependentValue);
  end else
  begin
    InternalSetValue(Null);
  end;
  Modified := False;
end;

procedure TCustomDBDateTimeEditEh.InternalSetControlText(const AText: String);
begin
  if FInternalTextSetting then Exit;
  FInternalTextSetting := True;
  try
    inherited InternalSetText(AText);
  finally
    FInternalTextSetting := False;
  end;
end;

procedure TCustomDBDateTimeEditEh.InternalSetText(const AText: String);
var
  AValue: Variant;
begin
  AValue := FValue;
  if IsMasked then
  begin
    if not TextDateTimeToVarValue(AText, AValue, False, True) then
      raise Exception.Create('Invalid datetime: "' + AText +'"');
  end else
    AValue := StrToDateTime(AText);
  FValue := AValue;
  InternalSetControlText(AText);
end;

procedure TCustomDBDateTimeEditEh.InternalSetValue(AValue: Variant);
begin
  if AValue = Null then
  begin
    InternalSetControlText('');
    FValue := Null;
  end else
  begin
    FValue := VarAsType(AValue, varDate);
    if IsMasked
      then InternalSetControlText(FormatDateTime(DateTimeFormat, FValue))
      else InternalSetControlText(DateTimeToStr(FValue));
  end;
end;

procedure TCustomDBDateTimeEditEh.IncrementItemAtCurPos(IsIncrease: Boolean);
var
  SStart, SLen: Integer;
  CleanFormat: String;
begin
  SStart := SelStart + 1;
  SLen := SelLength;
  CleanFormat := RemoveNonFormatDateTimeText(EditFormat);
  inherited InternalSetText(IncrementStrDateAtPos(Text, CleanFormat, IsIncrease, SStart, SLen));
  SetCursor(SStart - 1);
  SelLength := SLen;
end;

procedure TCustomDBDateTimeEditEh.UpdateValueFromText;
var
  s: String;
  DateTimeVal: TDateTime;
begin
  s := Text;
  try
    if IsMasked then
    begin
      TextDateTimeToVarValue(S, FValue, False, False);
    end else if TryStrToDateTime(S, DateTimeVal) then
      FValue := DateTimeVal
    else
      FValue := Null
  except
    on EConvertError do FValue := Null;
  end;
  UpdateImageIndex;
end;

procedure TCustomDBDateTimeEditEh.UpdateMask;
begin
  if Kind = dtkDateEh then
    FEditFormat := DefDateFormat(FFourDigitYear)
  else if Kind = dtkTimeEh then
    FEditFormat := DefTimeFormat
  else if Kind = dtkDateTimeEh then
    FEditFormat := DefDateFormat(FFourDigitYear) + ' ' + DefTimeFormat;

  if FEditFormat <> '' then
  begin
    FDateTimeFormat := DateTimeEditFormatToDisplayFormat(FEditFormat);
    SetControlEditMask(EditFormatToEditMask(EditFormat, FDateTimeMaskPos));
  end else
  begin
    FDateTimeFormat := '';
    SetControlEditMask('');
  end;
end;

function TCustomDBDateTimeEditEh.DateTimeFormat: String;
begin
  Result := FDateTimeFormat;
end;

procedure TCustomDBDateTimeEditEh.InternalUpdatePostData;
var
  v, fv: Variant;
  DateTimeStamp: TDateTimeStampEh;
begin
  v := GetVariantValue;
  if (FDataLink.Field <> nil)
    then fv := FDataLink.Field.Value
    else fv := FDataLink.DataIndependentValue;

  if IsMasked then
  begin
    DateTimeStamp := VarToDateTimeStamp(v);
    DateTimeStampToVarValue(DateTimeStamp, FDateTimeMaskPos, fv, True, False);
  end else
    fv := StrToDateTime(Text);

  FDataLink.SetValue(fv);
end;

function TCustomDBDateTimeEditEh.CreateEditButton: TEditButtonEh;
begin
  Result := TVisibleEditButtonEh.Create(Self);
end;

procedure TCustomDBDateTimeEditEh.EditButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FCalendarVisible and (GetCaptureControl = Sender) and
    (Sender = FButtonsBox.BtnCtlList[0].EditButtonControl) then
  begin
    ListPos := DropDownCalendar.ScreenToClient(TControl(Sender).ClientToScreen(Point(X, Y)));
    if PtInRect(DropDownCalendar.ClientRect, ListPos) then
    begin
      TControl(Sender).Perform(WM_CANCELMODE, 0, 0);
      MousePos := PointToSmallPoint(ListPos);
      MousePos.y := 0; 
      SendMessage(DropDownCalendar.Handle, WM_LBUTTONDOWN, 0, SmallPointToInteger(MousePos));
    end;
  end;
end;

function TCustomDBDateTimeEditEh.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result and (Shift = []) and not ReadOnly and IsMasked and FDataLink.Edit then
  begin
    IncrementItemAtCurPos(False);
    Result := True;
  end;
end;

function TCustomDBDateTimeEditEh.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result and (Shift = []) and not ReadOnly and IsMasked and FDataLink.Edit then
  begin
    IncrementItemAtCurPos(True);
    Result := True;
  end;
end;

procedure TCustomDBDateTimeEditEh.CMMouseWheel(var Message: TMessage);
begin
  if FCalendarVisible then
  begin
{$IFDEF CIL}
{$ELSE}
{$ENDIF}
    Message.Result := FDropDownCalendar.Perform(CM_MOUSEWHEEL, Message.WParam, Message.LParam);
    if Message.Result <> 0 then
      Exit;
  end;
  inherited;
end;

function TCustomDBDateTimeEditEh.GetDisplayTextForPaintCopy: String;
begin
  if (csDesigning in ComponentState) and not (FDataLink.Active) then
    Result := Name
  else if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
  begin
    if not FDataLink.Field.IsNull then
      if IsMasked
        then Result := FormatDateTime(DateTimeFormat, FDataLink.Field.AsDateTime)
        else Result := DateTimeToStr(FDataLink.Field.AsDateTime)
    else
      Result := '';
  end else
    Result := EditText;
end;

procedure TCustomDBDateTimeEditEh.FilterMRUItem(const AText: String; var Accept: Boolean);
var
  Bot: String;
  i, p: Integer;
begin
  p := Length(Text);
  for i := Length(Text) downto 1 do
    if (Text[i] = ' ') or (Text[i] = FormatSettings.DateSeparator)
      then Dec(p)
      else Break;
  Bot := Copy(Text, 1, p);
  Accept := (NlsCompareText(Copy(AText, 1, Length(Bot)), Bot) = 0);
end;

function TCustomDBDateTimeEditEh.IsEditFormatStored: Boolean;
begin
  Result := (Kind = dtkCustomEh);
end;

function TCustomDBDateTimeEditEh.IsKindStored: Boolean;
begin
  Result := (Kind <> dtkCustomEh);
end;

procedure TCustomDBDateTimeEditEh.SetEditFormat(const Value: String);
begin
  FKind := dtkCustomEh;
  if Value <> FEditFormat then
  begin
    FEditFormat := Value;
    UpdateMask;
    DataChange(nil);
  end;
end;

procedure TCustomDBDateTimeEditEh.SetKind(const Value: TDateTimeKindEh);
begin
  if Value <> FKind then
  begin
    FKind := Value;
    UpdateMask;
    DataChange(nil);
  end;
end;

procedure TCustomDBDateTimeEditEh.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('EditFormat', ReadEditFormat, WriteEditFormat, IsEditFormatStored);
end;

procedure TCustomDBDateTimeEditEh.ReadEditFormat(Reader: TReader);
begin
  EditFormat := Reader.ReadString;
end;

procedure TCustomDBDateTimeEditEh.WriteEditFormat(Writer: TWriter);
begin
  Writer.WriteString(EditFormat);
end;

function TCustomDBDateTimeEditEh.IsEmpty: Boolean;
begin
  Result := (FormatMaskText(EditMask, '') = Text);
end;

function TCustomDBDateTimeEditEh.TextDateTimeToVarValue(const AText: string; var AValue: Variant; const AAutoCorrect,
  ARaiseError: Boolean): Boolean;
var
  DateTimeStamp: TDateTimeStampEh;
begin
  DateTimeStrToDate(AText, FDateTimeMaskPos, DateTimeStamp);
  Result := DateTimeStampToVarValue(DateTimeStamp, FDateTimeMaskPos, AValue, AAutoCorrect, ARaiseError);
end;

{ TDropDownBoxEh }

procedure TDropDownBoxEh.Assign(Source: TPersistent);
begin
  if Source is TDropDownBoxEh then
  begin
    Align := TDropDownBoxEh(Source).Align;
    Rows := TDropDownBoxEh(Source).Rows;
    Width := TDropDownBoxEh(Source).Width;
    Sizable := TDropDownBoxEh(Source).Sizable;
  end else
    inherited Assign(Source);
end;

{ TCustomDBComboBoxEh }

constructor TCustomDBComboBoxEh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChanged;
  FKeyItems := TStringListEh.Create;
  TStringListEh(FKeyItems).CaseSensitive := True;
  TStringList(FKeyItems).OnChange := KeyItemsChanged;
  FVarValue := Null;
  FDropDownBox := CreateDropDownBox;
  FDropDownBox.Rows := 7;
  FItemIndex := -1;
  FCaseInsensitiveTextSearch := True;
  TStringList(FItems).CaseSensitive := not FCaseInsensitiveTextSearch;
end;

destructor TCustomDBComboBoxEh.Destroy;
begin
  Destroying;
  DataSource := nil;
  FreeAndNil(FDropDownBox);
  FreeAndNil(FKeyItems);
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TCustomDBComboBoxEh.CreateEditButton: TEditButtonEh;
begin
  Result := TVisibleEditButtonEh.Create(Self);
end;

function TCustomDBComboBoxEh.ConvertDataText(const Value: String): String;
var Index: Integer;
begin
  if TextListIndepended then
    Result := Value
  else
  begin
    if FKeyBased
      then Index := KeyItems.IndexOf(Value)
      else Index := Items.IndexOf(Value);
    if (Index >= 0) and (Index < Items.Count)
      then Result := Items.Strings[Index]
      else Result := '';
  end;
end;

function TCustomDBComboBoxEh.CreateDropDownBox: TDropDownBoxEh;
begin
  Result := TDropDownBoxEh.Create;
  Result.Rows := 7;
end;

function TCustomDBComboBoxEh.DefaultAlignment: TAlignment;
begin
  if FKeyBased
    then Result := taLeftJustify
    else Result := inherited DefaultAlignment;
end;

function TCustomDBComboBoxEh.GetVariantValue: Variant;
begin
  if FKeyBased
    then Result := FVarValue
    else Result := inherited GetVariantValue;
end;

function TCustomDBComboBoxEh.IsValidChar(InputChar: Char): Boolean;
begin
  if FKeyBased
    then Result := True
    else Result := inherited IsValidChar(InputChar);
end;

function TCustomDBComboBoxEh.LocateStr(const Str: String; PartialKey: Boolean): Boolean;

  function LocateItem: Integer;

    function Compare(const S1, S2: String): Integer;
    begin
      if FCaseInsensitiveTextSearch
        then Result := NlsCompareText(S1, S2)
        else Result := NlsCompareStr(S1, S2);
    end;

  var
    i: Integer;
    Str_Len : Integer;
    s: string;
  begin
    Result := -1;
    Str_Len := Length(Str);
    for i := 0 to FItemsCount - 1 do
    begin
      s := Items[i];

      if PartialKey then
        Delete(s, Str_Len + 1, MaxInt);

      if Compare(s, Str) = 0 then
      begin
        Result := i;
        Break;
      end;
    end;
  end;

var
  Index: Integer;
  OldIndex: Integer;
  LocOpt: TLocateOptions;
begin
  Result := False;
  OldIndex := ItemIndex;
  if not EditCanModify then Exit;
  try
    if PartialKey
      then LocOpt := [loPartialKey]
      else LocOpt := [];

    Index := StringsLocate(Items, Str, LocOpt);
    if (Index < 0) and CaseInsensitiveTextSearch then
    begin
      LocOpt := LocOpt + [loCaseInsensitive];
      Index := StringsLocate(Items, Str, LocOpt);
    end;
    
    if Index >= 0 then
    begin
      InternalSetItemIndex(Index);
      EditText := Items.Strings[Index];
      SelStart := Length(Text);
      SelLength := Length(Str) - SelStart;
    end
    else if not LimitTextToListValues then
      UpdateItemIndex; 
    if OldIndex <> Index then Result := True;
  except
    { If you attempt to search for a string larger than what the field
      can hold, and exception will be raised.  Just trap it and
      reset the SearchText back to the old value. }
    InternalSetItemIndex(OldIndex);
  end;
end;

function TCustomDBComboBoxEh.TextListIndepended: Boolean;
begin
  Result := not (FKeyBased);
end;

procedure TCustomDBComboBoxEh.DataChanged;
var
  AValue: Variant;
  Handled: Boolean;
begin
  if csDestroying in ComponentState then Exit;

  if FDataLink.Field <> nil then
  begin

    Handled := False;
    if Assigned(OnGetFieldData) then
    begin
      AValue := Unassigned;
      OnGetFieldData(Self, AValue, Handled);
    end;

    if not Handled then
    begin
      if (FFocused and FDataLink.CanModify) or FKeyBased
        then AValue := FDataLink.Field.Text
        else AValue := FDataLink.Field.DisplayText;

      if FAlignment <> FDataLink.Field.Alignment then Invalidate;
      if not (evEditMaskEh in FAssignedValues) then
        SetControlEditMask(FDataLink.Field.EditMask);
      if not (csDesigning in ComponentState) then
      begin
        if (FDataLink.Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) and not FKeyBased then
          MaxLength := FDataLink.Field.Size
      end;
    end;

    if (FFocused and FDataLink.CanModify) or FKeyBased
      then InternalSetValue(VarToStr(AValue))
      else EditText := VarToStr(AValue);
  end
  else if DataIndepended then
  begin
    if not (evEditMaskEh in FAssignedValues) then
      SetControlEditMask('');
    if FKeyBased
      then InternalSetValue(FDataLink.DataIndependentValue)
      else EditText := VarToStr(FDataLink.DataIndependentValue);
  end else
  begin
    if not (evEditMaskEh in FAssignedValues) then
      SetControlEditMask('');
    EditText := '';
  end;
  Modified := False;
end;

procedure TCustomDBComboBoxEh.EditButtonDownDefaultAction(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh; TopButton: Boolean;
  var AutoRepeat: Boolean; var Handled: Boolean);
begin
  if (EditButton.Style in [ebsUpDownEh, ebsAltUpDownEh]) then
  begin
    if not ReadOnly and FDataLink.Edit then
      SelectNextValue(TopButton);
    Handled := True;
  end else
  begin
    if not FDroppedDown then
    begin
      DropDownAction(EditButton, EditButtonControl, Handled);
      FNoClickCloseUp := True;
    end;
  end;
end;

procedure TCustomDBComboBoxEh.EditButtonClickDefaultAction(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh; TopButton: Boolean; var Handled: Boolean);
begin
  
end;

procedure TCustomDBComboBoxEh.DropDownAction(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh; var Handled: Boolean);
begin
  EditButtonControl.AlwaysDown := True;
  inherited DropDownAction(EditButton, EditButtonControl, Handled);
  DropDown(EditButton);
  Handled := True;
end;

procedure TCustomDBComboBoxEh.DropDown(AEditButton: TEditButtonEh = nil);

  function GetItemsMaxWidth: Integer;
  var
    i, w: Integer;
  begin
    Result := 0;
    if FCanvas = nil then
    begin
      FCanvas := TControlCanvas.Create;
      FCanvas.Control := Self;
    end;
    FCanvas.Handle := GetDC(0);
    FCanvas.Font := Font;
    for i := 0 to FItemsCount - 1 do
    begin
      w := FCanvas.TextWidth(Items[i]);
      if w > Result then Result := w;
    end;
    ReleaseDC(0, FCanvas.Handle);
    FCanvas.Handle := 0;
    Inc(Result, 5);
    if Images <> nil then Inc(Result, Images.Width + 4);
  end;

var
  P: TPoint;
  ADropDownAlign: TDropDownAlign;
  AEditButtonControl: TEditButtonControlEh;
  ppls: TComboBoxPopupListboxEh;
begin
  if not FListVisible then
  begin
    if AEditButton = nil then
      AEditButton := GetFirstDefaultActionEditButton;
    if AEditButton <> nil then
    begin
      AEditButtonControl := GetEditButtonControlByEditButton(AEditButton);
      SetEditButtonDroppedDown(AEditButton, AEditButtonControl);
    end;
    if Assigned(FOnDropDown) then FOnDropDown(Self);
    ppls := TComboBoxPopupListboxEh(PopupListbox);
    ppls.HandleNeeded;
    GetItemsList;
    ppls.Color := GetPopupListboxColor;
    ppls.Font := SelfPopupListboxFont;
    ppls.ImageList := Self.Images;
    ppls.ItemHeight := ppls.GetTextHeight;
    if (Images <> nil) and (EditImage.UseImageHeight) and (ppls.ItemHeight < Images.Height + 1) then
      ppls.ItemHeight := Images.Height;
//{$IFDEF FPC}
//    ppls.Items := Self.Items;
//{$ELSE}
    ppls.ExtItems := Self.Items;
    ppls.Count := ppls.ExtItems.Count;
//{$ENDIF}
    if Self.ItemIndex < ppls.Count then
      ppls.ItemIndex := Self.ItemIndex;
    ppls.RowCount := DropDownBox.Rows;
    if (FDropDownBox.Width = -1) then
      ppls.ClientWidth := GetItemsMaxWidth
    else if FDropDownBox.Width > 0 then
      ppls.Width := FDropDownBox.Width
    else
      ppls.Width := Self.Width;
    if (ppls.Width < Self.Width) then ppls.Width := Self.Width;
    if ppls.Count < ppls.RowCount then ppls.RowCount := ppls.Count;
    ppls.SizeGripAlwaysShow := Self.DropDownBox.Sizable;
    ADropDownAlign := FDropDownBox.Align;
    if inherited UseRightToLeftAlignment then
      if ADropDownAlign = daLeft then
        ADropDownAlign := daRight
      else if ADropDownAlign = daRight then
        ADropDownAlign := daLeft;
    P := AlignDropDownWindow(Self, PopupListbox, ADropDownAlign);
    PopupListbox.SetBounds(P.X, P.Y, PopupListbox.Width, PopupListbox.Height);

    ppls.MasterFocusControl := Self;

    if (PopupListbox is TPopupListboxFormEh) then
    begin
      TPopupListboxFormEh(PopupListbox).Show;
    end else
    begin
      SetWindowPos(PopupListbox.Handle, HWND_TOP {MOST}, P.X, P.Y, 0, 0,
        SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
      PopupListbox.Visible := True; 
    end;

    FListVisible := True;
    TComboBoxPopupListboxEh(PopupListbox).SizeGripResized := False;
    FDroppedDown := True;
  end;
end;

procedure TCustomDBComboBoxEh.CloseUp(Accept: Boolean);
begin
  if FListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    SetWindowPos(PopupListbox.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    PopupListbox.Visible := False;
    if TComboBoxPopupListboxEh(PopupListbox).SizeGripResized then
    begin
      DropDownBox.Rows := TComboBoxPopupListboxEh(PopupListbox).RowCount;
      DropDownBox.Width := TComboBoxPopupListboxEh(PopupListbox).Width;
    end;
    if (GetFocus = PopupListbox.Handle) then
      SetFocus;
    FListVisible := False;
    SetEditButtonClosedUp;
    inherited CloseUp(Accept);
    if Assigned(FOnClosingUp) then
      FOnClosingUp(Self, Accept);
    if Accept and not ReadOnly and FDataLink.Edit then
    begin
      InternalSetItemIndex(TComboBoxPopupListboxEh(PopupListbox).ItemIndex);
      if FFocused then SelectAll;
    end;
    if Assigned(FOnCloseUp) then
      FOnCloseUp(Self, Accept);
  end;
end;

procedure TCustomDBComboBoxEh.UpdateControlReadOnly;
begin
  if LimitTextToListValues then
    SetControlReadOnly(True)
  else
    inherited UpdateControlReadOnly;
end;

function TCustomDBComboBoxEh.GetPopupListbox: TWinControl;
begin
  if FPopupListbox = nil then
  begin
    if FPopupListboxClass <> nil
      then FPopupListbox := FPopupListboxClass.Create(Self)
      else FPopupListbox := TComboBoxPopupListboxEh.Create(Self);
    FPopupListbox.Visible := False;
    {$IFDEF FPC}
    {$ELSE}
    TComboBoxPopupListboxEh(FPopupListbox).Ctl3D := True;
    {$ENDIF}
    {$IFDEF FPC}
    {$ELSE}
    if not (FPopupListbox is TCustomForm) then
      FPopupListbox.Parent := Self; 
    {$ENDIF}
    if FPopupListbox.HandleAllocated then
      ShowWindow(FPopupListbox.Handle, SW_HIDE); 
    TComboBoxPopupListboxEh(FPopupListbox).OnMouseUp := ListMouseUp;
    TComboBoxPopupListboxEh(FPopupListbox).OnGetImageIndex := PopupListboxGetImageIndex;
  end;
  Result := FPopupListbox;
end;

procedure TCustomDBComboBoxEh.PopupListboxGetImageIndex(Sender: TObject; ItemIndex: Integer; var ImageIndex: Integer);
begin
  if Assigned(OnGetItemImageIndex) then
    OnGetItemImageIndex(Self, ItemIndex, ImageIndex);
end;

procedure TCustomDBComboBoxEh.ListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if TComboBoxPopupListboxEh(FPopupListbox).IsMouseUpCloseListbox then
      CloseUp(PtInRect(FPopupListbox.ClientRect, Point(X, Y)));
  end;
end;

procedure TCustomDBComboBoxEh.InternalSetItemIndex(const Value: Integer);
begin
  if FItemIndex <> Value then
  begin
    GetItemsList;
    if (Value >= 0) and (Value < FItemsCount) then
    begin
      FItemIndex := Value;
      if FKeyBased then
        FVarValue := KeyItems.Strings[FItemIndex];
    end else
    begin
      FItemIndex := -1;
      FVarValue := Null;
    end;
    UpdateImageIndex;
    if FListVisible then TComboBoxPopupListboxEh(PopupListbox).ItemIndex := FItemIndex;
    if FItemIndex >= 0 then
      EditText := Items.Strings[FItemIndex]
    else inherited InternalSetText('');
  end;
end;

procedure TCustomDBComboBoxEh.InternalSetText(const AText: String);
var Index: Integer;
begin
  if FKeyBased then
  begin
    Index := Items.IndexOf(AText);
    if (Index >= 0) and (Index < FItemsCount) then
    begin
      FItemIndex := Index;
      UpdateImageIndex;
      if FListVisible then TComboBoxPopupListboxEh(PopupListbox).ItemIndex := FItemIndex;
      FVarValue := KeyItems.Strings[Index];
      inherited InternalSetText(AText);
    end
  end else
  begin
    inherited InternalSetText(AText);
    UpdateItemIndex;
  end;
end;

procedure TCustomDBComboBoxEh.InternalSetValue(AValue: Variant);
begin
  if FKeyBased then
  begin
    FVarValue := AValue;
    if FVarValue = Null then
    begin
      inherited InternalSetText('');
      FItemIndex := -1;
    end else
    begin
      FItemIndex := KeyItems.IndexOf(VarToStr(AValue));
      if (FItemIndex >= 0) and (FItemIndex < FItemsCount)
        then inherited InternalSetText(Items.Strings[FItemIndex])
        else inherited InternalSetText('');
    end;
    UpdateImageIndex;
    if FListVisible then TComboBoxPopupListboxEh(PopupListbox).ItemIndex := FItemIndex;
  end else
  begin
    inherited InternalSetValue(AValue);
    UpdateItemIndex;
  end;
end;

procedure TCustomDBComboBoxEh.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ((Key = VK_UP) or (Key = VK_DOWN)) and (not WordWrap or (SelLength = Length(Text))) then
    if not ReadOnly and EditCanModify and not FListVisible then
    begin
      SelectNextValue(Key = VK_UP);
      Key := 0;
    end;
  if (Key = VK_DELETE) and
     LimitTextToListValues and
     ((Assigned(Field) and not Field.Required) or DataIndepended) and
     not ReadOnly and
     EditCanModify
  then
    InternalSetValue(Null);
  if (Shift = [ssCtrl]) and (Key = Word('V')) and LimitTextToListValues and not ReadOnly then
    SendMessage(Handle, WM_PASTE, 0, 0);
end;

procedure TCustomDBComboBoxEh.KeyPress(var Key: Char);
begin
  if FListVisible and CharInSetEh(Key, [#13, #27]) then
  begin
    CloseUp(Key = #13);
    Key := #0;
  end;
  inherited KeyPress(Key);
  case Key of
    #8: 
      if LimitTextToListValues and not ReadOnly then
      begin
        ProcessSearchStr(Key);
        Key := #0;
      end;
    {#13: 
    begin
      Key := #0;
      FDataLink.UpdateRecord;
      SelectAll;
    end;}
    #32..High(Char):
      begin
        if DropDownBox.AutoDrop and not FListVisible then DropDown;
        if LimitTextToListValues and not ReadOnly then
        begin
          ProcessSearchStr(GetCompleteKeyPress);
          Key := #0;
        end;
      end;
  end;
end;

procedure TCustomDBComboBoxEh.EditButtonClick(Sender: TObject);
begin
  inherited EditButtonClick(Sender);
end;

procedure TCustomDBComboBoxEh.EditButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Sender = FButtonsBox.BtnCtlList[0].EditButtonControl then
    TraceMouseMoveForPopupListbox(Sender, Shift, X, Y);
end;

procedure TCustomDBComboBoxEh.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and LimitTextToListValues and not PtInRect(ButtonRect, Point(X, Y)) and
    ButtonEnabled and not FDroppedDown and not (ssDouble in Shift) then
  begin
    if not FFocused then SetFocus;
    FNoClickCloseUp := True;
    DropDown;
  end;
end;

procedure TCustomDBComboBoxEh.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if TraceMouseMoveForPopupListbox(Self, Shift, X, Y) then
    Exit;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomDBComboBoxEh.Click;
begin
  inherited Click;
  if LimitTextToListValues and ButtonEnabled and FDroppedDown and not FNoClickCloseUp then
    CloseUp(False);
  FNoClickCloseUp := False;
end;

function TCustomDBComboBoxEh.TraceMouseMoveForPopupListbox(Sender: TObject;
  Shift: TShiftState; X, Y: Integer): Boolean;
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  Result := False;
  if FListVisible and (GetCaptureControl = Sender) then
  begin
    ListPos := PopupListbox.ScreenToClient(TControl(Sender).ClientToScreen(Point(X, Y)));
    if PtInRect(PopupListbox.ClientRect, ListPos) then
    begin
      TControl(Sender).Perform(WM_CANCELMODE, 0, 0);
      MousePos := PointToSmallPoint(ListPos);
      SendMessage(PopupListbox.Handle, WM_LBUTTONDOWN, 0, SmallPointToInteger(MousePos));
      Result := True;
    end;
  end;
end;

function TCustomDBComboBoxEh.ProcessSearchStr(const Str: String): Boolean;
var
  S, SearchText: string;
  OldSelLenght: Integer;
begin
  Result := False;
  if DataIndepended or (FDataLink.Field <> nil) then
    if EditCanModify then
    begin
      if (Length(Str) = 1) and (Str[1] = #8) then
      begin
        if Length(Text) = SelLength then
        begin
          SelStart := MAXINT;
          SelLength := -1;
        end else
        begin
          OldSelLenght := Abs(SelLength);
          SelStart := MAXINT;
          SelLength := -OldSelLenght - 1;
        end
      end else
      begin
        SearchText := Copy(Text, 1, SelStart);
        S := SearchText + Str;
        GetItemsList;
        if S <> '' then
          Result := LocateStr(S, True);
      end;
    end;
end;

procedure TCustomDBComboBoxEh.ResetMaxLength;
begin
  if (MaxLength > 0) then
    if FKeyBased
      then MaxLength := 0
      else inherited ResetMaxLength;
end;

procedure TCustomDBComboBoxEh.SetVariantValue(const VariantValue: Variant);
begin
  inherited SetVariantValue(VariantValue);
end;

procedure TCustomDBComboBoxEh.SetItemIndex(const Value: Integer);
begin
  if (csDesigning in ComponentState) and not DataIndepended then Exit;
  if not DataIndepended then DataSource.DataSet.Edit;
  InternalSetItemIndex(Value);
  try
    UpdateData;
  except
    FDataLink.Reset;
    raise;
  end;
end;

procedure TCustomDBComboBoxEh.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TCustomDBComboBoxEh.SetKeyItems(const Value: TStrings);
begin
  FKeyItems.Assign(Value);
end;

procedure TCustomDBComboBoxEh.ItemsChanged(Sender: TObject);
begin
  UpdateItems;
  UpdateItemIndex;
  ResetMaxLength;
end;

procedure TCustomDBComboBoxEh.KeyItemsChanged(Sender: TObject);
begin
  UpdateItems;
  UpdateItemIndex;
  ResetMaxLength;
  DataChange(nil);
end;

function Min(A, B: Integer): Integer;
begin
  if A > B then Result := B
  else Result := A;
end;

procedure TCustomDBComboBoxEh.UpdateItems;
begin
  FItemsCount := Items.Count;
  FKeyBased := False;
  if KeyItems.Count > 0 then
  begin
    FKeyBased := True;
    FItemsCount := Min(FItemsCount, KeyItems.Count);
    EditText := '';
  end;
  UpdateControlReadOnly;
end;

procedure TCustomDBComboBoxEh.UpdateItemIndex;
begin
  FItemIndex := Items.IndexOf(EditText);
  UpdatePopupListboxItemIndex;
  UpdateImageIndex;
end;

procedure TCustomDBComboBoxEh.UpdatePopupListboxItemIndex;
begin
  if FListVisible then
    TComboBoxPopupListboxEh(PopupListbox).ItemIndex := FItemIndex;
end;

function TCustomDBComboBoxEh.DefaultImageIndex: Integer;
begin
  Result := FDefaultItemIndex;
end;

procedure TCustomDBComboBoxEh.UpdateImageIndex;
begin
  FDefaultItemIndex := ItemIndex;
  if Assigned(OnGetItemImageIndex) then
    OnGetItemImageIndex(Self, ItemIndex, FDefaultItemIndex);
  inherited UpdateImageIndex;
end;

procedure TCustomDBComboBoxEh.WndProc(var Message: TMessage);
var
  ShiftState: TShiftState;
  KeyMsg: TWMKey;
begin
  if FListVisible then
  begin
    case Message.Msg of
      {$IFDEF FPC}
      CN_KEYDOWN, CN_SYSKEYDOWN,
      {$ELSE}
      {$ENDIF}
      WM_KEYDOWN, WM_SYSKEYDOWN{, WM_CHAR}:
{$IFDEF CIL}
{$ELSE}
{$ENDIF}
        begin
          KeyMsg := TWMKey(Message);
          ShiftState := KeyDataToShiftState(KeyMsg.KeyData);
          if GetEditButtonByShortCut(ShortCut(KeyMsg.CharCode, ShiftState)) = nil then
            if (KeyMsg.CharCode in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT])
              or ((KeyMsg.CharCode in [VK_HOME, VK_END]) and (ssCtrl in KeyDataToShiftState(KeyMsg.KeyData)))
            {or ((CharCode in [VK_LEFT, VK_RIGHT]) )}then
            begin
              SendMessage(PopupListbox.Handle, Message.Msg, Message.WParam, Message.LParam);
              Exit;
            end;
        end;
    end;
  end;
  inherited WndProc(Message);
end;

{$IFDEF FPC}
{$ELSE}
procedure TCustomDBComboBoxEh.CMCancelMode(var Message: TCMCancelMode);
  function CheckDataListChilds: Boolean;
  var i: Integer;
  begin
    Result := False;
    if PopupListbox <> nil then
      for i := 0 to PopupListbox.ControlCount - 1 do
        if PopupListbox.Controls[I] = Message.Sender then
        begin
          Result := True;
          Exit;
        end;
  end;
begin
  inherited;
  if (Message.Sender <> Self) and not ContainsControl(Message.Sender) and
    (Message.Sender <> PopupListbox) and not CheckDataListChilds
  then
    CloseUp(False);
end;
{$ENDIF}

procedure TCustomDBComboBoxEh.DefaultHandler(var Message);
var
  MouseMsg: TWMMouse;
begin
  case TMessage(Message).Msg of
    WM_LBUTTONDBLCLK, WM_LBUTTONDOWN, WM_LBUTTONUP,
      WM_MBUTTONDBLCLK, WM_MBUTTONDOWN, WM_MBUTTONUP,
      WM_RBUTTONDBLCLK, WM_RBUTTONDOWN, WM_RBUTTONUP:
      if LimitTextToListValues then
      begin
        MouseMsg := TWMMouse(Message);
        if MouseMsg.Msg = WM_RBUTTONUP then
          Perform(WM_CONTEXTMENU, Handle,
            SmallPointToInteger(PointToSmallPoint(
                ClientToScreen(Point(MouseMsg.XPos, MouseMsg.YPos)))) );
        Exit;
      end;
  end;
  inherited DefaultHandler(Message);
end;

procedure TCustomDBComboBoxEh.SelectNextValue(IsPrior: Boolean);
var
  OldItemIndex: Integer;
begin
  OldItemIndex := ItemIndex;
  if not EditCanModify then Exit;
  if IsPrior then
  begin
    if ItemIndex > 0 then
      InternalSetItemIndex(ItemIndex - 1)
    else if ItemIndex <> 0 then
      InternalSetItemIndex(FItemsCount - 1)
  end else if ItemIndex < FItemsCount - 1 then
    InternalSetItemIndex(ItemIndex + 1);
  if OldItemIndex <> ItemIndex then
  begin
    SelectAll;
  end;
end;

function TCustomDBComboBoxEh.GetPopupListboxColor: TColor;
begin
  Result := Self.Color;
end;

function TCustomDBComboBoxEh.SelfPopupListboxFont: TFont;
begin
  Result := Self.Font;
end;

procedure TCustomDBComboBoxEh.WMChar(var Message: TWMChar);
var
  OldSelStart: Integer;
begin
  inherited;
  if Message.CharCode = 0 then Exit;
  if not LimitTextToListValues and
     not (Message.CharCode = VK_DELETE) and
     not (ssCtrl in KeyDataToShiftState(Message.KeyData))
  then
  begin
    if not ((SelStart = Length(Text)) and (SelLength = 0)) or (Message.CharCode = VK_BACK) then
    begin
      OldSelStart := SelStart;
      GetItemsList;
      if LocateStr(Text, False) then
      begin
        SelStart := Length(Text);
        SelLength := OldSelStart - SelStart;
      end;
    end else
    begin
      ProcessSearchStr('');
    end;
  end;
end;

procedure TCustomDBComboBoxEh.WMClear(var Message: TWMCut);
begin
  if FKeyBased and EditCanModify then
    InternalSetValue(Null)
  else
    inherited;
end;

procedure TCustomDBComboBoxEh.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
end;

procedure TCustomDBComboBoxEh.WMKillFocus(var Message: TWMKillFocus);
begin
  if FListVisible and
     not (Message.FocusedWnd = PopupListbox.Handle) and
     not (WinServiceIsChild(PopupListbox.Handle, Message.FocusedWnd))
  then
    CloseUp(False);
  inherited;
end;

procedure TCustomDBComboBoxEh.WMPaste(var Message: TMessage);
begin
  if not LimitTextToListValues then
    inherited
  else if Clipboard.HasFormat(CF_TEXT) then
    ProcessSearchStr(Clipboard.AsText);
end;

procedure TCustomDBComboBoxEh.WMSetCursor(var Message: TWMSetCursor);
{$IFDEF FPC_CROSSP}
begin
  inherited;
end;
{$ELSE}
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if LimitTextToListValues
    then Windows.SetCursor(LoadCursorEh(0, idc_Arrow))
    else inherited;
end;
{$ENDIF} 

procedure TCustomDBComboBoxEh.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  if (Message.CharCode in [VK_RETURN, VK_ESCAPE]) and FListVisible then
  begin
    
    Message.Result := 1;
  end else
    inherited;
end;

procedure TCustomDBComboBoxEh.Change;
begin
  UpdateItemIndex;
  inherited Change;
end;

procedure TCustomDBComboBoxEh.InternalUpdatePostData;
begin
  if DataIndepended and not FKeyBased
    then FDataLink.SetText(EditText)
    else FDataLink.SetText(VarToStr(Value));
end;

procedure TCustomDBComboBoxEh.UpdateData;
var
  RecheckInList: Boolean;
begin
  if Assigned(FOnNotInList) {and Focused} then
  begin
    RecheckInList := False;
    if ItemIndex = -1 then
    begin
      FOnNotInList(Self, EditText, RecheckInList);
    end;
  end;
  inherited UpdateData;
end;

function TCustomDBComboBoxEh.GetImages: TCustomImageList;
begin
  Result := EditImage.Images;
end;

procedure TCustomDBComboBoxEh.SetImages(const Value: TCustomImageList);
begin
  EditImage.Images := Value;
  EditImage.Visible := True;
end;

procedure TCustomDBComboBoxEh.SetDropDownBox(const Value: TDropDownBoxEh);
begin
  FDropDownBox.Assign(Value);
end;

procedure TCustomDBComboBoxEh.CMMouseWheel(var Message: TMessage);
begin
  if FListVisible and not FWheelEventInListbox then
  begin
    FWheelEventInListbox := True;
    try
{$IFDEF CIL}
{$ELSE}
{$ENDIF}
      if FPopupListbox.Perform(CM_MOUSEWHEEL, Message.WParam, Message.LParam) <> 0 then
      begin
        Message.Result := 1;
        Exit;
      end;
    finally
      FWheelEventInListbox := False;
    end;
  end;
  inherited;
end;

function TCustomDBComboBoxEh.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result and (Shift = []) and not ReadOnly and FDataLink.Edit then
  begin
    SelectNextValue(False);
    Result := True;
  end;
end;

function TCustomDBComboBoxEh.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result and (Shift = []) and not ReadOnly and FDataLink.Edit then
  begin
    SelectNextValue(True);
    Result := True;
  end;
end;

procedure TCustomDBComboBoxEh.Clear;
begin
  if FKeyBased
    then Value := Null
    else inherited Clear;
end;

function TCustomDBComboBoxEh.GetDisplayTextForPaintCopy: String;
var
  Index: Integer;
begin
  if (csDesigning in ComponentState) and not (FDataLink.Active) then
    Result := Name
  else if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
  begin
    if FKeyBased then
    begin
      Index := KeyItems.IndexOf(FDataLink.Field.Text);
      if (Index >= 0) and (Index < FItemsCount) then
        Result := Items.Strings[Index];
    end else
      Result := FDataLink.Field.DisplayText;
    case CharCase of
      ecUpperCase: Result := NlsUpperCase(Result);
      ecLowerCase: Result := NlsLowerCase(Result);
    end;
  end else
    Result := EditText;
end;

procedure TCustomDBComboBoxEh.GetItemsList;
begin
  if Items.Count = 0 then
    if Assigned(OnGetItemsList) then
      OnGetItemsList(Self);
end;

function TCustomDBComboBoxEh.GetLimitTextToListValues: Boolean;
begin
  if LimitTextToListValuesStored
    then Result := FLimitTextToListValues
    else Result := DefaultLimitTextToListValues;
end;

function TCustomDBComboBoxEh.IsLimitTextToListValuesStored: Boolean;
begin
  Result := FLimitTextToListValuesStored;
end;

procedure TCustomDBComboBoxEh.SetLimitTextToListValues(const Value: Boolean);
begin
  if LimitTextToListValuesStored and (Value = FLimitTextToListValues) then Exit;
  LimitTextToListValuesStored := True;
  FLimitTextToListValues := Value;
end;

procedure TCustomDBComboBoxEh.SetLimitTextToListValuesStored(const Value: Boolean);
begin
  if (Value = True) and (IsLimitTextToListValuesStored = False) then
  begin
    FLimitTextToListValuesStored := True;
    FLimitTextToListValues := DefaultLimitTextToListValues;
  end else if (Value = False) and (IsLimitTextToListValuesStored = True) then
  begin
    FLimitTextToListValuesStored := False;
    FLimitTextToListValues := DefaultLimitTextToListValues;
  end;
end;

function TCustomDBComboBoxEh.DefaultLimitTextToListValues: Boolean;
begin
  Result := (KeyItems <> nil) and (KeyItems.Count > 0) and not Assigned(OnNotInList);
end;

procedure TCustomDBComboBoxEh.SetCaseInsensitiveTextSearch(
  const Value: Boolean);
begin
  if FCaseInsensitiveTextSearch <> Value then
  begin
    FCaseInsensitiveTextSearch := Value;
    TStringList(FItems).CaseSensitive := not FCaseInsensitiveTextSearch;
  end;
end;

{ TCustomDBNumberEditEh }

function IsValidFloat(const Value: string; var RetValue: Extended): Boolean;
var
  I: Integer;
  Buffer: array[0..63] of Char;
{$IFDEF CIL}
  DValue: Double;
{$ENDIF}
begin
  Result := False;
  for I := 1 to Length(Value) do
    if not ((Value[I] = FormatSettings.DecimalSeparator) or CharInSetEh(Value[I], [ '-', '+', '0'..'9', 'e', 'E'])) then
      Exit;
  if (Value = '+') or (Value = '-') then
  begin
    RetValue := 0;
    Result := True;
  end else
{$IFDEF CIL}
  begin
    DValue := RetValue;
    Result := TryStrToFloat(Value, DValue);
    RetValue := DValue;
  end;
{$ELSE}
    Result := TextToFloat(StrPLCopy(Buffer, Value,
      SizeOf(Buffer) - 1), RetValue, fvExtended);
{$ENDIF}
end;

function FormatFloatStr(const S: string; Thousands: Boolean): string;
var
  I, MaxSym, MinSym, Group: Integer;
  IsSign: Boolean;
begin
  Result := '';
  MaxSym := Length(S);
  IsSign := (MaxSym > 0) and CharInSetEh(S[1], ['-', '+']);
  if IsSign then MinSym := 2
  else MinSym := 1;
  I := Pos(FormatSettings.DecimalSeparator, S);
  if I > 0 then MaxSym := I - 1;
  I := Pos('E', NlsUpperCase(S));
  if I > 0 then MaxSym := Min(I - 1, MaxSym);
  Result := Copy(S, MaxSym + 1, MaxInt);
  Group := 0;
  for I := MaxSym downto MinSym do
  begin
    Result := S[I] + Result;
    Inc(Group);
    if (Group = 3) and Thousands and (I > MinSym) then
    begin
      Group := 0;
      Result := FormatSettings.ThousandSeparator + Result;
    end;
  end;
  if IsSign then Result := S[1] + Result;
end;

function CurrencyEditFormat: String;
var i: Integer;
begin
  Result := ',#.';
  for i := 1 to FormatSettings.CurrencyDecimals do
    Result := Result + '0';
end;

constructor TCustomDBNumberEditEh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  ControlStyle := ControlStyle - [csSetCaption];
  MaxLength := 0;
  FDecimalPlaces := 2;
  FIncrement := 1.0;
  { forces update }
  DataChange(nil);
end;

destructor TCustomDBNumberEditEh.Destroy;
begin
  inherited Destroy;
end;

function TCustomDBNumberEditEh.IsValidChar(Key: Char): Boolean;
var
  S: string;
  SelStart, SelStop, DecPos: Integer;
  RetValue: Extended;
begin
  Result := False;
  if (SelLength = 0) and CharInSetEh(Key, ['0'..'9']) and (IntDigitsInText >= 17-Integer(DecimalPlaces)) then
    Exit;
  if (Key = FormatSettings.DecimalSeparator) and (DecimalPlaces = 0) then
    Exit;
  S := EditText;
  GetSel(SelStart, SelStop);
  {System.}Delete(S, SelStart + 1, SelStop - SelStart);
  {System.}Insert(Key, S, SelStart + 1);
  S := TextToValText(S);
  DecPos := Pos(FormatSettings.DecimalSeparator, S);
  if (DecPos > 0) then
  begin
    SelStart := Pos('E', UpperCase(S));
    if (SelStart > DecPos) then DecPos := SelStart - DecPos
    else DecPos := Length(S) - DecPos;
    if DecPos > Integer(FDecimalPlaces) then Exit;
  end;
  if S  = '' then
    Result := True
  else
  begin
    Result := IsValidFloat(S, RetValue);
    if Result and (FMinValue >= 0) and (FMaxValue > 0) and (RetValue < 0) then
      Result := False;
  end;
end;

procedure TCustomDBNumberEditEh.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not ReadOnly and ((Key = VK_UP) or (Key = VK_DOWN)) then
  begin
    IncrementValue(Key = VK_UP);
    Key := 0;
  end;
end;

procedure TCustomDBNumberEditEh.KeyPress(var Key: Char);
begin
  if FCalculatorVisible and CharInSetEh(Key, [#13, #27]) then
  begin
    CloseUp(Key = #13);
    Key := #0;
  end;
  if (Key = #8) and (SelStart > 0) and (Text[SelStart] = FormatSettings.ThousandSeparator) then
  begin
    SelStart := SelStart - 1;
    Key := #0;
  end;
  inherited KeyPress(Key);
  if CharInSetEh(Key, ['.', ',']) then Key := Copy(FormatSettings.DecimalSeparator, 1, 1)[1];
  if (Key >= #32) and not IsValidChar(Key) then
  begin
    Key := #0;
  end
  else if Key = #27 then
  begin
    Reset;
    Key := #0;
  end;
end;

procedure TCustomDBNumberEditEh.SetDecimalPlaces(Value: Cardinal);
begin
  if FDecimalPlaces <> Value then
  begin
    FDecimalPlaces := Value;
    DataChange(nil);
    Invalidate;
  end;
end;

function TCustomDBNumberEditEh.FormatDisplayText(Value: Extended): string;
begin
  if DisplayFormat <> '' then
    Result := FormatFloat(DisplayFormat, Value)
  else if Currency then
    Result := CurrToStrF(Value, ffCurrency, FormatSettings.CurrencyDecimals)
  else
    Result := FloatToStr(Value);
end;

function TCustomDBNumberEditEh.GetDisplayText: string;
begin
  if FValue = Null then Result := ''
  else Result := FormatDisplayText(FValue);
end;

function TCustomDBNumberEditEh.GetVariantValue: Variant;
begin
  Result := FValue;
end;

procedure TCustomDBNumberEditEh.DataChanged;
var
  AValue: Variant;
  Handled: Boolean;
begin
  if (DisplayFormat = '') and Currency
    then FEditFormat := CurrencyEditFormat
    else FEditFormat := DisplayFormatToEditFormat(DisplayFormat);

  if FDataLink.Field <> nil then
  begin

    Handled := False;
    if Assigned(OnGetFieldData) then
    begin
      AValue := Unassigned;
      OnGetFieldData(Self, AValue, Handled);
    end;

    if not Handled then
    begin
      if not (evAlignmentEh in inherited AssignedValues) and
        (FAlignment <> FDataLink.Field.Alignment) then
      begin
        FAlignment := FDataLink.Field.Alignment;
        RecreateWndHandle;
      end;
      InternalSetValue(FDataLink.Field.Value);
    end else
      InternalSetValue(AValue);
  end
  else if DataIndepended then
    InternalSetValue(FDataLink.DataIndependentValue)
  else
  begin
    InternalSetValue(Null);
  end;
  UpdateControlReadOnly;
  Modified := False;
end;

function SimpleRoundTo(const AValue: Extended; const ADigit: Integer = -2): Extended;
var
  LFactor: Extended;
begin
  LFactor := IntPower(10, -ADigit);
  if AValue < 0 then
    Result := Trunc((AValue / LFactor) - 0.5) * LFactor
  else
    Result := Trunc((AValue / LFactor) + 0.5) * LFactor;
end;

function TCustomDBNumberEditEh.CheckValue(NewValue: Extended): Extended;

  function Degree10(ADegree: Integer): Extended;
  var
    i: Integer;
  begin
    Result := 10;
    for i := 1 to ADegree-1 do
      Result := Result*10;
  end;

begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then
  begin
    if (FMaxValue > FMinValue) then
    begin
      if NewValue < FMinValue then Result := FMinValue
      else if NewValue > FMaxValue then Result := FMaxValue;
    end else
    begin
      if FMaxValue = 0 then
      begin
        if NewValue < FMinValue then Result := FMinValue;
      end else if FMinValue = 0 then
      begin
        if NewValue > FMaxValue then Result := FMaxValue;
      end;
    end;
  end;
  if DecimalPlaces <= 37 then
    if DecimalPlaces > 0
      then Result := Round(Result * Degree10(DecimalPlaces)) / Degree10(DecimalPlaces)
      else Result := Round(Result);
end;

function TCustomDBNumberEditEh.DisplayFormatToEditFormat(const AFormat: string): string;
var i: Integer;
  C, Quote, E: Char;
  EPlus: String;
  ENullCount: Integer;
begin
  Result := '';
  Quote := #0;
  E := #0;
  EPlus := '';
  ENullCount := 0;
  for i := 1 to Length(AFormat) do
  begin
    C := AFormat[i];
    if CharInSetEh(C, ['''', '"']) then
    begin
      if C = Quote then Quote := #0 else Quote := C;
    end else if Quote <> #0 then
      Continue
    else if CharInSetEh(C, ['0', '#', '.', ',']) then
      if (C = '0') and (EPlus = 'E+') then
      begin
        if ENullCount >= 4 then Exit else Inc(ENullCount);
      end else
        Result := Result + C
    else if CharInSetEh(C, ['e', 'E']) then
    begin
      E := 'E';
      EPlus := '';
      Continue;
    end else if (C = '+') and (E = 'E') then
    begin
      E := #0;
      EPlus := 'E+';
      Continue;
    end else if C = ';' then Exit;
    E := #0;
    EPlus := '';
  end;
end;

procedure TCustomDBNumberEditEh.InternalSetControlText(const AText: String);
begin
  if FInternalTextSetting then Exit;
  FInternalTextSetting := True;
  try
    inherited InternalSetText(AText);
  finally
    FInternalTextSetting := False;
  end;
end;

procedure TCustomDBNumberEditEh.InternalSetText(const AText: String);
begin
  if AText = ''
    then InternalSetValue(Null)
    else InternalSetValue(StrToFloat(TextToValText(AText)));
end;

procedure TCustomDBNumberEditEh.InternalSetValue(AValue: Variant);
begin
  if AValue = Null then
  begin
    FValue := Null;
    InternalSetControlText('');
  end else
  begin
    FValue := VarAsType(AValue, varDouble);
    FDataLink.Modified;
    if FFocused and FDataLink.CanModify then
      ReformatEditText(FormatFloat(FEditFormat, FValue))
    else
      InternalSetControlText(DisplayText);
  end;
end;

procedure TCustomDBNumberEditEh.UpdateValueFromText;
var
  s: String;
begin
  s := TextToValText(EditText);
  if s = '' then
  begin
    FValue := Null;
    InternalSetControlText('');
  end else
  begin
    if (s = '+') or (s = '-')
      then FValue := CheckValue(0)
      else FValue := CheckValue(StrToFloat(s));
  end;
end;

procedure TCustomDBNumberEditEh.InternalUpdatePostData;
begin
  FDataLink.SetValue(Value);
end;

procedure TCustomDBNumberEditEh.SetMinValue(AValue: Extended);
begin
  if (evMinValueEh in FAssignedValues) and (AValue = FMinValue) then Exit;
  FMinValue := AValue;
  if not (csLoading in ComponentState) and DataIndepended then UpdateData;
  Include(FAssignedValues, evMinValueEh);
end;

procedure TCustomDBNumberEditEh.SetMaxValue(AValue: Extended);
begin
  if (evMaxValueEh in FAssignedValues) and (AValue = FMaxValue)
    then Exit;
  FMaxValue := AValue;
  if not (csLoading in ComponentState) and DataIndepended
    then UpdateData;
  Include(FAssignedValues, evMaxValueEh);
end;

function DelBSpace(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  Result := Copy(S, I, MaxInt);
end;

function DelESpace(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

function DelRSpace(const S: string): string;
begin
  Result := DelBSpace(DelESpace(S));
end;

function ReplaceStr(const S, Srch, Replace: string): string;
var
  I: Integer;
  Source: string;
begin
  Source := S;
  Result := '';
  repeat
    I := Pos(Srch, Source);
    if I > 0 then
    begin
      Result := Result + Copy(Source, 1, I - 1) + Replace;
      Source := Copy(Source, I + Length(Srch), MaxInt);
    end
    else Result := Result + Source;
  until I <= 0;
end;

function TCustomDBNumberEditEh.TextToValText(const AValue: string): string;
var
  i: Integer;
  NumberStared: Boolean;
  ExpFound: Boolean;
  DecSepFound: Boolean;
  SignFound: Boolean;
  OneAltDecimalSeparatorFound: Boolean;
begin
  Result := '';
  NumberStared := False;
  ExpFound := False;
  DecSepFound := False;
  SignFound := False;
  OneAltDecimalSeparatorFound := False;

  for i := 1 to Length(AValue) do
  begin
    if (AValue[I] = FormatSettings.DecimalSeparator) and not DecSepFound then
      DecSepFound := True;
  end;

  if not DecSepFound then
  begin
    for i := 1 to Length(AValue) do
    begin
      if (AValue[I] = EhLibManager.AltDecimalSeparator) and not OneAltDecimalSeparatorFound then
        OneAltDecimalSeparatorFound := True
      else if (AValue[I] = EhLibManager.AltDecimalSeparator) and OneAltDecimalSeparatorFound then
      begin
        OneAltDecimalSeparatorFound := False;
        Break;
      end;
    end;
  end;

  for i := 1 to Length(AValue) do
  begin
    if (AValue[I] = FormatSettings.DecimalSeparator) then
    begin
      Result := Result + AValue[I];
    end else if (AValue[I] = EhLibManager.AltDecimalSeparator) and OneAltDecimalSeparatorFound then
    begin
      Result := Result + AValue[I];
    end else if CharInSetEh(AValue[I], ['-', '+']) and not SignFound and not NumberStared then
    begin
      Result := Result + AValue[I];
      SignFound := True;
    end else if CharInSetEh(AValue[I], ['e', 'E']) and not ExpFound then
    begin
      Result := Result + AValue[I];
      ExpFound := True;
    end else if CharInSetEh(AValue[I], ['0'..'9']) then
    begin
      Result := Result + AValue[I];
      NumberStared := True;
    end;
  end;

  if FormatSettings.DecimalSeparator <> FormatSettings.ThousandSeparator then
    Result := StringReplace(Result, FormatSettings.ThousandSeparator, '', [rfReplaceAll]);

  if OneAltDecimalSeparatorFound and
     (FormatSettings.DecimalSeparator <> EhLibManager.AltDecimalSeparator) and
     (FormatSettings.ThousandSeparator <> EhLibManager.AltDecimalSeparator)
  then
    Result := ReplaceStr(Result, EhLibManager.AltDecimalSeparator, FormatSettings.DecimalSeparator);
end;

procedure TCustomDBNumberEditEh.ReformatEditText(const NewText: String);
var
  S: string;
  IsEmpty: Boolean;
  OldLen, SelStart, SelStop: Integer;
begin
  try
    S := NewText;
    OldLen := Length(S);
    IsEmpty := (OldLen = 0) or (S = '-');
    if HandleAllocated then GetSel(SelStart, SelStop);
    if not IsEmpty then S := TextToValText(S);
    S := FormatFloatStr(S, Pos(',', FEditFormat) > 0);
    if S <> Text then
    begin
      InternalSetControlText(S);
      if HandleAllocated and (GetFocus = Handle) and not (csDesigning in ComponentState) then
      begin
        Inc(SelStart, Length(S) - OldLen);
        SetCursor(SelStart);
      end;
    end;
  finally
  end;
end;

procedure TCustomDBNumberEditEh.Change;
begin
  if not FInternalTextSetting then
  begin
    ReformatEditText(inherited Text);
    UpdateValueFromText;
  end;
  inherited Change;
end;

procedure TCustomDBNumberEditEh.CreateParams(var Params: TCreateParams);
const
  Alignments: array[Boolean, TAlignment] of DWORD =
  ((ES_LEFT, ES_RIGHT, ES_CENTER), (ES_RIGHT, ES_LEFT, ES_CENTER));
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Alignments[UseRightToLeftAlignment, Alignment];
  FAlignment := Alignment;
end;

procedure TCustomDBNumberEditEh.WMPaste(var Message: TMessage);
var
  S: string;
begin
  S := EditText;
  try
    Value := CheckValue(StrToFloat(TextToValText(Clipboard.AsText)));
    UpdateValueFromText;
  except
    EditText := S;
    SelectAll;
    if CanFocus then SetFocus;
  end;
end;

function TCustomDBNumberEditEh.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 1.0;
end;

procedure TCustomDBNumberEditEh.IncrementValue(IsIncrease: Boolean);
var Sign, ev: Extended;
begin
  if IsIncrease then Sign := 1 else Sign := -1;
  if Increment = 0 then Exit;
  if EditCanModify then
  begin
    if Value = Null
      then ev := Increment
      else ev := Value + Increment * Sign;
    InternalSetValue(CheckValue(ev));
    if FFocused then SelectAll;
  end;
end;

function TCustomDBNumberEditEh.GetDisplayFormat: string;
begin
  if evDisplayFormatEh in FAssignedValues then Result := FDisplayFormat
  else Result := DefaultDisplayFormat;
end;

procedure TCustomDBNumberEditEh.SetDisplayFormat(const Value: string);
begin
  if (evDisplayFormatEh in FAssignedValues) and (Value = FDisplayFormat) then Exit;
  FDisplayFormat := Value;
  Include(FAssignedValues, evDisplayFormatEh);
  Invalidate;
  DataChange(nil);
end;

function TCustomDBNumberEditEh.IsDisplayFormatStored: Boolean;
begin
  Result := (evDisplayFormatEh in FAssignedValues);
end;

function TCustomDBNumberEditEh.DefaultDisplayFormat: String;
begin
  if Assigned(Field) then
  {$IFDEF FPC}
  {$ELSE}
    if Field is TSQLTimeStampField then
      Result := TSQLTimeStampField(Field).DisplayFormat
    else if Field is TAggregateField then
      Result := TAggregateField(Field).DisplayFormat
    else
  {$ENDIF}
        if Field is TDateTimeField then Result := TDateTimeField(Field).DisplayFormat
        else if Field is TNumericField then Result := TNumericField(Field).DisplayFormat
        else Result := ''
      else Result := '';
end;

function TCustomDBNumberEditEh.GetCurrency: Boolean;
begin
  if evCurrencyEh in FAssignedValues then Result := FCurrency
  else Result := DefaultCurrency;
end;

function TCustomDBNumberEditEh.IsCurrencyStored: Boolean;
begin
  Result := (evCurrencyEh in FAssignedValues);
end;

procedure TCustomDBNumberEditEh.SetCurrency(const Value: Boolean);
begin
  if (evCurrencyEh in FAssignedValues) and (Value = FCurrency) then Exit;
  FCurrency := Value;
  Include(FAssignedValues, evCurrencyEh);
  Invalidate;
  DataChange(nil);
end;

function TCustomDBNumberEditEh.DefaultCurrency: Boolean;
begin
  if Assigned(Field) then
    if Field is TFMTBCDField
      then Result := TFMTBCDField(Field).Currency
    else
{$IFDEF FPC}
{$ELSE}
      if Field is TAggregateField then Result := TAggregateField(Field).Currency
      else
{$ENDIF}
        if Field is TBCDField
          then Result := TBCDField(Field).Currency
        else if Field is TFloatField
          then Result := TFloatField(Field).Currency
          else Result := False
      else Result := False;
end;

function TCustomDBNumberEditEh.IsMaxValueStored: Boolean;
begin
  Result := (evMaxValueEh in FAssignedValues);
end;

function TCustomDBNumberEditEh.IsMinValueStored: Boolean;
begin
  Result := (evMinValueEh in FAssignedValues);
end;

function TCustomDBNumberEditEh.GetMaxValue: Extended;
begin
  if evMaxValueEh in FAssignedValues
    then Result := FMaxValue
    else Result := DefaultMaxValue;
end;

function TCustomDBNumberEditEh.GetMinValue: Extended;
begin
  if evMinValueEh in FAssignedValues
    then Result := FMinValue
    else Result := DefaultMinValue;
end;

function TCustomDBNumberEditEh.DefaultMaxValue: Extended;
begin
  if Assigned(Field) then
    if Field is TIntegerField then Result := TIntegerField(Field).MaxValue
    else if Field is TBCDField then Result := TBCDField(Field).MaxValue
    else if Field is TFloatField then Result := TFloatField(Field).MaxValue
    
    else Result := 0
  else Result := 0;
end;

function TCustomDBNumberEditEh.DefaultMinValue: Extended;
begin
  if Assigned(Field) then
    if Field is TIntegerField then Result := TIntegerField(Field).MinValue
    else if Field is TBCDField then Result := TBCDField(Field).MinValue
    else if Field is TFloatField then Result := TFloatField(Field).MinValue
    
    else Result := 0
  else Result := 0;
end;

function TCustomDBNumberEditEh.DefaultAlignment: TAlignment;
begin
  if Assigned(Field)
    then Result := inherited DefaultAlignment
    else Result := taRightJustify;
end;

function TCustomDBNumberEditEh.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result and (Shift = []) and not ReadOnly and FDataLink.Edit then
  begin
    IncrementValue(False);
    Result := True;
  end;
end;

function TCustomDBNumberEditEh.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result and (Shift = []) and not ReadOnly and FDataLink.Edit then
  begin
    IncrementValue(True);
    Result := True;
  end;
end;

function TCustomDBNumberEditEh.GetDropDownCalculator: TWinControl;
begin
  if FDropDownCalculator = nil then
  begin
    FDropDownCalculator := EhLibManager.PopupCalculatorClass.Create(Self);
  end;
  Result := FDropDownCalculator;
end;

procedure TCustomDBNumberEditEh.EditButtonDownDefaultAction(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh; TopButton: Boolean;
  var AutoRepeat: Boolean; var Handled: Boolean);
begin
  if (EditButton.Style in [ebsUpDownEh, ebsAltUpDownEh]) then
  begin
    if not ReadOnly and FDataLink.Edit then
      IncrementValue(TopButton);
    Handled := True;
  end else
  begin
    if not FDroppedDown then
    begin
      DropDownAction(EditButton, EditButtonControl, Handled);
      FNoClickCloseUp := True;
    end;
  end;
end;

procedure TCustomDBNumberEditEh.EditButtonClickDefaultAction(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh; TopButton: Boolean; var Handled: Boolean);
begin
  
end;

procedure TCustomDBNumberEditEh.DropDownAction(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh; var Handled: Boolean);
begin
  EditButtonControl.AlwaysDown := True;
  inherited DropDownAction(EditButton, EditButtonControl, Handled);
  DropDown;
  if FCalculatorVisible then
    SetEditButtonDroppedDown(EditButton, EditButtonControl);
  Handled := True;
end;

procedure TCustomDBNumberEditEh.CloseUp(Accept: Boolean);
var
  PopupCalculatorIntf: IPopupCalculatorEh;
begin
  if FCalculatorVisible then
  begin
    FCalculatorVisible := False;

    Supports(DropDownCalculator, IPopupCalculatorEh, PopupCalculatorIntf);
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);

    if HandleAllocated and
       ((GetFocus = DropDownCalculator.Handle) or
        (GetParent(GetFocus) = DropDownCalculator.Handle))
    then
    begin
      SetFocus;
    end;

    PopupCalculatorIntf.Hide;

    ShowCaret(Handle);
    FDroppedDown := False;
    inherited CloseUp(Accept);
    PostMessage(Handle, CM_IGNOREEDITDOWN, Integer(FButtonsBox.BtnCtlList[0].EditButtonControl.Tag), 0);
    if Accept and not ReadOnly and FDataLink.Edit then
    begin
      if PopupCalculatorIntf <> nil then
      begin
        if VarType(PopupCalculatorIntf.Value) in
             [varDouble, varSmallint, varInteger, varSingle, varCurrency]
        then
          InternalSetValue(PopupCalculatorIntf.Value);
      end;
      if FFocused then SelectAll;
    end;
    if Assigned(FOnCloseUp) then FOnCloseUp(Self, Accept);
    SetEditButtonClosedUp;
  end;
end;

procedure TCustomDBNumberEditEh.DropDown;
var
  P: TPoint;
  AAlignment: TDropDownAlign;
  PopupCalculatorIntf: IPopupCalculatorEh;
begin
  if not FCalculatorVisible then
  begin
    if Assigned(FOnDropDown) then FOnDropDown(Self);
    Supports(DropDownCalculator, IPopupCalculatorEh, PopupCalculatorIntf);

    DropDownCalculator.HandleNeeded;
    if VarIsNull(Value)
      then PopupCalculatorIntf.Value := 0
      else PopupCalculatorIntf.Value := Value;
    PopupCalculatorIntf.Flat := Flat;
    PopupCalculatorIntf.SetTextHeight(GetFontTextHeight(Canvas, Font));

    if inherited UseRightToLeftAlignment
      then AAlignment := daRight
      else AAlignment := daLeft;

    P := AlignDropDownWindow(Self, DropDownCalculator, AAlignment);
    DropDownCalculator.SetBounds(P.X, P.Y, DropDownCalculator.Width, DropDownCalculator.Height);

    PopupCalculatorIntf.Show(P, CloseWinCallbackProc);

    FCalculatorVisible := True;
    FDroppedDown := True;
    HideCaret(Handle);
    SelLength := 0;
  end;
end;

procedure TCustomDBNumberEditEh.CloseWinCallbackProc(Control: TWinControl;
  Accept: Boolean);
begin
  CloseUp(Accept);
end;

{$IFDEF FPC}
{$ELSE}
procedure TCustomDBNumberEditEh.CMCancelMode(var Message: TCMCancelMode);
  function CheckActiveListChilds: Boolean;
  var i: Integer;
  begin
    Result := False;
    if DropDownCalculator <> nil then
      for i := 0 to DropDownCalculator.ControlCount - 1 do
        if DropDownCalculator.Controls[I] = Message.Sender then
        begin
          Result := True;
          Exit;
        end;
  end;
begin
  inherited;
  if (Message.Sender = Self) or
    ((Message.Sender <> DropDownCalculator) and
      not ContainsControl(Message.Sender) and not CheckActiveListChilds)
  then
    CloseUp(False);
end;
{$ENDIF}

procedure TCustomDBNumberEditEh.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  if (Message.CharCode in [VK_RETURN, VK_ESCAPE]) and FCalculatorVisible then
  begin
    
    Message.Result := 1;
  end else
    inherited;
end;

procedure TCustomDBNumberEditEh.WMKillFocus(var Message: TWMKillFocus);
begin
  {$IFDEF FPC_CROSSP}
  {$ELSE}
  if FCalculatorVisible and not (Message.FocusedWnd = DropDownCalculator.Handle) then
    CloseUp(False);
  {$ENDIF}
  inherited;
end;

procedure TCustomDBNumberEditEh.WndProc(var Message: TMessage);
var
  WMKey: TWMKey;
begin
  if FCalculatorVisible then
  begin
    case Message.Msg of
      wm_KeyDown, wm_SysKeyDown, wm_Char:
{$IFDEF CIL}
{$ELSE}
{$ENDIF}
        begin
          WMKey := TWMKey(Message);
          if (WMKey.CharCode in [8, 13]) or ((WMKey.CharCode >= 32) and (WMKey.CharCode < 127)) then
          begin
            SendMessage(DropDownCalculator.Handle, Message.Msg, Message.WParam, Message.LParam);
            Exit;
          end;
        end;
    end;
  end;
  inherited WndProc(Message);
end;

procedure TCustomDBNumberEditEh.CMMouseWheel(var Message: TMessage);
begin
  if FCalculatorVisible then
{$IFDEF CIL}
{$ELSE}
{$ENDIF}
      if FDropDownCalculator.Perform(CM_MOUSEWHEEL, Message.WParam, Message.LParam) <> 0 then
      begin
        Exit;
      end;
  inherited;
end;

function TCustomDBNumberEditEh.CreateEditButton: TEditButtonEh;
begin
  Result := TDropDownEditButtonEh.Create(Self);
end;

function TCustomDBNumberEditEh.IntDigitsInText: Integer;
var
  i: Integer;
  AText: String;
begin
  AText := Text;
  Result := 0;
  for i := 1 to Length(AText) do
  begin
    if CharInSetEh(AText[i], ['0'..'9']) then
      Inc(Result);
    if AText[i] = FormatSettings.DecimalSeparator then Exit;
  end;
end;

{ TCustomDBCheckBoxEh }

constructor TCustomDBCheckBoxEh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  {$IFDEF FPC}
  ControlStyle := ControlStyle - [csOpaque] + [csParentBackground];
  {$ELSE}
  if SysLocale.FarEast and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    ImeMode := imDisable;
  {$ENDIF}
  Width := 97;
  Height := 17;
  TabStop := True;
  ControlStyle := ControlStyle + [csReplicatable] - [csDoubleClicks];
  FAlignment := taRightJustify;

  FValueCheck := STextTrue;
  FValueUncheck := STextFalse;
  FDataLink := TFieldDataLinkEh.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := InternalUpdateData;

  FState := cbUnchecked;
  FDynProps := TDynVarsEh.Create(Self);

  FDataLink.DataIndependentValue := False;
end;

destructor TCustomDBCheckBoxEh.Destroy;
begin
  Destroying;
  DataSource := nil;
  FreeAndNil(FDataLink);
  FreeAndNil(FCanvas);
  FreeAndNil(FDynProps);
  inherited Destroy;
end;

procedure TCustomDBCheckBoxEh.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomDBCheckBoxEh.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := ObjectToIntPtr(FDataLink);
end;

procedure TCustomDBCheckBoxEh.DataChange(Sender: TObject);
begin
  if (csDestroying in Componentstate) then Exit;
  InternalSetState(GetFieldState);
  FModified := False;
end;

function TCustomDBCheckBoxEh.DataIndepended: Boolean;
begin
  Result := FDataLink.DataIndepended;
end;

function TCustomDBCheckBoxEh.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TCustomDBCheckBoxEh.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

function TCustomDBCheckBoxEh.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TCustomDBCheckBoxEh.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomDBCheckBoxEh.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TCustomDBCheckBoxEh.GetFieldState: TCheckBoxState;
var
  Text: string;
begin
  if FDatalink.DataIndepended then
  begin
    Result := cbGrayed;
    if VarEquals(FDatalink.DataIndependentValue, True) then
      Result := cbChecked
    else if VarEquals(FDatalink.DataIndependentValue, False) then
      Result := cbUnchecked;
  end else if FDatalink.Field <> nil then
    if (FDataLink.Field.DataType = ftBoolean) and
            (FValueCheck = STextTrue) and
            (FValueUncheck = STextFalse) then
    begin
      if FDataLink.Field.IsNull then
        Result := cbGrayed
      else if FDataLink.Field.AsBoolean then
        Result := cbChecked
      else
        Result := cbUnchecked
    end else
    begin
      Result := cbGrayed;
      Text := FDataLink.Field.Text;
      if ValueMatch(FValueCheck, Text) then Result := cbChecked else
        if ValueMatch(FValueUncheck, Text) then Result := cbUnchecked;
    end
  else
    Result := cbUnchecked;
end;

function TCustomDBCheckBoxEh.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomDBCheckBoxEh.InternalSetState(Value: TCheckBoxState);
begin
  if FState <> Value then
  begin
    FState := Value;
    {$IFDEF FPC_CROSSP}
    {$ELSE}
    if HandleAllocated then
      SendMessage(Handle, BM_SETCHECK, Integer(FState), 0);
    {$ENDIF}
    if not ClicksDisabled then
      inherited Click;
    Invalidate;
    FModified := True;
  end;
end;

procedure TCustomDBCheckBoxEh.InternalUpdateData(Sender: TObject);
begin
  UpdateData;
end;

procedure TCustomDBCheckBoxEh.InternalUpdatePostData;
var
  Pos: Integer;
  S: string;
begin
  if FDataLink.DataIndepended then
  begin
    if State = cbGrayed then
      FDataLink.SetValue(Null)
    else if Checked then
      FDataLink.SetValue(True)
    else
      FDataLink.SetValue(False);
  end else
    if State = cbGrayed then
      FDataLink.Field.Clear
    else
      if FDataLink.Field.DataType = ftBoolean then
        FDataLink.Field.AsBoolean := Checked
      else
      begin
        if Checked then S := FValueCheck else S := FValueUncheck;
        Pos := 1;
        FDataLink.Field.Text := ExtractFieldName(S, Pos);
      end;
end;

procedure TCustomDBCheckBoxEh.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ':
      FDataLink.Edit;
    #27:
      FDataLink.Reset;
  end;
end;

procedure TCustomDBCheckBoxEh.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Char(Key) = ' ') and not FToggleKeyDown then
  begin
    FToggleKeyDown := True;
    Invalidate;
  end;
end;

procedure TCustomDBCheckBoxEh.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if (Char(Key) = ' ') and FToggleKeyDown then
  begin
    FToggleKeyDown := False;
  end;
end;

procedure TCustomDBCheckBoxEh.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and
     (FDataLink <> nil) and
     (AComponent = DataSource)
  then
    DataSource := nil;
end;

procedure TCustomDBCheckBoxEh.Paint;
begin
{$IFDEF FPC}
  {$IFDEF MSWINDOWS}
  if ThemesEnabled then
    ThemeServices.DrawParentBackground(Handle, Canvas.Handle, nil, True)
  else
    PerformEraseBackground(Self, Canvas.Handle);
  {$ELSE}
  Canvas.Brush.Color := Parent.Color;
  Canvas.FillRect(ClientRect);
  {$ENDIF}
{$ELSE}
{$ENDIF} 

  if csPaintCopy in ControlState
    then DrawState(GetFieldState, False, False, False)
    else DrawState(State, Focused, FMouseAboveControl,
           (FMouseAboveControl and (csClicked in ControlState)) or FToggleKeyDown);
end;

procedure TCustomDBCheckBoxEh.DrawState(AState: TCheckBoxState; AFocused, AMouseAboveControl, ADown: Boolean);
var
  CheckRect, TextRect: TRect;
begin
  if (not UseRightToLeftAlignment and (Alignment = taLeftJustify)) or
     ( UseRightToLeftAlignment and (Alignment = taRightJustify)) then
  begin
    Canvas.Brush.Color := Color;
    CheckRect := AdjustCheckBoxRect(Rect(0,0,Width,Height), taRightJustify, tlCenter);
    TextRect := Rect(0, 0, CheckRect.Left - 5, Height);

    DrawCaptionRect(TextRect, AFocused, AMouseAboveControl, ADown);
    DrawCheckBoxRect(CheckRect, AState, AFocused, AMouseAboveControl, ADown);
  end else
  begin
    Canvas.Brush.Color := Color;
    CheckRect := AdjustCheckBoxRect(Rect(0,0,Width,Height), taLeftJustify, tlCenter);
    TextRect := Rect(CheckRect.Right + 5, 0, Width, Height);

    DrawCaptionRect(TextRect, AFocused, AMouseAboveControl, ADown);
    DrawCheckBoxRect(CheckRect, AState, AFocused, AMouseAboveControl, ADown);
  end;
end;

procedure TCustomDBCheckBoxEh.DrawCheckBoxRect(ARect: TRect; AState: TCheckBoxState; AFocused, AMouseAboveControl, ADown: Boolean);
var
  Active: Boolean;
begin
  Active := AMouseAboveControl or AFocused or ( AlwaysShowBorder and not ThemesEnabled );
  PaintButtonControlEh(Canvas, ARect, Color, bcsCheckboxEh,  Ord(ADown),
    Flat, Active, Enabled or (csDesigning in ComponentState), AState);
end;

procedure TCustomDBCheckBoxEh.DrawCaptionRect(ARect: TRect; AFocused, AMouseAboveControl, ADown: Boolean);
var
  TextSize: TSize;
  VTextMarg, HTextMarg: Integer;
  TextRect: TRect;
  Flags: Integer;
  C: TColor;
  BS: TBrushStyle;
begin
  Canvas.Font := Font;
  Canvas.Font.Color := StyleServices.GetSystemColor(Font.Color);
  Canvas.Brush.Color := StyleServices.GetSystemColor(Color);
  BS := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  {$IFDEF FPC}
  Flags := DT_SINGLELINE;
  TextRect := Rect(0,0,0,0);
  {$ELSE}
  if WordWrap then
  begin
    Flags := DT_WORDBREAK;
    TextRect := Rect(0,0,ARect.Right-ARect.Left,0);
  end else
  begin
    Flags := DT_SINGLELINE;
    TextRect := Rect(0,0,0,0);
  end;
  {$ENDIF}
  DrawTextEh(Canvas.Handle, Caption, Length(Caption), TextRect, Flags + DT_CALCRECT);
  TextSize.cx := (TextRect.Right - TextRect.Left);
  TextSize.cy := (TextRect.Bottom - TextRect.Top);
  VTextMarg := Height div 2 - TextSize.cy div 2;
  HTextMarg := ARect.Left;
  TextRect := Rect(HTextMarg, VTextMarg, HTextMarg + TextSize.cx, VTextMarg + TextSize.cy);

  if UseRightToLeftAlignment and (Alignment = taRightJustify) then
    OffsetRect(TextRect, ARect.Right - TextRect.Right, 0);
  if Enabled or (csDesigning in ComponentState) then
    DrawTextEh(Canvas.Handle, Caption, Length(Caption), TextRect, Flags)
  else
  begin
    C := Canvas.Font.Color;
    Canvas.Font.Color := clHighlightText;
    OffsetRect(TextRect, 1, 1);
    DrawTextEh(Canvas.Handle, Caption, Length(Caption), TextRect, Flags);
    OffsetRect(TextRect, -1, -1);
    Canvas.Font.Color := clGrayText;
    DrawTextEh(Canvas.Handle, Caption, Length(Caption), TextRect, Flags);
    Canvas.Font.Color := C;
  end;

  Canvas.Brush.Style := BS;
  InflateRect(TextRect, 1, 1);

  Inc(TextRect.Bottom);
  if TextRect.Left < 0 then TextRect.Left := 0;
  if TextRect.Top < 0 then TextRect.Top := 0;
  if TextRect.Right > Width then TextRect.Right := Width;
  if TextRect.Bottom > Height then TextRect.Bottom := Height;
  if AFocused then
    Canvas.DrawFocusRect(TextRect);
end;

function TCustomDBCheckBoxEh.PostDataEvent: Boolean;
begin
  Result := False;
  FDataPosting := True;
  try
    if Assigned(FOnUpdateData) then FOnUpdateData(Self, Result);
  finally
    FDataPosting := False;
  end;
end;

procedure TCustomDBCheckBoxEh.SetAlignment(const Value: TLeftRight);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWndHandle;
  end;
end;

procedure TCustomDBCheckBoxEh.SetChecked(Value: Boolean);
begin
  if Value then State := cbChecked else State := cbUnchecked;
end;

procedure TCustomDBCheckBoxEh.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TCustomDBCheckBoxEh.SetDataSource(Value: TDataSource);
begin
  if FDataLink.DataSourceFixed and
     ((csLoading in ComponentState) or (csDestroying in ComponentState))
  then
    DoNothing()
  else
    FDataLink.DataSource := Value;

  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TCustomDBCheckBoxEh.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    RecreateWndHandle;
  end;
end;

procedure TCustomDBCheckBoxEh.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TCustomDBCheckBoxEh.SetState(const Value: TCheckBoxState);
begin
  if (csDesigning in ComponentState) and not FDataLink.DataIndepended then Exit;
  if not DataIndepended then DataSource.DataSet.Edit;
  InternalSetState(Value);
  if FDataPosting then Exit;
  try
    UpdateData;
  except
    FDataLink.Reset;
    raise;
  end;
end;

procedure TCustomDBCheckBoxEh.SetValueCheck(const Value: string);
begin
  FValueCheck := Value;
  DataChange(Self);
end;

procedure TCustomDBCheckBoxEh.SetValueUncheck(const Value: string);
begin
  FValueUncheck := Value;
  DataChange(Self);
end;

function TCustomDBCheckBoxEh.IsValueCheckedStored: Boolean;
begin
  Result := (FValueCheck <> STextTrue);
end;

function TCustomDBCheckBoxEh.IsValueUncheckedStored: Boolean;
begin
  Result := (FValueUncheck <> STextFalse);
end;

procedure TCustomDBCheckBoxEh.Toggle;
begin
  if FDataLink.Edit then
  begin
    case State of
      cbUnchecked:
        if AllowGrayed
          then InternalSetState(cbGrayed)
          else InternalSetState(cbChecked);
      cbChecked: InternalSetState(cbUnchecked);
      cbGrayed: InternalSetState(cbChecked);
    end;
    FDataLink.Modified;
    Invalidate;
  end;
end;

procedure TCustomDBCheckBoxEh.UpdateData;
begin
  if not PostDataEvent then
    InternalUpdatePostData;
end;

function TCustomDBCheckBoxEh.UseRightToLeftAlignment: Boolean;
begin
  Result := inherited UseRightToLeftAlignment;
end;

function TCustomDBCheckBoxEh.ValueMatch(const ValueList, Value: string): Boolean;
var
  Pos: Integer;
begin
  Result := False;
  Pos := 1;
  while (Pos <= Length(ValueList)) do
    if NlsCompareText(ExtractFieldName(ValueList, Pos), Value) = 0 then
    begin
      Result := True;
      Break;
    end;
  if not Result then
    if (ValueList = '') or ((Pos = Length(ValueList) + 1) and (ValueList[Pos-1] = ';')) then
      Result := (Value = '');
end;

procedure TCustomDBCheckBoxEh.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDBCheckBoxEh.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDBCheckBoxEh.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      if not (csDesigning in ComponentState) and not Focused then
      begin
        FClicksDisabled := True;
        {$IFDEF FPC_CROSSP}
        {$ELSE}
        Windows.SetFocus(Handle);
        {$ENDIF}
        FClicksDisabled := False;
        if not Focused then Exit;
      end;
    CN_COMMAND:
      if FClicksDisabled then Exit;
  end;

  inherited WndProc(Message);
end;

function TCustomDBCheckBoxEh.GetChecked: Boolean;
begin
  Result := State = cbChecked;
end;

procedure TCustomDBCheckBoxEh.Click;
begin
  Toggle;
  inherited Changed;
end;

procedure TCustomDBCheckBoxEh.CreateWnd;
begin
  inherited CreateWnd;
  {$IFDEF FPC_CROSSP}
  {$ELSE}
  SendMessage(Handle, BM_SETCHECK, Integer(FState), 0);
  {$ENDIF}
end;

function TCustomDBCheckBoxEh.GetControlsAlignment: TAlignment;
begin
  if not UseRightToLeftAlignment then
    Result := taRightJustify
  else
    if FAlignment = taRightJustify then
      Result := taLeftJustify
    else
      Result := taRightJustify;
end;

procedure TCustomDBCheckBoxEh.WMSize(var Message: TWMSize);
begin
  inherited;
  Invalidate;
end;

{$IFDEF FPC}
{$ELSE}
procedure TCustomDBCheckBoxEh.CMCtl3DChanged(var Message: TMessage);
begin
  RecreateWndHandle;
end;

procedure TCustomDBCheckBoxEh.CMDialogChar(var Message: TCMDialogChar);
begin
  if IsAccel(Message.CharCode, Caption) and CanFocus then
  begin
    SetFocus;
    if Focused then Toggle;
    Message.Result := 1;
  end else
    inherited;
end;
{$ENDIF}

procedure TCustomDBCheckBoxEh.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDBCheckBoxEh.CNCommand(var Message: TWMCommand);
begin
  {$IFDEF FPC_CROSSP}
  {$ELSE}
  if Message.NotifyCode = BN_CLICKED then Toggle;
  {$ENDIF}
end;

procedure TCustomDBCheckBoxEh.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Invalidate;
end;

procedure TCustomDBCheckBoxEh.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Invalidate;
end;

procedure TCustomDBCheckBoxEh.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseAboveControl := True;
  Invalidate;
end;

procedure TCustomDBCheckBoxEh.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseAboveControl := False;
  Invalidate;
end;

procedure TCustomDBCheckBoxEh.WMCancelMode(var Message: TMessage);
var
  ButtonDownInControlState: Boolean;
begin
  ButtonDownInControlState := csLButtonDown in ControlState;
  inherited;
  if csCaptureMouse in ControlStyle then
  begin
    MouseCapture := False;
    if ButtonDownInControlState then
      Perform(WM_LBUTTONUP, 0, Integer($FFFFFFFF));
  end;
end;

function TCustomDBCheckBoxEh.IsStateStored: Boolean;
begin
  Result := (DataIndepended and (State <> cbUnchecked));
end;

procedure TCustomDBCheckBoxEh.SetAlwaysShowBorder(const Value: Boolean);
begin
  if FAlwaysShowBorder <> Value then
  begin
    FAlwaysShowBorder := Value;
    Invalidate;
  end;
end;

procedure TCustomDBCheckBoxEh.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TCustomDBCheckBoxEh.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  inherited;
  if (Message.CharCode = VK_ESCAPE) and Modified then
    Message.Result := 1;
end;

function TCustomDBCheckBoxEh.GetModified: Boolean;
begin
  Result := FModified;
end;

procedure TCustomDBCheckBoxEh.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
    {$IFDEF FPC}
    {$ELSE}
    TControlCanvas(FCanvas).UpdateTextFlags;
    {$ENDIF}
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TCustomDBCheckBoxEh.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

procedure TCustomDBCheckBoxEh.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if ThemeServices.ThemesEnabled
    then Message.Result := Perform(CN_CTLCOLORSTATIC, WPARAM(Message.DC),0)
    else inherited;
end;

procedure TCustomDBCheckBoxEh.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ValueChecked', ReadValueChecked, WriteValueChecked, IsValueCheckedStored);
  Filer.DefineProperty('ValueUnchecked', ReadValueUnchecked, WriteValueUnchecked, IsValueUncheckedStored);
end;

procedure TCustomDBCheckBoxEh.ReadValueChecked(Reader: TReader);
begin
  ValueChecked := Reader.ReadString;
end;

procedure TCustomDBCheckBoxEh.WriteValueChecked(Writer: TWriter);
begin
  Writer.WriteString(ValueChecked);
end;

procedure TCustomDBCheckBoxEh.ReadValueUnchecked(Reader: TReader);
begin
  ValueUnchecked := Reader.ReadString;
end;

procedure TCustomDBCheckBoxEh.WriteValueUnchecked(Writer: TWriter);
begin
  Writer.WriteString(ValueUnchecked);
end;

procedure TCustomDBCheckBoxEh.SetDynProps(const Value: TDynVarsEh);
begin
  FDynProps.Assign(Value);
end;

procedure TCustomDBCheckBoxEh.RecreateWndHandle;
begin
  {$IFDEF FPC}
  RecreateWnd(Self);
  {$ELSE}
  RecreateWnd;
  {$ENDIF}
end;

type

  TDBMemoStringsEh = class(TStrings)
  private
    Memo: TCustomDBMemoEh;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetTextStr: string; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure SetTextStr(const Value: string); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

{ TDBMemoStringsEh }

function TDBMemoStringsEh.GetCount: Integer;
{$IFDEF FPC_CROSSP}
begin
  Result := 0;
end;
{$ELSE}
var
  CharIdxOfLine: Integer;
begin
  Result := 0;
  {$IFDEF FPC}
  if Memo.HandleAllocated or (Memo.Text <> '') then
  {$ELSE}
  if Memo.HandleAllocated or (Memo.WindowText <> nil) then
  {$ENDIF}
  begin
    Result := SendMessage(Memo.Handle, EM_GETLINECOUNT, 0, 0);
    CharIdxOfLine := SendMessage(Memo.Handle, EM_LINEINDEX, Result - 1, 0);
    if SendMessage(Memo.Handle, EM_LINELENGTH, CharIdxOfLine, 0) = 0 then
      Dec(Result);
  end;
end;
{$ENDIF} 

function TDBMemoStringsEh.Get(Index: Integer): string;
{$IFDEF FPC_CROSSP}
begin
  Result := '';
end;
{$ELSE}
var
  Text: array[0..4095] of Char;
  StrLen: Integer;
begin
  Word((@Text)^) := Length(Text);
  {$IFDEF EH_LIB_16}
  StrLen := SendMessage(Memo.Handle, EM_GETLINE, Index, LPARAM(@Text));
  {$ELSE}
  {$HINTS OFF}
  StrLen := SendMessage(Memo.Handle, EM_GETLINE, Index, Longint(@Text));
  {$HINTS ON}
  {$ENDIF}
  SetString(Result, Text, StrLen);
end;
{$ENDIF} 

procedure TDBMemoStringsEh.Put(Index: Integer; const S: string);
{$IFDEF FPC_CROSSP}
begin
end;
{$ELSE}
var
  SelStart: Integer;
  LineLen: Integer;
begin
  SelStart := SendMessage(Memo.Handle, EM_LINEINDEX, Index, 0);
  if SelStart >= 0 then
  begin
    LineLen := SendMessage(Memo.Handle, EM_LINELENGTH, SelStart, 0);
    SendMessage(Memo.Handle, EM_SETSEL, SelStart, SelStart + LineLen);
    Memo.SetEditMode;
    {$HINTS OFF}
    SendMessage(Memo.Handle, EM_REPLACESEL, 0, Longint(PChar(S)));
    {$HINTS ON}
    Memo.PutToFieldAfterChange;
  end;
end;
{$ENDIF} 

procedure TDBMemoStringsEh.Insert(Index: Integer; const S: string);
{$IFDEF FPC_CROSSP}
begin
end;
{$ELSE}
var
  SelStart, LineLen: Integer;
  Line: string;
begin
  if Index >= 0 then
  begin
    SelStart := SendMessage(Memo.Handle, EM_LINEINDEX, Index, 0);
    if SelStart >= 0 then
      Line := S + sLineBreak
    else
    begin
      SelStart := SendMessage(Memo.Handle, EM_LINEINDEX, Index - 1, 0);
      if SelStart < 0 then Exit;
      LineLen := SendMessage(Memo.Handle, EM_LINELENGTH, SelStart, 0);
      if LineLen = 0 then Exit;
      Inc(SelStart, LineLen);
      Line := sLineBreak + s;
    end;
    SendMessage(Memo.Handle, EM_SETSEL, SelStart, SelStart);
    Memo.SetEditMode;
    {$HINTS OFF}
    SendMessage(Memo.Handle, EM_REPLACESEL, 0, Longint(PChar(Line)));
    {$HINTS ON}
    Memo.PutToFieldAfterChange;
  end;
end;
{$ENDIF} 

procedure TDBMemoStringsEh.Delete(Index: Integer);
{$IFDEF FPC_CROSSP}
begin
end;
{$ELSE}
const
  Empty: PChar = '';
var
  SelStart, SelEnd: Integer;
  LineLen: Integer;
begin
  SelStart := SendMessage(Memo.Handle, EM_LINEINDEX, Index, 0);
  if SelStart >= 0 then
  begin
    SelEnd := SendMessage(Memo.Handle, EM_LINEINDEX, Index + 1, 0);
    if SelEnd < 0 then
    begin
      LineLen := SendMessage(Memo.Handle, EM_LINELENGTH, SelStart, 0);
      SelEnd := SelStart + LineLen;
    end;
    SendMessage(Memo.Handle, EM_SETSEL, SelStart, SelEnd);
    Memo.SetEditMode;
    {$HINTS OFF}
    SendMessage(Memo.Handle, EM_REPLACESEL, 0, Longint(Empty));
    {$HINTS ON}
    Memo.PutToFieldAfterChange;
  end;
end;
{$ENDIF} 

procedure TDBMemoStringsEh.Clear;
begin
  Memo.Clear;
end;

procedure TDBMemoStringsEh.SetUpdateState(Updating: Boolean);
begin
  {$IFDEF FPC_CROSSP}
  {$ELSE}
  if Memo.HandleAllocated then
  begin
    SendMessage(Memo.Handle, WM_SETREDRAW, Ord(not Updating), 0);
    if not Updating then
    begin   
      Memo.Perform(CM_SHOWINGCHANGED,0,0); 
      Memo.Refresh;
    end;
  end;
  {$ENDIF} 
end;

function TDBMemoStringsEh.GetTextStr: string;
begin
  Result := Memo.Text;
end;

procedure TDBMemoStringsEh.SetTextStr(const Value: string);
{$IFDEF FPC_CROSSP}
begin
end;
{$ELSE}
var
  NewText: string;
begin
  NewText := AdjustLineBreaks(Value);
  if (Length(NewText) <> Memo.GetTextLen) or (NewText <> Memo.Text) then
  begin
    Memo.SetEditMode;
{$IFDEF EH_LIB_12}
    if SendTextMessage(Memo.Handle, WM_SETTEXT, 0, NewText) = 0 then
{$ELSE}
    if SendMessage(Memo.Handle, WM_SETTEXT, 0, StringToLParam(NewText)) = 0 then
{$ENDIF}
      {$IFDEF FPC}
      raise EInvalidOperation.Create('SInvalidMemoSize');
      {$ELSE}
      raise EInvalidOperation.Create(SInvalidMemoSize);
      {$ENDIF}
    Memo.PutToFieldAfterChange;
    Memo.Perform(CM_TEXTCHANGED, 0, 0);
  end;
end;
{$ENDIF} 

{ TCustomDBMemoEh }

constructor TCustomDBMemoEh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 185;
  Height := 89;
  AutoSize := False;
  FWordWrap := True;
  FWantReturns := True;
  FLines := TDBMemoStringsEh.Create;
  TDBMemoStringsEh(FLines).Memo := Self;
  {$IFDEF FPC}
  {$ELSE}
  ParentBackground := False;
  {$ENDIF}
end;

destructor TCustomDBMemoEh.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TCustomDBMemoEh.CreateParams(var Params: TCreateParams);
const
  Alignments: array[Boolean, TAlignment] of DWORD =
    ((ES_LEFT, ES_RIGHT, ES_CENTER),(ES_RIGHT, ES_LEFT, ES_CENTER));
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL
    {$IFDEF FPC}
    , WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL
    {$ELSE}
    {$ENDIF}
  );
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WordWraps[FWordWrap] or ES_MULTILINE or
    Alignments[UseRightToLeftAlignment, FAlignment] or ScrollBar[FScrollBars];
end;

function TCustomDBMemoEh.GetCaretPos: TPoint;
{$IFDEF FPC_CROSSP}
begin
  Result := Point(0, 0);
end;
{$ELSE}
{$IFDEF EH_LIB_12}
var
  SelStart, SelEnd: Integer;
{$ENDIF}
begin
  {$IFDEF EH_LIB_12}
  SendGetIntMessage(Handle, EM_GETSEL, SelStart, SelEnd);
  Result.X := SelStart;
  {$ELSE}
  MessageSendGetSel(Handle, Result.X, Result.Y);
  {$ENDIF}
  Result.Y := SendMessage(Handle, EM_LINEFROMCHAR, Result.X, 0);
  Result.X := Result.X - SendMessage(Handle, EM_LINEINDEX, -1, 0);
end;
{$ENDIF} 

procedure TCustomDBMemoEh.SetCaretPos(const Value: TPoint);
{$IFDEF FPC_CROSSP}
begin
end;
{$ELSE}
var
  CharIdx: Integer;
begin
  CharIdx := SendMessage(Handle, EM_LINEINDEX, Value.y, 0) + Value.x;
  SendMessage(Handle, EM_SETSEL, CharIdx, CharIdx);
end;
{$ENDIF} 

procedure TCustomDBMemoEh.Loaded;
begin
  inherited Loaded;
  Modified := False;
end;

procedure TCustomDBMemoEh.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
end;

procedure TCustomDBMemoEh.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWndHandle;
  end;
end;

function TCustomDBMemoEh.GetWordWrap: Boolean;
begin
  Result := inherited WordWrap;
end;

function TCustomDBMemoEh.IsLinesStored: Boolean;
begin
  Result := DataIndepended;
end;

procedure TCustomDBMemoEh.SetWordWrap(Value: Boolean);
begin
  inherited WordWrap := Value;
end;

procedure TCustomDBMemoEh.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if FWantTabs
    then Message.Result := Message.Result or DLGC_WANTTAB
    else Message.Result := Message.Result and not DLGC_WANTTAB;
  if not FWantReturns then
    Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;

procedure TCustomDBMemoEh.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key = Char(VK_RETURN)) and not FWantReturns then
    Key := #0;
end;

procedure TCustomDBMemoEh.PutToFieldAfterChange;
begin
  if FDataPosting then Exit;
  try
    UpdateData;
  except
    FDataLink.Reset;
    raise;
  end;
end;

procedure TCustomDBMemoEh.SetEditMode;
begin
  if (csDesigning in ComponentState) and not DataIndepended then Exit;
  if not DataIndepended then DataSource.DataSet.Edit;
end;


{ TCustomDBImageEh }

procedure DefaultFormDBImageEhPopupMenu(DBImage: TCustomDBImageEh; PopupMenu: TPopupMenu);
var
  MenuItem: TMenuItem;
begin
  if DBImage.CanModify then
  begin
    MenuItem := TMenuItem.Create(PopupMenu);
    MenuItem.Caption := EhLibLanguageConsts.PictureEditorDialog_Cut;
    MenuItem.OnClick := DBImage.MenuItemCut;
    PopupMenu.Items.Add(MenuItem);
  end;

  MenuItem := TMenuItem.Create(PopupMenu);
  MenuItem.Caption := EhLibLanguageConsts.PictureEditorDialog_Copy;
  MenuItem.OnClick := DBImage.MenuItemCopy;
  PopupMenu.Items.Add(MenuItem);

  if DBImage.CanModify and Clipboard.HasFormat(CF_BITMAP) then
  begin
    MenuItem := TMenuItem.Create(PopupMenu);
    MenuItem.Caption := EhLibLanguageConsts.PictureEditorDialog_Paste;
    MenuItem.OnClick := DBImage.MenuItemPaste;
    PopupMenu.Items.Add(MenuItem);
  end;

  if DBImage.CanModify then
  begin
    MenuItem := TMenuItem.Create(PopupMenu);
    MenuItem.Caption := EhLibLanguageConsts.PictureEditorDialog_Clear;
    MenuItem.OnClick := DBImage.MenuItemDelete;
    PopupMenu.Items.Add(MenuItem);
  end;

  MenuItem := TMenuItem.Create(PopupMenu);
  MenuItem.Caption := '-';
  PopupMenu.Items.Add(MenuItem);

  if DBImage.CanModify then
  begin
    MenuItem := TMenuItem.Create(PopupMenu);
    MenuItem.Caption := EhLibLanguageConsts.PictureEditorDialog_Load;
    MenuItem.OnClick := DBImage.MenuItemLoad;
    PopupMenu.Items.Add(MenuItem);
  end;

  if DBImage.Picture.Graphic <> nil then
  begin
    MenuItem := TMenuItem.Create(PopupMenu);
    MenuItem.Caption := EhLibLanguageConsts.PictureEditorDialog_Save;
    MenuItem.OnClick := DBImage.MenuItemSave;
    PopupMenu.Items.Add(MenuItem);
  end;

  MenuItem := TMenuItem.Create(PopupMenu);
  MenuItem.Caption := '-';
  PopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(PopupMenu);
  MenuItem.Caption := EhLibLanguageConsts.PictureEditorDialog_Reset + ' (Esc)';
  MenuItem.OnClick := DBImage.MenuItemDefaultZoom;
  PopupMenu.Items.Add(MenuItem);
end;

procedure DefaultDBImageEhEditButtonDefaultAction(EditControl: TControl;
    EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh;
    IsMouseDown: Boolean; var Handled: Boolean);
var
  p: TPoint;
  APopupMenu: TPopupMenu;
  DBImageEh: TCustomDBImageEh;
  APicture: TPicture;
begin
  DBImageEh := (EditControl as TCustomDBImageEh);

  if (EditButton.Style in [ebsDropDownEh, ebsAltUpDownEh]) and IsMouseDown then
  begin

    if Assigned(EditButton.DropdownMenu) then
      APopupMenu := EditButton.DropdownMenu
    else if (EditButton.Action = nil) and (DBImageEh.GetPopupMenu <> nil) then
      APopupMenu := DBImageEh.GetPopupMenu
    else
      APopupMenu := nil;

    if Assigned(APopupMenu) then
    begin
      P := TControl(EditButtonControl).ClientToScreen(Point(0, TControl(EditButtonControl).Height));
      if APopupMenu.Alignment = paRight then
        Inc(P.X, TControl(EditButtonControl).Width);
      APopupMenu.Popup(p.X, p.y);
      KillMouseUp(EditButtonControl);
      EditButtonControl.Perform(WM_LBUTTONUP, 0, 0);

      Handled := True;
    end;

  end else if not IsMouseDown and (EditButton.Style = ebsEllipsisEh) then
  begin
    APicture := TPicture.Create;
    APicture.Graphic := DBImageEh.Picture.Graphic;
    try
      if ShowPictureEditDialogEhProg(APicture) then
      begin
        if DBImageEh.DataLink.Edit then
        begin
          DBImageEh.Picture.Graphic := APicture.Graphic;
          DBImageEh.UpdateData(nil);
        end;
      end;
    finally
      APicture.Free;
    end;
  end;
end;

constructor TCustomDBImageEh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle +
    [csOpaque, csReplicatable, csNeedsBorderPaint, csCaptureMouse];
  if not NewStyleControls then
    ControlStyle := ControlStyle + [csFramed];
  Width := 100;
  Height := 100;
  TabStop := True;
  ParentColor := False;
  FPicture := TPictureEh.Create;
  FPicture.OnChange := PictureChanged;
  FBorderStyle := bsSingle;
  FAutoDisplay := True;
  FDataLink := TFieldDataLinkEh.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDynProps := TDynVarsEh.Create(Self);
  DoubleBuffered := True;
  FPicturePlacement := ipReduceFitEh;
  FZoomAllowed := True;
  FZoom := 100;

  FEditButton := CreateEditButton;
  FEditButton.OnChanged := EditButtonChanged;
  FEditButton.OnRefComponentChanged := EditButtonImagesRefComponentNotifyEvent;

  FEditButtonControl := CreateEditButtonControl;
  FEditButtonControl.Parent := Self;

  FSelectionDrawStyle := sdsDefaultEh;

  FControlLabel := TControlLabelEh.Create(Self);
  FControlLabel.FreeNotification(Self);
  FControlLabel.FocusControl := Self;
  FControlLabelLocation := TControlLabelLocationEh.Create(Self);
  FEmptyDataInfo := TDBImageEmptyDataInfoEh.Create(Self);
end;

destructor TCustomDBImageEh.Destroy;
begin
  DataSource := nil;
  FreeAndNil(FPicture);
  FreeAndNil(FDataLink);
  FreeAndNil(FDynProps);
  FreeAndNil(FEditButton);
  FreeAndNil(FEditButtonControl);
  FreeAndNil(FControlLabel);
  FreeAndNil(FControlLabelLocation);
  FreeAndNil(FEmptyDataInfo);
  inherited Destroy;
end;

function TCustomDBImageEh.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCustomDBImageEh.SetDataSource(Value: TDataSource);
begin
  if Value <> DataSource then
  begin
    if FDataLink.DataSourceFixed and
       ((csLoading in ComponentState) or (csDestroying in ComponentState))
    then
      DoNothing()
    else
      FDataLink.DataSource := Value;

    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

function TCustomDBImageEh.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TCustomDBImageEh.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TCustomDBImageEh.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomDBImageEh.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TCustomDBImageEh.SetSelectionDrawStyle(
  const Value: TSelectionDrawStyleEh);
begin
  if FSelectionDrawStyle = Value then
  begin
    FSelectionDrawStyle := Value;
    Invalidate;
  end;
end;

function TCustomDBImageEh.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TCustomDBImageEh.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic is TBitmap then
    Result := TBitmap(FPicture.Graphic).Palette;
end;

procedure TCustomDBImageEh.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then LoadPicture;
  end;
end;

procedure TCustomDBImageEh.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWndHandle;
  end;
end;

procedure TCustomDBImageEh.SetPicture(Value: TPictureEh);
begin
  FPicture.Assign(Value);
end;

function TCustomDBImageEh.IsEmpty: Boolean;
var
  Picture: TPicture;
  DelPicture: Boolean;
begin
  DelPicture := False;
  if (csPaintCopy in ControlState) then
  begin
    if Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
    begin
      Picture := GetPictureForField(FDataLink.Field);
      DelPicture := True;
    end else
      Picture := nil;
  end else
    Picture := Self.Picture;

  if (Picture = nil) or
     (Picture.Graphic = nil) or
     Picture.Graphic.Empty
  then
    Result := True
  else
    Result := False;

  if DelPicture then
    Picture.Free;
end;

procedure TCustomDBImageEh.Paint;
var
  Size: TSize;
  R: TRect;
  S: string;
  DrawPict: TPictureEh;
  Form: TCustomForm;
  Pal: HPalette;
  FitRect: TRect;
  ActualPlacement: TImagePlacementEh;
  ThemDet: TThemedElementDetails;
{$IFDEF EH_LIB_16}
  Style: TCustomStyleServices;
{$ELSE}
  Style: TThemeServices;
{$ENDIF}
begin
{$IFDEF EH_LIB_16}
  Style := StyleServices;
{$ELSE}
  Style := ThemeServices;
{$ENDIF}
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;

  if Focused then
  begin
    if ActualSelectionDrawStyle = sdsThemedEh then
    begin
{$IFDEF EH_LIB_16}
      ThemDet := Style.GetElementDetails(tlGroupHeaderCloseSelected);
{$ELSE}
      ThemDet.Element := teListView;
      ThemDet.Part := 6;
      ThemDet.State := 11;
{$ENDIF}
      Style.DrawElement(Canvas.Handle, ThemDet, ClientRect, nil);
    end else if ActualSelectionDrawStyle = sdsClassicEh then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(ClientRect);
    end;
  end;

  if FPictureLoaded or (csPaintCopy in ControlState) then
  begin
    DrawPict := TPictureEh.Create;
    Pal := 0;
    try
      if (csPaintCopy in ControlState) and
        Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
      begin
        AssignPictureFromImageField(FDataLink.Field, DrawPict);
      end
      else
      begin
        DrawPict.Assign(Picture);
        if Focused and (DrawPict.Graphic <> nil) and (DrawPict.Graphic.Palette <> 0) then
        begin { Control has focus, so realize the bitmap palette in foreground }
          Pal := SelectPalette(Handle, DrawPict.Graphic.Palette, False);
          RealizePalette(Handle);
        end;
      end;
      {if Stretch then
        if (DrawPict.Graphic = nil) or DrawPict.Graphic.Empty then
          FillRect(ClientRect)
        else
          StretchDraw(ClientRect, DrawPict.Graphic)
      else}
      begin
        SetRect(R, 0, 0, DrawPict.Width, DrawPict.Height);
        FitRect := ClientRect;
        ActualPlacement := PicturePlacement;
        if Zoom <> 100 then
        begin
          FitRect := DrawPict.GetDestRect(FitRect, ActualPlacement);
          FitRect := ScaleRect(FitRect, Zoom);
          ActualPlacement := ipFitEh;
        end;
        if (FImagePos.X <> 0) or (FImagePos.Y <> 0) then
          OffsetRect(FitRect, FImagePos.X, FImagePos.Y);
        DrawPict.PaintTo(Canvas, FitRect, ActualPlacement, Point(0, 0), EmptyRect);
      end;
    finally
      if Pal <> 0 then SelectPalette(Canvas.Handle, Pal, True);
      DrawPict.Free;
    end;
  end else
  begin
    Canvas.Font := Self.Font;
    if FDataLink.Field <> nil
      then S := FDataLink.Field.DisplayLabel
      else S := Name;
    S := '(' + S + ')';
    Size := Canvas.TextExtent(S);
    R := ClientRect;
    Canvas.TextRect(R, (R.Right - Size.cx) div 2, (R.Bottom - Size.cy) div 2, S);
  end;
  Form := GetParentForm(Self);
  if (ActualSelectionDrawStyle = sdsFramedEh) and
     (Form <> nil) and
     (Form.ActiveControl = Self) and
      not (csDesigning in ComponentState) and
      not (csPaintCopy in ControlState)
  then
  begin
    Canvas.Brush.Color := clWindowFrame;
    Canvas.FrameRect(ClientRect);
  end;
  if EmptyDataInfo.Showing then
    EmptyDataInfo.PaintEmptyDataInfo;
end;

procedure TCustomDBImageEh.PictureChanged(Sender: TObject);
begin
  if FPictureLoaded then FDataLink.Modified;
  FPictureLoaded := True;
  Invalidate;
end;

procedure TCustomDBImageEh.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (Operation = opRemove) and
       (FDataLink <> nil) and
       (AComponent = DataSource)
    then
      DataSource := nil;
    CheckEditButtonsRemoveNotification(AComponent);
  end;
end;

procedure TCustomDBImageEh.LoadPicture;
begin
  if not FPictureLoaded and (not Assigned(FDataLink.Field) or FDataLink.Field.IsBlob) then
  begin
    AssignPictureFromImageField(FDataLink.Field, Picture);
    ResetZoom;
    ResetPos;
  end;
end;

procedure TCustomDBImageEh.DataChange(Sender: TObject);
begin
  Picture.Graphic := nil;
  FPictureLoaded := False;
  if FAutoDisplay then
    LoadPicture;
  if ControlLabel <> nil then
    ControlLabel.UpdateCaption;
end;

procedure TCustomDBImageEh.UpdateData(Sender: TObject);
begin
  if not DataLink.DataIndepended then
    DataLink.Field.Assign(Picture.Graphic);
end;

procedure TCustomDBImageEh.MenuItemCopy(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TCustomDBImageEh.MenuItemCut(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TCustomDBImageEh.MenuItemPaste(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TCustomDBImageEh.MenuItemLoad(Sender: TObject);
var
  OpenDialog: TOpenPictureDialog;
begin
  OpenDialog := TOpenPictureDialog.Create(Self);
  try
    OpenDialog.Title := SLoadPictureTitle;
    if OpenDialog.Execute then
    begin
      if FDataLink.Edit then
      begin
        Picture.LoadFromFile(OpenDialog.Filename);
        UpdateData(nil);
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TCustomDBImageEh.MenuItemSave(Sender: TObject);
var
  sd: TSavePictureDialog;
begin
  if Picture.Graphic <> nil then
  begin
    sd := TSavePictureDialog.Create(Self);
    try
      sd.Title := SSavePictureTitle;
      sd.DefaultExt := GraphicExtension(TGraphicClass(Picture.Graphic.ClassType));
      sd.Filter := GraphicFilter(TGraphicClass(Picture.Graphic.ClassType));
      if sd.Execute then
        Picture.SaveToFile(sd.Filename);
    finally
      sd.Free;
    end;
  end;
end;

procedure TCustomDBImageEh.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FMouseDownPos := Point(X, Y);
  FImageMouseDownPos := FImagePos;
end;

procedure TCustomDBImageEh.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then
  begin
    TemporaryMoveImageTo(Point(FImageMouseDownPos.X + (X - FMouseDownPos.X),
                         FImageMouseDownPos.Y + (Y - FMouseDownPos.Y)));
  end;
end;

procedure TCustomDBImageEh.MenuItemDelete(Sender: TObject);
begin
  Picture.Graphic := nil;
end;

procedure TCustomDBImageEh.MenuItemDefaultZoom(Sender: TObject);
begin
  ResetZoom;
  ResetPos;
end;

procedure TCustomDBImageEh.CopyToClipboard;
begin
  if Picture.Graphic <> nil then
    Clipboard.Assign(Picture);
end;

procedure TCustomDBImageEh.CutToClipboard;
begin
  if Picture.Graphic <> nil then
    if FDataLink.Edit then
    begin
      CopyToClipboard;
      Picture.Graphic := nil;
    end;
end;

procedure TCustomDBImageEh.PasteFromClipboard;
begin
  if Clipboard.HasFormat(CF_BITMAP) and FDataLink.Edit then
    Picture.Bitmap.Assign(Clipboard);
end;

procedure TCustomDBImageEh.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FBorderStyle = bsSingle then
    if NewStyleControls and Ctl3D
      then Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE
      else Params.Style := Params.Style or WS_BORDER;
  Params.WindowClass.style := Params.WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TCustomDBImageEh.CreateWnd;
begin
  inherited CreateWnd;
  UpdateEditButtonControlList;
  UpdateEditButtonControlsState;
end;

procedure TCustomDBImageEh.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_INSERT:
      if ssShift in Shift then PasteFromClipBoard else
        if ssCtrl in Shift then CopyToClipBoard;
    VK_DELETE:
      if ssShift in Shift then CutToClipBoard;
  end;
end;

procedure TCustomDBImageEh.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    ^X: CutToClipBoard;
    ^C: CopyToClipBoard;
    ^V: PasteFromClipBoard;
    #13: LoadPicture;
    #27:
      begin
        ResetZoom;
        ResetPos;
      end;
  end;
end;

procedure TCustomDBImageEh.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TCustomDBImageEh.ButtonEnabled: Boolean;
begin
  Result := Enabled and Assigned(FDataLink) and FDataLink.Active;
end;

function TCustomDBImageEh.CanModify: Boolean;
begin
  Result := False;
  if ReadOnly then Exit;
  Result := FDatalink.CanModify;
end;

procedure TCustomDBImageEh.CMEnter(var Message: TCMEnter);
begin
  Invalidate; { Draw the focus marker }
  inherited;
end;

procedure TCustomDBImageEh.CMExit(var Message: TCMExit);
begin
  try
    if Assigned(DataSource) and Assigned(DataSource.DataSet) and
       (DataSource.DataSet.State in [dsInsert, dsEdit]) then
      FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  Invalidate; { Erase the focus marker }
  inherited;
end;

procedure TCustomDBImageEh.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if not FPictureLoaded then
    Invalidate;
end;

procedure TCustomDBImageEh.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if TabStop and CanFocus then
    SetFocus;
  inherited;
end;

procedure TCustomDBImageEh.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  LoadPicture;
  inherited;
end;

procedure TCustomDBImageEh.WMCut(var Message: TMessage);
begin
  CutToClipboard;
end;

procedure TCustomDBImageEh.WMCopy(var Message: TMessage);
begin
  CopyToClipboard;
end;

procedure TCustomDBImageEh.WMPaste(var Message: TMessage);
begin
  PasteFromClipboard;
end;

procedure TCustomDBImageEh.WMSize(var Message: TMessage);
begin
  inherited;
  UpdateEditButtonControlList;
  Invalidate;
end;

function TCustomDBImageEh.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    ((FDataLink <> nil) and FDataLink.ExecuteAction(Action));
end;

function TCustomDBImageEh.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    ((FDataLink <> nil) and FDataLink.UpdateAction(Action));
end;

procedure TCustomDBImageEh.SetPicturePlacement(const Value: TImagePlacementEh);
begin
  if FPicturePlacement <> Value then
  begin
    FPicturePlacement := Value;
    Invalidate;
  end;
end;

procedure TCustomDBImageEh.SetDynProps(const Value: TDynVarsEh);
begin
  FDynProps.Assign(Value);
end;

procedure TCustomDBImageEh.SetEditButton(const Value: TEditButtonEh);
begin
  FEditButton.Assign(Value);
end;

procedure TCustomDBImageEh.SetZoom(const Value: Integer);
begin
  if FZoom <> Value then
  begin
    FZoom := Value;
    if FZoom < 0 then FZoom := 0;
    Invalidate;
  end;
end;

procedure TCustomDBImageEh.TemporaryMoveImageTo(AImagePos: TPoint);
begin
  FImagePos := AImagePos;
  Invalidate;
end;

procedure TCustomDBImageEh.TemporaryZoomTo(ZoomPercent: Integer);
begin
  Zoom := ZoomPercent;
  FZoomIsTemporary := True;
end;

procedure TCustomDBImageEh.ResetPos;
begin
  FImagePos := Point(0,0);
  Invalidate;
end;

procedure TCustomDBImageEh.ResetZoom;
begin
  Zoom := 100;
end;

function TCustomDBImageEh.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (Result = nil) and not (csDestroying in Componentstate) then
    Result := GetSystemPopupMenu;
end;

procedure TCustomDBImageEh.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  inherited DoContextPopup(MousePos, Handled);
end;

function TCustomDBImageEh.GetSystemPopupMenu: TPopupMenu;
begin
  if FSystemPopupMenu = nil then
    FSystemPopupMenu := TPopupMenu.Create(Self);
  Result := FSystemPopupMenu;
  FormPopupMenu(Result);
end;

function TCustomDBImageEh.IsPictureStored: Boolean;
begin
  Result := DataLink.DataIndepended;
end;

procedure TCustomDBImageEh.FormPopupMenu(APopupMenu: TPopupMenu);
begin
  APopupMenu.Items.Clear;
  DBImageEhFormPopupMenuProc(Self, APopupMenu);
end;

function TCustomDBImageEh.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result and ZoomAllowed then
  begin
    if WheelDelta > 0
      then TemporaryZoomTo(Zoom+10)
      else TemporaryZoomTo(Zoom-10);
    Result := True;
  end;
end;

function TCustomDBImageEh.ScaleRect(const ARect: TRect; ZoomPercent: Integer): TRect;
begin
  Result := ARect;
  Result.Right := Result.Right * ZoomPercent div 100;
  Result.Bottom := Result.Bottom * ZoomPercent div 100;
end;

function TCustomDBImageEh.CreateEditButton: TEditButtonEh;
begin
  Result := TEditButtonEh.Create(Self);
end;

function TCustomDBImageEh.CreateEditButtonControl: TEditButtonControlEh;
begin
  Result := TEditButtonControlEh.Create(Self);
  Result.ControlStyle := Result.ControlStyle + [csReplicatable];
  Result.Width := 10;
  Result.Height := 17;
  Result.Visible := True;
  Result.Transparent := False;
end;

procedure TCustomDBImageEh.EditButtonChanged(Sender: TObject);
begin
  if not HandleAllocated then Exit;
  UpdateEditButtonControlList;
  UpdateEditButtonControlsState;
  Invalidate;
end;

procedure TCustomDBImageEh.CheckEditButtonsRemoveNotification(
  AComponent: TComponent);

  procedure CheckButtonRemoveNotification(EditButton: TEditButtonEh);
  begin
    if EditButton.Images.NormalImages = AComponent then
      EditButton.Images.NormalImages := nil;
    if EditButton.Images.HotImages = AComponent then
      EditButton.Images.HotImages := nil;
    if EditButton.Images.PressedImages = AComponent then
      EditButton.Images.PressedImages := nil;
    if EditButton.Images.DisabledImages = AComponent then
      EditButton.Images.DisabledImages := nil;
  end;

begin
  if csDestroying in ComponentState then Exit;
  CheckButtonRemoveNotification(EditButton);
end;

procedure TCustomDBImageEh.EditButtonImagesRefComponentNotifyEvent(
  Sender: TObject; RefComponent: TComponent);

  procedure UpdateButtonFreeNotifications(EditButton: TEditButtonEh);
  begin
    if EditButton.Images.NormalImages = RefComponent then
      RefComponent.FreeNotification(Self);
    if EditButton.Images.HotImages = RefComponent then
      RefComponent.FreeNotification(Self);
    if EditButton.Images.PressedImages = RefComponent then
      RefComponent.FreeNotification(Self);
    if EditButton.Images.DisabledImages = RefComponent then
      RefComponent.FreeNotification(Self);
  end;

begin
  Invalidate;
  if RefComponent = nil then Exit;
  UpdateButtonFreeNotifications(EditButton);
end;

procedure TCustomDBImageEh.UpdateEditButtonControlList;
var
  MinButtonHeight: Integer;
begin
  MinButtonHeight := MAXINT;
  EditButtonControlLineRec.ButtonLine := nil;
  EditButtonControlLineRec.EditButtonControl := FEditButtonControl;

  EditButtonControlLineRec.EditButton := FEditButton;
  ResetEditButtonControl(EditButtonControlLineRec, 0, False, 16, MinButtonHeight);
  FEditButtonControl.OnDown := EditButtonDown;
  FEditButtonControl.OnClick := EditButtonClick;
  FEditButtonControl.OnMouseMove := EditButtonMouseMove;
  FEditButtonControl.OnMouseUp := EditButtonMouseUp;
  FEditButtonControl.Tag := 0;
  if MinButtonHeight <> MAXINT then
    FEditButtonControl.SetBounds(ClientWidth-FEditButtonControl.Width-2, 2, FEditButtonControl.Width, MinButtonHeight);
end;

procedure TCustomDBImageEh.UpdateEditButtonControlsState;
var
  AEditButton: TEditButtonEhCracker;
begin
  if not Enabled
    then FEditButtonControl.Enabled := ButtonEnabled
    else FEditButtonControl.Enabled := Self.EditButton.Enabled;

  AEditButton := TEditButtonEhCracker(Self.EditButton);
  AEditButton.FParentDefinedDefaultAction :=
    not Assigned(AEditButton.OnClick) and
    not Assigned(AEditButton.OnDown) and
    (AEditButton.DropDownFormParams.DropDownForm = nil) and
    (AEditButton.DropDownFormParams.DropDownFormClassName = '');
end;

procedure TCustomDBImageEh.EditButtonClick(Sender: TObject);
var
  Handled: Boolean;
begin
  Handled := False;
  if (Sender = FEditButtonControl) then
  begin
    EditButton.Click(Sender, Handled);
    if not Handled and Assigned(FOnButtonClick) then
      FOnButtonClick(Sender, Handled);
  end;
  if not Handled and FDroppedDown and not FNoClickCloseUp and
    (Sender = FEditButtonControl)
  then
    CloseUp(False);

  if not Handled and
     EditButton.DefaultAction
    then
    begin
      DBImageEhEditButtonDefaultActionProc(Self, EditButton,
          FEditButtonControl, False, Handled);
    end;
end;

procedure TCustomDBImageEh.EditButtonDown(Sender: TObject; TopButton: Boolean;
  var AutoRepeat, Handled: Boolean);
begin
  SetFocus;
  Handled := False;
  if (Sender = FEditButtonControl) then
  begin
    if not FEditButtonControl.Enabled then Exit;
    if Assigned(FOnButtonDown) then
      FOnButtonDown(Sender, TopButton, AutoRepeat, Handled);
    if not Handled then
      CheckEditButtonDownForDropDownForm(Self, FDataLink, Field, Text,
        EditButton, FEditButtonControl,
        OnOpenDropDownForm, DropDownFormCallbackProc,
        Handled);
    if not Handled then
    begin
      if EditButton.DefaultAction and
         (@DBImageEhEditButtonDefaultActionProc <> nil)
      then
        DBImageEhEditButtonDefaultActionProc(Self, EditButton,
          FEditButtonControl, True, Handled);
    end;
  end;
end;

procedure TCustomDBImageEh.EditButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TCustomDBImageEh.EditButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  DoClick := (X >= 0) and (X < TControl(Sender).ClientWidth) and
    (Y >= 0) and (Y <= TControl(Sender).ClientHeight);
  if not DoClick then
    FNoClickCloseUp := False;
end;

function TCustomDBImageEh.ActualSelectionDrawStyle: TSelectionDrawStyleEh;
begin
  if (SelectionDrawStyle = sdsThemedEh) and ThemedSelectionEnabled then
    Result := sdsThemedEh
  else if SelectionDrawStyle = sdsDefaultEh then
    Result := sdsFramedEh
  else
    Result := SelectionDrawStyle;
end;

procedure TCustomDBImageEh.ButtonDown(IsDownButton: Boolean);
begin
end;

procedure TCustomDBImageEh.DropDown;
begin
  FEditButtonControl.AlwaysDown := True;
end;

procedure TCustomDBImageEh.CloseUp(Accept: Boolean);
begin
  FEditButtonControl.AlwaysDown := False;
end;

procedure TCustomDBImageEh.DropDownFormCallbackProc(DropDownForm: TCustomForm;
  Accept: Boolean; DynParams: TDynVarsEh; SysParams: TDropDownFormSysParams);
var
  ADataSet: TDataSet;
  Fields: TFieldListEh;
  I: Integer;
  DataSetWasInEditState: Boolean;
  ASysParams: TEditControlDropDownFormSysParams;
  DDFormCallParams: TDropDownFormCallParamsEh;
begin
  FEditButtonControl.AlwaysDown := False;

  ASysParams := TEditControlDropDownFormSysParams(SysParams);
  if ASysParams.FEditButton <> nil then
    DDFormCallParams := ASysParams.FEditButton.DropDownFormParams
  else
    DDFormCallParams := nil;

  try
  try

  if Accept then
  begin
    if (DDFormCallParams.PassParams in [pspFieldValueEh, pspRecordValuesEh]) or
       (DDFormCallParams.AssignBackFieldNames <> '') then
    begin
      ADataSet := FDataLink.DataSet;
      DataSetWasInEditState := False;
      if ADataSet <> nil then
      begin
        DataSetWasInEditState := (ADataSet.State in [dsEdit, dsInsert]);
        if not DataSetWasInEditState then
          ADataSet.Edit;
      end;
      if DDFormCallParams.AssignBackFieldNames <> '' then
      begin
        Fields := TFieldListEh.Create;
        try
          ADataSet.GetFieldList(Fields, DDFormCallParams.AssignBackFieldNames);
          for I := 0 to Fields.Count - 1 do
            TField(Fields[I]).Value := DynParams[TField(Fields[I]).FieldName].Value;
        finally
          Fields.Free;
        end;
      end else
      begin
        
      end;

      if (ADataSet <> nil) and not DataSetWasInEditState then
        ADataSet.Post;
    end;

  end;

  DropDownForm.Hide;
  if DDFormCallParams.SaveFormSize then
  begin
    DDFormCallParams.FormWidth := DropDownForm.Width;
    if DDFormCallParams.OldFormWidth > 0 then
      DropDownForm.Width := DDFormCallParams.OldFormWidth;
    DDFormCallParams.FormHeight := DropDownForm.Height;
    if DDFormCallParams.OldFormHeight > 0 then
      DropDownForm.Height := DDFormCallParams.OldFormHeight;
  end;

  if Assigned(OnCloseDropDownForm) then
    OnCloseDropDownForm(Self, nil, Accept, DDFormCallParams.DropDownForm, DynParams);

  if ASysParams.FEditButton <> nil
    then PostMessage(Handle, WM_USER, WPARAM(Handle), LPARAM(ASysParams.FEditButton))
    else PostMessage(Handle, WM_USER, WPARAM(Handle), LPARAM(Self));

  finally
    DynParams.Free;
    SysParams.Free;
  end;
  except
    TCustomDropDownFormEh(DropDownForm).KeepFormVisible := True;
    Application.HandleException(Self);
    TCustomDropDownFormEh(DropDownForm).KeepFormVisible := False;
  end;

end;

{$IFDEF FPC}
function TCustomDBImageEh.Ctl3D: Boolean;
begin
  Result := True;
end;
{$ENDIF}

procedure TCustomDBImageEh.SetControlLabelParams(const Value: TControlLabelLocationEh);
begin
  FControlLabelLocation.Assign(Value);
end;

function TCustomDBImageEh.GetControlLabelCaption: String;
begin
  if Field <> nil
    then Result := Field.DisplayName
    else Result := Name;
end;

function TCustomDBImageEh.GetControlTextBaseLine: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  {$WARNINGS OFF}
  SaveFont := SelectObject(DC, Font.Handle);
  {$WARNINGS ON}
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
    if Ctl3D {and not Flat} then I := 1 else I := 0;
    I := GetSystemMetrics(SM_CYBORDER) * 2 + I;
  end else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Result := Metrics.tmHeight + I;
end;

procedure TCustomDBImageEh.AdjustLabelBounds;
var
  NewPos: TPoint;
begin
  if FControlLabel = nil then Exit;
  FControlLabelLocation.CalcLabelPosForControl(FControlLabel.Width, FControlLabel.Height, NewPos);
  FControlLabel.SetBounds(NewPos.X, NewPos.Y, FControlLabel.Width, FControlLabel.Height);
end;

procedure TCustomDBImageEh.LabelSpacingChanged;
begin
  if not (csLoading in ComponentState) then
    AdjustLabelBounds;
end;

procedure TCustomDBImageEh.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FControlLabel <> nil then
    FControlLabel.UpdateVisibility;
end;

procedure TCustomDBImageEh.CMBiDiModeChanged(var Message: TMessage);
begin
  if FControlLabel <> nil then
    FControlLabel.BiDiMode := BiDiMode;
end;

procedure TCustomDBImageEh.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FControlLabel = nil then Exit;
  FControlLabel.Parent := AParent;
  FControlLabel.UpdateVisibility;
  FControlLabel.UpdateCaption;
end;

procedure TCustomDBImageEh.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  AdjustLabelBounds;
end;

procedure TCustomDBImageEh.SetBounds(ALeft: Integer; ATop: Integer;
  AWidth: Integer; AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  AdjustLabelBounds;
end;

procedure TCustomDBImageEh.SetEmptyDataInfo(const Value: TDBImageEmptyDataInfoEh);
begin
  FEmptyDataInfo.Assign(Value);
end;

{ TDBImageEmptyDataInfoEh }

constructor TDBImageEmptyDataInfoEh.Create(AControl: TCustomDBImageEh);
begin
  inherited Create(AControl);
end;

destructor TDBImageEmptyDataInfoEh.Destroy;
begin
  inherited Destroy;
end;

function TDBImageEmptyDataInfoEh.GetControlAlignment: TAlignment;
begin
  Result := taCenter;
end;

function TDBImageEmptyDataInfoEh.GetControl: TCustomDBImageEh;
begin
  Result := TCustomDBImageEh(FControl);
end;

function TDBImageEmptyDataInfoEh.ControlIsEmpty: Boolean;
begin
  Result := GetControl.IsEmpty;
end;

function TDBImageEmptyDataInfoEh.GetControlFont: TFont;
begin
  Result := GetControl.Font;
end;

function TDBImageEmptyDataInfoEh.GetControlColor: TColor;
begin
  Result := GetControl.Color;
end;

procedure TDBImageEmptyDataInfoEh.GetDrawRect(var ADrawRect: TRect);
var
  w, h: Integer;
  ts: TSize;
  AText: String;
begin
  AText := Text;
  w := GetControl.ClientWidth; 
  h := GetControl.ClientHeight; 

  GetControl.Canvas.Font := FFont;

  ts := GetControl.Canvas.TextExtent(AText);
  ts.cy := ts.cy + 2; 

  ADrawRect := Bounds((w shr 1) - (ts.cx shr 1), (h shr 1) - (ts.cy shr 1), ts.cx, ts.cy);
end;

procedure TDBImageEmptyDataInfoEh.PaintEmptyDataInfo;
var
  AText: String;
  ADrawRect: TRect;
begin
  AText := Text;

  GetDrawRect(ADrawRect);

  GetControl.Canvas.Font := FFont;
  InflateRect(ADrawRect, 5, 5);
  GetControl.Canvas.Brush.Color := GetPaintColor;
  WriteTextEh(GetControl.Canvas, ADrawRect, True, 6, 6, AText,
    taCenter, tlCenter, False, False, 0, 0, False, True);

  begin
    GetControl.Canvas.Brush.Color := ApproximateColor(GetControl.Canvas.Font.Color, GetControl.Color, 192);
    InflateRect(ADrawRect, 1, 1);
    GetControl.Canvas.FrameRect(ADrawRect);
  end;
end;

{ TCustomDBRadioGroupEh }

constructor TCustomDBRadioGroupEh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLinkEh.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := InternalUpdateData;
  FValues := TStringList.Create;
end;

destructor TCustomDBRadioGroupEh.Destroy;
begin
  Destroying;
  DataSource := nil;
  FreeAndNil(FDataLink);
  FreeAndNil(FValues);
  inherited Destroy;
end;

procedure TCustomDBRadioGroupEh.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
     (FDataLink <> nil) and
     (AComponent = DataSource)
  then
    DataSource := nil;
end;

function TCustomDBRadioGroupEh.UseRightToLeftAlignment: Boolean;
begin
  Result := inherited UseRightToLeftAlignment;
end;

procedure TCustomDBRadioGroupEh.DataChange(Sender: TObject);
begin
  DataChanged;
end;

procedure TCustomDBRadioGroupEh.DataChanged;
var
  AValue: Variant;
  Handled: Boolean;
begin
  if FDataLink.Field <> nil then
  begin

    Handled := False;
    if Assigned(OnGetFieldData) then
    begin
      AValue := Unassigned;
      OnGetFieldData(Self, AValue, Handled);
    end;

    if not Handled then
      AValue := FDataLink.Field.Text;

    InternalSetValue(AValue);
  end
  else if DataIndepended then
  begin
    InternalSetValue(VarToStr(FDataLink.DataIndependentValue));
  end else
  begin
    InternalSetValue('');
  end;
end;

procedure TCustomDBRadioGroupEh.InternalUpdateData(Sender: TObject);
begin
  UpdateData;
end;

procedure TCustomDBRadioGroupEh.UpdateData;
begin
  if not PostDataEvent then
    InternalUpdatePostData;
end;

function TCustomDBRadioGroupEh.PostDataEvent: Boolean;
begin
  Result := False;
  FDataPosting := True;
  try
    if Assigned(FOnUpdateData) then FOnUpdateData(Self, Result);
  finally
    FDataPosting := False;
  end;
end;

procedure TCustomDBRadioGroupEh.InternalUpdatePostData;
begin
  FDataLink.SetText(Value);
end;

function TCustomDBRadioGroupEh.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCustomDBRadioGroupEh.SetDataSource(Value: TDataSource);
begin
  if Value <> DataSource then
  begin
    if FDataLink.DataSourceFixed and
       ((csLoading in ComponentState) or (csDestroying in ComponentState))
    then
      DoNothing()
    else
      FDataLink.DataSource := Value;

    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

function TCustomDBRadioGroupEh.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TCustomDBRadioGroupEh.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TCustomDBRadioGroupEh.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomDBRadioGroupEh.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TCustomDBRadioGroupEh.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TCustomDBRadioGroupEh.GetButtonValue(Index: Integer): string;
begin
  if (Index < FValues.Count) and (FValues[Index] <> '') then
    Result := FValues[Index]
  else if Index < Items.Count then
    Result := Items[Index]
  else
    Result := '';
end;

procedure TCustomDBRadioGroupEh.SetValue(const Value: string);
begin
  if (csDesigning in ComponentState) and not DataIndepended then Exit;
  if not DataIndepended then DataSource.DataSet.Edit;
  InternalSetValue(Value);
  if FDataPosting then Exit;
  try
    UpdateData;
  except
    FDataLink.Reset;
    raise;
  end;
end;

procedure TCustomDBRadioGroupEh.InternalSetValue(const Value: string);
var
  WasFocused: Boolean;
  I, Index: Integer;
begin
  if FValue <> Value then
  begin
    if (csDestroying in ComponentState) then
    begin
      FValue := Value;
      Exit;
    end;

    FInSetValue := True;
    try
    {$IFDEF FPC}
    WasFocused := (ItemIndex > -1) ;
    {$ELSE}
    WasFocused := (ItemIndex > -1) and (Buttons[ItemIndex].Focused);
    {$ENDIF}
      Index := -1;
      for I := 0 to Items.Count - 1 do
        if Value = GetButtonValue(I) then
        begin
          Index := I;
          Break;
        end;
      ItemIndex := Index;
      
      {$IFDEF FPC}
      if WasFocused and (ItemIndex <> -1) then
        {$IFDEF FPC}
          ; 
        {$ELSE}
        Buttons[ItemIndex].SetFocus;
        {$ENDIF}
      {$ELSE}
      if WasFocused and (ItemIndex <> -1) then
        Buttons[ItemIndex].SetFocus;
      {$ENDIF}
    finally
      FInSetValue := False;
    end;
    FValue := Value;
    Change;
  end;
end;

procedure TCustomDBRadioGroupEh.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    if ItemIndex >= 0
      then TRadioButton(Controls[ItemIndex]).SetFocus
      else TRadioButton(Controls[0]).SetFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomDBRadioGroupEh.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;

procedure TCustomDBRadioGroupEh.Click;
begin
  if not FInSetValue then
  begin
    inherited Click;
    if ItemIndex >= 0 then
      Value := GetButtonValue(ItemIndex);
    if FDataLink.Editing then
      FDataLink.Modified;
  end;
end;

procedure TCustomDBRadioGroupEh.SetItems(Value: TStrings);
begin
  Items.Assign(Value);
  DataChange(Self);
end;

procedure TCustomDBRadioGroupEh.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
  DataChange(Self);
end;

procedure TCustomDBRadioGroupEh.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomDBRadioGroupEh.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ': FDataLink.Edit;
    #27: FDataLink.Reset;
  end;
end;

function TCustomDBRadioGroupEh.CanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

function TCustomDBRadioGroupEh.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (DataLink <> nil) and
    DataLink.ExecuteAction(Action);
end;

function TCustomDBRadioGroupEh.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (DataLink <> nil) and
    DataLink.UpdateAction(Action);
end;

procedure TCustomDBRadioGroupEh.SetDynProps(const Value: TDynVarsEh);
begin
  FDynProps.Assign(Value);
end;

function TCustomDBRadioGroupEh.CreateDataLink: TFieldDataLinkEh;
begin
  Result := TFieldDataLinkEh.Create;
end;

function TCustomDBRadioGroupEh.DataIndepended: Boolean;
begin
  Result := FDataLink.DataIndepended;
end;

{$IFDEF FPC}
{$ELSE}
{ TCustomDBRichEditEh }

procedure DefaultDBRichEditEhEditButtonDefaultAction(EditControl: TControl;
    EditButton: TEditButtonEh; EditButtonControl: TEditButtonControlEh;
    IsMouseDown: Boolean; var Handled: Boolean);
var
  RichEditControl: TCustomDBRichEditEh;
  ADropDownFormParams: TDropDownFormCallParamsEhCracker;

  AForm: TCustomForm;
  FDynParamsInteractorItfs: IDynParamsInteractableEh;
  DDParams: TDynVarsEh;
  OutDDParams: TDynVarsEh;
begin
  RichEditControl := (EditControl as TCustomDBRichEditEh);

  if (EditButton.Style in [ebsDropDownEh, ebsAltUpDownEh]) and IsMouseDown then
  begin
    ADropDownFormParams := TDropDownFormCallParamsEhCracker(EditButton.DropDownFormParams);

    ADropDownFormParams.FEditButton := EditButton;
    ADropDownFormParams.FEditButtonControl := EditButtonControl;
    ADropDownFormParams.FEditControl := RichEditControl;
    ADropDownFormParams.FOnOpenDropDownFormProc := RichEditControl.OnOpenDropDownForm;
    ADropDownFormParams.FOnCloseDropDownFormProc := RichEditControl.DropDownFormCloseProc;
    ADropDownFormParams.FDataLink := RichEditControl.FDataLink;
    ADropDownFormParams.FField := RichEditControl.Field;
    ADropDownFormParams.FOnSetVarValueProc := RichEditControl.SetVarValue;
    ADropDownFormParams.FOnGetVarValueProc := RichEditControl.GetVarValue;
    ADropDownFormParams.FOnGetActualDropDownFormProc := RichEditControl.GetDefaultDropDownForm;

    ADropDownFormParams.CheckShowDropDownForm(Handled);
  end else if (EditButton.Style = ebsEllipsisEh) and not IsMouseDown then
  begin
    AForm := TRichEditWinEh.GetGlobalRef;

    if Supports(AForm, IDynParamsInteractableEh, FDynParamsInteractorItfs) then
    begin
      DDParams := TDynVarsEh.Create(nil);
      try
      DDParams.CreateDynVar('', RichEditControl.RtfText);
      FDynParamsInteractorItfs.SetInDynParams(DDParams);

      if TRichEditWinEh.GetGlobalRef.ShowModal = mrOk then
      begin
        FDynParamsInteractorItfs.GetOutDynParams(OutDDParams);
        RichEditControl.RtfText := VarToStr(OutDDParams.Items[0].Value);
      end;
      finally
        DDParams.Free;
      end;
      Handled := True;
    end;
  end;
end;

constructor TCustomDBRichEditEh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  FAutoDisplay := True;
  FDataLink := TFieldDataLinkEh.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;

  FButtonsBox := TEditButtonsBoxEh.Create(Self);
  FButtonsBox.Parent := Self;
  FButtonsBox.Visible := False;
  FButtonsBox.SetBounds(0,0,0,0);
  FButtonsBox.OnDown := EditButtonDown;
  FButtonsBox.OnClick := EditButtonClick;
  FButtonsBox.OnMouseMove := EditButtonMouseMove;
  FButtonsBox.OnMouseUp := EditButtonMouseUp;
  FButtonsBox.OnCreateEditButtonControl := CreateEditButtonControl;

  FEditButtons := CreateEditButtons;
  FEditButtons.OnChanged := EditButtonChanged;
  FEditButtons.OnRefComponentChanged := EditButtonImagesRefComponentNotifyEvent;

  FControlLabel := TControlLabelEh.Create(Self);
  FControlLabel.FreeNotification(Self);
  FControlLabel.FocusControl := Self;
  FControlLabelLocation := TControlLabelLocationEh.Create(Self);
end;

destructor TCustomDBRichEditEh.Destroy;
begin
  DataSource := nil;
  FreeAndNil(FDataLink);
  FreeAndNil(FEditButtons);
  FreeAndNil(FControlLabel);
  FreeAndNil(FControlLabelLocation);
  inherited Destroy;
end;

procedure TCustomDBRichEditEh.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('RtfText', ReadRtfText,
    WriteRtfText, HandleAllocated and (Lines.Count > 0) and DataIndepended);
end;

procedure TCustomDBRichEditEh.ReadLines_Data(Stream: TStream);
begin
  Lines.LoadFromStream(Stream);
end;

procedure TCustomDBRichEditEh.WriteLines_Data(Stream: TStream);
begin
  Lines.SaveToStream(Stream);
end;

procedure TCustomDBRichEditEh.ReadRtfText(Reader: TReader);
begin
  RtfText := Reader.ReadString;
end;

procedure TCustomDBRichEditEh.WriteRtfText(Writer: TWriter);
begin
  Writer.WriteString(RtfText);
end;

procedure TCustomDBRichEditEh.Loaded;
begin
  inherited Loaded;
end;

procedure TCustomDBRichEditEh.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN {or CS_DROPSHADOW};
end;

procedure TCustomDBRichEditEh.CreateWnd;
begin
  Inc(FCreatingWnd);
  try
    inherited CreateWnd;
  finally
    Dec(FCreatingWnd);
  end;
  EditButtonChanged(nil);
  if FInternalRtfText <> '' then
  begin
    LoadMemoFromString(FInternalRtfText);
    FInternalRtfText := '';
  end;
end;

procedure TCustomDBRichEditEh.DestroyWnd;
begin
{$IFDEF EH_LIB_14}
  if (csRecreating in ControlState) then
{$ENDIF}
    FInternalRtfText := RtfText;
  inherited DestroyWnd;
end;

procedure TCustomDBRichEditEh.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
     (FDataLink <> nil) and
     (AComponent = DataSource)
  then
    DataSource := nil;
end;

function TCustomDBRichEditEh.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TCustomDBRichEditEh.BeginEditing: Boolean;
begin
  Result := FDataLink.Editing;
  if not FDataLink.Editing then
  try
    if (FDataLink.Field <> nil) and (FDataLink.Field.IsBlob) then
      FDataSave := FDataLink.Field.AsString;
    Result := FDataLink.Edit;
  finally
    FDataSave := '';
  end;
end;

procedure TCustomDBRichEditEh.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FMemoLoaded then
  begin
    if (Key = VK_DELETE) or
       (Key = VK_BACK) or
       ((Key = VK_INSERT) and (ssShift in Shift)) or
       (((Key = Ord('V')) or (Key = Ord('X'))) and (ssCtrl in Shift)) then
    begin
      if not BeginEditing then
        Key := 0;
    end;
  end;
end;

procedure TCustomDBRichEditEh.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if not DataIndepended then
  begin
    if FMemoLoaded then
    begin
      if (Key >= #32) and
         (FDataLink.Field <> nil) and
          not FDataLink.Field.IsValidChar(Key) then
      begin
        MessageBeep(0);
        Key := #0;
      end;
      case Key of
        ^H, ^I, ^J, ^M, ^V, ^X, #32..High(Char):
          begin
            if not BeginEditing then
              Key := #0;
          end;
        #27:
          FDataLink.Reset;
      end;
    end else
    begin
      if Key = #13 then LoadMemo;
      Key := #0;
    end;
  end;
end;

procedure TCustomDBRichEditEh.Change;
begin
  if FMemoLoaded then
    FDataLink.Modified;
  FMemoLoaded := True;
  inherited Change;
end;

function TCustomDBRichEditEh.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCustomDBRichEditEh.SetDataSource(Value: TDataSource);
begin
  if Value <> DataSource then
  begin
    if FDataLink.DataSourceFixed and
       ((csLoading in ComponentState) or (csDestroying in ComponentState))
    then
      DoNothing()
    else
      FDataLink.DataSource := Value;

    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TCustomDBRichEditEh.SetDynProps(const Value: TDynVarsEh);
begin
  FDynProps.Assign(Value);
end;

function TCustomDBRichEditEh.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TCustomDBRichEditEh.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TCustomDBRichEditEh.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomDBRichEditEh.SetReadOnly(Value: Boolean);
begin
  if FDataLink.ReadOnly <> Value then
  begin
    FDataLink.ReadOnly := Value;
    EditingChange(nil);
  end;
end;

function TCustomDBRichEditEh.GetRtfText: String;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    Lines.SaveToStream(Stream);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TCustomDBRichEditEh.SetRtfText(const Value: String);
begin
  if (csDesigning in ComponentState) and not DataIndepended then Exit;
  if not DataIndepended then DataSource.DataSet.Edit;
  if HandleAllocated
    then LoadMemoFromString(Value)
    else FInternalRtfText := Value;
  if FDataPosting then Exit;
  try
    UpdateData(Self);
  except
    FDataLink.Reset;
    raise;
  end;
end;

function TCustomDBRichEditEh.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCustomDBRichEditEh.LoadMemoFromString(const Data: string);
const
  RTFHeader = '{\rtf';  { Do not localize }
  URTFHeader = '{urtf'; { Do not localize }
var
  Stream: TStringStream;
begin
  if (Pos(RTFHeader, Data) = 1) or (Pos(URTFHeader, Data) = 1) then
  begin
    Stream := TStringStream.Create(Data);
    try
      Lines.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end
  else
    Text := Data;
end;

procedure TCustomDBRichEditEh.LoadMemo;
const
  RTFHeader = '{\rtf';  { Do not localize }
  URTFHeader = '{urtf'; { Do not localize }
var
  Data: string;
  Stream: TStringStream;
begin
  if not FMemoLoaded and Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
  begin
    try
      Data := FDataLink.Field.AsString;
      if (Pos(RTFHeader, Data) = 1) or (Pos(URTFHeader, Data) = 1) then
      begin
        Stream := TStringStream.Create(Data);
        try
          Lines.LoadFromStream(Stream);
        finally
          Stream.Free;
        end;
      end
      else
        Text := Data;
      FMemoLoaded := True;
    except
      on E:EOutOfResources do
        Lines.Text := Format('(%s)', [E.Message]);
    end;
    EditingChange(Self);
  end;
end;

procedure TCustomDBRichEditEh.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    if FDataLink.Field.IsBlob then
    begin
      if FAutoDisplay or (FDataLink.Editing and FMemoLoaded) then
      begin
        { Check if the data has changed since we read it the first time }
        if (FDataSave <> '') and (FDataSave = FDataLink.Field.AsString) then Exit;
        FMemoLoaded := False;
        LoadMemo;
      end else
      begin
        Text := Format('(%s)', [FDataLink.Field.DisplayLabel]);
        FMemoLoaded := False;
      end;
    end else
    begin
      if FFocused and FDataLink.CanModify
        then Text := FDataLink.Field.Text
        else Text := FDataLink.Field.DisplayText;
      FMemoLoaded := True;
    end
  else if not DataIndepended then
  begin
    if csDesigning in ComponentState
      then Text := Name
      else Text := '';
    FMemoLoaded := False;
  end;
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
  if ControlLabel <> nil then
    ControlLabel.UpdateCaption;
end;

function TCustomDBRichEditEh.DataIndepended: Boolean;
begin
  Result := FDataLink.DataIndepended;
end;

procedure TCustomDBRichEditEh.EditingChange(Sender: TObject);
begin
  if DataIndepended
    then inherited ReadOnly := ReadOnly
    else inherited ReadOnly := not (FDataLink.Editing and FMemoLoaded);
end;

procedure TCustomDBRichEditEh.EMSetCharFormat(var Message: TMessage);
begin
  if FCreatingWnd = 0 then
    BeginEditing;
  inherited;
end;

procedure TCustomDBRichEditEh.EMSetParaFormat(var Message: TMessage);
begin
  BeginEditing;
  inherited;
end;

procedure TCustomDBRichEditEh.UpdateData(Sender: TObject);
var
  Stream: TStringStream;
begin
  if DataIndepended then
  else if FDataLink.Field.IsBlob then
  begin
    Stream := TStringStream.Create('');
    try
      Lines.SaveToStream(Stream);
      FDataLink.Field.AsString := Stream.DataString;
    finally
      Stream.Free;
    end;
  end else
    FDataLink.Field.AsString := Text;
end;

procedure TCustomDBRichEditEh.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    FDataLink.Reset;
  end;
end;

procedure TCustomDBRichEditEh.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if not DataIndepended and FDataLink.CanModify and not ReadOnly then
    inherited ReadOnly := False;
end;

procedure TCustomDBRichEditEh.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetFocused(False);
  inherited;
end;

procedure TCustomDBRichEditEh.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then
      LoadMemo;
  end;
end;

procedure TCustomDBRichEditEh.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not FMemoLoaded
    then LoadMemo
    else inherited;
end;

procedure TCustomDBRichEditEh.WMCut(var Message: TMessage);
begin
  if BeginEditing then
    inherited;
end;

procedure TCustomDBRichEditEh.WMPaste(var Message: TMessage);
begin
  if BeginEditing then
    inherited;
end;

procedure TCustomDBRichEditEh.WMClear(var Message: TMessage);
begin
  if BeginEditing then
    inherited;
end;


procedure TCustomDBRichEditEh.WMPaint(var Message: TWMPaint);
begin
  DefaultHandler(Message);
end;

procedure TCustomDBRichEditEh.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;

function TCustomDBRichEditEh.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action);
  if not Result then
    Result := (FDataLink <> nil) and FDataLink.ExecuteAction(Action);
end;

function TCustomDBRichEditEh.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action);
  if not Result then
    Result := (FDataLink <> nil) and FDataLink.UpdateAction(Action);
end;

procedure TCustomDBRichEditEh.SetEditButtons(const Value: TEditButtonsEh);
begin
  FEditButtons.Assign(Value);
end;

procedure TCustomDBRichEditEh.EditButtonClick(Sender: TObject);
var
  Handled: Boolean;
  i: Integer;
begin
  Handled := False;
  if (Sender is TEditButtonControlEh) then
    for i := 0 to Length(FButtonsBox.BtnCtlList) - 1 do
      if (Sender = FButtonsBox.BtnCtlList[i].EditButtonControl) then
        EditButtons[i].Click(Sender, Handled);
  if not Handled then
  begin
    if FDroppedDown and
       not FNoClickCloseUp and
      (Sender = FButtonsBox.BtnCtlList[0].EditButtonControl)
    then
      CloseUp(False)
    else if (@DBRichEditEhEditButtonDefaultActionProc <> nil)
    then
    begin
      for i := 0 to Length(FButtonsBox.BtnCtlList) - 1 do
        if (Sender = FButtonsBox.BtnCtlList[i].EditButtonControl) and
            EditButtons[i].DefaultAction
        then
          DBRichEditEhEditButtonDefaultActionProc(Self, EditButtons[i],
            FButtonsBox.BtnCtlList[i].EditButtonControl, False, Handled);
    end;
  end;
  FNoClickCloseUp := False;
end;

procedure TCustomDBRichEditEh.CloseUp(Accept: Boolean);
begin
  FButtonsBox.BtnCtlList[0].EditButtonControl.AlwaysDown := False;
end;

procedure TCustomDBRichEditEh.EditButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  DoClick := (X >= 0) and (X < TControl(Sender).ClientWidth) and
    (Y >= 0) and (Y <= TControl(Sender).ClientHeight);
  if not DoClick then
    FNoClickCloseUp := False;
end;

procedure TCustomDBRichEditEh.EditButtonDown(Sender: TObject; TopButton: Boolean;
  var AutoRepeat: Boolean; var Handled: Boolean);
var
  i: Integer;
  p: TPoint;
begin
  SetFocus;
  Handled := False;
  if (Sender is TEditButtonControlEh) then
    for i := 0 to Length(FButtonsBox.BtnCtlList) - 1 do
      if (Sender = FButtonsBox.BtnCtlList[i].EditButtonControl) then
      begin
        if Assigned(EditButtons[i].OnDown) then
          EditButtons[i].OnDown(Sender, TopButton, AutoRepeat, Handled);
        if not Handled then
          CheckShowDropDownForm(EditButtons[i], FButtonsBox.BtnCtlList[i].EditButtonControl, Handled);
        if not Handled then
          if Assigned(EditButtons[i].DropdownMenu) then
          begin
            P := TControl(Sender).ClientToScreen(Point(0, TControl(Sender).Height));
            if EditButtons[i].DropdownMenu.Alignment = paRight then
              Inc(P.X, TControl(Sender).Width);
            EditButtons[i].DropdownMenu.Popup(p.X, p.y);
            KillMouseUp(TControl(Sender));
            TControl(Sender).Perform(WM_LBUTTONUP, 0, 0);
          end;
        if not Handled and
           EditButtons[i].DefaultAction and
           (@DBRichEditEhEditButtonDefaultActionProc <> nil)
        then
          DBRichEditEhEditButtonDefaultActionProc(Self, EditButtons[i],
            FButtonsBox.BtnCtlList[i].EditButtonControl, True, Handled);
      end;
end;

procedure TCustomDBRichEditEh.CheckShowDropDownForm(EditButton: TEditButtonEh;
  EditButtonControl: TEditButtonControlEh; var Handled: Boolean);
var
  ADropDownFormParams: TDropDownFormCallParamsEhCracker;
begin
  ADropDownFormParams := TDropDownFormCallParamsEhCracker(EditButton.DropDownFormParams);

  ADropDownFormParams.FEditButton := EditButton;
  ADropDownFormParams.FEditButtonControl := EditButtonControl;
  ADropDownFormParams.FEditControl := Self;
  ADropDownFormParams.FOnOpenDropDownFormProc := OnOpenDropDownForm;
  ADropDownFormParams.FOnCloseDropDownFormProc := DropDownFormCloseProc;
  ADropDownFormParams.FDataLink := FDataLink;
  ADropDownFormParams.FField := Field;
  ADropDownFormParams.FOnSetVarValueProc := SetVarValue;
  ADropDownFormParams.FOnGetVarValueProc := GetVarValue;
  ADropDownFormParams.FOnGetActualDropDownFormProc := nil;

  ADropDownFormParams.CheckShowDropDownForm(Handled);
end;

procedure TCustomDBRichEditEh.EditButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TCustomDBRichEditEh.DropDownFormCloseProc(EditControl: TControl;
  Button: TEditButtonEh; Accept: Boolean; DropDownForm: TCustomForm;
  DynParams: TDynVarsEh);
var
  I: Integer;
begin
  for i := 0 to Length(FButtonsBox.BtnCtlList) - 1 do
    FButtonsBox.BtnCtlList[i].EditButtonControl.AlwaysDown := False;
  if Assigned(OnCloseDropDownForm) then
    OnCloseDropDownForm(EditControl, Button, Accept, DropDownForm, DynParams);
end;

procedure TCustomDBRichEditEh.DropDownFormCallbackProc(
  DropDownForm: TCustomForm; Accept: Boolean; DynParams: TDynVarsEh;
  SysParams: TDropDownFormSysParams);
var
  I: Integer;
begin
  for i := 0 to Length(FButtonsBox.BtnCtlList) - 1 do
    FButtonsBox.BtnCtlList[i].EditButtonControl.AlwaysDown := False;

  DefaultDropDownFormCallbackProc(Self, FDataLink, DropDownForm,
  Accept, DynParams, SysParams,
  SetVarValue, OnCloseDropDownForm);
end;

procedure TCustomDBRichEditEh.GetDefaultDropDownForm(
  var DropDownForm: TCustomForm; var FreeFormOnClose: Boolean);
begin
  DropDownForm := DefaultDBRichEditEhDropDownFormClass.GetGlobalRef;
  if DropDownForm <> nil then
    FreeFormOnClose := False;
end;

procedure TCustomDBRichEditEh.SetVarValue(const VarValue: Variant);
begin
  RtfText := VarValue;
end;

procedure TCustomDBRichEditEh.GetVarValue(var VarValue: Variant);
begin
  VarValue := RtfText;
end;

procedure TCustomDBRichEditEh.SetEditRect;
var
  Loc: TRect;
begin
  if not HandleAllocated then Exit;
  SetRect(Loc, 0, 0, ClientWidth, ClientHeight);
  CalcEditRect(Loc);
  SendStructMessage(Handle, EM_SETRECTNP, 0, Loc);
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TCustomDBRichEditEh.CalcEditRect(var ARect: TRect);
var
  smRes: LRESULT;
begin
  if inherited UseRightToLeftAlignment
    then SetRect(ARect, FButtonsBox.ButtonsWidth, 0, ClientWidth, ClientHeight)
    else SetRect(ARect, 0, 0, ClientWidth - FButtonsBox.ButtonsWidth, ClientHeight);

  if ThemesEnabled and not Ctl3D and (BorderStyle = bsSingle) then
  begin
    smRes := SendMessage(Handle, EM_GETMARGINS, 0, 0);
    ARect.Left := ARect.Left + LoWord(smRes) - 1;
    ARect.Right := ARect.Right - HiWord(smRes) + 1;
  end;
end;

procedure TCustomDBRichEditEh.CreateEditButtonControl(
  var EditButtonControl: TEditButtonControlEh);
begin
  EditButtonControl := TEditButtonControlEh.Create(Self);
  EditButtonControl.ControlStyle := EditButtonControl.ControlStyle + [csReplicatable];
  EditButtonControl.Width := 10;
  EditButtonControl.Height := 17;
  EditButtonControl.Visible := True;
  EditButtonControl.Transparent := False;
end;

function TCustomDBRichEditEh.CreateEditButtons: TEditButtonsEh;
begin
  Result := TEditButtonsEh.Create(Self, TVisibleEditButtonEh);
end;

procedure TCustomDBRichEditEh.EditButtonChanged(Sender: TObject);
begin
  if not HandleAllocated then Exit;
  UpdateEditButtonControlList;
  UpdateEditButtonControlsState;
  SetEditRect;
  Invalidate;
end;

procedure TCustomDBRichEditEh.UpdateEditButtonControlList;
var
  i: Integer;
  AButtonRect: TRect;
begin
  FButtonsBox.BeginLayout;

  FButtonsBox.ButtonsCount := EditButtons.Count;
  FButtonsBox.MaxButtonHeight := ButtonRect.Bottom - ButtonRect.Top;

  for i := 0 to EditButtons.Count - 1 do
    FButtonsBox.BtnCtlList[i].EditButton := EditButtons[i];
  FButtonsBox.EndLayout;

  AButtonRect := ButtonRect;

  if FButtonsBox.ButtonsWidth > 0 then
  begin
    FButtonsBox.SetBounds(AButtonRect.Left, AButtonRect.Top, AButtonRect.Right-AButtonRect.Left, AButtonRect.Bottom-AButtonRect.Top);
    FButtonsBox.Visible := True;
    ShowWindow(FButtonsBox.Handle, SW_SHOWNORMAL);
  end else
  begin
    FButtonsBox.Visible := False;
    ShowWindow(FButtonsBox.Handle, SW_HIDE);
  end;
end;

procedure TCustomDBRichEditEh.UpdateEditButtonControlsState;
var
  i: Integer;
  DefaultActionSet: Boolean;
  EditButton: TEditButtonEhCracker;
begin
  FButtonsBox.BorderActive := True;
  FButtonsBox.UpdateEditButtonControlsState;

  DefaultActionSet := False;

  for i := 0 to EditButtons.Count-1 do
  begin
    if not DefaultActionSet then
    begin
      EditButton := TEditButtonEhCracker(EditButtons[i]);
      EditButton.FParentDefinedDefaultAction :=
        not Assigned(EditButton.OnClick) and
        not Assigned(EditButton.OnDown) and
        (EditButton.DropDownFormParams.DropDownForm = nil) and
        (EditButton.DropDownFormParams.DropDownFormClassName = '');
      DefaultActionSet := EditButton.FParentDefinedDefaultAction;
    end else
      TEditButtonEhCracker(EditButtons[i]).FParentDefinedDefaultAction := False;
  end;
end;

procedure TCustomDBRichEditEh.EditButtonImagesRefComponentNotifyEvent(
  Sender: TObject; RefComponent: TComponent);

  procedure UpdateButtonFreeNotifications(EditButton: TEditButtonEh);
  begin
    if EditButton.Images.NormalImages = RefComponent then
      RefComponent.FreeNotification(Self);
    if EditButton.Images.HotImages = RefComponent then
      RefComponent.FreeNotification(Self);
    if EditButton.Images.PressedImages = RefComponent then
      RefComponent.FreeNotification(Self);
    if EditButton.Images.DisabledImages = RefComponent then
      RefComponent.FreeNotification(Self);
  end;

var
  i: Integer;
begin
  Invalidate;
  if RefComponent = nil then Exit;
  for i := 0 to EditButtons.Count-1 do
    UpdateButtonFreeNotifications(EditButtons[i]);
end;

function TCustomDBRichEditEh.ButtonRect: TRect;
begin
  if NewStyleControls and not Ctl3D and (BorderStyle = bsSingle)
    then Result := Rect(ClientWidth - FButtonsBox.ButtonsWidth - 1, 1, ClientWidth - 1, ClientHeight - 1)
    else Result := Rect(ClientWidth - FButtonsBox.ButtonsWidth, 0, ClientWidth, ClientHeight);
  if inherited UseRightToLeftAlignment then
    OffsetRect(Result, FButtonsBox.ButtonsWidth - ClientWidth, 0);
end;

procedure TCustomDBRichEditEh.SetControlLabelParams(const Value: TControlLabelLocationEh);
begin
  FControlLabelLocation.Assign(Value);
end;

function TCustomDBRichEditEh.GetControlLabelCaption: String;
begin
  if Field <> nil
    then Result := Field.DisplayName
    else Result := Name;
end;

function TCustomDBRichEditEh.GetControlTextBaseLine: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: Windows.TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  {$WARNINGS OFF}
  SaveFont := SelectObject(DC, Font.Handle);
  {$WARNINGS ON}
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
    if Ctl3D {and not Flat} then I := 1 else I := 0;
    I := GetSystemMetrics(SM_CYBORDER) * 2 + I;
  end else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Result := Metrics.tmHeight + I;
end;

procedure TCustomDBRichEditEh.AdjustLabelBounds;
var
  NewPos: TPoint;
begin
  if FControlLabel = nil then Exit;
  FControlLabelLocation.CalcLabelPosForControl(FControlLabel.Width, FControlLabel.Height, NewPos);
  FControlLabel.SetBounds(NewPos.X, NewPos.Y, FControlLabel.Width, FControlLabel.Height);
end;

procedure TCustomDBRichEditEh.LabelSpacingChanged;
begin
  if not (csLoading in ComponentState) then
    AdjustLabelBounds;
end;

procedure TCustomDBRichEditEh.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FControlLabel <> nil then
    FControlLabel.UpdateVisibility;
end;

procedure TCustomDBRichEditEh.CMBiDiModeChanged(var Message: TMessage);
begin
  if FControlLabel <> nil then
    FControlLabel.BiDiMode := BiDiMode;
end;

procedure TCustomDBRichEditEh.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FControlLabel = nil then Exit;
  FControlLabel.Parent := AParent;
  FControlLabel.UpdateVisibility;
  FControlLabel.UpdateCaption;
end;

procedure TCustomDBRichEditEh.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  AdjustLabelBounds;
end;

procedure TCustomDBRichEditEh.SetBounds(ALeft: Integer; ATop: Integer;
  AWidth: Integer; AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  AdjustLabelBounds;
end;

procedure TCustomDBRichEditEh.SetEnableChangeNotification(const Value: Boolean);
var
  EventMask: Longint;
begin
  if Value
    then EventMask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0) or ENM_CHANGE
    else EventMask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0) and not ENM_CHANGE;
  SendMessage(Handle, EM_SETEVENTMASK, 0, EventMask);
end;

procedure TCustomDBRichEditEh.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_LBUTTONDOWN) or
     (Message.Msg = WM_RBUTTONDOWN) or
     (Message.Msg = WM_MBUTTONDOWN) then
  begin
    SetEnableChangeNotification(False);
    try
      inherited WndProc(Message);
    finally
      SetEnableChangeNotification(True);
    end;
  end else
    inherited WndProc(Message);
end;

procedure TCustomDBRichEditEh.Resize;
begin
  inherited Resize;

  if not HandleAllocated then Exit;
  UpdateEditButtonControlList;
end;

{$ENDIF} 

{ TEditButtonsBoxEh }

constructor TEditButtonsBoxEh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csParentBackground];
  Cursor := crArrow;
end;

destructor TEditButtonsBoxEh.Destroy;
begin
  inherited Destroy;
end;

procedure TEditButtonsBoxEh.BeginLayout;
begin
  Inc(FLayoutCount);
end;

procedure TEditButtonsBoxEh.EndLayout;
begin
  Dec(FLayoutCount);
  if FLayoutCount = 0 then
    LayoutChanged;
end;

procedure TEditButtonsBoxEh.LayoutChanged;
begin
  UpdateEditButtonControlList;
end;

procedure TEditButtonsBoxEh.SetBorderActive(const Value: Boolean);
begin
  if FBorderActive <> Value then
  begin
    FBorderActive := Value;
    Invalidate;
  end;
end;

procedure TEditButtonsBoxEh.SetButtonsCount(const Value: Integer);
var
  i: Integer;
  OldEditButtonControlsCount: Integer;
begin
  if Value < Length(BtnCtlList) then
  begin
    for i := Value to Length(BtnCtlList) - 1 do
    begin
      BtnCtlList[i].EditButtonControl.Free;
      BtnCtlList[i].ButtonLine.Free;
    end;
    SetLength(FBtnCtlList, Value);
  end else
  begin
    OldEditButtonControlsCount := Length(BtnCtlList);
    SetLength(FBtnCtlList, Value);
    for i := OldEditButtonControlsCount to Value - 1 do
    begin
      OnCreateEditButtonControl(BtnCtlList[i].EditButtonControl);
      BtnCtlList[i].EditButtonControl.Parent := Self;
      BtnCtlList[i].ButtonLine := TShape.Create(Self);
      BtnCtlList[i].ButtonLine.Parent := Self;
    end;
  end;
end;

function TEditButtonsBoxEh.GetButtonsCount: Integer;
begin
  Result := Length(FBtnCtlList);
end;

procedure TEditButtonsBoxEh.UpdateEditButtonControlList;
var
  i, Indent, MinButtonHeight: Integer;
  ebclr: TEditButtonControlLineRec;
begin
  MinButtonHeight := MAXINT;

  for i := 0 to Length(FBtnCtlList)-1 do
  begin
    ResetEditButtonControl(FBtnCtlList[i], i, Flat, MaxButtonHeight, MinButtonHeight);
    FBtnCtlList[i].EditButtonControl.OnDown := OnDown;
    FBtnCtlList[i].EditButtonControl.OnClick := OnClick;
    FBtnCtlList[i].EditButtonControl.OnMouseMove := OnMouseMove;
    FBtnCtlList[i].EditButtonControl.OnMouseUp := OnMouseUp;
    FBtnCtlList[i].EditButtonControl.Tag := i;
  end;

  FButtonsWidth := 0;
  for i := 0 to Length(FBtnCtlList)-1 do
   begin
    Inc(FButtonsWidth, FBtnCtlList[i].EditButtonControl.Width);
    Inc(FButtonsWidth, FBtnCtlList[i].ButtonLine.Width);
   end;

  if inherited UseRightToLeftAlignment
    then Indent := FButtonsWidth
    else Indent := 0;

  if MinButtonHeight <> MAXINT then
  begin
    for i := 0 to Length(FBtnCtlList)-1 do
    begin
      ebclr := FBtnCtlList[i];
      if inherited UseRightToLeftAlignment then
      begin
        ebclr.EditButtonControl.SetBounds(Indent - ebclr.EditButtonControl.Width, 0, ebclr.EditButtonControl.Width, MinButtonHeight);
        Dec(Indent, ebclr.EditButtonControl.Width);
        ebclr.ButtonLine.SetBounds(Indent - ebclr.ButtonLine.Width, 0, ebclr.ButtonLine.Width, MinButtonHeight);
        Dec(Indent, ebclr.ButtonLine.Width);
      end else
      begin
        ebclr.EditButtonControl.SetBounds(Indent, 0, ebclr.EditButtonControl.Width, MinButtonHeight);
        Inc(Indent, ebclr.EditButtonControl.Width);
        ebclr.ButtonLine.SetBounds(Indent, 0, ebclr.ButtonLine.Width, MinButtonHeight);
        Inc(Indent, ebclr.ButtonLine.Width);
      end;
    end;
  end;

  if Flat and (FButtonsWidth > 0) and not ThemesEnabled then
    Dec(FButtonsWidth);

  FButtonHeight := MinButtonHeight;
end;

procedure TEditButtonsBoxEh.UpdateEditButtonControlsState;
var
  i: Integer;
begin
  if Length(FBtnCtlList) = 0 then Exit;
  for i := 0 to Length(FBtnCtlList) - 1 do
  begin
  if not Enabled
    then FBtnCtlList[i].EditButtonControl.Enabled := False
    else FBtnCtlList[i].EditButtonControl.Enabled := FBtnCtlList[i].EditButton.Enabled;
    FBtnCtlList[i].EditButtonControl.Active := FBorderActive;
    if FBorderActive
      then FBtnCtlList[i].ButtonLine.Pen.Color := clBtnFace
      else FBtnCtlList[i].ButtonLine.Pen.Color := Color;
  end;
end;

procedure TEditButtonsBoxEh.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  inherited;
end;

procedure TEditButtonsBoxEh.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
end;

{$IFDEF FPC}
function TEditButtonsBoxEh.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result := True;
end;
{$ENDIF}

{ TControlLabelEh }

constructor TControlLabelEh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'SubLabel';  { do not localize }
  SetSubComponent(True);
  {$IFDEF FPC}
  AdjustSize;
  {$ELSE}
  AdjustBounds;
  {$ENDIF}
  inherited Visible := False;
  FVisible := False;
  UpdateCaption;
end;

{$IFDEF FPC}
procedure TControlLabelEh.AdjustSize;
begin
  inherited AdjustSize;
  (Owner as IControlLabelOwnerEh).AdjustLabelBounds;
end;
{$ELSE}
procedure TControlLabelEh.AdjustBounds;
begin
  inherited AdjustBounds;
  (Owner as IControlLabelOwnerEh).AdjustLabelBounds;
end;
{$ENDIF}

function TControlLabelEh.IsCaptionStored: Boolean;
begin
  Result := FCaptionStored;
end;

function TControlLabelEh.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;

procedure TControlLabelEh.SetCaption(const Value: TCaption);
begin
  if Value = '' then
  begin
    FCaptionStored := False;
    UpdateCaption;
  end else
  begin
    inherited Caption := Value;
    FCaptionStored := True;
    Invalidate;
  end;
end;

procedure TControlLabelEh.UpdateCaption;
begin
  if not FCaptionStored and (FocusControl <> nil) then
    inherited Caption := (FocusControl as IControlLabelOwnerEh).GetControlLabelCaption;
end;

function TControlLabelEh.GetLeft: Integer;
begin
  Result := inherited Left;
end;

function TControlLabelEh.GetTop: Integer;
begin
  Result := inherited Top;
end;

function TControlLabelEh.GetHeight: Integer;
begin
  Result := inherited Height;
end;

procedure TControlLabelEh.SetHeight(const Value: Integer);
begin
  SetBounds(Left, Top, Width, Value);
end;

function TControlLabelEh.IsWidthStored: Boolean;
begin
  Result := (Visible = True);
end;

procedure TControlLabelEh.Loaded;
begin
  inherited Loaded;
  UpdateParent;
end;

function TControlLabelEh.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TControlLabelEh.SetWidth(const Value: Integer);
begin
  SetBounds(Left, Top, Value, Height);
end;

function TControlLabelEh.IsHeightStored: Boolean;
begin
  Result := (Visible = True);
end;

function TControlLabelEh.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TControlLabelEh.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    UpdateParent;
    UpdateVisibility;
    Invalidate;
  end;
end;

procedure TControlLabelEh.UpdateVisibility;
var
  NewVisible: Boolean;
begin
  if csDesigning in ComponentState then
  begin
    if Visible
      then ControlStyle := ControlStyle - [csNoDesignVisible]
      else ControlStyle := ControlStyle + [csNoDesignVisible]
  end;
  NewVisible := Visible and TControl(Owner).Visible;
  if NewVisible = inherited Visible then
    Perform(CM_VISIBLECHANGED, Ord(NewVisible), 0)
  else
    inherited Visible := NewVisible;
end;

procedure TControlLabelEh.UpdateParent;
begin
  if Visible and
     (FocusControl <> nil) and
     not (csLoading in ComponentState)
  then
    Parent := FocusControl.Parent
  else
    Parent := nil;
end;

{ TControlLabelLocationEh }

constructor TControlLabelLocationEh.Create(AEditControl: TControl);
begin
  inherited Create;
  FEditControl := AEditControl;
  FSpacing := 3;
  FOffset := 0;
end;

destructor TControlLabelLocationEh.Destroy;
begin
  inherited Destroy;
end;

procedure TControlLabelLocationEh.CalcLabelPosForControl(LabelWidth,
  LabelHeight: Integer; var LabelPos: TPoint);
var
  ExtraSpacing: Integer;
begin
  if LabelSpacingBound = sbNearBoundEh then
    ExtraSpacing := 0
  else if Position in [lpAboveLeftEh, lpAboveCenterEh, lpAboveRightEh,
                       lpBelowLeftEh, lpBelowCenterEh, lpBelowRightEh] then
    ExtraSpacing := LabelHeight
  else
    ExtraSpacing := LabelWidth;

  if Position = lpAboveLeftEh then
  begin
    LabelPos.Y := FEditControl.Top - LabelHeight - Spacing + ExtraSpacing;
    LabelPos.X := FEditControl.Left + Offset;

  end else if Position = lpAboveCenterEh then
  begin
    LabelPos.Y := FEditControl.Top - LabelHeight - Spacing + ExtraSpacing;
    LabelPos.X := FEditControl.Left + (FEditControl.Width - LabelWidth) div 2 + Offset;

  end else if Position = lpAboveRightEh then
  begin
    LabelPos.Y := FEditControl.Top - LabelHeight - Spacing + ExtraSpacing;
    LabelPos.X := FEditControl.Left + FEditControl.Width - LabelWidth + Offset;

  end else if Position = lpBelowLeftEh then
  begin
    LabelPos.Y := FEditControl.Top + FEditControl.Height + Spacing + ExtraSpacing;
    LabelPos.X := FEditControl.Left + Offset;

  end else if Position = lpBelowCenterEh then
  begin
    LabelPos.Y := FEditControl.Top + FEditControl.Height + Spacing + ExtraSpacing;
    LabelPos.X := FEditControl.Left + (FEditControl.Width - LabelWidth) div 2 + Offset;

  end else if Position = lpBelowRightEh then
  begin
    LabelPos.Y := FEditControl.Top + FEditControl.Height + Spacing + ExtraSpacing;
    LabelPos.X := FEditControl.Left + FEditControl.Width  - LabelWidth + Offset;

  end else if Position = lpLeftTopEh then
  begin
    LabelPos.Y := FEditControl.Top + Offset;
    LabelPos.X := FEditControl.Left - LabelWidth - Spacing + ExtraSpacing;

  end else if Position = lpLeftTextBaselineEh then
  begin
    LabelPos.Y := FEditControl.Top + (FEditControl as IControlLabelOwnerEh).GetControlTextBaseLine - LabelHeight + Offset;
    LabelPos.X := FEditControl.Left - LabelWidth - Spacing + ExtraSpacing;

  end else if Position = lpLeftCenterEh then
  begin
    LabelPos.Y := FEditControl.Top + (FEditControl.Height - LabelHeight) div 2 + Offset;
    LabelPos.X := FEditControl.Left - LabelWidth - Spacing + ExtraSpacing;

  end else if Position = lpLeftBottomEh then
  begin
    LabelPos.Y := FEditControl.Top + FEditControl.Height - LabelHeight + Offset;
    LabelPos.X := FEditControl.Left - LabelWidth - Spacing + ExtraSpacing;

  end else if Position = lpRightTopEh then
  begin
    LabelPos.Y := FEditControl.Top + Offset;
    LabelPos.X := FEditControl.Left + FEditControl.Width + Spacing + ExtraSpacing;

  end else if Position = lpRightTextBaselineEh then
  begin
    LabelPos.Y := FEditControl.Top + (FEditControl as IControlLabelOwnerEh).GetControlTextBaseLine - LabelHeight + Offset;
    LabelPos.X := FEditControl.Left + FEditControl.Width + Spacing + ExtraSpacing;

  end else if Position = lpRightCenterEh then
  begin
    LabelPos.Y := FEditControl.Top + (FEditControl.Height - LabelHeight) div 2 + Offset;
    LabelPos.X := FEditControl.Left + FEditControl.Width + Spacing + ExtraSpacing;

  end else if Position = lpRightBottomEh then
  begin
    LabelPos.Y := FEditControl.Top + FEditControl.Height - LabelHeight + Offset;
    LabelPos.X := FEditControl.Left + FEditControl.Width + Spacing + ExtraSpacing;
  end;
end;

procedure TControlLabelLocationEh.SetOffset(const Value: Integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    (FEditControl as IControlLabelOwnerEh).LabelSpacingChanged;
  end;
end;

procedure TControlLabelLocationEh.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    (FEditControl as IControlLabelOwnerEh).LabelSpacingChanged;
  end;
end;

procedure TControlLabelLocationEh.SetPosition(const Value: TLabelPositionEh);
begin
  if Value <> Position then
  begin
    FPosition := Value;
    (FEditControl as IControlLabelOwnerEh).LabelSpacingChanged;
  end;
end;

procedure TControlLabelLocationEh.SetLabelSpacingBound(const Value: TSpacingBoundEh);
begin
  if Value <> FLabelSpacingBound then
  begin
    FLabelSpacingBound := Value;
    (FEditControl as IControlLabelOwnerEh).LabelSpacingChanged;
  end;
end;

initialization

  FlatButtonWidth := GetDefaultFlatButtonWidth;

  DBEditEhEditButtonDefaultActionProc := DefaultDBEditEhEditButtonDefaultAction;
  DefaultDBEditEhDropDownFormClass := TMemoEditWinEh;

  DBImageEhFormPopupMenuProc := DefaultFormDBImageEhPopupMenu;
  DBImageEhEditButtonDefaultActionProc := DefaultDBImageEhEditButtonDefaultAction;

  DBEditEhEditButtonDefaultActionProc := DefaultDBEditEhEditButtonDefaultAction;

  {$IFDEF FPC}
  {$ELSE}
  DBRichEditEhEditButtonDefaultActionProc := DefaultDBRichEditEhEditButtonDefaultAction;
  DefaultDBRichEditEhDropDownFormClass := TRichEditWinEh;
  {$ENDIF}
end.
