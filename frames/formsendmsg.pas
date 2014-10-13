unit formsendmsg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, superobject, RadioSystem, RadioMessage;

type

  { TSendMsgForm }

  TSendMsgForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ModuleName: TComboBox;
    MsgId: TComboBox;
    ParamH: TComboBox;
    ParamL: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MsgIdChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ParamHChange(Sender: TObject);
  private
    FMsgDoc: ISuperObject;
    procedure SetMsgDoc(AValue: ISuperObject);
    procedure LoadArray(A: TSuperArray; Combo: TComboBox);
    function  GetValue(Combo: TComboBox): PtrUInt;
    function  FindItemByName(const PossibleArray: ISuperObject; const AName: string): ISuperObject;
  public
    property MsgDoc: ISuperObject read FMsgDoc write SetMsgDoc;
  end;

var
  SendMsgForm: TSendMsgForm;

implementation

uses
  util_math, util_config;

{$R *.lfm}

{ TSendMsgForm }

procedure TSendMsgForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSendMsgForm.FormCreate(Sender: TObject);
begin
  MsgDoc := TSuperObject.ParseFile(GetResFullName('messages.json'), False);
end;

procedure TSendMsgForm.MsgIdChange(Sender: TObject);
var
  T: ISuperObject;
  I: Integer;
begin
  if MsgId.ItemIndex < 0 then Exit;
  ParamH.Clear;
  T := FindItemByName(FMsgDoc, MsgId.Text);
  if Assigned(T) then
  begin
    T := T.O['paramh.vals'];
    if Assigned(T) and T.IsType(stArray) then
      LoadArray(T.AsArray, ParamH);
  end;
end;

procedure TSendMsgForm.OKButtonClick(Sender: TObject);
var
  Id: Integer;
begin
  if (ModuleName.ItemIndex < 0) or (MsgId.ItemIndex < 0) then
  begin
    MessageDlg('Error', 'Please choose module and message.',  mtError, [mbOK], 0);
    Exit;
  end;
  Id := Integer(MsgId.Items.Objects[MsgId.ItemIndex]);

  if Id < RM_AUDIO_IN_START then
  begin
    if MessageDlg('Warning', 'You have chosen to send a common (system level) message, which is STRONGLY not recommended.'#10#10 +
                             'Sure to proceed?',  mtWarning, mbYesNo, 0) <> mrYes then
      Exit;
  end;

  if (ParamH.Text = '') or (ParamL.Text = '') then
  begin
    MessageDlg('Error', 'Please fill in ParamH/ParamL.',  mtError, [mbOK], 0);
    Exit;
  end;
  RadioPostMessage(Id,
                   GetValue(ParamH),
                   GetValue(ParamL),
                   ModuleName.Text);
end;

procedure TSendMsgForm.ParamHChange(Sender: TObject);
var
  T: ISuperObject;
begin
  if ParamH.ItemIndex < 0 then Exit;

  ParamL.Clear;
  T := FindItemByName(FMsgDoc, MsgId.Text);
  if Assigned(T) then
  begin
    T := FindItemByName(T.O['paramh.vals'], ParamH.Text);
    if Assigned(T) then
    begin
      T := T.O['paraml.vals'];
      if Assigned(T) and T.IsType(stArray) then
        LoadArray(T.AsArray, ParamL);
    end;
  end;
end;

procedure TSendMsgForm.SetMsgDoc(AValue: ISuperObject);
var
  I: Integer;
begin
  if FMsgDoc = AValue then Exit;
  FMsgDoc := AValue;
  MsgId.Clear;
  if not Assigned(FMsgDoc) then Exit;
  if not FMsgDoc.IsType(stArray) then FMsgDoc := nil;
  LoadArray(FMsgDoc.AsArray, MsgId);
end;

procedure TSendMsgForm.LoadArray(A: TSuperArray; Combo: TComboBox);
var
  I: Integer;
begin
  for I := 0 to A.Length - 1 do
  begin
    Combo.AddItem(A.O[I].S['name'], TObject(Pointer(A.O[I].I['id'])));
  end;
end;

function TSendMsgForm.GetValue(Combo: TComboBox): PtrUInt;
begin
  if Combo.ItemIndex >= 0 then
    Result := PtrUInt(Combo.Items.Objects[Combo.ItemIndex])
  else begin
    Result := AtoI(Combo.Text);
  end;
end;

function TSendMsgForm.FindItemByName(const PossibleArray: ISuperObject;
  const AName: string): ISuperObject;
var
  A: TSuperArray;
  I: Integer;
begin
  Result := nil;
  if not Assigned(PossibleArray) then Exit;
  if not PossibleArray.IsType(stArray) then Exit;
  A := PossibleArray.AsArray;

  for I := 0 to A.Length - 1 do
  begin
    if A.O[I].S['name'] = AName then
    begin
      Result := A.O[I];
      Break;
    end;
  end;
end;

end.

