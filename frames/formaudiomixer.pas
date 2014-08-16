unit formaudiomixer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, RadioSystem, RadioMessage, RadioModule, Math;

type

  TStreamCtrl = record
    Output: TComboBox;
    Vol: TTrackBar;
    Bass: TTrackBar;
    Treble: TTrackBar;
    VolGain: Integer;
    VolLabel: TLabel;
    BassLabel: TLabel;
    TrebleLabel: TLabel;
  end;

  { TAudioMixerForm }

  TAudioMixerForm = class(TForm)
    Bevel1: TBevel;
    OutputSel: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ScrollBox1: TScrollBox;
    VolTrack: TTrackBar;
    BassTrack: TTrackBar;
    TrebleTrack: TTrackBar;
    procedure BassTrackChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OutputSelChange(Sender: TObject);
    procedure TrebleTrackChange(Sender: TObject);
    procedure VolTrackChange(Sender: TObject);
    procedure VolTrackClick(Sender: TObject);
  private
    FChannelNumber: Integer;
    FLastStreamNo: Integer;
    FModule: TRadioModule;
    FStreamCtrls: array of TStreamCtrl;
    procedure SendStreamNumMessage;
    procedure CreateCtrls;
    procedure SetModule(AValue: TRadioModule);
    function  TryGetTag(Sender: TObject): Integer;
  public

    procedure ShowUI(ChannelNumber: Integer);
    procedure ConfigChannel(const Index: Integer; const MixMethod: Integer;
              TotalGain, BassGain, TrebleGain: Integer);   // gain in tenth dB

    property Module: TRadioModule read FModule write SetModule;
  end;

var
  AudioMixerForm: TAudioMixerForm;

implementation

uses
  util_math;

{$R *.lfm}

{ TAudioMixerForm }

procedure TAudioMixerForm.FormShow(Sender: TObject);
begin
  if Length(FStreamCtrls) <> FChannelNumber then CreateCtrls;
end;

procedure TAudioMixerForm.OutputSelChange(Sender: TObject);
var
  I, V: Integer;
begin
  V := Max(0, (Sender as TComboBox).ItemIndex);
  I := TryGetTag(Sender);
  // SendStreamNumMessage;
  if I >= 0 then
    RadioPostMessage(RM_AUDIOMIXER_SET_STREAM_OUPUT, I, V, FModule);
end;

procedure TAudioMixerForm.TrebleTrackChange(Sender: TObject);
var
  I: Integer;
begin
  I := TryGetTag(Sender);
  if I >= 0 then
  begin
    FStreamCtrls[I].TrebleLabel.Caption := Format('Treble'#13'%ddB', [(Sender as TTrackBar).Position]);
    RadioPostMessage(RM_AUDIOMIXER_SET_STREAM_TREBLE_GAIN, I, (Sender as TTrackBar).Position * 10, FModule);
  end;
end;

procedure TAudioMixerForm.VolTrackChange(Sender: TObject);
var
  I, V: Integer;
begin
  I := TryGetTag(Sender);
  V := (Sender as TTrackBar).Position;
  if V = 0 then Exit;
  (Sender as TTrackBar).Position := 0;
  if I >= 0 then
  begin
    FStreamCtrls[I].VolLabel.Caption := Format('Vol'#13'%ddB', [FStreamCtrls[I].VolGain]);
    Inc(FStreamCtrls[I].VolGain, V);
    RadioPostMessage(RM_AUDIOMIXER_SET_STREAM_TOTAL_GAIN, I, FStreamCtrls[I].VolGain * 10, FModule);
  end;
end;

procedure TAudioMixerForm.VolTrackClick(Sender: TObject);
begin

end;

procedure TAudioMixerForm.SendStreamNumMessage;
var
  I: Integer;
  J: Integer = 0;
begin
  if not Assigned(FModule) then Exit;
  for I := 0 to High(FStreamCtrls) do
  begin
    if FStreamCtrls[I].Output.ItemIndex > AUDIOMIXER_STREAM_OUTPUT_OFF then J := I + 1;
  end;
  if J <> FLastStreamNo then
  begin
    FLastStreamNo := J;
    RadioPostMessage(RM_AUDIOMIXER_CFG, AUDIOMIXER_STREAM_NUM, FLastStreamNo, FModule);
  end;
end;

procedure TAudioMixerForm.BassTrackChange(Sender: TObject);
var
  I: Integer;
begin
  I := TryGetTag(Sender);
  if I >= 0 then
  begin
    FStreamCtrls[I].BassLabel.Caption := Format('Bass'#13'%ddB', [(Sender as TTrackBar).Position]);
    RadioPostMessage(RM_AUDIOMIXER_SET_STREAM_BASS_GAIN, I, (Sender as TTrackBar).Position * 10, FModule);
  end;
end;

procedure TAudioMixerForm.CreateCtrls;
const
  WAYS: array [0..8] of string = (
  'OFF',
  'IQ -> IQ',
  'QI -> QI',
  'I+Q -> I',
  'I+Q -> Q',
  'I -> I',
  'I -> Q',
  'Q -> I',
  'Q -> Q');
var
  ALeft: Integer = 0;
  I: Integer;
  Bevel: TBevel;

  function AddLable(const T, L: Integer; const Cap: string): TLabel;
  var
    Title: TLabel;
  begin
    Title := TLabel.Create(ScrollBox1);
    Title.Top := T;
    Title.Left := ALeft + L;
    Title.Caption := Cap;
    Title.Parent := ScrollBox1;
    Result := Title;
  end;

begin
  Width := 130 * FChannelNumber + 20;
  while ScrollBox1.ControlCount > 0 do
    ScrollBox1.Controls[0].Free;
  SetLength(FStreamCtrls, FChannelNumber);
  for I := 0 to FChannelNumber - 1 do
  begin
    with FStreamCtrls[I] do
    begin
      AddLable(16, 40, Format('Channel %d', [I + 1]));

      Output := TComboBox.Create(ScrollBox1);
      Output.Parent := ScrollBox1;
      Output.Style := csDropDownList;
      Output.Items.AddStrings(WAYS);
      Output.Top := 40;
      Output.Left := ALeft + 16;
      Output.OnChange := @OutputSelChange;
      Output.ItemIndex := AUDIOMIXER_STREAM_OUTPUT_OFF;
      Output.Tag := I;

      Vol := TTrackBar.Create(ScrollBox1);
      with Vol do
      begin
        Orientation := trVertical;
        Parent := ScrollBox1;
        Height := 120;
        Max := 20;
        Min := -20;
        Frequency := 4;
        Top := 80;
        Left := ALeft + 16;
        Reversed := True;
        OnClick := @VolTrackChange;
        Tag := I;
      end;

      Bass := TTrackBar.Create(ScrollBox1);
      with Bass do
      begin
        Orientation := trVertical;
        Parent := ScrollBox1;
        Height := 120;
        Max := 10;
        Min := -10;
        Frequency := 2;
        Top := 80;
        Left := ALeft + 48;
        Reversed := True;
        OnClick := @BassTrackChange;
        Tag := I;
      end;

      Treble := TTrackBar.Create(ScrollBox1);
      with Treble do
      begin
        Orientation := trVertical;
        Parent := ScrollBox1;
        Height := 120;
        Max := 10;
        Min := -10;
        Frequency := 2;
        Top := 80;
        Left := ALeft + 80;
        Reversed := True;
        OnClick := @TrebleTrackChange;
        Tag := I;
      end;

      VolLabel := AddLable(200, 16, 'Vol');
      BassLabel := AddLable(200, 48, 'Bass');
      TrebleLabel := AddLable(200, 80, 'Treble');

      if I = FChannelNumber - 1 then Break;

      Bevel := TBevel.Create(ScrollBox1);
      Bevel.Parent := ScrollBox1;
      Bevel.Left := ALeft + 128;
      Bevel.Top := 16;
      Bevel.Height := 208;
      Bevel.Width := 2;

      Inc(ALeft, 130);
    end;
  end;
end;

procedure TAudioMixerForm.SetModule(AValue: TRadioModule);
begin
  if FModule = AValue then Exit;
  FModule := AValue;
end;

function TAudioMixerForm.TryGetTag(Sender: TObject): Integer;
begin
  Result := -1;
  if not Assigned(FModule) then Exit;
  if not (Sender is TComponent) then Exit;
  Result := (Sender as TComponent).Tag;
end;

procedure TAudioMixerForm.ShowUI(ChannelNumber: Integer);
begin
  FChannelNumber := ChannelNumber;
  Show;
end;

procedure TAudioMixerForm.ConfigChannel(const Index: Integer;
  const MixMethod: Integer; TotalGain, BassGain, TrebleGain: Integer);
begin
  if (Index < 0) or (Index > High(FStreamCtrls)) then Exit;
  with FStreamCtrls[Index] do
  begin
    Output.ItemIndex := MixMethod;
    VolGain := (TotalGain + 5) div 10;
    Bass.Position := (BassGain + 5) div 10;
    Treble.Position := (TrebleGain + 5) div 10;
    TrebleLabel.Caption := Format('Treble'#13'%ddB', [Bass.Position]);
    BassLabel.Caption := Format('Bass'#13'%ddB', [Treble.Position]);
    VolLabel.Caption := Format('Vol'#13'%ddB', [VolGain]);
  end;
end;

end.

