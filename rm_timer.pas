unit rm_timer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule, ExtCtrls, radiomessage;

type

  PTimerNode = ^TTimerNode;
  TTimerNode = record
    Next: PTimerNode;
    Id: PtrUInt;
    Obj: TTimer;
  end;

  { TRadioTimer }

  TRadioTimer = class(TRadioModule)
  private
    FHead: TTimerNode;
    procedure Lock;
    procedure Unlock;
    procedure OnTimer(Sender: TObject);
    // *lock* required when call FindTimer
    function  FindTimer(const AId: PtrUInt): PTimerNode;
  protected
    procedure ProccessCustomMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure CreateTimer(const AId: PtrUInt; const AInterval: Cardinal);
    procedure DeleteTimer(const AId: PtrUInt);

    procedure Describe(Strs: TStrings); override;
  end;

implementation

uses
  RadioSystem;

{ TRadioTimer }

procedure TRadioTimer.Lock;
begin
  RadioGlobalLock
end;

procedure TRadioTimer.Unlock;
begin
  RadioGlobalUnlock;
end;

procedure TRadioTimer.OnTimer(Sender: TObject);
var
  P: PTimerNode;
  T: TTimer;
  M: TRadioMessage;
begin
  T := Sender as TTimer;
  P := PTimerNode(Pointer(T.Tag));
  with M do
  begin
    Id := radiomessage.RM_TIMER;
    Sender := Name;
    ParamH := P^.Id and $FFFF;
    ParamL := T.Interval;
  end;
  RadioPostMessage(M, Cardinal(P^.Id shr 32));
end;

function TRadioTimer.FindTimer(const AId: PtrUInt): PTimerNode;
begin
  Result := FHead.Next;
  while Assigned(Result) and (Result^.Id <> Id) do Result := Result^.Next;
end;

procedure TRadioTimer.ProccessCustomMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  case Msg.Id of
    RM_CREATE_TIMER:
      begin
        CreateTimer(Msg.ParamH, Cardinal(Msg.ParamL));
        GraphInvalidate;
      end;
    RM_DELETE_TIMER:
      begin
        DeleteTimer(Msg.ParamH);
        GraphInvalidate;
      end
    else
      inherited;
  end;
end;

procedure TRadioTimer.CreateTimer(const AId: PtrUInt; const AInterval: Cardinal
  );
label
  Quit;
var
  P: PTimerNode;
begin
  if AInterval = 0 then Exit;
  Lock;
  P := FindTimer(Id);
  if Assigned(P) then goto Quit;
  New(P);
  P^.Id := Id;
  P^.Next := FHead.Next;
  FHead.Next := P;
  P^.Obj := TTimer.Create(nil);
  with P^.Obj do
  begin
    Interval := AInterval;
    OnTimer := @Self.OnTimer;
    Tag := PtrInt(P);
    Enabled := True;
  end;
Quit:
  Unlock;
end;

procedure TRadioTimer.DeleteTimer(const AId: PtrUInt);
var
  P: PTimerNode;
begin
  Lock;
  P := FindTimer(Id);
  if Assigned(P) then
  begin
    P^.Obj.Free;
    FHead.Next := P^.Next;
    Dispose(P);
  end;
  Unlock;
end;

procedure TRadioTimer.Describe(Strs: TStrings);
begin
  if Assigned(FHead.Next) then
    Strs.Add('^bTimers running')
  else
    Strs.Add('^bNo timer');
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioTimer.ClassType));

end.

