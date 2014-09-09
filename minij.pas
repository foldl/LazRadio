(******************************************************************************

About (mini)J expression:
    1. data can be addressed by b(bit), u8(byte), u16(word), u32(dword), i8(char), i16(short), i32(int)
    2. (halfly) supported vocabulary:
       =
       <         <.         <:
       >         >.         >:
       +         +.         +:
       *         *.         *:
       -
       %
       ^
       $
       {

*******************************************************************************)

unit MiniJ;

{$mode objfpc}{$H+}{$R-}

interface

uses
  Classes, SysUtils, SuperObject, Contnrs, fgl, Math;

type

  TRealList = specialize TFPGList<Real>;

  TGetVariableEvent = procedure(Sender: TObject; const Name: string; var P: PInt64; var bIsVector: Boolean; var VectorLen: Integer) of object;

  TMiniJTokenType = (jSymbol, jInt, jOp, jOpen, jClose, jEof, jError);
  TTokenTypeSet = set of TMiniJTokenType;

  TMiniJToken = record
    T: TMiniJTokenType;
    Symbol: string;
    Value: Real;
    OpIndex: Integer;
  end;

  TMiniJValueType = (jvScaler, jvVector);

  PMiniJA = ^TMiniJA;
  TMiniJA = record
  case T: TMiniJValueType of
      jvScaler:
        (
          V: Real
        );
      jvVector:
        (
          R: Integer;
          P: array [0..0] of Real;
        )
  end;

  TMiniJDyadic = function(V1: PMiniJA; const Op: string; V2: PMiniJA): PMiniJA of object;
  TMiniJMonadic = function(const Op: string; V: PMiniJA): PMiniJA of object;
  TMiniJThreadDyadic = function(const V1, V2: Real): Real of object;
  TMiniJThreadMonadic = function(const V: Real): Real of object;

  TMiniJVoc = record
    Voc: string;
    Monadic: TMiniJMonadic;
    Dyadic: TMiniJDyadic;
    ThreadDyadic: TMiniJThreadDyadic;
    ThreadMonadic: TMiniJThreadMonadic;
  end;

  TCharSet = set of Char;

  { TMiniJInterpreter }

  TMiniJInterpreter = class
  private
    FOnGetVariable: TGetVariableEvent;
    FSource: string;
    FExpr: string;
    FCursor: Integer;
    FCurToken: TMiniJToken;
    FVocMap: TSuperTableString;
    FSymValue: TSuperTableString;
    FVocabulary: array of TMiniJVoc;
    FValues: TList;
    function AllocA(const VecLen: Integer): PMiniJA;
    procedure Skip(const S: TCharSet);
    procedure Reset;
    function DoNextToken: TMiniJToken;
    function NextToken: TMiniJToken;
    function RequireToken(S: TTokenTypeSet): TMiniJToken;

    function DyEqual(const V1, V2: Real): Real;
    function DyLess(const V1, V2: Real): Real;
    function DyMin(const V1, V2: Real): Real;
    function DyLessEq(const V1, V2: Real): Real;
    function DyLarger(const V1, V2: Real): Real;
    function DyMax(const V1, V2: Real): Real;
    function DyLargerEq(const V1, V2: Real): Real;
    function DyTimes(const V1, V2: Real): Real;
    function DyPlus(const V1, V2: Real): Real;
    function DyMinus(const V1, V2: Real): Real;
    function DyDivide(const V1, V2: Real): Real;
    function DyPower(const V1, V2: Real): Real;
    function DyResidue(const V1, V2: Real): Real;
    function DyFrom(V1: PMiniJA; const Op: string; V2: PMiniJA): PMiniJA;
    function DyAppend(V1: PMiniJA; const Op: string; V2: PMiniJA): PMiniJA;
    function DyCopy(V1: PMiniJA; const Op: string; V2: PMiniJA): PMiniJA;

    function MoFloor(const V: Real): Real;
    function MoDecrement(const V: Real): Real;
    function MoCeiling(const V: Real): Real;
    function MoIncrement(const V: Real): Real;
    function MoReciprocal(const V: Real): Real;
    function MoShape(const Op: string; V: PMiniJA): PMiniJA;
    function MoTally(const Op: string; V: PMiniJA): PMiniJA;
    function MoNeg(const V: Real): Real;

    function DoEvalExpr: PMiniJA;
    function Call(const OpIndex: Integer; V: PMiniJA): PMiniJA; overload;
    function Call(V1: PMiniJA; const OpIndex: Integer; V2: PMiniJA): PMiniJA;
    function Call(V1: PMiniJA; F: TMiniJThreadDyadic; V2: PMiniJA): PMiniJA;
    function Call(F: TMiniJThreadMonadic; V: PMiniJA): PMiniJA;

    function GetValue(const Sym: string): PMiniJA;

  public
    constructor Create;
    destructor Destroy; override;

    function EvalExpr(const Expr: string): PMiniJA;

    property OnGetVariable: TGetVariableEvent read FOnGetVariable write FOnGetVariable;
  end;

implementation

function AllocA(const VecLen: Integer): PMiniJA;
var
  S: Integer;
begin
  if VecLen <= 1 then
  begin
    Result := PMiniJA(GetMem(SizeOf(TMiniJA)));
    FillByte(Result^, SizeOf(TMiniJA), 0);
    Result^.T := jvScaler;
    if VecLen >= 0 then
    begin
      Result^.T := jvVector;
      Result^.R := VecLen;
    end;
  end
  else begin
    S := SizeOf(TMiniJA) + (VecLen - 1) * SizeOf(Int64);
    Result := PMiniJA(GetMem(S));
    FillByte(Result^, SizeOf(TMiniJA), 0);
    Result^.T := jvVector;
    Result^.R := VecLen;
  end;
end;

{ TMiniJInterpreter }

function TMiniJInterpreter.AllocA(const VecLen: Integer): PMiniJA;
begin
  Result := MiniJ.AllocA(VecLen);
  FValues.Add(Result);
end;

procedure TMiniJInterpreter.Skip(const S: TCharSet);
begin
  while (FCursor <= Length(FExpr)) and (FExpr[FCursor] in S) do
    Inc(FCursor);
end;

procedure TMiniJInterpreter.Reset;
var
  P: Pointer;
  I: Integer;
begin
  FSymValue.Clear(True);
  for P in FValues do
    FreeMem(P);
  FValues.Clear;
end;

function TMiniJInterpreter.DoNextToken: TMiniJToken;
var
  I: Integer;
  S: string;
  T: ISuperObject;
begin
  Result.T := jEof;
  Skip([' ', #9, #10, #13]);
  if FCursor > Length(FExpr) then Exit;
  I := FCursor;
  if FExpr[FCursor] in ['a'..'z', 'A'..'Z'] then
  begin
    Result.T := jSymbol;
    Skip(['a'..'z', 'A'..'Z', '_', '0'..'9']);
    Result.Symbol := Copy(FExpr, I, FCursor - I);
  end
  else if FExpr[FCursor] in ['0'..'9'] then
  begin
    Result.T := jInt;
    Skip(['.', '0'..'9']);
    Result.Value := StrToInt(Copy(FExpr, I, FCursor - I));
  end
  else if FExpr[FCursor] in ['_'] then
  begin
    Result.T := jInt;
    Inc(FCursor); Inc(I);
    Skip(['.', '0'..'9']);
    Result.Value := - StrToIntDef(Copy(FExpr, I, FCursor - I), 0);
  end
  else if FExpr[FCursor] in ['(', ')'] then
  begin
    if FExpr[FCursor] = '(' then
      Result.T := jOpen
    else
      Result.T := jClose;
    Inc(FCursor);
  end
  else begin
    Result.T := jOp;
    Result.OpIndex := -1;
    for I := 2 downto 1 do
    begin
      S := Copy(FExpr, FCursor, I);
      if Length(S) <> I then Continue;
      T := FVocMap.O[S];
      if Assigned(T) then
      begin
        Inc(FCursor, I);
        Result.OpIndex := T.AsInteger;
        Result.Symbol  := S;
        Break;
      end;
    end;
    if Result.OpIndex < 0 then
    begin
      Inc(FCursor);
      Result.T := jError;
    end
    else;
  end;
end;

function TMiniJInterpreter.NextToken: TMiniJToken;
begin
  FCurToken := DoNextToken;
  Result := FCurToken;
end;

function TMiniJInterpreter.RequireToken(S: TTokenTypeSet): TMiniJToken;
begin
  Result := NextToken;
  if not (Result.T in S) then raise Exception.Create('RequireToken failed');
end;

function TMiniJInterpreter.DyEqual(const V1, V2: Real): Real;
begin
  if V1 = V2 then Result := 1.0 else Result := 0.0;
end;

function TMiniJInterpreter.DyLess(const V1, V2: Real): Real;
begin
  if V1 < V2 then Result := 1.0 else Result := 0.0;
end;

function TMiniJInterpreter.DyMin(const V1, V2: Real): Real;
var
  I: Integer;
begin
  if V1 < V2 then Result := V1 else Result := V2;
end;

function TMiniJInterpreter.DyLessEq(const V1, V2: Real): Real;
begin
  if V1 <= V2 then Result := 1.0 else Result := 0.0;
end;

function TMiniJInterpreter.DyLarger(const V1, V2: Real): Real;
begin
  if V1 > V2 then Result := 1.0 else Result := 0.0;
end;

function TMiniJInterpreter.DyMax(const V1, V2: Real): Real;
begin
  if V1 > V2 then Result := V1 else Result := V2;
end;

function TMiniJInterpreter.DyLargerEq(const V1, V2: Real): Real;
var
  I: Integer;
begin
  if V1 >= V2 then Result := 1.0 else Result := 0.0;
end;

function TMiniJInterpreter.DyTimes(const V1, V2: Real): Real;
begin
  Result := V1 * V2;
end;

function TMiniJInterpreter.DyPlus(const V1, V2: Real): Real;
begin
  Result := V1 + V2;
end;

function TMiniJInterpreter.DyMinus(const V1, V2: Real): Real;
begin
  Result := V1 - V2;
end;

function TMiniJInterpreter.DyDivide(const V1, V2: Real): Real;
begin
  Result := V1 / V2;
end;

function TMiniJInterpreter.DyPower(const V1, V2: Real): Real;
begin
  Result := power(V1, V2);
end;

function TMiniJInterpreter.DyResidue(const V1, V2: Real): Real;
var
  Q: Integer;
begin
  Q := Trunc(V1 / V2);
  Result := V1 - V2 * Q;
end;

function TMiniJInterpreter.DyFrom(V1: PMiniJA; const Op: string; V2: PMiniJA
  ): PMiniJA;
var
  I: Integer;

  function Get(F: Real): Real;
  var
    X: Integer;
  begin
    X := Round(F) mod V2^.R;
    Result := V2^.P[X];
  end;

begin
  if V2^.T <> jvVector then raise Exception.Create('} vector required');
  if V2^.R <= 0 then raise Exception.Create('rank <= 0');
  if V1^.T = jvScaler then
  begin
    Result := AllocA(-1);
    Result^.P[0] := Get(V1^.P[0]);
  end
  else begin
    Result := AllocA(V1^.R);
    for I := 0 to V1^.R - 1 do
      Result^.P[I] := Get(V1^.P[1]);
  end;
end;

function TMiniJInterpreter.DyAppend(V1: PMiniJA; const Op: string; V2: PMiniJA
  ): PMiniJA;
var
  I: Integer;
begin
  case V1^.T of
    jvScaler:
      case V2^.T of
        jvScaler:
          begin
            Result := AllocA(2);
            Result^.P[0] := V1^.P[0];
            Result^.P[1] := V2^.P[0];
          end;
        jvVector:
          begin
            Result := AllocA(V2^.R + 1);
            Result^.P[0] := V1^.P[0];
            for I := 0 to V2^.R - 1 do
              Result^.P[I + 1] := V2^.P[I];
          end;
      end;
    jvVector:
      case V2^.T of
        jvScaler:
          begin
            Result := AllocA(V1^.R + 1);
            Result^.P[0] := V2^.P[0];
            for I := 0 to V1^.R - 1 do
              Result^.P[I + 1] := V1^.P[I];
          end;
        jvVector:
          begin
            Result := AllocA(V1^.R + V2^.R);
            for I := 0 to V1^.R - 1 do
              Result^.P[I] := V1^.P[I];
            for I := 0 to V2^.R - 1 do
              Result^.P[I + V1^.R] := V2^.P[I];
          end;
      end;
  end;
end;

function TMiniJInterpreter.DyCopy(V1: PMiniJA; const Op: string; V2: PMiniJA
  ): PMiniJA;
var
  I: Integer;
  L: Integer = 0;
  T: Integer;
begin
  if (V1^.T <> jvVector) or (V2^.T <> jvVector) then raise Exception.Create('# vector required');
  if V1^.R <> V2^.R then  raise Exception.Create('} vector rank not eq');
  for I := 0 to V1^.R - 1 do
  begin
    T := Round(V1^.P[I]);
    if T > 0 then Inc(L, T);
  end;
  Result := AllocA(L);
  L := 0;
  for I := 0 to V1^.R - 1 do
  begin
    T := Round(V1^.P[I]);
    while T > 0 do
    begin
      Dec(T);
      Result^.P[L] := V2^.P[I];
      Inc(L);
    end;
  end;
end;

function TMiniJInterpreter.MoFloor(const V: Real): Real;
begin
  Result := floor(V);
end;

function TMiniJInterpreter.MoDecrement(const V: Real): Real;
begin
  Result := V - 1;
end;

function TMiniJInterpreter.MoCeiling(const V: Real): Real;
begin
  Result := ceil(V);
end;

function TMiniJInterpreter.MoIncrement(const V: Real): Real;
begin
  Result := V + 1;
end;

function TMiniJInterpreter.MoReciprocal(const V: Real): Real;
begin
  Result := 1/ V;
end;

function TMiniJInterpreter.MoShape(const Op: string; V: PMiniJA): PMiniJA;
begin
  if V^.T = jvScaler then
  begin
    Result := AllocA(0)
  end
  else begin
    Result := AllocA(1);
    Result^.P[0] := V^.R;
  end;
end;

function TMiniJInterpreter.MoTally(const Op: string; V: PMiniJA): PMiniJA;
begin
  Result := AllocA(-1);
  if V^.T = jvScaler then
    Result^.P[0] := 1
  else
    Result^.P[0] := V^.R;
end;

function TMiniJInterpreter.MoNeg(const V: Real): Real;
begin
  Result := -V;
end;

function TMiniJInterpreter.DoEvalExpr: PMiniJA;
var
  T1: TMiniJToken;
  T2: TMiniJToken;
  V1: PMiniJA;
  V2: PMiniJA;
  Left: PMiniJA;
  function GetConst: PMiniJA;
  var
    L: TRealList;
    I: Integer;
  begin
    L := TRealList.Create;
    L.Add(T1.Value);
    T1 := RequireToken([jInt, jOp, jClose, jEof]);
    while T1.T = jInt do
    begin
      L.Add(T1.Value);
      T1 := NextToken;;
    end;
    if L.Count > 1 then
      Result := AllocA(L.Count)
    else
      Result := AllocA(-1);
    for I := 0 to L.Count - 1 do Result^.P[I] := L[I];
    L.Free;
  end;

begin
  T1 := NextToken;
  case T1.T of
    jSymbol:
      begin
        V1 := GetValue(T1.Symbol);
        T2 := RequireToken([jOp, jClose, jEof]);
        if T2.T in [jEof, jClose] then
        begin
          Result := V1;
          Exit;
        end;
        Result := Call(V1, T2.OpIndex, DoEvalExpr());
      end;
    jInt:
      begin
        V1 := GetConst;
        T2 := FCurToken;
        if T2.T in [jEof, jClose] then
        begin
          Result := V1;
          Exit;
        end;
        V2 := DoEvalExpr();
        Result := Call(V1, T2.OpIndex, V2);
      end;
    jOp:
      begin
        Result := Call(T1.OpIndex, DoEvalExpr());
      end;
    jOpen:
      begin
        V1 := DoEvalExpr();
        if FCurToken.T <> jClose then raise Exception.Create(') missing');
        T2 := RequireToken([jOp, jClose, jEof]);
        if T2.T in [jEof, jClose] then
        begin
          Result := V1;
          Exit;
        end;
        Result := Call(V1, T2.OpIndex, DoEvalExpr());
      end;
    jClose:
      begin
        raise Exception.Create('DoEvalExpr: jClose');
      end;
    jEof:
      begin
        raise Exception.Create('DoEvalExpr: jEof');
      end;
    jError:
      begin
        raise Exception.Create('DoEvalExpr: jError');
      end;
  end;
end;

function TMiniJInterpreter.Call(const OpIndex: Integer; V: PMiniJA): PMiniJA;
begin
  if Assigned(FVocabulary[OpIndex].ThreadMonadic) then
    Result := Call(FVocabulary[OpIndex].ThreadMonadic, V)
  else if Assigned(FVocabulary[OpIndex].Monadic) then
    Result := FVocabulary[OpIndex].Monadic(FVocabulary[OpIndex].Voc, V)
  else
    raise Exception.Create('unsupported monadic ' + FVocabulary[OpIndex].Voc);
end;

function TMiniJInterpreter.Call(V1: PMiniJA; const OpIndex: Integer; V2: PMiniJA
  ): PMiniJA;
begin
  if Assigned(FVocabulary[OpIndex].ThreadDyadic) then
    Result := Call(V1, FVocabulary[OpIndex].ThreadDyadic, V2)
  else if Assigned(FVocabulary[OpIndex].Dyadic) then
    Result := FVocabulary[OpIndex].Dyadic(V1, FVocabulary[OpIndex].Voc, V2)
  else
    raise Exception.Create('unsupported dyadic ' + FVocabulary[OpIndex].Voc);
end;

function TMiniJInterpreter.Call(V1: PMiniJA; F: TMiniJThreadDyadic; V2: PMiniJA
  ): PMiniJA;
var
  I: Integer;
begin
  case V1^.T of
    jvScaler:
      case V2^.T of
        jvScaler:
          begin
            Result := AllocA(-1);
            Result^.P[0] := F(V1^.P[0], V2^.P[0]);
          end;
        jvVector:
          begin
            Result := AllocA(V2^.R);
            for I := 0 to V2^.R - 1 do
            begin
              Result^.P[I] := F(V1^.P[0], V2^.P[I]);
            end;
          end;
      end;
    jvVector:
      case V2^.T of
        jvScaler:
          begin
            Result := AllocA(V1^.R);
            for I := 0 to V1^.R - 1 do
            begin
              Result^.P[I] := F(V1^.P[I], V2^.P[0]);
            end;
          end;
        jvVector:
          begin
            if V1^.R <> V2^.R then raise Exception.Create('length error');
            Result := AllocA(V2^.R);
            for I := 0 to V2^.R - 1 do
            begin
              Result^.P[I] := F(V1^.P[I], V2^.P[I]);
            end;
          end;
      end;
  end;
end;

function TMiniJInterpreter.Call(F: TMiniJThreadMonadic; V: PMiniJA): PMiniJA;
var
  I: Integer;
begin
  case V^.T of
    jvScaler:
      begin
        Result := AllocA(-1);
        Result^.P[0] := F(V^.P[0]);
      end;
    jvVector:
      begin
        Result := AllocA(V^.R);
        for I := 0 to V^.R - 1 do
        begin
          Result^.P[I] := F(V^.P[I]);
        end;
      end;
  end;
end;

function TMiniJInterpreter.GetValue(const Sym: string): PMiniJA;
var
  P: PInt64;
  bIsVector: Boolean;
  VectorLen: Integer;
begin
  Result := PMiniJA(Pointer(FSymValue.I[Sym]));
  if Result = nil then
  begin
    FOnGetVariable(Self, Sym, P, bIsVector, VectorLen);
    if not bIsVector then VectorLen := -1;
    Result := AllocA(VectorLen);
    if not bIsVector then VectorLen := 1;
    while VectorLen >= 1 do
    begin
      Dec(VectorLen);
      Result^.P[VectorLen] := P[VectorLen];
    end;
    FSymValue.I[Sym] := PtrInt(Result);
    Freemem(P);
  end;
end;

constructor TMiniJInterpreter.Create;
const
  all: string = '= < <. <: > >. >: + * - % ^ | , $ { #';
var
  L: TStringList;
  I: Integer;
begin
  FValues := TList.Create;
  FSymValue := TSuperTableString.Create;
  FVocMap := TSuperTableString.Create;
  L := TStringList.Create;
  L.StrictDelimiter := True;
  L.Delimiter := ' ';
  L.DelimitedText := all;
  SetLength(FVocabulary, L.Count);
  for I := 0 to L.Count - 1 do
  begin
    FVocabulary[I].Voc := L[I];
    FVocabulary[I].Voc := L[I];
    FVocMap.I[L[I]] := I;
  end;

  with FVocabulary[FVocMap.I['=']] do
  begin
    ThreadDyadic := @DyEqual;
  end;

  with FVocabulary[FVocMap.I['<']] do
  begin
    ThreadDyadic := @DyLess;
  end;

  with FVocabulary[FVocMap.I['<.']] do
  begin
    ThreadDyadic := @DyMin;
    ThreadMonadic := @MoFloor;
  end;

  with FVocabulary[FVocMap.I['<:']] do
  begin
    ThreadMonadic := @MoDecrement;
    ThreadDyadic := @DyLessEq;
  end;

  with FVocabulary[FVocMap.I['>']] do
  begin
    ThreadDyadic := @DyLarger
  end;

  with FVocabulary[FVocMap.I['>.']] do
  begin
    ThreadMonadic := @MoCeiling;
    ThreadDyadic := @DyMax;
  end;

  with FVocabulary[FVocMap.I['>:']] do
  begin
    ThreadMonadic := @MoIncrement;
    ThreadDyadic := @DyLargerEq;
  end;

  with FVocabulary[FVocMap.I['+']] do
  begin
    ThreadDyadic := @DyPlus;
  end;

  with FVocabulary[FVocMap.I['*']] do
  begin
    ThreadDyadic := @DyTimes;
  end;

  with FVocabulary[FVocMap.I['-']] do
  begin
    ThreadMonadic := @MoNeg;
    ThreadDyadic := @DyMinus;
  end;

  with FVocabulary[FVocMap.I['%']] do
  begin
    ThreadMonadic := @MoReciprocal;
    ThreadDyadic := @DyDivide;
  end;

  with FVocabulary[FVocMap.I['^']] do
  begin
    ThreadDyadic := @DyPower;
  end;

  with FVocabulary[FVocMap.I['$']] do
  begin
    Monadic := @MoShape;
  end;

  with FVocabulary[FVocMap.I['|']] do
  begin
    ThreadDyadic := @DyResidue;
  end;

  with FVocabulary[FVocMap.I[',']] do
  begin
    Dyadic := @DyAppend;
  end;

  with FVocabulary[FVocMap.I['{']] do
  begin
    Dyadic := @DyFrom;
  end;

  with FVocabulary[FVocMap.I['#']] do
  begin
    Monadic := @MoTally;
    Dyadic := @DyCopy;
  end;

  L.Free;
end;

destructor TMiniJInterpreter.Destroy;
begin
  Reset;
  FValues.Free;
  FSymValue.Free;
  FVocMap.Free;
  inherited Destroy;
end;

function TMiniJInterpreter.EvalExpr(const Expr: string): PMiniJA;
var
  T: TMiniJToken;
begin
  Reset;
  FExpr := Expr;
  FCursor := 1;
  Result := DoEvalExpr;
  if FCurToken.T <> jEof then raise Exception.Create('error');
end;

end.

