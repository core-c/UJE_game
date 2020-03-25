unit uThreadMove;
interface
uses Windows, Classes, uEntities,
     ExtCtrls;

type
  AEntity = array of TEntity;
  PAEntity = ^AEntity;

  TThreadMove = class(TThread)
  private
    busy,
    fPaused: boolean;
    //ITimer: TTimer;
    Units: PAEntity;
    PerformanceFreq, PerformanceCount, LastPerformanceCount: Int64;
    procedure SetPaused(const Value: boolean);
  protected
    procedure Move;
    procedure Execute; override;
  published
    property Paused: boolean   read fPaused write SetPaused;
  public
    constructor Create(const aUnits: PAEntity);
    destructor Destroy; override;
  end;

implementation

{ TThreadMove }
constructor TThreadMove.Create(const aUnits: PAEntity);
begin
(*
  ITimer := TTimer.Create(nil);
  ITimer.Interval := 10;
  ITimer.OnTimer := Interpolate;
*)
  fPaused := false;
  busy := false;
  Units := aUnits;
  QueryPerformanceFrequency(PerformanceFreq);
  QueryPerformanceCounter(LastPerformanceCount);
  inherited Create(False);
end;

destructor TThreadMove.Destroy;
begin
(*
  ITimer.Enabled := false;
  ITimer.Free;
  ITimer := nil;
*)
  inherited;
end;

procedure TThreadMove.SetPaused(const Value: boolean);
begin
  fPaused := Value;
end;

procedure TThreadMove.Execute;
var code: integer;
    success: boolean;
begin
  inherited;
  //
(*
  ITimer := TTimer.Create(nil);
  ITimer.Interval := 10;
  ITimer.OnTimer := Interpolate;
  ITimer.Enabled := true;
*)

  repeat
//    QueryPerformanceCounter(PerformanceCount);
//    Interval := PerformanceCount - LastPerformanceCount;
//  Duration := (Interval/PerformanceFreq);   //tijdsduur in seconden
//    if Duration >= 0.050 then begin

      if not fPaused then
        if not busy then
          Synchronize(Move);

//      LastPerformanceCount := PerformanceCount;
//    end;
    Sleep(1);
  until Terminated;
  CheckThreadError(success);
  if not success then
    CheckThreadError(code);
  //
end;

procedure TThreadMove.Move;
var Entity: TEntity;
    u: integer;
    Interval: Int64;
    Duration: single;
begin
  if Terminated then Exit;
  busy := true;

  // bewegingen/rijden
  for u:=Low(Units^) to High(Units^) do begin
    Entity := Units^[u];
    if Entity.canMove then
      {if not (Entity as TEntityMover).isMoving then  //! nog fixen..}
        if (Entity as TEntityMover).hasAI_move then
          if (Entity as TEntityMover).useAI_move then
            // laat deze entity bewegen..
            (Entity as TEntityMover).AI_move;
  end;

  busy := false;
end;

end.
 