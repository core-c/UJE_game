unit uThreadConstruct;
interface
uses Classes, uEntities, uGameTech;

type
  TConstructingInfo = record
    HUDIndex,                       //
    ButtonSetIndex,                 //
    ButtonIndex: integer;           // de index van de knop (van het te maken ding) in de buttonset voor deze entity (-1 = nvt)
    AssignedEntity: PEntity;

    qpcStart, qpcEnd: Int64;        // queryperformancecounters  tbv. bouwen-progressie
    Percentage: integer;
    qpcLastBlink: Int64;            // queryperformancecounter   tbv. knipperen

    Done: boolean;                  // deze knop is klaar, en doet verder niks meer
    ToggledOn: boolean;             // de knop in-/uit-gedrukt status

    Queued: integer;                // het aantal nog te bouwen

    MarkRemove: boolean;            // markering: "wis deze entry"
  end;



  TThreadConstruct = class(TThread)
  private
    busy,
    fPaused: boolean;
    PerformanceFreq, PerformanceCount, LastPerformanceCount: Int64;
    AConstructingInfo : array of TConstructingInfo;
    TheGame: PEntityGame;
    TheGameTech: PGameTech;
    procedure SetPaused(const Value: boolean);
  protected
    procedure Construct;
    procedure Execute; override;
  published
    property Paused: boolean   read fPaused write SetPaused;
  public
    constructor Create(const aGame: PEntityGame;
                       const aGameTech: PGameTech);
    destructor Destroy; override;
    //
    procedure RemoveMarkedEntries;
    procedure Add(const aHUDIndex,
                        aButtonSetIndex,
                        aButtonIndex: integer;
                  aAssignedEntity: PEntity);
    procedure Cancel(const aHUDIndex,
                           aButtonSetIndex,
                           aButtonIndex: integer);
    //
    function GetPercentage(const aHUDIndex,
                                 aButtonSetIndex,
                                 aButtonIndex: integer;
                           aAssignedEntity: PEntity) : integer;
  end;



  
implementation
uses Windows;

constructor TThreadConstruct.Create(const aGame: PEntityGame;
                                    const aGameTech: PGameTech);
begin
  fPaused := false;
  busy := false;
  SetLength(AConstructingInfo, 0);
  QueryPerformanceFrequency(PerformanceFreq);
  TheGame := aGame;
  TheGameTech := aGameTech;
  inherited Create(False);
end;

destructor TThreadConstruct.Destroy;
begin
  SetLength(AConstructingInfo, 0);
  inherited;
end;

procedure TThreadConstruct.SetPaused(const Value: boolean);
begin
  fPaused := Value;
end;


procedure TThreadConstruct.Execute;
var code: integer;
    success: boolean;
begin
  inherited;
  repeat
    if not fPaused then
      if not busy then
        Synchronize(Construct);
    Sleep(1);
  until Terminated;
  CheckThreadError(success);
  if not success then
    CheckThreadError(code);
end;


procedure TThreadConstruct.Construct;
var c: integer;
    Button: TEntityButton;
    Tech: TTech;
begin
  if Terminated then Exit;
  
  for c:=Low(AConstructingInfo) to High(AConstructingInfo) do
    with AConstructingInfo[c] do begin
      // de knop
      Button := TheGame^.HUD[HUDIndex].ButtonSets[ButtonSetIndex].Buttons[ButtonIndex];
      Tech := TheGameTech^.TechTree[Button.TechIndex].Tech;

      // toggle ingedrukt/uitgedrukt
      if Button.Toggle then begin
        ToggledOn := not ToggledOn;
        Percentage := 0;
        Done := Button.Once;
        // "druk" de knop IN of UIT
        TheGame^.ConstructingEntity := AssignedEntity;
        if ToggledOn then begin
          if Assigned(Button.OnDown) then Button.OnDown(self); //Button.DoClick;
        end else
          if Assigned(Button.OnUp) then Button.OnUp(self); //Button.DoClick;
        // wis deze entry
        MarkRemove := true;
        //
        Continue;
      end;

      // constructie maken:
      // is de knop al klaar met bouwen??
      if Button.Ready>=1 then begin
        // knipperen nu aan/uit??
        if Button.Blinking then begin
          QueryPerformanceCounter(LastPerformanceCount);
          if (LastPerformanceCount-qpcLastBlink) > 0.25*PerformanceFreq then begin
            qpcLastBlink := LastPerformanceCount;
            Button.BlinkingOn := not Button.BlinkingOn;
          end;
        end;
      end else begin

        if (Button.QueueCount>0) and (Queued=0) and (Button.Ready=0) then begin
          MarkRemove := true;
        end else begin
          QueryPerformanceCounter(LastPerformanceCount);
          Percentage := Round((LastPerformanceCount - qpcStart) / (qpcEnd - qpcStart) * 100.0);  //floor round..
          Button.Percentage := Percentage;
          if Percentage >= 100 then begin
            // constructie is klaar:
            Button.Percentage := 100;

            // "druk" op de knop
(*
TheGame^.isConstructing := false;
            TheGame^.ConstructingEntity := AssignedEntity;
            if Assigned(Button.OnUp) then Button.OnUp(self); //Button.DoClick;
*)
            // laat de knop knipperen..
            Button.Ready := Button.Ready + 1;
            QueryPerformanceCounter(qpcLastBlink);
            Button.Blinking := true;
            Button.BlinkingOn := true;

            //
            Percentage := 0;
            if Button.Once then begin
              Done := true;
              ToggledOn := true;
              MarkRemove := true;
            end else begin
              // voorbereiden om opnieuw te bouwen
              if Button.QueueCount > 0 then begin
                if Queued > 0 then Dec(Queued);
                if Queued = 0 then begin
                  // wis deze construction-entry
(*if Button.Ready=0 then
                  MarkRemove := true;*)
                end else begin
                  QueryPerformanceCounter(qpcStart);
                  qpcEnd := qpcStart + (Tech.CreationDuration * PerformanceFreq);
                end;
                //
                Button.Queued := Queued;
              end;
            end;
          end;
        end;

      end;
    end;

  // wis gemarkeerde entries
  RemoveMarkedEntries;
end;

procedure TThreadConstruct.Add(const aHUDIndex,
                                     aButtonSetIndex,
                                     aButtonIndex: integer;
                               aAssignedEntity: PEntity);
var Len, c: integer;
    b: boolean;
    Button: TEntityButton;
    Tech: TTech;
begin
  Button := TheGame.HUD[aHUDIndex].ButtonSets[aButtonSetIndex].Buttons[aButtonIndex];
  Tech := TheGameTech.TechTree[Button.TechIndex].Tech;

  // test of de knop al is opgenomen in de array
  for c:=Low(AConstructingInfo) to High(AConstructingInfo) do
    with AConstructingInfo[c] do begin
      b := ((HUDIndex=aHUDIndex) and (ButtonSetIndex=aButtonSetIndex) and (ButtonIndex=aButtonIndex));
      if b then Break;
    end;
  // zoja? test of de knop een queue heeft
  if b then begin
    if (Button.QueueCount>0) and (AConstructingInfo[c].Queued<Button.QueueCount) then begin
      // verhoog het aantal in de knop's queue
      Inc(AConstructingInfo[c].Queued);
      Button.Queued := AConstructingInfo[c].Queued;
    end; // else: de knop heeft geen queue..negeer de knopdruk.
  end else begin
    // de knop is nog niet opgenomen in de constructie-array
    Len := Length(AConstructingInfo);
    SetLength(AConstructingInfo, Len+1);
    with AConstructingInfo[Len] do begin
      HUDIndex := aHUDIndex;
      ButtonSetIndex := aButtonSetIndex;
      ButtonIndex := aButtonIndex;
      AssignedEntity := aAssignedEntity;
      //
      QueryPerformanceCounter(qpcStart);
      qpcEnd := qpcStart + (Tech.CreationDuration * PerformanceFreq);
      Percentage := 0;
      //
      QueryPerformanceCounter(qpcLastBlink);
      //
      Done := false;
      ToggledOn := Button.Toggle;
      Queued := 1;
      Button.Queued := Queued;
      //
      MarkRemove := false;
    end;
  end;
end;


procedure TThreadConstruct.Cancel(const aHUDIndex,
                                        aButtonSetIndex,
                                        aButtonIndex: integer);
begin
  //
end;


procedure TThreadConstruct.RemoveMarkedEntries;
var cc,c,r, Len, Last: integer;
begin
  cc := Low(AConstructingInfo);
  repeat
    Len := Length(AConstructingInfo);
    Last := Len-1;
    for c:=cc to Last do
      if AConstructingInfo[c].MarkRemove then begin
        // verwisselen met de laatste, laatste dan wissen..
        if c<>Last then AConstructingInfo[c] := AConstructingInfo[Last];
        SetLength(AConstructingInfo, Len-1);
        if c<Last then begin
          cc := c;
          Break;
        end;
      end;
  until c>=Last;
end;

function TThreadConstruct.GetPercentage(const aHUDIndex,
                                              aButtonSetIndex,
                                              aButtonIndex: integer;
                                        aAssignedEntity: PEntity): integer;
var c: integer;
    b: boolean;
begin
  Result := 0;
  b := false;
  for c:=Low(AConstructingInfo) to High(AConstructingInfo) do
    with AConstructingInfo[c] do begin
      b := ((HUDIndex=aHUDIndex) and (ButtonSetIndex=aButtonSetIndex) and
            (ButtonIndex=aButtonIndex) and (AssignedEntity=aAssignedEntity));
      if b then Break;
    end;
  if not b then Exit;

  Result := AConstructingInfo[c].Percentage;
end;

end.
