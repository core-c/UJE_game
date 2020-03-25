unit uSelection;
interface
uses Types;

type
  // "Selecteren en groeperen" waarvoor geldt:
  // * Van een tegenstander mag/kun je maar 1 unit/gebouw tegelijkertijd selecteren.
  //     Als gevolg daarvan kun je dus altijd maar 1 tegenstander-unit/gebouw in een groep plaatsen.
  // * Men kan alleen entities aan een selectie/groep toevoegen welke dezelfde ClassType hebben.
  // * Een unit van een ander team kun je alleen pixel-selecteren (LMB-down,LMB-up), ..Die ene unit naar een nieuwe selectie. ..
  //

  PGroup = ^TGroup;
  TGroup = record
    Indexes: array of integer; // indexes naar spawnpoints/units ..etc
    Count: integer;            // het aantal indexes in de array
    ClassType_: cardinal;      // de classtype van de dingen opgenomen in deze groep
    Team: integer;             // de eigenaar van de units/gebouwen in de groep
  end;

  TSelection = class(TObject)
  private
    IncludeOponents: boolean;  // ook tegenstanders aan kunnen klikken / selecteren??
    Groups: array[0..9] of TGroup;
    //tbv. property
    local_AllowSelecting: boolean;
    procedure SetAllowSelecting(const Value: boolean);
  published
    property AllowSelecting: boolean     read local_AllowSelecting     write SetAllowSelecting;
  public
    SelectedSpawnpoints: TGroup; // indexes naar Game.Map.Spawnpoints
    SelectedUnits: TGroup;       // indexes naar Game.Map.Units
    SelectedPowerpylons: TGroup; // indexes naar Game.Map.Powerpylons
    SelectionRect: TRect;        // selectie-rechthoek op scherm over units/buildings
    //
    constructor Create;
    destructor Destroy; override;

    //
    procedure SetSelectionRectCoords1(const X,Y: integer);                         // X,Y in scherm-coords
    procedure SetSelectionRectCoords2(const X,Y: integer);                         // X,Y in scherm-coords
    procedure InvalidateSelectionRect;
    function IsPixelSelectionRect : boolean;

    // tbv. het selecteren van knoppen en 3D-objecten
    procedure SelectButton(const X,Y: integer); overload;                          // X,Y in scherm-coords
    procedure SelectButton(const HUDIndex: integer; const X,Y: integer); overload; // X,Y in scherm-coords
    procedure SelectObjects(const NewSelection: boolean; const X,Y: integer);      // X,Y in scherm-coords

    procedure CancelButton(const X,Y: integer); overload;                          // X,Y in scherm-coords
    procedure CancelButton(const HUDIndex: integer; const X,Y: integer); overload; // X,Y in scherm-coords

    procedure NoSelectedUnits;
    function FindSelectedUnitIndex(const Index: integer) : integer;
    procedure AddSelectedUnit(const Index: integer);
    procedure RemoveSelectedUnit(const Index: integer);
    procedure ToggleSelectedUnit(const Index: integer);
    //
    procedure NoSelectedSpawnpoints;
    function FindSelectedSpawnpointIndex(const Index: integer) : integer;
    procedure AddSelectedSpawnpoint(const Index: integer);
    procedure RemoveSelectedSpawnpoint(const Index: integer);
    procedure ToggleSelectedSpawnpoint(const Index: integer);

    procedure NoSelectedPowerpylons;
    function FindSelectedPowerpylonIndex(const Index: integer) : integer;
    procedure AddSelectedPowerpylon(const Index: integer);
    procedure RemoveSelectedPowerpylon(const Index: integer);
    procedure ToggleSelectedPowerpylon(const Index: integer);

    // groepen
    procedure NoGroups;
    procedure CreateGroup(const GroupNr: integer); // aGroup<aGroupNr> := de huidige selectie
    procedure SelectGroup(const GroupNr: integer); // de huidige selectie := aGroup<aGroupNr>
    procedure AddGroupToSelection(const GroupNr: integer);

  end;


var Selection: TSelection;


implementation
uses uEntities, uOpenGL, OpenGL, uConst, u3DTypes, uCalc, uWavefront, Unit1,
     MMSystem;

//-----------------------------------------------
constructor TSelection.Create;
begin
  local_AllowSelecting := true;
  NoSelectedUnits;
  NoSelectedSpawnpoints;
  NoSelectedPowerpylons;
  NoGroups;
  SelectedSpawnpoints.ClassType_ := cltEntitySpawnpoint;
  SelectedPowerpylons.ClassType_ := cltEntityPowerpylon;
//  SelectedSpawnpoints.Count := 0;
  SelectedUnits.ClassType_ := cltEntityOreMiner;
end;

destructor TSelection.Destroy;
var g: integer;
begin
  SetLength(SelectedUnits.Indexes, 0);
  SetLength(SelectedSpawnpoints.Indexes, 0);
  SetLength(SelectedPowerpylons.Indexes, 0);
  for g:=0 to 9 do SetLength(Groups[g].Indexes, 0);
  inherited;
end;

//-----------------------------------------------
procedure TSelection.SetAllowSelecting(const Value: boolean);
begin
  if Value<>local_AllowSelecting then
    local_AllowSelecting := Value;
end;


//-----------------------------------------------
procedure TSelection.SetSelectionRectCoords1(const X, Y: integer);
begin
//  if not local_AllowSelecting then Exit;
  with SelectionRect do begin
    Left := X;
    Top := Y;
    Right := X;
    Bottom := Y;
  end;
end;

procedure TSelection.SetSelectionRectCoords2(const X, Y: integer);
begin
  with SelectionRect do begin
    Right := X;
    Bottom := Y;
  end;
end;

procedure TSelection.InvalidateSelectionRect;
begin
  SetSelectionRectCoords1(0,0);
end;

function TSelection.IsPixelSelectionRect: boolean;
var deltax,deltay: integer;
begin
  with SelectionRect do begin
    //Result := ((Left=Right) and (Top=Bottom));
    deltax := abs(Right-Left);
    deltay := abs(Bottom-Top);
    Result := ((deltax<=1) or (deltay<=1)); // geen 0- of 1-dimensionale array pickmatrixen!! error
  end;
end;


//-----------------------------------------------
procedure TSelection.SelectButton(const HUDIndex, X,Y: integer);
var ButtonIndex: integer;
    Button: TEntityButton;
begin
  InvalidateSelectionRect;

  with Game.HUD[HUDIndex] do begin

    // controleer de HUD-SysButtons
    ButtonIndex := OverSysButton(X,Y);
    if ButtonIndex <> -1 then begin // SysButton clicked..
      if SysButtons[ButtonIndex].Enabled then SysButtons[ButtonIndex].DoClick;
      Exit;
    end else begin

      // controleer de vaste knoppen (onder de radar)
      ButtonIndex := OverFixedButton(X,Y);
      if ButtonIndex <> -1 then begin // FixedButton clicked..
        Button := ButtonSets[0].Buttons[ButtonIndex];
        if Button.Enabled and (not Button.Done) then begin
          if Button.Ready>=1 then begin
            Button.Ready := Button.Ready - 1;
            //Button.Percentage := 0;
            Button.Blinking := false;
            Button.BlinkingOn := false;
            Game.ConstructingEntity := ButtonSets[0].AssignedEntity;
            if Assigned(Button.OnUp) then Button.OnUp(self); //Button.DoClick;
          end else begin

//          Game.ConstructingEntity := ButtonSets[0].AssignedEntity;
////          ButtonSets[0].Buttons[ButtonIndex].DoClick;
fMain.thrConstruct.Add(HUDIndex,0,ButtonIndex,ButtonSets[0].AssignedEntity);

          end;
        end;
        Exit;
      end;

      // controleer de HUD-EntityButtons
      if ButtonsPtr=nil then Exit;
      ButtonIndex := OverButton(X,Y);
      if ButtonIndex <> -1 then begin // Button clicked..
        if ButtonsPtr.Buttons[ButtonIndex].Enabled then begin
          if not ButtonsPtr.Buttons[ButtonIndex].Done then begin
            Game.ConstructingEntity := ButtonsPtr.AssignedEntity;
            ButtonsPtr.Buttons[ButtonIndex].DoClick;
//fMain.thrConstruct.Add(HUDIndex,ButtonsPtr.ThisIndex,ButtonIndex,ButtonsPtr.AssignedEntity);
          end;
        end;
        Exit;
      end;
    end;
  end;
end;

procedure TSelection.SelectButton(const X, Y: integer);
var h: integer;
begin
  for h:=Low(Game.HUD) to High(Game.HUD) do SelectButton(h, X,Y);
end;


procedure TSelection.CancelButton(const HUDIndex, X, Y: integer);
var ButtonIndex: integer;
    Button: TEntityButton;
    b: boolean;
begin
  InvalidateSelectionRect;

  with Game.HUD[HUDIndex] do begin

    // controleer de vaste knoppen (onder de radar)
    ButtonIndex := OverFixedButton(X,Y);
    if ButtonIndex <> -1 then begin // FixedButton clicked..
      Button := ButtonSets[0].Buttons[ButtonIndex];
      if Button.Enabled and (not Button.Done) then begin
        // is er iets klaar onder deze knop??.. of nog bezig met bouwen??..
        b := ((Button.Percentage in [0..99]) or (Button.Ready>=1));
(*
        if Button.Ready>=1 then Button.Ready := Button.Ready - 1;
        Button.Blinking := false;
        Button.BlinkingOn := false;
*)
        if b then fMain.thrConstruct.Cancel(HUDIndex,0,ButtonIndex);
      end;
      Exit;
    end;

    // controleer de HUD-EntityButtons
//nog maken..
  end;
end;

procedure TSelection.CancelButton(const X, Y: integer);
var h: integer;
begin
  for h:=Low(Game.HUD) to High(Game.HUD) do CancelButton(h, X,Y);
end;


procedure TSelection.SelectObjects(const NewSelection: boolean; const X,Y: integer);
var Spawnpoint: TEntitySpawnpoint;
    Powerpylon: TEntityPowerpylon;
    TheUnit: TEntity{OreMiner};
    Index: integer;
    pl,pr,pb,pt,ph,pw,cx,cy: integer;
    only1: boolean;
    RayDirX,RayDirY,RayDirZ: GLdouble;
    RayOrg, RayDir, ip: TVector;  // ip = IntersectionPoint
    s,p,u: integer;
    BoundingBox, bb: TBoundingBox;
    V: TVector;
    distance, closestDistance: single;
    closest: integer;
begin
  if not local_AllowSelecting then Exit;
  only1 := IsPixelSelectionRect;
  if NewSelection then begin
    NoSelectedSpawnpoints;
    NoSelectedUnits;
    NoSelectedPowerpylons;
  end;

  IncludeOponents := (NewSelection and only1);

  try
    if only1 then begin

      // begin met een "leeg" HUD-menu
      Game.HUD[0].AssignEntity(nil);

      // een ray door  muiscursorpos  en  near- & far-plane
      OGL.CalcMouseRay(X,Y, RayDirX,RayDirY,RayDirZ);
      RayDir := UnitVector(Vector(RayDirX,RayDirY,RayDirZ));
      RayOrg := OGL.Camera.Position;

      // doorloop alle spawnpoints en test of de (AA)boundingbox wordt gesneden/geraakt
      if Length(Game.Map.Spawnpoints)>0 then begin
        BoundingBox := WF.ObjectByName('windmill').BoundingBox;
        closestDistance := 3.4E38;
        closest := -1; // er is nog geen enkel spawnpoint getest
        for s:=Low(Game.Map.Spawnpoints) to High(Game.Map.Spawnpoints) do begin
          Spawnpoint := Game.Map.Spawnpoints[s];
          if not IncludeOponents then // het selecteren van tegenstanders kan/mag niet??
            if Spawnpoint.Team <> 0 then begin
              PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
              Continue; //ander team??
            end;

          // de (AA)boundingbox op de positie van het spawnpoint
          bb.Min := AddVector(BoundingBox.Min, Spawnpoint.Location);
          bb.Max := AddVector(BoundingBox.Max, Spawnpoint.Location);

          if not LineBoxIntersectionPoint2(RayOrg,RayDir, bb, ip) then Continue;
          // ..de lijn snijdt/raakt met de boundingbox.

          // de afstand tot het oogpunt bepalen,
          // om alleen het dichtstbijzijnde spawnpoint te selecteren
          V := SubVector(Spawnpoint.Location, RayOrg);
          distance := VectorLength(V);
          if distance < closestDistance then begin
            closestDistance := distance;
            closest := s;                // index onthouden
          end;
        end;
        // de lijn snijdt met de boundingbox
        if closest<>-1 then begin
          ToggleSelectedSpawnpoint(closest);
(*
          // vul de HUD met knoppen voor deze entity
          if SelectedSpawnpoints.Count=1 then Game.HUD[0].AssignEntity(Game.Map.Spawnpoints[closest])
                                         else Game.HUD[0].AssignEntity(nil);
*)
          Exit;
        end;
      end; //spawnpoints

      // doorloop alle powerpylons en test of de (AA)boundingbox wordt gesneden/geraakt
      if Length(Game.Map.Powerpylons)>0 then begin
        BoundingBox := WF.ObjectByName('electro-pole-1').BoundingBox;
        closestDistance := 3.4E38;
        closest := -1; // er is nog geen enkele powerpylon getest
        for p:=Low(Game.Map.Powerpylons) to High(Game.Map.Powerpylons) do begin
          Powerpylon := Game.Map.Powerpylons[p];
          if not IncludeOponents then // het selecteren van tegenstanders kan/mag niet??
            if Powerpylon.Team <> 0 then begin
              PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
              Continue; //ander team??
            end;

          // de (AA)boundingbox op de positie van de powerpylon
          bb.Min := AddVector(BoundingBox.Min, Powerpylon.Location);
          bb.Max := AddVector(BoundingBox.Max, Powerpylon.Location);

          if not LineBoxIntersectionPoint2(RayOrg,RayDir, bb, ip) then Continue;
          // ..de lijn snijdt/raakt met de boundingbox.

          // de afstand tot het oogpunt bepalen,
          // om alleen het dichtstbijzijnde powerpylon te selecteren
          V := SubVector(Powerpylon.Location, RayOrg);
          distance := VectorLength(V);
          if distance < closestDistance then begin
            closestDistance := distance;
            closest := p;                // index onthouden
          end;
        end;
        // de lijn snijdt met de boundingbox
        if closest<>-1 then begin
          ToggleSelectedPowerpylon(closest);
          Exit;
        end;
      end; //powerpylons

      // doorloop alle units..
      if Length(Game.Map.Units)>0 then begin
        BoundingBox := WF.ObjectByName('harvester').BoundingBox;
        closestDistance := 3.4E38;
        closest := -1;
        for u:=Low(Game.Map.Units) to High(Game.Map.Units) do begin
          TheUnit := Game.Map.Units[u];
          if not IncludeOponents then
            if TheUnit.Team <> 0 then begin
              PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
              Continue;
            end;
          bb.Min := AddVector(BoundingBox.Min, TheUnit.Location);
          bb.Max := AddVector(BoundingBox.Max, TheUnit.Location);
          if not LineBoxIntersectionPoint2(RayOrg,RayDir, bb, ip) then Continue;
          V := SubVector(TheUnit.Location, RayOrg);
          distance := VectorLength(V);
          if distance < closestDistance then begin
            closestDistance := distance;
            closest := u;
          end;
        end;
        if closest<>-1 then begin
          ToggleSelectedUnit(closest);
(*
          // vul de HUD met knoppen voor deze entity
          if SelectedUnits.Count=1 then Game.HUD[0].AssignEntity(Game.Map.Units[closest])
                                   else Game.HUD[0].AssignEntity(nil);
*)
          Exit;
        end;
      end;

      // er is niks geselecteerd, men klikt niet op een gebouw/unit..
      Game.HUD[0].AssignEntity(nil);

    end {only1} else begin {selectionrect} //-------------------------------------

      if Length(Game.Map.Units)>0 then begin
        with SelectionRect do begin
          if Right<Left then begin pr:=Left; pl:=Right; end else begin pr:=Right; pl:=Left; end;
          if Bottom<Top then begin pt:=Bottom; pb:=Top; end else begin pt:=Top; pb:=Bottom; end;
          ph := pb-pt;
          pw := pr-pl;
          cx := pl + (pw div 2);
          cy := pt + (ph div 2);
        end;
        if (pw>1) and (ph>1) then begin // een "leeg" selectionrect = alles selecteren?? willen we niet..
          glMatrixMode(GL_PROJECTION);
          glPushMatrix;
            OGL.SetupProjectionPicking(cx,cy,pw,ph);   //(left,right,right-left,bottom-top)
            // doorloop alle units..
            for u:=Low(Game.Map.Units) to High(Game.Map.Units) do begin
              TheUnit := Game.Map.Units[u];
              if not IncludeOponents then
                if TheUnit.Team <> 0 then Continue;
              if OGL.Frustum.PointInside(TheUnit.Location) then ToggleSelectedUnit(u);
            end;
          glMatrixMode(GL_PROJECTION);
          glPopMatrix;
          glMatrixMode(GL_MODELVIEW);
        end;
(*
        // als er 1 unit is geselecteerd, dan de knoppen in HUD afbeelden..
        if SelectedUnits.Count=1 then Game.HUD[0].AssignEntity(Game.Map.Units[SelectedUnits.Indexes[0]])
                                 else Game.HUD[0].AssignEntity(nil);
*)
      end;

    end; {selectionrect}
  finally
    // invalideer de selectie-rect
    InvalidateSelectionRect;
  end;
end;



//-----------------------------------------------
procedure TSelection.NoSelectedSpawnpoints;
var s: integer;
begin
  if SelectedSpawnpoints.Count=0 then Exit;
  for s:=Low(SelectedSpawnpoints.Indexes) to High(SelectedSpawnpoints.Indexes) do
    Game.Map.Spawnpoints[SelectedSpawnpoints.Indexes[s]].Selected := false;
  SetLength(SelectedSpawnpoints.Indexes, 0);
  SelectedSpawnpoints.Count := 0;
end;

function TSelection.FindSelectedSpawnpointIndex(const Index: integer): integer;
var s: integer;
begin
  Result := -1;  //ongeldige index
  for s:=Low(SelectedSpawnpoints.Indexes) to High(SelectedSpawnpoints.Indexes) do
    if SelectedSpawnpoints.Indexes[s] = Index then begin
      Result := s;
      Exit;
    end;
end;

procedure TSelection.AddSelectedSpawnpoint(const Index: integer);
var Len,i,s: integer;
begin
  s := FindSelectedSpawnpointIndex(Index);
  if s<>-1 then Exit; // zit al in de array??
  Len := SelectedSpawnpoints.Count;

  if Len>0 then begin
    // alleen spawnpoints met dezelfde eigenaar in een selectie
    if SelectedSpawnpoints.Team <> Game.Map.Spawnpoints[Index].Team then begin
      PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
      Exit;
    end else
      // een selectie bevat maximaal 1 tegenstander spawnpoint
      if SelectedSpawnpoints.Team<>0 then
        {if Game.Map.Spawnpoints[Index].Team<>0 then} begin
          PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
          Exit;
        end;
  end else //Len=0
    // als de huidige selectie een unit bevat, dan geen spawnpoint toevoegen.
    if {(Len=0) and} (SelectedUnits.Count>0) then begin
      PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
      Exit;
    end;
  //
  SetLength(SelectedSpawnpoints.Indexes, Len+1);
  SelectedSpawnpoints.Count := Len+1;
  SelectedSpawnpoints.Indexes[Len] := Index;
  SelectedSpawnpoints.Team := Game.Map.Spawnpoints[Index].Team;
  Game.Map.Spawnpoints[Index].Selected := true;
  PlaySound(SoundAcknowledged,0,SND_ASYNC+SND_NODEFAULT);
end;

procedure TSelection.RemoveSelectedSpawnpoint(const Index: integer);
var Len, s, tmp: integer;
begin
  s := FindSelectedSpawnpointIndex(Index);
  if s = -1 then Exit; // == -1    zit niet in de array
  // verwissel element "s" met het laatste element
  Len := SelectedSpawnpoints.Count;
  SelectedSpawnpoints.Indexes[s] := SelectedSpawnpoints.Indexes[Len-1];
  SetLength(SelectedSpawnpoints.Indexes, Len-1);
  SelectedSpawnpoints.Count := Len-1;
  Game.Map.Spawnpoints[Index].Selected := false;
end;

procedure TSelection.ToggleSelectedSpawnpoint(const Index: integer);
var s: integer;
begin
  s := FindSelectedSpawnpointIndex(Index);  // <>-1 = Unit nu al geselecteerd
  if s = -1 then AddSelectedSpawnpoint(Index)
            else RemoveSelectedSpawnpoint(Index);
  // vul de HUD met knoppen voor deze entity
  if SelectedSpawnpoints.Count=1 then Game.HUD[0].AssignEntity(@Game.Map.Spawnpoints[Index])
                                 else Game.HUD[0].AssignEntity(nil);
end;



//-----------------------------------------------
procedure TSelection.NoSelectedUnits;
var u: integer;
begin
  if SelectedUnits.Count=0 then Exit;
  for u:=Low(SelectedUnits.Indexes) to High(SelectedUnits.Indexes) do
    Game.Map.Units[SelectedUnits.Indexes[u]].Selected := false;
  SetLength(SelectedUnits.Indexes, 0);
  SelectedUnits.Count := 0;
end;

function TSelection.FindSelectedUnitIndex(const Index: integer): integer;
var u: integer;
begin
  Result := -1;  //ongeldige index
  for u:=Low(SelectedUnits.Indexes) to High(SelectedUnits.Indexes) do
    if SelectedUnits.Indexes[u] = Index then begin
      Result := u;
      Exit;
    end;
end;

procedure TSelection.AddSelectedUnit(const Index: integer);
var Len,i,u: integer;
begin
  u := FindSelectedUnitIndex(Index);
  if u<>-1 then Exit; // zit al in de array??
  Len := SelectedUnits.Count;
  //
  if Len>0 then begin
    // alleen units met dezelfde eigenaar in een selectie
    if SelectedUnits.Team <> Game.Map.Units[Index].Team then begin
      PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
      Exit;
    end;
    // een selectie bevat maximaal 1 tegenstander unit
    if SelectedUnits.Team<>0 then
      if Game.Map.Units[Index].Team<>0 then begin
        PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
        Exit;
      end;
  end;
  // als de huidige selectie een gebouw bevat, dan geen unit toevoegen.
  if (Len=0) and ((SelectedSpawnpoints.Count>0) or (SelectedPowerpylons.Count>0)) then begin
    PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
    Exit;
  end;
  //
  SetLength(SelectedUnits.Indexes, Len+1);
  SelectedUnits.Count := Len+1;
  SelectedUnits.Indexes[Len] := Index;
  SelectedUnits.Team := Game.Map.Units[Index].Team;
  Game.Map.Units[Index].Selected := true;
  PlaySound(SoundAcknowledged,0,SND_ASYNC+SND_NODEFAULT);
end;

procedure TSelection.RemoveSelectedUnit(const Index: integer);
var Len, u, tmp: integer;
begin
  u := FindSelectedUnitIndex(Index);
  if u = -1 then Exit; // == -1    zit niet in de array
  // verwissel element "u" met het laatste element
  Len := SelectedUnits.Count;
  SelectedUnits.Indexes[u] := SelectedUnits.Indexes[Len-1];
  SetLength(SelectedUnits.Indexes, Len-1);
  SelectedUnits.Count := Len-1;
  Game.Map.Units[Index].Selected := false;
end;

procedure TSelection.ToggleSelectedUnit(const Index: integer);
var u: integer;
begin
  u := FindSelectedUnitIndex(Index);  // <>-1 = Unit nu al geselecteerd
  if u = -1 then AddSelectedUnit(Index)
            else RemoveSelectedUnit(Index);
  // vul de HUD met knoppen voor deze entity
  if SelectedUnits.Count=1 then Game.HUD[0].AssignEntity(@Game.Map.Units[Index])
                           else Game.HUD[0].AssignEntity(nil);
end;



//-----------------------------------------------
procedure TSelection.NoSelectedPowerpylons;
var p: integer;
begin
  if SelectedPowerpylons.Count=0 then Exit;
  for p:=Low(SelectedPowerpylons.Indexes) to High(SelectedPowerpylons.Indexes) do
    Game.Map.Powerpylons[SelectedPowerpylons.Indexes[p]].Selected := false;
  SetLength(SelectedPowerpylons.Indexes, 0);
  SelectedPowerpylons.Count := 0;
end;

function TSelection.FindSelectedPowerpylonIndex(const Index: integer): integer;
var p: integer;
begin
  Result := -1;  //ongeldige index
  for p:=Low(SelectedPowerpylons.Indexes) to High(SelectedPowerpylons.Indexes) do
    if SelectedPowerpylons.Indexes[p] = Index then begin
      Result := p;
      Exit;
    end;
end;

procedure TSelection.AddSelectedPowerpylon(const Index: integer);
var Len,p: integer;
begin
  p := FindSelectedPowerpylonIndex(Index);
  if p<>-1 then Exit; // zit al in de array??
  Len := SelectedPowerpylons.Count;

  if Len>0 then begin
    // alleen powerpylons met dezelfde eigenaar in een selectie
    if SelectedPowerpylons.Team <> Game.Map.Powerpylons[Index].Team then begin
      PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
      Exit;
    end else
      // een selectie bevat maximaal 1 tegenstander powerpylon
      if SelectedPowerpylons.Team<>0 then
        {if Game.Map.Powerpylons[Index].Team<>0 then} begin
          PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
          Exit;
        end;
  end else //Len=0
    // als de huidige selectie een unit bevat, dan geen powerpylon toevoegen.
    if {(Len=0) and} (SelectedUnits.Count>0) then begin
      PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
      Exit;
    end;
  //
  SetLength(SelectedPowerpylons.Indexes, Len+1);
  SelectedPowerpylons.Count := Len+1;
  SelectedPowerpylons.Indexes[Len] := Index;
  SelectedPowerpylons.Team := Game.Map.Powerpylons[Index].Team;
  Game.Map.Powerpylons[Index].Selected := true;
  PlaySound(SoundAcknowledged,0,SND_ASYNC+SND_NODEFAULT);
end;

procedure TSelection.RemoveSelectedPowerpylon(const Index: integer);
var Len, p, tmp: integer;
begin
  p := FindSelectedPowerpylonIndex(Index);
  if p = -1 then Exit; // == -1    zit niet in de array
  // verwissel element "p" met het laatste element
  Len := SelectedPowerpylons.Count;
  SelectedPowerpylons.Indexes[p] := SelectedPowerpylons.Indexes[Len-1];
  SetLength(SelectedPowerpylons.Indexes, Len-1);
  SelectedPowerpylons.Count := Len-1;
  Game.Map.Powerpylons[Index].Selected := false;
end;

procedure TSelection.ToggleSelectedPowerpylon(const Index: integer);
var p: integer;
begin
  p := FindSelectedPowerpylonIndex(Index);  // <>-1 = Unit nu al geselecteerd
  if p = -1 then AddSelectedPowerpylon(Index)
            else RemoveSelectedPowerpylon(Index);
  // vul de HUD met knoppen voor deze entity
  if SelectedPowerpylons.Count=1 then Game.HUD[0].AssignEntity(@Game.Map.Powerpylons[Index])
                                 else Game.HUD[0].AssignEntity(nil);
end;




//-----------------------------------------------
procedure TSelection.NoGroups;
var g: integer;
begin
  for g:=0 to 9 do
    with Groups[g] do begin
      SetLength(Indexes, 0);
      Count := 0;
    end;
end;

procedure TSelection.CreateGroup(const GroupNr: integer);
var s,u: integer;
begin
  if (GroupNr<0) or (GroupNr>9) then Exit;

  if SelectedSpawnpoints.Count>0 then begin
    SetLength(Groups[GroupNr].Indexes, SelectedSpawnpoints.Count);
    for s:=Low(SelectedSpawnpoints.Indexes) to High(SelectedSpawnpoints.Indexes) do
      Groups[GroupNr].Indexes[s] := SelectedSpawnpoints.Indexes[s];
    Groups[GroupNr].Count := SelectedSpawnpoints.Count;
    Groups[GroupNr].ClassType_ := SelectedSpawnpoints.ClassType_;
    Groups[GroupNr].Team := SelectedSpawnpoints.Team;
    PlaySound(SoundAcknowledged,0,SND_ASYNC+SND_NODEFAULT);
    Exit;
  end;

  if SelectedPowerpylons.Count>0 then begin
    SetLength(Groups[GroupNr].Indexes, SelectedPowerpylons.Count);
    for s:=Low(SelectedPowerpylons.Indexes) to High(SelectedPowerpylons.Indexes) do
      Groups[GroupNr].Indexes[s] := SelectedPowerpylons.Indexes[s];
    Groups[GroupNr].Count := SelectedPowerpylons.Count;
    Groups[GroupNr].ClassType_ := SelectedPowerpylons.ClassType_;
    Groups[GroupNr].Team := SelectedPowerpylons.Team;
    PlaySound(SoundAcknowledged,0,SND_ASYNC+SND_NODEFAULT);
    Exit;
  end;

  if SelectedUnits.Count>0 then begin
    SetLength(Groups[GroupNr].Indexes, SelectedUnits.Count);
    for u:=Low(SelectedUnits.Indexes) to High(SelectedUnits.Indexes) do
      Groups[GroupNr].Indexes[u] := SelectedUnits.Indexes[u];
    Groups[GroupNr].Count := SelectedUnits.Count;
    Groups[GroupNr].ClassType_ := SelectedUnits.ClassType_;
    Groups[GroupNr].Team := SelectedUnits.Team;
    PlaySound(SoundAcknowledged,0,SND_ASYNC+SND_NODEFAULT);
  end;
end;

procedure TSelection.SelectGroup(const GroupNr: integer);
var s,p,u: integer;
begin
  if (GroupNr<0) or (GroupNr>9) then Exit;

  NoSelectedUnits;
  NoSelectedSpawnpoints;
  NoSelectedPowerpylons;

  if Groups[GroupNr].Count = 0 then begin
    PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
    Exit;
  end;

  case Groups[GroupNr].ClassType_ of
    cltEntitySpawnpoint: begin
        SetLength(SelectedSpawnpoints.Indexes, Groups[GroupNr].Count);
        for s:=Low(Groups[GroupNr].Indexes) to High(Groups[GroupNr].Indexes) do begin
          SelectedSpawnpoints.Indexes[s] := Groups[GroupNr].Indexes[s];
          Game.Map.Spawnpoints[SelectedSpawnpoints.Indexes[s]].Selected := true;
        end;
        SelectedSpawnpoints.Team := Groups[GroupNr].Team;
        SelectedSpawnpoints.Count := Groups[GroupNr].Count;
        //
if SelectedSpawnpoints.Count=1 then Game.HUD[0].AssignEntity(@Game.Map.Spawnpoints[SelectedSpawnpoints.Indexes[0]])
                               else Game.HUD[0].AssignEntity(nil);
      end;

    cltEntityPowerpylon: begin
        SetLength(SelectedPowerpylons.Indexes, Groups[GroupNr].Count);
        for p:=Low(Groups[GroupNr].Indexes) to High(Groups[GroupNr].Indexes) do begin
          SelectedPowerpylons.Indexes[p] := Groups[GroupNr].Indexes[p];
          Game.Map.Powerpylons[SelectedPowerpylons.Indexes[p]].Selected := true;
        end;
        SelectedPowerpylons.Team := Groups[GroupNr].Team;
        SelectedPowerpylons.Count := Groups[GroupNr].Count;
        //
if SelectedPowerpylons.Count=1 then Game.HUD[0].AssignEntity(@Game.Map.Powerpylons[SelectedPowerpylons.Indexes[0]])
                               else Game.HUD[0].AssignEntity(nil);
      end;

    cltEntityOreMiner: begin
        SetLength(SelectedUnits.Indexes, Groups[GroupNr].Count);
        for u:=Low(Groups[GroupNr].Indexes) to High(Groups[GroupNr].Indexes) do begin
          SelectedUnits.Indexes[u] := Groups[GroupNr].Indexes[u];
          Game.Map.Units[SelectedUnits.Indexes[u]].Selected := true;
        end;
        SelectedUnits.Team := Groups[GroupNr].Team;
        SelectedUnits.Count := Groups[GroupNr].Count;
        //
if SelectedUnits.Count=1 then Game.HUD[0].AssignEntity(@Game.Map.Units[SelectedUnits.Indexes[0]])
                         else Game.HUD[0].AssignEntity(nil);
      end;
  end;
  PlaySound(SoundAcknowledged,0,SND_ASYNC+SND_NODEFAULT);
end;

procedure TSelection.AddGroupToSelection(const GroupNr: integer);
var s,p,u,i: integer;
    isEmpty: boolean;
begin
  if (GroupNr<0) or (GroupNr>9) then Exit;
  if Groups[GroupNr].Count=0 then Exit;

  isEmpty := false;
  // bestaat de huidige selectie uit spawnpoints, powerpylons of units??
  if SelectedSpawnpoints.Count>0 then begin
    // als de huidige selectie een tegenstander unit/gebouw betreft,
    // dan geen groep toevoegen. /nop
    if SelectedSpawnpoints.Team<>0 then begin
      PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
      Exit;
    end;
  end else
    if SelectedPowerpylons.Count>0 then begin
      // als de huidige selectie een tegenstander unit/gebouw betreft,
      // dan geen groep toevoegen. /nop
      if SelectedPowerpylons.Team<>0 then begin
        PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
        Exit;
      end;
    end else
      if SelectedUnits.Count>0 then begin
        if SelectedUnits.Team<>0 then begin
          PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
          Exit;
        end;
      end else // counts zijn allemaal 0
        isEmpty := true;

  // niks toevoegen als de groep bestaat uit een tegenstander unit/gebouw..
  // tenzij de huidige selectie leeg is.
  if Groups[GroupNr].Team<>0 then
    if not isEmpty then begin
      PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
      Exit;
    end;


  // niks toevoegen als de groep andere entitie-types bevat dan de selectie
  if SelectedSpawnpoints.Count>0 then begin
    if Groups[GroupNr].ClassType_<>SelectedSpawnpoints.ClassType_ then begin
      PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
      Exit;
    end;
  end else
    if SelectedPowerpylons.Count>0 then begin
      if Groups[GroupNr].ClassType_<>SelectedPowerpylons.ClassType_ then begin
        PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
        Exit;
      end;
    end else
      if SelectedUnits.Count>0 then
        if Groups[GroupNr].ClassType_<>SelectedUnits.ClassType_ then begin
          PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
          Exit;
        end;


  // voeg de groep toe aan de huidige selectie
  case Groups[GroupNr].ClassType_ of
    cltEntitySpawnpoint: begin
        // als het spawnpoint al is opgenomen in de selectie dan nu niet toevoegen...
        for s:=Low(Groups[GroupNr].Indexes) to High(Groups[GroupNr].Indexes) do begin
          if FindSelectedSpawnpointIndex(Groups[GroupNr].Indexes[s]) <> -1 then Continue; //al geselecteerd??
          i := SelectedSpawnpoints.Count;
          SetLength(SelectedSpawnpoints.Indexes, i+1);
          SelectedSpawnpoints.Count := Length(SelectedSpawnpoints.Indexes);
          SelectedSpawnpoints.Indexes[i] := Groups[GroupNr].Indexes[s];
          Game.Map.Spawnpoints[SelectedSpawnpoints.Indexes[i]].Selected := true;
        end;
      end;
    cltEntityPowerpylon: begin
        // als de powerpylon al is opgenomen in de selectie dan nu niet toevoegen...
        for p:=Low(Groups[GroupNr].Indexes) to High(Groups[GroupNr].Indexes) do begin
          if FindSelectedPowerpylonIndex(Groups[GroupNr].Indexes[p]) <> -1 then Continue; //al geselecteerd??
          i := SelectedPowerpylons.Count;
          SetLength(SelectedPowerpylons.Indexes, i+1);
          SelectedPowerpylons.Count := Length(SelectedPowerpylons.Indexes);
          SelectedPowerpylons.Indexes[i] := Groups[GroupNr].Indexes[p];
          Game.Map.Powerpylons[SelectedPowerpylons.Indexes[i]].Selected := true;
        end;
      end;
    cltEntityOreMiner: begin
        // als de unit al is opgenomen in de selectie dan nu niet toevoegen...
        for u:=Low(Groups[GroupNr].Indexes) to High(Groups[GroupNr].Indexes) do begin
          if FindSelectedUnitIndex(Groups[GroupNr].Indexes[u]) <> -1 then Continue;
          i := SelectedUnits.Count;
          SetLength(SelectedUnits.Indexes, i+1);
          SelectedUnits.Count := Length(SelectedUnits.Indexes);
          SelectedUnits.Indexes[i] := Groups[GroupNr].Indexes[u];
          Game.Map.Units[SelectedUnits.Indexes[i]].Selected := true;
        end;
      end;
  end;
  PlaySound(SoundAcknowledged,0,SND_ASYNC+SND_NODEFAULT);
end;



initialization
  Selection := TSelection.Create;
finalization
  Selection.Free;
end.
