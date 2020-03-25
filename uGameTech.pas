unit uGameTech;
interface
uses Classes, uEntities;

type
  PTech = ^TTech;
  TTech = record
    ClassType_: cardinal;              // de entity-class
    Name: string;                      // naam van de technology
    ButtonImage: string;               // het plaatje voor op de knop
    CreationCosts: single;             // de kosten om deze tech. te maken
    CreationDuration: integer;         // de tijd (in seconden) om deze tech. te maken
    Available: boolean;                // beschikbaar??
    Action: TNotifyEvent;              // de uit te voeren procedure (als men op de HUD-knop drukt)
  end;

  PTechTree = ^TTechTree;
  TTechTree = record
    // tree
    Parent: integer; //PTechTree;
    Children: array of integer; //PTechTree;
    //
    Tech: TTech;
  end;


  PGameTech = ^TGameTech;
  TGameTech = class(TObject)
  private
    procedure AddTechToTree(aParent: integer; //PTechTree;
                            aClassType: cardinal;
                            aName: string;
                            aButtonImage: string;
                            aCreationCosts: single;
                            aCreationDuration: integer;
                            aAction: TNotifyEvent);
  public
    TechTree: array of TTechTree;
    //
    constructor Create;
    destructor Destroy; override;

    // knoppen maken voor alle technology
    procedure CreateButtonSets(var aHUD: TEntityHUD;
                               RootIndex: integer); overload;   //PTechTree
    procedure CreateButtonSets(var aHUD: TEntityHUD); overload;

    // TNotifyEvent tbv. button.onup
    procedure SpawnNewOreMiner(Sender: TObject);
    procedure Spawn10NewOreMiner(Sender: TObject);
    procedure Spawn100NewOreMiner(Sender: TObject);
    procedure Spawn1000NewOreMiner(Sender: TObject);
    procedure UpgradeOreMiner(Sender: TObject);
    //
    procedure ConstructSpawnpoint(Sender: TObject);    // kies een plek om een spawnpoint te bouwen
    procedure SpawnNewSpawnpoint(const X,Y: integer);  // plaats een nieuw spawnpoint (X,Y = schermcoords)
    //
    procedure ConstructPowerpylon(Sender: TObject);    // kies een plek om een powerpylon te bouwen
    procedure SpawnNewPowerpylon(const X,Y: integer);  // plaats een nieuwe powerpylon
    //
    procedure ConstructPowerline(Sender: TObject);     // kies een (andere) powerpylon om een powerline te bevestigen
    procedure SpawnNewPowerline(const X,Y: integer);   // plaats een nieuwe powerline
  end;


var GameTech: TGameTech;



implementation
uses u3DTypes, uCalc, uConst, OpenGL, uOpenGL, SysUtils, MMSystem;

//-----------------------------------------------
constructor TGameTech.Create;
begin
  SetLength(TechTree, 0);

  // De technology-tree opbouwen:
  // de parameter "aParent" bepaalt onder welke tak een nieuwe tech komt te hangen.

  // 0
  AddTechToTree(-1, cltEntityRoot, 'Root', '', 0.0, 0, nil);  // een dummy als root-node (om de children te bevatten)
    // 1
    AddTechToTree(0, cltEntityRoot, 'Spawnpoint', 'button_windmill.tga', 1000.0, 30, ConstructSpawnpoint);
      // 2
      AddTechToTree(1, cltEntitySpawnpoint, 'Oreminer', 'button_oreminer.tga', 300.0, 20, SpawnNewOreMiner);
        // 3
        AddTechToTree(2, cltEntityOreMiner, 'OreminerUpgrade1', 'button_oreminer_upgrade1.tga', 500.0, 60, UpgradeOreMiner);
        // 4
        AddTechToTree(2, cltEntityOreMiner, 'OreminerUpgrade2', 'button_oreminer_upgrade2.tga', 500.0, 60, UpgradeOreMiner);
        // 5
        AddTechToTree(2, cltEntityOreMiner, 'OreminerUpgrade3', 'button_oreminer_upgrade3.tga', 500.0, 60, UpgradeOreMiner);
        // 6
        AddTechToTree(2, cltEntityOreMiner, 'OreminerUpgrade4', 'button_oreminer_upgrade4.tga', 500.0, 60, UpgradeOreMiner);
        // 7
        AddTechToTree(2, cltEntityOreMiner, 'OreminerUpgrade5', 'button_oreminer_upgrade5.tga', 500.0, 60, UpgradeOreMiner);
      // 8
      AddTechToTree(1, cltEntitySpawnpoint, 'SpawnpointUpgrade1', 'button_oreminer.tga', 300.0, 20, Spawn10NewOreMiner);
      // 9
      AddTechToTree(1, cltEntitySpawnpoint, 'SpawnpointUpgrade2', 'button_oreminer.tga', 300.0, 20, Spawn100NewOreMiner);
      // 10
      AddTechToTree(1, cltEntitySpawnpoint, 'SpawnpointUpgrade3', 'button_oreminer.tga', 300.0, 20, Spawn1000NewOreMiner);
    // 11
    AddTechToTree(0, cltEntityRoot, 'Powerpylon', 'button_powerpylon.tga', 200.0, 5, ConstructPowerpylon);
      // 12
      AddTechToTree(11, cltEntityPowerpylon, 'Powerline', 'button_powerline.tga', 100.0, 20, ConstructPowerline);
end;

destructor TGameTech.Destroy;
begin
  SetLength(TechTree, 0);
  inherited;
end;


//-----------------------------------------------
procedure TGameTech.AddTechToTree(aParent: integer; //PTechTree;
                                  aClassType: cardinal;
                                  aName: string;
                                  aButtonImage: string;
                                  aCreationCosts: single;
                                  aCreationDuration: integer;
                                  aAction: TNotifyEvent);
var TT, NewTT: TTechTree;
    LenT, LenC, t: integer;
begin
  with NewTT.Tech do begin
    ClassType_ := aClassType;
    Name := aName;
    ButtonImage := aButtonImage;
    CreationCosts := aCreationCosts;
    CreationDuration := aCreationDuration;
    Available := (aParent = -1);
    Action := aAction;
  end;
  NewTT.Parent := aParent;
  // de nieuwe entry toevoegen aan de tree
  LenT := Length(TechTree);
  Setlength(TechTree, LenT+1);
  TechTree[LenT] := NewTT;

  if (aParent>=LenT) then raise Exception.Create('Foute parent opgegeven in de TechTree.');

  // toevoegen (als child) aan een reeds bestaande parent??
  if aParent <> -1 then begin
    LenC := Length(TechTree[aParent].Children);
    SetLength(TechTree[aParent].Children, LenC+1);
    TechTree[aParent].Children[LenC] := LenT;
  end;
end;


procedure TGameTech.CreateButtonSets(var aHUD: TEntityHUD;
                                     RootIndex: integer); //
var LenBS, LenB, LenC, c,t,sibling: integer;
begin
  if Length(TechTree)=0 then Exit;

  with TechTree[RootIndex] do begin
    // check of dit geen Root-tech is..
    if Parent <> -1 then begin
      // een buttonset bijmaken
      with Tech do begin
        LenBS := Length(aHUD.ButtonSets);
        aHUD.CreateButtonSet(Name,ClassType_);
      end;
      // Een non root-tech zit altijd in een button-set met zijn siblings.
      for c:=Low(TechTree[Parent].Children) to High(TechTree[Parent].Children) do begin
        sibling := TechTree[Parent].Children[c];
        with TechTree[sibling].Tech do begin
          // maak een knop
          LenB := Length(aHUD.ButtonSets[LenBS].Buttons);
          aHUD.ButtonSets[LenBS].AddButton(Name,ButtonImage);
          aHUD.ButtonSets[LenBS].Buttons[LenB].OnUp:= Action;
          aHUD.ButtonSets[LenBS].Buttons[LenB].TechIndex := sibling;
        end;
      end;
    end;
  end;
end;

procedure TGameTech.CreateButtonSets(var aHUD: TEntityHUD);
begin
  CreateButtonSets(aHUD, 0);
end;




//-----------------------------------------------
procedure TGameTech.SpawnNewOreMiner(Sender: TObject);
var V: TVector;
    x,z: single;
    Len: integer;
    Entity: PEntity;
    TileX, TileY: integer;
begin
  Game.isConstructing := false;
  // bepaal het geselecteerde spawnpoint
  Entity := Game.HUD[0].ButtonsPtr.AssignedEntity;
  if Entity^.ClassType_<>cltEntitySpawnPoint then Exit;  // het moet wel een geselecteerd spawnpoint zijn..

  // * Maak een unit
  TileX := (Entity^ as TEntitySpawnPoint).TileX;
  TileY := (Entity^ as TEntitySpawnPoint).TileY;
  x := Game.Map.Terrain.Tiles[TileX,TileY].CenterX;
  z := Game.Map.Terrain.Tiles[TileX,TileY].CenterY;
  V := Vector(x,Game.Map.Terrain.GetHeightAt(x,z),z);
  Len := Length(Game.Map.Units);
  Game.Map.AddUnit(c_UNIT_OREMINER,0,V);
  with Game.Map.Units[Len] do begin
    case ClassType_ of
      cltEntityOreMiner: with (Game.Map.Units[Len] as TEntityOreMiner) do begin
        MaxSpeed := c_MAXSPEED_OREMINER;
        isReturning := true;
        CalcNextRoute;
      end;
    end;
  end;
end;

procedure TGameTech.Spawn10NewOreMiner(Sender: TObject);
var i: integer;
begin
  for i:=0 to 9 do SpawnNewOreMiner(Sender);
end;

procedure TGameTech.Spawn100NewOreMiner(Sender: TObject);
var i: integer;
begin
  for i:=0 to 99 do SpawnNewOreMiner(Sender);
end;

procedure TGameTech.Spawn1000NewOreMiner(Sender: TObject);
var i: integer;
begin
  for i:=0 to 999 do SpawnNewOreMiner(Sender);
end;



//-----------------------------------------------
procedure TGameTech.UpgradeOreMiner(Sender: TObject);
var Entity: PEntity;
begin
  // Game.ConstructingEntity bevat de entity die iets bouwt
  Game.isConstructing := false;
  Entity := Game.ConstructingEntity;

  {Entity := Game.HUD[0].ButtonsPtr.AssignedEntity;}
  if Entity^.ClassType_ <> cltEntityOreMiner then Exit;  // het moet wel een geselecteerde oreminer zijn..
  (Entity^ as TEntityOreMiner).Upgrade;
  //opnieuw afbeelden voor deze entity (indien deze entity nu is geselecteerd)
  if Game.HUD[0].ButtonsPtr.AssignedEntity = Game.ConstructingEntity then Game.HUD[0].AssignEntity(Entity);
end;



//-----------------------------------------------
procedure TGameTech.ConstructSpawnpoint(Sender: TObject);
begin
  Game.StartConstructing(cltEntitySpawnpoint);
end;

procedure TGameTech.SpawnNewSpawnpoint(const X, Y: integer);
var objX,objY,objZ: GLdouble;
    V: TVector;
    Len, TileX,TileY: integer;
    s: string;
begin
  // bepaal de object-coords
  OGL.ScreenToObjectCoords(X,Y, objX,objY,objZ);
  V := Vector(objX,objY,objZ);
  // controleer of op de map is geklikt
  if not Game.Map.IsOnTerrain(V) then begin
    PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
    Exit;
  end;

  // test of de tile nog vrij is
  Game.Map.LocationToTile(V, TileX,TileY);
  if Game.Map.Terrain.Tiles[TileX,TileY].Team <> c_NOTEAM then begin
    PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
    Exit;
  end;

  // voeg een spawnpoint toe
  Len := Length(Game.Map.Spawnpoints);
  s := '<GAME.MAP.SPAWNPOINT'+IntToStr(Len)+' NAME>';
  Game.Map.AddSpawnpoint(s,0, TileX,TileY);
  PlaySound(Soundacknowledged,0,SND_ASYNC+SND_NODEFAULT);

  Game.StopConstructing;
end;



//-----------------------------------------------
procedure TGameTech.ConstructPowerpylon(Sender: TObject);
begin
  Game.StartConstructing(cltEntityPowerpylon);
end;

procedure TGameTech.SpawnNewPowerpylon(const X, Y: integer);
var objX,objY,objZ: GLdouble;
    V: TVector;
    Len, TileX,TileY: integer;
    s: string;
begin
  // bepaal de object-coords
  OGL.ScreenToObjectCoords(X,Y, objX,objY,objZ);
  V := Vector(objX,objY,objZ);

  // controleer of op de map is geklikt
  if not Game.Map.IsOnTerrain(V) then begin
    PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
    Exit;
  end;

  // test of de tile nog vrij is
  Game.Map.LocationToTile(V, TileX,TileY);
  if Game.Map.Terrain.Tiles[TileX,TileY].Team <> c_NOTEAM then begin
    PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
    Exit;
  end;

  // voeg een powerpylon toe
  Len := Length(Game.Map.Powerpylons);
  s := '<GAME.MAP.POWERPYLON'+IntToStr(Len)+' NAME>';
  Game.Map.AddPowerpylon(s,0, TileX,TileY);
  PlaySound(Soundacknowledged,0,SND_ASYNC+SND_NODEFAULT);

  Game.StopConstructing;
end;



//-----------------------------------------------
procedure TGameTech.ConstructPowerline(Sender: TObject);
begin
  Game.StartConstructing(cltEntityPowerline);
end;

procedure TGameTech.SpawnNewPowerline(const X, Y: integer);
var objX,objY,objZ: GLdouble;
    V: TVector;
    Len, TileX,TileY, i: integer;
    s: string;
    pFound: PEntity;
begin
//  if Game.ConstructingEntity^.ClassType_ <> cltEntityPowerpylon then Exit;

  // bepaal de object-coords
  OGL.ScreenToObjectCoords(X,Y, objX,objY,objZ);
  V := Vector(objX,objY,objZ);

  // controleer of op de map is geklikt
  if not Game.Map.IsOnTerrain(V) then begin
    PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
    Exit;
  end;

  // de tile moet niet meer vrij zijn, maar in bezit van het eigen team
  Game.Map.LocationToTile(V, TileX,TileY);
  if Game.Map.Terrain.Tiles[TileX,TileY].Team <> c_TEAM0 then begin
    PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
    Exit;
  end;

  // test of de powerline is verbonden tussen 2 geldige entities
  // Het moet zijn: Game.ConstructingEntity & (een powerpylon of spawnpoint)
  pFound := nil;
  for i:=0 to Length(Game.Map.Spawnpoints)-1 do
    if (Game.Map.Spawnpoints[i].TileX=TileX) and (Game.Map.Spawnpoints[i].TileY=TileY) then begin
      pFound := @Game.Map.Spawnpoints[i];
      if PEntitySpawnpoint(pFound)^.ConnectedTo <> nil then pFound := nil; // al verbonden..
      Break;
    end;
  if pFound=nil then
    for i:=0 to Length(Game.Map.Powerpylons)-1 do
      if (Game.Map.Powerpylons[i].TileX=TileX) and (Game.Map.Powerpylons[i].TileY=TileY) then begin
        pFound := @Game.Map.Powerpylons[i];
        if pFound = Game.ConstructingEntity then pFound := nil; // niet aan zichzelf verbinden..
        if pFound <> nil then
          if PEntityPowerpylon(pFound)^.ConnectedTo1 <> nil then pFound := nil; // al verbonden..
        Break;
      end;
  if pFound=nil then begin
    PlaySound(SoundNoOperation,0,SND_ASYNC+SND_NODEFAULT);
    Exit;
  end;

  // voeg een powerline toe
  Len := Length(Game.Map.Powerlines);
  s := '<GAME.MAP.POWERLINE'+IntToStr(Len)+' NAME>';
  Game.Map.AddPowerline(s,0, TileX,TileY, Game.ConstructingEntity, pFound);
  PlaySound(Soundacknowledged,0,SND_ASYNC+SND_NODEFAULT);

  Game.StopConstructing;
end;




//-----------------------------------------------
initialization
  GameTech := TGameTech.Create;

finalization
  GameTech.Free;

end.
