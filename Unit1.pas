unit Unit1;
interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, AppEvnts,
  uEntities, uOpenGL, uThreadMove, uThreadConstruct;

type
  TfMain = class(TForm)
    TimerServerFrame: TTimer;
    TimerFPS: TTimer;
    ApplicationEvents1: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,NewHeight: Integer; var Resize: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TimerServerFrameTimer(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TimerFPSTimer(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure ApplicationEvents1Restore(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  private
    // muis bewegingen
    CenterX, CenterY: integer;   //het midden van het scherm in scherm-coördinaten
    LastX, LastY: Integer;       //vorige muiscursor-positie
    DeltaX, DeltaY: Single;      //laatste verschillen in cursor-positie.
    CameraIndex, CameraMaxIndex: integer;        //een index-nummer om de verschillende camera's te refereren
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSysCommand(var Msg : TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;  //een form-move
    procedure ToggleMouseControl;
    procedure MouseControlOff;
    procedure ToggleVSync;
    procedure ToggleFloatingCamera;
    procedure ToggleShowRoutes;
    procedure ToggleShowAreas;
    procedure ToggleShowTerrain;
    procedure ToggleShowFog;
    procedure ToggleHelp;
    procedure SwitchCamera;
    procedure CalculateCenter;
  public
    // de threads
    thrUnitsAImove: TThreadMove;        // bewegingen
    thrConstruct: TThreadConstruct;     // bouwen
    //
    HelpText: TStringList;  //de help tekst
    procedure InitGraphics;
    procedure StartGame;
    procedure StopGame;
  end;

var
  fMain: TfMain;


implementation
uses uConst, uDisplay, u3DTypes, uCalc, uWavefront, uSelection, uGameTech,
     MMSystem;
{$R *.dfm}

procedure TfMain.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // OWNDC vlag zetten om tegen te gaan dat bij elk Paint-event de RC opnieuw wordt aangemaakt
  // maar dat dit maar 1 keer gebeurd tijdens het Create-event.
  // VREDRAW & HREDRAW vlaggen zetten als er tijdens een resize moet worden getekend..
  Params.WindowClass.style := (Params.WindowClass.style or CS_OWNDC {or CS_VREDRAW or CS_HREDRAW});
  // !NB: CS_OWNDC gaat mis tijdens een fullscreen-swap..daarom dan even GL uitschakelen..

  // fullscreen venster eigenschappen
  //Params.WindowClass.style := (Params.WindowClass.style {or WS_POPUP or WS_CLIPSIBLINGS or WS_CLIPCHILDREN});
end;

procedure TfMain.FormCreate(Sender: TObject);
var AppPath: string;
begin
  DecimalSeparator := '.';  // floats en strings
  Cursor := crDefault;      // de standaard muiscursor gebruiken

  // het OpenGL-object instantiëren
  OGL := TOGL.Create;
  // de helptext stringlist
  HelpText := TStringList.Create;
  // texture zoekpaden instellen
  AppPath := ExtractFilePath(Application.ExeName);
  OGL.Textures.AddSearchDir(AppPath +'textures\');
  OGL.Textures.AddSearchDir(AppPath +'textures\BG\');
  OGL.Textures.AddSearchDir(AppPath +'models\');
  OGL.Textures.AddSearchDir(AppPath);
  // de standaard camera gebruiken
  OGL.Camera.Default;
  OGL.Camera.Floating := true; //op 1 hoogte blijven OF vrij rondvliegen
  //OGL.Camera.SetSpeed(1.0);
  OGL.Camera.SetPositionY(20.0);
(*
  // FPS-meting
  FPS_ := 0;
  FPSCount := 0;
  PerformanceCount := 0;
  QueryPerformanceFrequency(PerformanceFreq);
*)
(*
  // stel de priority van dit process in..
  //   NORMAL_PRIORITY_CLASS:
  //     Specify this class for a process with no special scheduling needs.
  //   IDLE_PRIORITY_CLASS:
  //     Specify this class for a process whose threads run only when the system is idle.
  //   HIGH_PRIORITY_CLASS:
  //     Specify this class for a process that performs time-critical tasks that must be executed immediately.
  //   REALTIME_PRIORITY_CLASS:
  //     Specify this class for a process that has the highest possible priority.
  PriorityClass := GetPriorityClass(GetCurrentProcess);
  Priority      := GetThreadPriority(GetCurrentThread);
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);
*)

  CameraIndex := 0;     // 0     = standaard OGL.Camera
                        // 1..n  = alle game-units
//  CameraMaxIndex := Length(Game.Map.Units);

  // WaveFront models
  WF := TObjWavefront.Create(@OGL);

end;

procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  StopGame;
(*
  // priority herstellen..
  SetThreadPriority(GetCurrentThread, Priority);
  SetPriorityClass(GetCurrentProcess, PriorityClass)
*)
//  TimerFPS.Enabled := false;
//  Player.Camera.MouseControlled := false;
//  OGL.MouseLook := false;
  MouseControlOff;
  // OpenGL uitschakelen
  if OGL.Active then OGL.Disable;
end;

procedure TfMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // de standaard muiscursor gebruiken
  Cursor := crDefault; //ShowCursor(true);
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  // het WaveFront-object vrijgeven
  WF.Free;
  // het OpenGL-object vrijgeven
  OGL.Free;
  // de Helptext stringList vrijgeven
  HelpText.Free;
(*
  // ALT-TAB & CTRL-ALT-DEL gebruik inschakelen
  SystemParametersInfo(SPI_SCREENSAVERRUNNING, word(false), @Dummy, 0);
*)
end;


procedure TfMain.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;  //form achtergrond paint-event als "klaar" markeren..
end;

procedure TfMain.WMSysCommand(var Msg: TWMSysCommand);
begin
  // geen screensaver toestaan..
  if Msg.cmdType = SC_SCREENSAVE then Msg.Result := 1
                                 else inherited;
end;

procedure TfMain.WMMove(var Msg: TWMMove);
begin
  inherited; //Left & Top porperties laten bijwerken..
  CalculateCenter;
end;


procedure TfMain.ApplicationEvents1Minimize(Sender: TObject);
begin
(*
  if IsMultiThread then
    if Assigned(thrUnitsAImove) then
      thrUnitsAImove.Paused := true;
*)
//  WindowState := wsMinimized;
end;

procedure TfMain.ApplicationEvents1Restore(Sender: TObject);
begin
(*
  if IsMultiThread then
    if Assigned(thrUnitsAImove) then
      thrUnitsAImove.Paused := false;
*)
//  WindowState := wsNormal;
end;


procedure TfMain.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
  // het midden van het form bepalen
  CalculateCenter;
  // vorige cursor-positie
//  if Player.Camera.MouseControlled then begin
  if OGL.Camera.MouseControlled then begin
    LastX := CenterY;
    LastY := CenterX;
  end else begin
    LastX := 0;
    LastY := 0;
  end;
  // OpenGL projectie
  OGL.Resize(ClientWidth, ClientHeight);
  Game.RealignHUDs(ClientWidth, ClientHeight);
  if OGL.Active then Display.RenderFrame;  //afbeelden tijdens een resize..
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  HelpText.Clear;
  // opengl initiëren
  InitGraphics;
  //ShowWindow(Application.Handle, SW_HIDE);
  StartGame;
end;

procedure TfMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
  //---
  procedure DoMouse();
  var R: TVector;
  begin
      // rekening houden met de bewegings-gevoeligheid
      DeltaX := DeltaX * OGL.Camera.SensitivityX;
      DeltaY := DeltaY * OGL.Camera.SensitivityY;
      // draai de camera rond op de huidige positie om de Y-as (linksom/rechtsom)
      R := Vector(0.0, DeltaX, 0.0);
      OGL.Camera.RotateLineOfSight(R);
      // verplaats LookAt voor bewegingen omhoog/omlaag
      OGL.Camera.Target.Y := OGL.Camera.Target.Y - DeltaY/200.0;
  end;
  //---
begin
  if not Visible then Exit;
  Display.ShiftPressed := (ssShift in Shift);
  Display.CtrlPressed := (ssCtrl in Shift);
  Display.CtrlShiftPressed := (Display.ShiftPressed or Display.CtrlPressed);

  Display.MouseX := X;
  Display.MouseY := Y;

  // linksom/rechtsom roteren van de camera gaat prima om de Y-as.
  // Als we willen roteren om de X-as (boven/onder), dan moet de UpY-vector
  // ook worden aangepast, anders komt de camera nooit op z'n kop!..
  // Het beeld van een player-model in-game flipt nooit over (zodat het beeld nooit op z'n kop zal zijn).
  // Als de camera omhoog moet kijken, en nooit moet flippen, dan verhoog ik de
  // Camera.LookAt Y-positie. De richting van kijken zal dan nooit flippen want
  // de camera blijft dan altijd vooruit kijken (alleen wat hoger steeds).
  // De lengte van de LineOfSight-vector wordt ook steeds groter (bij verhogen van
  // de Cam.LookAt.Y coördinaat)..Evt. een UnitVector van maken indien nodig..

  // camera-besturing met de muis?
  if OGL.Camera.MouseControlled then begin
    //de muis delta's..
    DeltaX := Mouse.CursorPos.X - CenterX;
    DeltaY := Mouse.CursorPos.Y - CenterY;
    if (DeltaX<>0) or (DeltaY<>0) then begin
      // cursor weer in het midden van het scherm/form plaatsen.
      Mouse.CursorPos := Point(CenterX, CenterY);
      DoMouse;
    end;
  end else begin // geen camera-besturing met de muis..
    // linker muisknop ingedrukt?
    if (ssLeft in Shift) then begin
      // een selectie aan het maken van een stel units in beeld..
      Selection.SetSelectionRectCoords2(X,Y);
    end;
    // rechter muisknop ingedrukt?
    if (ssRight in Shift) then begin
      //de muis delta's..
      DeltaX := (X - LastX)*2;
      DeltaY := (Y - LastY)*2;
      DoMouse;
    end;
  end;
  // laatste cursor-positie onthouden..
  LastX := X;
  LastY := Y;
end;

procedure TfMain.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//var P: TPoint;
begin
  Display.ShiftPressed := (ssShift in Shift);
  Display.CtrlPressed := (ssCtrl in Shift);
  Display.CtrlShiftPressed := (Display.ShiftPressed or Display.CtrlPressed);
  case Button of
    mbLeft : begin
        Display.LMB := true;

       // mouse-look inschakelen indien nodig..
//       if not OGL.Camera.MouseControlled then ToggleMouseControl;

        Selection.SetSelectionRectCoords1(X,Y);
      end;
    mbMiddle : begin
        Display.MMB := true;
      end;
    mbRight : begin
        Display.RMB := true;
(*
        P := ClientToScreen(Point(X,Y));
        popupView.Popup(P.X,P.Y);
*)
      end;
  end;
end;

procedure TfMain.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ShiftPressed: boolean;
    clickedHUD: integer;
    V: TVector;
begin
  ShiftPressed := (ssShift in Shift);
  Display.ShiftPressed := ShiftPressed;
  Display.CtrlPressed := (ssCtrl in Shift);
  Display.CtrlShiftPressed := (Display.ShiftPressed or Display.CtrlPressed);
  case Button of
    mbLeft : begin
        Display.LMB := false;
        Selection.SetSelectionRectCoords2(X,Y);
        clickedHUD := Game.OverHUD(X, Y);
        if clickedHUD<>-1 then
          Selection.SelectButton(clickedHUD,X,Y)
        else begin
          if Game.isConstructing then Game.ConstructBuilding(X,Y)
                                 else Selection.SelectObjects((not ShiftPressed),X,Y);
        end;
      end;
    mbMiddle : begin
        Display.MMB := false;
      end;
    mbRight : begin
        Display.RMB := false;
        clickedHUD := Game.OverHUD(X, Y);
        if clickedHUD<>-1 then
          Selection.CancelButton(clickedHUD,X,Y)
        else begin
          if Game.isConstructing then Game.StopConstructing
                                 else Display.UnitGoto(X,Y);  //!!!!!DEBUG!!!!!test
        end;
      end;
  end;
end;

procedure TfMain.FormDblClick(Sender: TObject);
begin
  //
end;

procedure TfMain.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var speed, height: single;
begin
  if OGL.Camera.Floating then begin
    speed := OGL.Camera.Speed {/ UnitsPerArea};
    OGL.Camera.Move(-speed);
  end else begin
    height := OGL.Camera.Position.Y;
    if height<100.0 then height := height + 1.0;
    OGL.Camera.SetPositionY(height);
  end;
end;

procedure TfMain.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var speed, height: single;
    loc, V: TVector;
begin
  if OGL.Camera.Floating then begin
    speed := OGL.Camera.Speed {/ UnitsPerArea};
    OGL.Camera.Move(speed);
  end else begin
    height := OGL.Camera.Position.Y;
    if height>5.0 then height := height - 1.0;
    OGL.Camera.SetPositionY(height);
(*
    // evt. inzommen op het geselecteerde object
    if Length(Display.SelectedUnits)=0 then Exit;
    loc := Game.Map.Units[Display.SelectedUnits[0]].Location;
    V := ScaleVector(AddVector(loc, OGL.Camera.Position), 0.5);
    OGL.Camera.Position := V;
    V := ScaleVector(AddVector(loc, OGL.Camera.Target), 0.01);
    OGL.Camera.Target := V;
*)
  end;
end;



procedure TfMain.TimerFPSTimer(Sender: TObject);
begin
  OGL.FPSTimer;
end;

procedure TfMain.TimerServerFrameTimer(Sender: TObject);
begin
//  Game.ServerFrame;
end;






procedure TfMain.CalculateCenter;
//var W,H: integer;
begin
  // het midden van het form bepalen in scherm-coördinaten
  {W := Width - ClientWidth;
  H := Height - ClientHeight;}
  CenterX := Left+(Width div 2);
  CenterY := Top+(Height div 2);
end;

procedure TfMain.ToggleMouseControl;
begin
  OGL.Camera.ToggleMouseControl;
  if OGL.Camera.MouseControlled then begin
    Cursor := crNone;
    SetCaptureControl(fMain);      //mouse-event capture
    SetCursorPos(CenterX, CenterY);
  end else begin
    {SetCaptureControl(nil);}      //mouse-event capture opheffen..
    ReleaseCapture;                //mouse-event capture opheffen..
    Cursor := crDefault;
  end;
end;

procedure TfMain.MouseControlOff;
begin
  if not OGL.Camera.MouseControlled then Exit;
  OGL.Camera.MouseControlled := false;
  {SetCaptureControl(nil);}      //mouse-event capture opheffen..
  ReleaseCapture;                //mouse-event capture opheffen..
  Cursor := crDefault;
end;

procedure TfMain.ToggleVSync;
var b: boolean;
begin
  if OGL.GetVSync(b) then OGL.SetVSync(not b);
end;

procedure TfMain.ToggleFloatingCamera;
begin
  OGL.Camera.Floating := not OGL.Camera.Floating;
end;

procedure TfMain.ToggleShowRoutes;
begin
  Game.ShowRoutes := not Game.ShowRoutes
end;

procedure TfMain.ToggleShowAreas;
begin
  Game.ShowTiles := not Game.ShowTiles;
end;

procedure TfMain.ToggleShowTerrain;
begin
  Game.ShowTerrain := not Game.ShowTerrain;
end;

procedure TfMain.ToggleShowFog;
begin
  Game.ShowFog := not Game.ShowFog;
  OGL.Fog := Game.ShowFog;
end;




procedure TfMain.ToggleHelp;
begin
  if HelpText.Count = 0 then begin
    // help tekst opmaken..
    HelpText.Add('F1           Help');
    HelpText.Add('ESC          cancel mouse-look');
    HelpText.Add('M            Mouse-look');
    HelpText.Add('C            switch Camera');
    HelpText.Add('F            toggle Floating camera');
    HelpText.Add('V            toggle Vsync');
    HelpText.Add('B            toggle BoundingBox');
    HelpText.Add('G            toggle selectionmark');
    HelpText.Add('F4           toggle fog');
    HelpText.Add('F5           toggle terrain');
    HelpText.Add('F6           toggle areas');
    HelpText.Add('F8           toggle route');
    HelpText.Add('F9           skybox');
    HelpText.Add('HOME         Reset camera');
  end else
    // help tekst verwijderen
    HelpText.Clear;
end;

procedure TfMain.SwitchCamera;
begin
  Inc(CameraIndex);
  if CameraIndex > CameraMaxIndex then CameraIndex := 0;
  Case CameraIndex of
    0: OGL.ResetCamera;
  //  1..CameraMaxIndex: OGL.Camera := Game.Map.Units[CameraIndex].Camera;
  else
    OGL.Camera := @Game.Map.Units[CameraIndex-1].Camera^;
  end;
end;


procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var CtrlPressed, ShiftPressed, CtrlShiftPressed: boolean;
//---
  procedure checkGroup(const aGroupNr: integer);
  begin
    if CtrlShiftPressed then begin
      Selection.AddGroupToSelection(aGroupNr);
      Selection.CreateGroup(aGroupNr)
    end else
      if CtrlPressed then
        Selection.CreateGroup(aGroupNr)
      else
      if ShiftPressed then
        Selection.AddGroupToSelection(aGroupNr)
      else
        Selection.SelectGroup(aGroupNr);
  end;
//---
begin
  CtrlPressed := (ssCtrl in Shift);
  ShiftPressed := (ssShift in Shift);
  CtrlShiftPressed := (CtrlPressed and ShiftPressed);
  Display.ShiftPressed := ShiftPressed;
  Display.CtrlPressed := CtrlPressed;
  Display.CtrlShiftPressed := CtrlShiftPressed;
  case Key of
    // form
    VK_ESCAPE: MouseControlOff;
    VK_F1: ToggleHelp;
    Ord('m'), Ord('M'): ToggleMouseControl;                        //camera rotatie met de muis besturen
    Ord('c'), Ord('C'): SwitchCamera;                              //camera rotatie met de muis besturen
    Ord('f'), Ord('F'): ToggleFloatingCamera;                      //camera besturing "op 1 hoogte / vrije hoogte" wisselen
    Ord('v'), Ord('V'): ToggleVSync;                               //monitor VSync "aan/uit" wisselen
    Ord('b'), Ord('B'): Game.ShowBoundingBoxes := not Game.ShowBoundingBoxes; //
    Ord('g'), Ord('G'): Game.ShowSelectionMarks := not Game.ShowSelectionMarks; //
    VK_F9: OGL.SkyBox.TogglePaused;                                //een skybox afbeelden of zwarte achtergrond
    VK_F4: ToggleShowFog;                                          //mist tekenen
    VK_F5: ToggleShowTerrain;                                      //terrein tekenen
    VK_F6: ToggleShowAreas;                                        //areas tekenen in ore-kleuren
    VK_F8: ToggleShowRoutes;                                       //lijnen tekenen naar destination
    VK_HOME: OGL.ResetCamera{OGL.Camera.Default};                  //standaard camera instellingen
    // groepen
    Ord('1'): checkGroup(1);
    Ord('2'): checkGroup(2);
    Ord('3'): checkGroup(3);
    Ord('4'): checkGroup(4);
    Ord('5'): checkGroup(5);
    Ord('6'): checkGroup(6);
    Ord('7'): checkGroup(7);
    Ord('8'): checkGroup(8);
    Ord('9'): checkGroup(9);
    Ord('0'): checkGroup(0);
//    VK_LCONTROL: ;
  end;
end;

procedure TfMain.InitGraphics;
begin
  if OGL.Active then Exit;
  OGL.Enable(Handle);

  // de camera wat bewegen, anders kan de speler niet rondsturen..
  OGL.Camera.Position := Vector(300,500,700);
//  OGL.Camera.Target := Vector(500,0,500);
  OGL.Camera.Move(0.00001);

  OGL.SetVSync(true);

  // SkyBox textures aanmaken..
  OGL.SkyBox.Active := OGL.SkyBox.InitTextures('Terra');

  // standaard textures aanmaken..
  Display.Radar_TextureHandle := OGL.Textures.LoadTexture('radar.bmp');                    // radar
  Display.Selected_TextureHandle := OGL.Textures.LoadTexture('selected.tga');              // selectionMark
  Display.Button_TextureHandle := OGL.Textures.LoadTexture('button_template.tga');         // tbv knop
  Display.ButtonOver_TextureHandle := OGL.Textures.LoadTexture('buttonover_template.tga'); // tbv knop
  Display.ButtonDown_TextureHandle := OGL.Textures.LoadTexture('buttondown_template.tga'); // tbv knop
  Display.RoundedHUD_TextureHandle := OGL.Textures.LoadTexture('hud_rounded.tga');         // tbv HUD
  Display.RoundedRectMask_TextureHandle := OGL.Textures.LoadTexture('mask_rounded.tga');   // tbv HUD
  Display.Number0_TextureHandle := OGL.Textures.LoadTexture('numbers\0.tga');                      // getal 0 tbv knop
  Display.Number1_TextureHandle := OGL.Textures.LoadTexture('numbers\1.tga');
  Display.Number2_TextureHandle := OGL.Textures.LoadTexture('numbers\2.tga');
  Display.Number3_TextureHandle := OGL.Textures.LoadTexture('numbers\3.tga');
  Display.Number4_TextureHandle := OGL.Textures.LoadTexture('numbers\4.tga');
  Display.Number5_TextureHandle := OGL.Textures.LoadTexture('numbers\5.tga');
  Display.Number6_TextureHandle := OGL.Textures.LoadTexture('numbers\6.tga');
  Display.Number7_TextureHandle := OGL.Textures.LoadTexture('numbers\7.tga');
  Display.Number8_TextureHandle := OGL.Textures.LoadTexture('numbers\8.tga');
  Display.Number9_TextureHandle := OGL.Textures.LoadTexture('numbers\9.tga');                      // getal 9 tbv knop

//  if not WF.LoadFromFile('models\huiske.obj', 0.01) then ShowMessage('Error loading: models\huiske.obj');
  if not WF.LoadFromFile('models\windmill.obj', 1) then ShowMessage('Error loading: models\windmill.obj');
  if not WF.LoadFromFile('models\harvester.obj', 1) then ShowMessage('Error loading: models\harvester.obj');
  if not WF.LoadFromFile('models\windmill-prop.obj', 1) then ShowMessage('Error loading: models\windmill-prop.ob');
  if not WF.LoadFromFile('models\electro-pole-1.obj', 1) then ShowMessage('Error loading: models\electro-pole-1.obj');

  Display.Init;
end;




procedure TfMain.StartGame;
var V: TVector;
    x,z: single;
begin
  if not OGL.Active then Exit;
  if Assigned(Game) then Exit; // er kan maar 1 game zijn..

  // * Maak een nieuw spel..
  Game := TEntityGame.Create('<GAME NAME>');

  // *   Maak een HUD.
  Game.AddHUD(0, 'HUD_main', hudAlignBottom, 40,-1,c_HUDWIDTH,c_HUDHEIGHT, 0.3,0.3,0.35,1.0, '');
  // *     Met een radar
  Game.HUD[0].AddGFXRadar('Radar');
  // *     Maak Button-sets voor de rest van de techs
//GameTech.CreateButtonSets(Game.HUD[0]);
//GameTech.CreateButtonSets(Game.HUD[0],0);
  GameTech.CreateButtonSets(Game.HUD[0],1);
  GameTech.CreateButtonSets(Game.HUD[0],2);
  GameTech.CreateButtonSets(Game.HUD[0],3);
  GameTech.CreateButtonSets(Game.HUD[0],12);
(*
  // *   Maak nieuwe button-sets voor elke (toepasselijke) entity
  Game.HUD[0].CreateButtonSet('Spawnpoint', cltEntitySpawnpoint);
  Game.HUD[0].ButtonSets[0].AddButton('','button_oreminer.tga');
  Game.HUD[0].ButtonSets[0].Buttons[0].OnUp := SpawnNewOreMiner;
  Game.HUD[0].ButtonSets[0].AddButton('', 'button_oreminer.tga');
  Game.HUD[0].ButtonSets[0].Buttons[1].OnUp := Spawn10NewOreMiner;
  Game.HUD[0].ButtonSets[0].AddButton('', 'button_oreminer.tga');
  Game.HUD[0].ButtonSets[0].Buttons[2].OnUp := Spawn100NewOreMiner;
  Game.HUD[0].ButtonSets[0].AddButton('', 'button_oreminer.tga');
  Game.HUD[0].ButtonSets[0].Buttons[3].OnUp := Spawn1000NewOreMiner;
  Game.HUD[0].ButtonSets[0].Buttons[3].Enabled := false;
  //
  Game.HUD[0].CreateButtonSet('OreMiner', cltEntityOreMiner);
  Game.HUD[0].ButtonSets[1].AddButton('','button_oreminer_upgrade1.tga');
  Game.HUD[0].ButtonSets[1].Buttons[0].OnUp := UpgradeOreMiner;
  Game.HUD[0].ButtonSets[1].AddButton('','button_oreminer_upgrade2.tga');
  Game.HUD[0].ButtonSets[1].Buttons[1].OnUp := UpgradeOreMiner;
  Game.HUD[0].ButtonSets[1].Buttons[1].Enabled := false;
  Game.HUD[0].ButtonSets[1].AddButton('','button_oreminer_upgrade3.tga');
  Game.HUD[0].ButtonSets[1].Buttons[2].OnUp := UpgradeOreMiner;
  Game.HUD[0].ButtonSets[1].Buttons[2].Enabled := false;
  Game.HUD[0].ButtonSets[1].AddButton('','button_oreminer_upgrade4.tga');
  Game.HUD[0].ButtonSets[1].Buttons[3].OnUp := UpgradeOreMiner;
  Game.HUD[0].ButtonSets[1].Buttons[3].Enabled := false;
  Game.HUD[0].ButtonSets[1].AddButton('','button_oreminer_upgrade5.tga');
  Game.HUD[0].ButtonSets[1].Buttons[4].OnUp := UpgradeOreMiner;
  Game.HUD[0].ButtonSets[1].Buttons[4].Enabled := false;
*)
  // een 2e HUD
  Game.AddHUD(1, 'HUD_stats', hudAlignRight, -1,250,113,70, 0.3,0.3,0.35,1.0, '');
  Game.HUD[1].AddGFXFPS('FPS');


  // *   Maak de resources.
  Game.AddResource('Metal', clBlue, '','', 110.00);    // 0
  Game.AddResource('Gold', clYellow, '','', 360.00);   // 1
  Game.AddResource('Choco', $00100040, '','', 10.00);  // 2

  // *   Maak een nieuw team.
  Game.AddTeam('<GAME.TEAM0 NAME>',clRed);                      // de menselijke speler's team
  Game.AddTeam('<GAME.TEAM1 NAME>',clBlue);

  // *     Maak een nieuwe speler.
  Game.Teams[0].AddPlayer('<GAME.TEAM0.PLAYER0 NICK>',clRed);   // de menselijke speler
  Game.Teams[1].AddPlayer('<GAME.TEAM1.PLAYER0 NICK>',clBlue);

  // *   Maak een nieuwe map.
  Game.CreateMap('<GAME.MAP NAME>');

  // *   Maak een terrein (van een heightmap)
  Game.Map.CreateTerrain('textures/heightmap.bmp', 0,500);
(*
  // *   Vul areas met resources.
  Game.Map.CreateRandomOreFields($01,5);  //$07
  Game.Map.CreateRandomOreFields($02,2);
  Game.Map.CreateRandomOreFields($04,1);
*)
  // *     Maak nieuwe spawnpoints.
  Game.Map.AddSpawnpoint('<GAME.MAP.SPAWNPOINT0 NAME>',0, 5,5);  // team0 in het midden van de map
  Game.Map.AddSpawnpoint('<GAME.MAP.SPAWNPOINT1 NAME>',0, 5,4);  // team0 2e spawn


  // * Maak nieuwe units
  Game.Map.AddUnit(c_UNIT_OREMINER,0,Game.Map.Spawnpoints[0].Location);
  (Game.Map.Units[0] as TEntityMover).MaxSpeed := c_MAXSPEED_OREMINER/2; // halve snelheid
  Game.Map.AddUnit(c_UNIT_OREMINER,0,Game.Map.Spawnpoints[1].Location);
  //
  Game.Map.AddSpawnpoint('<GAME.MAP.SPAWNPOINT2 NAME>',1, 7,2);  // team1 spawn
  Game.Map.AddUnit(c_UNIT_OREMINER,1,Game.Map.Spawnpoints[2].Location);


  // tbv alle camera's doorlopen
  CameraMaxIndex := Length(Game.Map.Units);


  // THREADS
//  TimerServerFrame.Enabled := true;
   thrUnitsAImove := TThreadMove.Create(@Game.Map.Units);
   {thrUnitsAImove.Priority := tpHigher;
   thrUnitsAImove.Resume;}

   thrConstruct := TThreadConstruct.Create(@Game, @GameTech);
end;

procedure TfMain.StopGame;
begin
  thrUnitsAImove.Terminate;
  thrConstruct.Terminate;
  Game.Free;
  Game := nil;
end;






end.
