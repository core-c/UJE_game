unit uDisplay;
interface
uses OpenGL, u3DTypes, Types, uEntities;

(**
const
  // constanten tbv. OpenGL selectmode
  selectNames_Spawnpoints = 1;
  selectNames_Units       = 10000;
  selectNames_Last        = 100000;
**)

type
  TDisplay = class(TObject)
  private
    NVisibleUnits: integer;
    //tbv. property
    local_ShowAreas: boolean;
  public
    MouseX, MouseY: integer;            // huidige muiscursor coords
    LMB,MMB,RMB: boolean;               // huidige muistoetsen ingedrukt??
    ShiftPressed, CtrlPressed, CtrlShiftPressed: boolean; // ..
    //
    Radar_TextureHandle,
    Selected_TextureHandle,
    Button_TextureHandle,
    ButtonOver_TextureHandle,
    ButtonDown_TextureHandle,
    RoundedHUD_TextureHandle,
    RoundedRectMask_TextureHandle,
    Number0_TextureHandle,
    Number1_TextureHandle,
    Number2_TextureHandle,
    Number3_TextureHandle,
    Number4_TextureHandle,
    Number5_TextureHandle,
    Number6_TextureHandle,
    Number7_TextureHandle,
    Number8_TextureHandle,
    Number9_TextureHandle: GLuint;
    //
    constructor Create;
    destructor Destroy; override;
    //
    procedure Init;
    //
    procedure RenderFrame;
(*    procedure RenderRadar;*)
    procedure RenderMap;
    procedure RenderCrosshair;
    procedure RenderBoundingBox(const Box: TBoundingBox); overload;
    procedure RenderBoundingBox(const Box: TBoundingBox; const Location: TVector; const R,G,B: GLfloat); overload;
    procedure RenderBoundingBox(const Box: TBoundingBox; const Entity: TEntity); overload;
    //
    procedure RenderHUD;
    //
    procedure RenderUnits;
    procedure RenderUnit(Index:integer);
(**    procedure SELECTMODE_RenderUnits(const firstname:GLuint);**)
    //
    procedure RenderSpawnpoints;
    procedure RenderSpawnpoint(Index:integer);
(**    procedure SELECTMODE_RenderSpawnpoints(const firstname:GLuint);**)
    //
    procedure RenderPowerpylons;
    procedure RenderPowerpylon(Index:integer);

    procedure RenderPowerlines;
    procedure RenderPowerline(Index:integer);

    // picking en selecting
    procedure RenderSelectionRect; // selectie-rect
    procedure RenderSelectionMark(const Entity: TEntity; const Size: single); //selectie om unit
    //
    procedure RenderConstructing;

    procedure UnitGoto(const ScreenX, ScreenY: integer);
    procedure RenderDestination(const Entity: TEntity; const Size: single);
  end;

var Display: TDisplay;


implementation
uses uConst, uOpenGL, uCalc, uCamera, uSkyBox, uFont, Unit1, uSelection, 
     SysUtils, Math,
     Graphics, StrUtils,
     uWavefront, uFrustum,
     Controls;








{--- TDisplay -----------------------------------}
constructor TDisplay.Create;
begin
  //
end;

destructor TDisplay.Destroy;
begin
  //
  inherited;
end;




//-----------------------------------------------
procedure TDisplay.Init;
begin
  glPolygonStipple(@HalfTone);
end;



//-----------------------------------------------
// 3D
procedure TDisplay.RenderMap;
var x,y: integer;
    c: TColor;
    r,g,b: single;
    rec: TRect;
    Tile: PEntityTile;
//    V1,V2,V3,V4: TVector;
begin
  glEnable(GL_DEPTH_TEST);
  glFrontFace(GL_CCW);
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);

  // teken het terrein
  if Game.ShowTerrain then Game.Map.Terrain.Render;

  if Game.ShowTiles then
    // teken de areas
    for y:=0 to Game.Map.Terrain.Height-2 do
      for x:=0 to Game.Map.Terrain.Width-2 do begin
        Tile := @Game.Map.Terrain.Tiles[x,y];
  (*
    // doe een simpele/snelle occlusion-test
    V1 := Vector(Area.MinX,0,Area.MinY);
    V2 := Vector(Area.MaxX,0,Area.MaxY);
    V3 := Vector(Area.MinX,0,Area.MaxY);
    V4 := Vector(Area.MaxX,0,Area.MinY);
    if not OGL.Frustum.PointInFrustum(V1) then
    if not OGL.Frustum.PointInFrustum(V2) then
    if not OGL.Frustum.PointInFrustum(V3) then
    if not OGL.Frustum.PointInFrustum(V4) then Continue;
  *)

        if Tile^.Team = c_NOTEAM then begin
          //*c := Area.Color
          r := Tile.ColorR;
          g := Tile.ColorR;
          b := Tile.ColorR;
        end else begin
          //c := Game.Teams[Area.Team].Color;
          with Game.Teams[Tile^.Team] do begin
            r := ColorR;
            g := ColorR;
            b := ColorR;
          end;
        end;
        //*TColorToRGB(c, r,g,b);
        rec := Rect(Floor(Tile^.MinX),Floor(Tile^.MinY), Floor(Tile^.MaxX),Floor(Tile^.MaxY));
        // area
        glBegin(GL_QUADS);
          glColor3f(r,g,b);
          glVertex3f(rec.Left,0,rec.Bottom);
          glVertex3f(rec.Right,0,rec.Bottom);
          glVertex3f(rec.Right,0,rec.Top);
          glVertex3f(rec.Left,0,rec.Top);
        glEnd;
        // gridlijnen
        glPushAttrib(GL_LINE_BIT);
        glLineWidth(2);
        glBegin(GL_LINES);
          glColor3f(1,1,0);
          glVertex3f(rec.Left,0.1,rec.Bottom);
          glVertex3f(rec.Right,0.1,rec.Bottom);
          //
          glVertex3f(rec.Right,0.1,rec.Bottom);
          glVertex3f(rec.Right,0.1,rec.Top);
        glEnd;
        glPopAttrib;
      end;
(*
  // teken de spawnpunten (met windmolens)
  RenderSpawnpoints;
  // teken de powerpylons
  RenderPowerpylons;
  // teken de powerlines
  RenderPowerlines;
*)
end;


//-----------------------------------------------
// 3D
procedure TDisplay.RenderSpawnpoint(Index: integer);
var Spawnpoint: TEntitySpawnpoint;
    Tile: PEntityTile;
    loc: TVector;
    Objwm,Objwmp: TWavefront;
    BoundingBox: TBoundingBox;
    c: TColor;
    r,g,b: single;
    deg: single;
    ofsX,ofsZ: single;
begin
  if (Index<0) or (Index>Length(Game.Map.Spawnpoints)-1) then Exit;
//glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_ADD);

  Spawnpoint := Game.Map.Spawnpoints[Index];
  Tile := @Game.Map.Terrain.Tiles[Spawnpoint.TileX,Spawnpoint.TileY];
  with Spawnpoint.Location do loc := Vector(X,Y,Z);

  Objwm := WF.ObjectByName('windmill');
  // doe een simpele/snelle occlusion-test
  {if not OGL.Frustum.PointInside(loc) then Exit;}
  if not OGL.Frustum.AABBInside(loc,Objwm.BoundingBox) then Exit;
  // render de windmolen-standaard..
  Objwm.Render(loc);

  // team-kleur instellen
  with Game.Teams[Spawnpoint.Team] do glBlendColorEXT(ColorR,ColorG,ColorB,1);

  // render de windmolen-rotor
  Objwmp := WF.ObjectByName('windmill-prop');
  deg := Objwmp.Rotation.Z;
  glPushMatrix;
    glPushMatrix;
      glTranslatef(loc.X,loc.Y+28,loc.Z); // de propellor zit op 28 units hoogte
      glRotatef(deg,0,0,1);
      Objwmp.Render(NullVector{loc});
    glPopMatrix;
    ofsX := (c_TILESIZE/8);
    ofsZ := -ofsX;
    glTranslatef(loc.X+ofsX,loc.Y,loc.Z+ofsZ); // de propellor zit op 28 units hoogte
    WF.ObjectByName('electro-pole-1').Render(NullVector);
  glPopMatrix;

  Objwmp.Rotation.Z := deg+0.3;

  if Spawnpoint.Selected then begin
    RenderSelectionMark(Spawnpoint, 20.0);  // teken en selectie marker om de spawn
    RenderBoundingBox(Objwm.BoundingBox, Spawnpoint );
  end;
end;

procedure TDisplay.RenderSpawnpoints;
var s: integer;
begin
  for s:=Low(Game.Map.Spawnpoints) to High(Game.Map.Spawnpoints) do RenderSpawnpoint(s);
end;



//-----------------------------------------------
// 3D
procedure TDisplay.RenderPowerpylon(Index: integer);
var Powerpylon: TEntityPowerpylon;
    Tile: PEntityTile;
    loc: TVector;
    Objwm: TWavefront;
begin
  if (Index<0) or (Index>Length(Game.Map.Powerpylons)-1) then Exit;

  Powerpylon := Game.Map.Powerpylons[Index];
  Tile := @Game.Map.Terrain.Tiles[Powerpylon.TileX,Powerpylon.TileY];
  with Powerpylon.Location do loc := Vector(X,Y,Z);

  Objwm := WF.ObjectByName('electro-pole-1');
  // doe een simpele/snelle occlusion-test
  {if not OGL.Frustum.PointInside(loc) then Exit;}
  if not OGL.Frustum.AABBInside(loc,Objwm.BoundingBox) then Exit;
  // render de mast..
  Objwm.Render(loc);

  // team-kleur instellen
  with Game.Teams[Powerpylon.Team] do glBlendColorEXT(ColorR,ColorG,ColorB,1);

  if Powerpylon.Selected then begin
    RenderSelectionMark(Powerpylon, 20.0);  // teken en selectie marker om de spawn
    RenderBoundingBox(Objwm.BoundingBox, Powerpylon);
  end;
end;

procedure TDisplay.RenderPowerpylons;
var p: integer;
begin
  for p:=Low(Game.Map.Powerpylons) to High(Game.Map.Powerpylons) do RenderPowerpylon(p);
end;



//-----------------------------------------------
// 3D
procedure TDisplay.RenderPowerline(Index: integer);
var Powerline: TEntityPowerline;
begin
  if (Index<0) or (Index>Length(Game.Map.Powerlines)-1) then Exit;

  Powerline := Game.Map.Powerlines[Index];

  // team-kleur instellen
  with Game.Teams[Powerline.Team] do glBlendColorEXT(ColorR,ColorG,ColorB,1);

  Powerline.Render;
(*
  if Powerline.Selected then
    RenderSelectionMark(Powerline, 20.0);  // teken en selectie marker om de powerline
*)
end;

procedure TDisplay.RenderPowerlines;
var p: integer;
begin
  for p:=Low(Game.Map.Powerlines) to High(Game.Map.Powerlines) do RenderPowerline(p);
end;



//-----------------------------------------------
// 2D
procedure TDisplay.RenderCrosshair;
const InnerSize=4; OuterSize=10;
var W,H: integer;
    C: TPoint;
begin
  C := OGL.Center;
  // instellen om 2D te tekenen
  {OGL.SetupFor2D;}
  // teken de crosshair
  glDisable(GL_LIGHTING);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
  glBegin(GL_LINES);
    glColor4f(1.0,1.0,0.0, 1.0);
    // boven
    glVertex2i(C.X, C.Y+OuterSize);
    glVertex2i(C.X, C.Y+InnerSize);
    // onder
    glVertex2i(C.X, C.Y-InnerSize);
    glVertex2i(C.X, C.Y-OuterSize);
    // links
    glVertex2i(C.X-OuterSize, C.Y);
    glVertex2i(C.X-InnerSize, C.Y);
    // rechts
    glVertex2i(C.X+InnerSize, C.Y);
    glVertex2i(C.X+OuterSize, C.Y);
  glEnd;
end;


//-----------------------------------------------
procedure TDisplay.RenderBoundingBox(const Box: TBoundingBox; const Location: TVector; const R,G,B: GLfloat);
var V1,V2,V3,V4,V5,V6,V7,V8: TVector;
begin
  if not Game.ShowBoundingBoxes then Exit;
  // alle 8 punten van de boundingbox                             //       V8______V2
  V1 := AddVector(Box.Min, Location);                             //       /|    /|
  V2 := AddVector(Box.Max, Location);                             //    V5/_|___/ |
  V3 := Vector(V2.X,V1.Y,V1.Z);                                   //      | | V4| |
  V4 := Vector(V2.X,V2.Y,V1.Z);                                   //      | V6__|_|V7
  V5 := Vector(V1.X,V2.Y,V1.Z);                                   //      | /   | /
  V6 := Vector(V1.X,V1.Y,V2.Z);                                   //      |/____|/
  V7 := Vector(V2.X,V1.Y,V2.Z);                                   //     V1     V3
  V8 := Vector(V1.X,V2.Y,V2.Z);                                   //

  glEnable(GL_DEPTH_TEST);
  glFrontFace(GL_CCW);
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);
  glPolygonMode(GL_FRONT, GL_LINE);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
  glColor3f(R,G,B);
  glBegin(GL_QUADS);
    with V1 do glVertex3f(X,Y,Z);
    with V3 do glVertex3f(X,Y,Z);
    with V4 do glVertex3f(X,Y,Z);
    with V5 do glVertex3f(X,Y,Z);

    with V3 do glVertex3f(X,Y,Z);
    with V7 do glVertex3f(X,Y,Z);
    with V2 do glVertex3f(X,Y,Z);
    with V4 do glVertex3f(X,Y,Z);

    with V7 do glVertex3f(X,Y,Z);
    with V6 do glVertex3f(X,Y,Z);
    with V8 do glVertex3f(X,Y,Z);
    with V2 do glVertex3f(X,Y,Z);

    with V6 do glVertex3f(X,Y,Z);
    with V1 do glVertex3f(X,Y,Z);
    with V5 do glVertex3f(X,Y,Z);
    with V8 do glVertex3f(X,Y,Z);

    with V5 do glVertex3f(X,Y,Z);
    with V4 do glVertex3f(X,Y,Z);
    with V2 do glVertex3f(X,Y,Z);
    with V8 do glVertex3f(X,Y,Z);

    with V6 do glVertex3f(X,Y,Z);
    with V7 do glVertex3f(X,Y,Z);
    with V3 do glVertex3f(X,Y,Z);
    with V1 do glVertex3f(X,Y,Z);
  glEnd;
  glPolygonMode(GL_FRONT, GL_FILL);
  glEnable(GL_DEPTH_TEST);
end;

procedure TDisplay.RenderBoundingBox(const Box: TBoundingBox;
                                     const Entity: TEntity);
var R,G,B: single;
begin
  if not Game.ShowBoundingBoxes then Exit;
  //*TColorToRGB(Game.Teams[Entity.Team].Color, R,G,B);
  //*RenderBoundingBox(Box, Entity.Location, R,G,B);
  with Game.Teams[Entity.Team] do RenderBoundingBox(Box, Entity.Location, ColorR,ColorG,ColorB);
end;

procedure TDisplay.RenderBoundingBox(const Box: TBoundingBox);
begin
  if not Game.ShowBoundingBoxes then Exit;
  RenderBoundingBox(Box, NullVector, 0.5,0.5,0.5);
end;


//-----------------------------------------------
// 2D
(*
procedure TDisplay.RenderRadar;
const MarginTop=10;  MarginRight=10;  IndicatorLength=8;
var X,Y: integer;
    V: TVector;
begin
  if Radar_TextureHandle = 0 then Exit;
  // 2D tekenen..
  {OGL.SetupFor2D;}
  X := OGL.Width-96-MarginRight;
  Y := OGL.Height-96-MarginTop;
//  Y := 96+MarginTop;

  glScissor(X, Y, 96, 96);
  glEnable(GL_SCISSOR_TEST);

  // het plaatje om de rand van de radar
  glFrontFace(GL_CCW);
  {glCullFace(GL_FRONT);}
  glDisable(GL_CULL_FACE);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_COLOR);  //one,one
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, Radar_TextureHandle);
  glBegin(GL_QUADS);
    glTexCoord2f(0,0);  glVertex2f(X,Y+96);
    glTexCoord2f(0,1);  glVertex2f(X,Y);
    glTexCoord2f(1,1);  glVertex2f(X+96,Y);
    glTexCoord2f(1,0);  glVertex2f(X+96,Y+96);
  glEnd;
  glDisable(GL_TEXTURE_2D);

  // rest nog de camera/player-positie & -richting markeren..
  // De player staat relatief gezien altijd in het midden van het radarbeeld.
  // Een punt zetten in het midden van het radarbeeld..
  glPointSize(3);
  X := X + 48;
  Y := Y + 48;
  glBegin(GL_POINTS);
    glColor3f(1.0, 1.0, 0.0);
    glVertex2f(X, Y);
  glEnd;
  // Een lijntje tekenen in de richting waarin de camera kijkt
  // De negatieve Z-as wijst in de richting van het noorden op het radar.
  V := OGL.Camera.LineOfSight;
  glBegin(GL_LINES);
    glVertex2f(X, Y);
    glVertex2f(X+(V.X*IndicatorLength), Y+(V.Z*IndicatorLength));
  glEnd;
  //

  glDisable(GL_SCISSOR_TEST);

{
  // het radar staat in de rechter-bovenhoek van het scherm.
  // Het radar-masker is 96x96 pixels.
  glClearStencil(0);
  glEnable(GL_STENCIL_TEST);
  // het masker tekenen
  glClear(GL_STENCIL_BUFFER_BIT);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
  // teken de circel vorm van het masker
  glFrontFace(GL_CW);
  glBegin(GL_TRIANGLES);
    glColor3f(0.0, 0.0, 0.0);
    glVertex2f(X+0,Y+0);
    glVertex2f(X+100,Y+100);
    glVertex2f(X+100,Y+0);
  glEnd;
  // teken het radar
  glStencilFunc(GL_NOTEQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glBegin(GL_QUADS);
    glColor4f(1.0,1.0,1.0,1.0);
    glVertex2f(X+0,Y+0);
    glVertex2f(X+0,Y+300);
    glVertex2f(X+300,Y+300);
    glVertex2f(X+300,Y+0);
  glEnd;
  //
  glDisable(GL_STENCIL_TEST);
}
end;
*)

//-----------------------------------------------
// 2D
procedure TDisplay.RenderHUD;
const AlphaOver: array[0..3] of GLFloat = (0,0,0, 0.4);
      cFPSTEXTWIDTH = 26;
var rec: TRect;
    h, b,bs: integer;
    bx,by,bxw,byh: integer;
    bxc,byc: integer;
    bqw,bqh: single;
    ButtonsWidth: integer;
    CheckOverButtons: boolean;
    i,j,f,p: integer;
    disk : PGLUquadric;
//--
  procedure RenderButton(var aHUD: TEntityHUD; var aButton: TEntityButton);
  var radarh: integer;
  begin
    with aHUD do begin
      with aButton do begin
        if not Visible then Exit;

        bx := pxLeft + OffsetX;
        bxw := pxRight + OffsetX;
        by := OGL.Height - pxTop - OffsetY;
        byh := OGL.Height - pxBottom - OffsetY;
        bxc := bx + (c_BUTTONWIDTH div 2);
        byc := byh + (c_BUTTONHEIGHT div 2);
        bqw := c_BUTTONWIDTH/2/12.5;
        bqh := c_BUTTONHEIGHT/2/12.5;

        // knop afbeelding
        if TextureHandle<>0 then begin
          glEnable(GL_TEXTURE_2D);
          glBindTexture(GL_TEXTURE_2D, TextureHandle);
          glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
          glEnable(GL_BLEND);
          glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
//!          glDisable(GL_BLEND);
//!          glEnable(GL_ALPHA_TEST);
//!          glAlphaFunc(GL_GREATER, 0);
          glBegin(GL_QUADS);
            glTexCoord2f(0,1);  glVertex2f(bx,  by);
            glTexCoord2f(1,1);  glVertex2f(bxw, by);
            glTexCoord2f(1,0);  glVertex2f(bxw, byh);
            glTexCoord2f(0,0);  glVertex2f(bx,  byh);
          glEnd;
//!          glDisable(GL_ALPHA_TEST);
        end;

        // knop niet ingeschakeld??
        if not Enabled then begin
          glEnable(GL_POLYGON_STIPPLE);
          glPolygonStipple(@HalfTone);   // een grijs patroon
          glDisable(GL_TEXTURE_2D);
          glDisable(GL_BLEND);
          glColor3f(0.3,0.3,0.3);
          glBegin(GL_QUADS);
            glVertex2f(bx,  by);
            glVertex2f(bxw, by);
            glVertex2f(bxw, byh);
            glVertex2f(bx,  byh);
          glEnd;
          glDisable(GL_POLYGON_STIPPLE);
        end else //not enabled

          // reeds gekocht/gebouwd?? en knop nu helemaal klaar/uitgebouwd..
          if Done then begin
            glColor3f(0,0,0);
            glEnable(GL_TEXTURE_2D);
            glEnable(GL_BLEND);
            glBindTexture(GL_TEXTURE_2D, ButtonOver_TextureHandle);
            glBlendFunc(GL_ONE, GL_ONE);   //A=1.0
            glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_ADD);
            glBegin(GL_QUADS);
              glTexCoord2f(0,1);  glVertex2f(bx,  by);
              glTexCoord2f(1,1);  glVertex2f(bxw, by);
              glTexCoord2f(1,0);  glVertex2f(bxw, byh);
              glTexCoord2f(0,0);  glVertex2f(bx,  byh);
            glEnd;
          end else begin

            // een constructie is klaar om te worden geplaatst??
            if (Ready>=1) and Blinking and BlinkingOn then begin
              // knop laten knipperen
              glDisable(GL_TEXTURE_2D);
              glEnable(GL_BLEND);
              glBlendFunc(GL_ONE, GL_ONE);
              glColor4f(1,1,0,0.3);
              glBegin(GL_QUADS);
                glVertex2f(bx,  by);
                glVertex2f(bxw, by);
                glVertex2f(bxw, byh);
                glVertex2f(bx,  byh);
              glEnd;
            end else begin

              p := 100 - Percentage;
              // afbeelden precentage gereed tijdens bouwen
              if (p in [1..99]) then begin
                glDisable(GL_TEXTURE_2D);
                glEnable(GL_BLEND);
                glBlendFunc(GL_ONE, GL_ONE);
                glColor4f(1,1,0,0.3);

                //disk := gluNewQuadric;
                //gluPartialDisk(disk, 0, aButton.Width, 12, 1, 0, aButton.Percentage*3.6);
                //gluDeleteQuadric(disk);

                if (p>=13) then begin
                  glBegin(GL_TRIANGLES);
                    glVertex2f(bxc, byc);
                    glVertex2f(bxw, by);
                    glVertex2f(bxc, by);
                  glEnd;
                  if (p>=25) then begin
                    glBegin(GL_TRIANGLES);
                      glVertex2f(bxc, byc);
                      glVertex2f(bxw, byc);
                      glVertex2f(bxw, by);
                    glEnd;
                    if (p>=38) then begin
                      glBegin(GL_TRIANGLES);
                        glVertex2f(bxw, byh);
                        glVertex2f(bxw, byc);
                        glVertex2f(bxc, byc);
                      glEnd;
                      if (p>=50) then begin
                        glBegin(GL_TRIANGLES);
                          glVertex2f(bxc, byh);
                          glVertex2f(bxw, byh);
                          glVertex2f(bxc, byc);
                        glEnd;
                        if (p>=63) then begin
                          glBegin(GL_TRIANGLES);
                            glVertex2f(bx, byh);
                            glVertex2f(bxc, byh);
                            glVertex2f(bxc, byc);
                          glEnd;
                          if (p>=75) then begin
                            glBegin(GL_TRIANGLES);
                              glVertex2f(bx, byh);
                              glVertex2f(bxc, byc);
                              glVertex2f(bx, byc);
                            glEnd;
                            if (p>=88) then begin
                              glBegin(GL_TRIANGLES);
                                glVertex2f(bx, byc);
                                glVertex2f(bxc, byc);
                                glVertex2f(bx, by);
                              glEnd;
                              // [88..100]
                              glBegin(GL_TRIANGLES);
                                glVertex2f(bxc, byc);
                                glVertex2f(bxc-Floor(bqw*(100.0-p)), by);
                                glVertex2f(bx, by);
                              glEnd;
                            end else begin
                              // [75..87]
                              glBegin(GL_TRIANGLES);
                                glVertex2f(bx, byc);
                                glVertex2f(bxc, byc);
                                glVertex2f(bx, by-Floor(bqh*(87.5-p)));
                              glEnd;
                            end;
                          end else begin
                            // [63..74]
                            glBegin(GL_TRIANGLES);
                              glVertex2f(bx, byh);
                              glVertex2f(bxc, byc);
                              glVertex2f(bx, byc-Floor(bqh*(75.0-p)));
                            glEnd;
                          end;
                        end else begin
                          // [50..62]
                          glBegin(GL_TRIANGLES);
                            glVertex2f(bxc, byh);
                            glVertex2f(bxc, byc);
                            glVertex2f(bx+Floor(bqw*(62.5-p)), byh);
                          glEnd;
                        end;
                      end else begin
                        // [38..49]
                        glBegin(GL_TRIANGLES);
                          glVertex2f(bxw, byh);
                          glVertex2f(bxc, byc);
                          glVertex2f(bxc+Floor(bqw*(50.0-p)), byh);
                        glEnd;
                      end;
                    end else begin
                      // [25..37]
                      glBegin(GL_TRIANGLES);
                        glVertex2f(bxw, byc);
                        glVertex2f(bxc, byc);
                        glVertex2f(bxw, byh+Floor(bqh*(37.5-p)));
                      glEnd;
                    end;
                  end else begin
                    // [13..24]
                    glBegin(GL_TRIANGLES);
                      glVertex2f(bxw, by);
                      glVertex2f(bxc, byc);
                      glVertex2f(bxw, byc+Floor(bqh*(25.0-p)));
                    glEnd;
                  end;
                end else begin
                  // [0..12]
                  glBegin(GL_TRIANGLES);
                    glVertex2f(bxc, by);
                    glVertex2f(bxc, byc);
                    glVertex2f(bxw-Floor(bqw*(12.5-p)), by);
                  glEnd;
                end;
              end;
            end;

            // heeft deze knop een queue??
            if (QueueCount>0) and (Queued>0) then begin
              // een evt. aantal nog in de queue afbeelden
              glEnable(GL_TEXTURE_2D);
              case Queued of
                1: glBindTexture(GL_TEXTURE_2D, Number1_TextureHandle);
                2: glBindTexture(GL_TEXTURE_2D, Number2_TextureHandle);
                3: glBindTexture(GL_TEXTURE_2D, Number3_TextureHandle);
                4: glBindTexture(GL_TEXTURE_2D, Number4_TextureHandle);
                5: glBindTexture(GL_TEXTURE_2D, Number5_TextureHandle);
                6: glBindTexture(GL_TEXTURE_2D, Number6_TextureHandle);
                7: glBindTexture(GL_TEXTURE_2D, Number7_TextureHandle);
                8: glBindTexture(GL_TEXTURE_2D, Number8_TextureHandle);
                9: glBindTexture(GL_TEXTURE_2D, Number9_TextureHandle);
              end;
              glEnable(GL_BLEND);
//              glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_COLOR);
              glBlendFunc(GL_CONSTANT_COLOR_EXT, GL_ONE_MINUS_SRC_COLOR);  // constante kleur
              glBlendColorEXT(1,0,0, 1);                                   //
              glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
              glColor4f(0,0,0,0.5);
              glBegin(GL_QUADS);
                glTexCoord2f(0,1);  glVertex2f(bx,  by);
                glTexCoord2f(1,1);  glVertex2f(bxw, by);
                glTexCoord2f(1,0);  glVertex2f(bxw, byh);
                glTexCoord2f(0,0);  glVertex2f(bx,  byh);
              glEnd;
            end;

          end;

        // knop template
        if Enabled then
          if (Button_TextureHandle<>0) and (ButtonOver_TextureHandle<>0) and (ButtonDown_TextureHandle<>0) then begin
            glColor3f(0,0,0);
            glEnable(GL_TEXTURE_2D);
            glEnable(GL_BLEND);
            if CheckOverButtons and OverButton(MouseX, MouseY) then begin
              if LMB then begin
                // onMouseDown
                glBindTexture(GL_TEXTURE_2D, ButtonDown_TextureHandle);
                glBlendFunc(GL_ONE, GL_ONE);   //A=1.0
              end else begin
                // onMouseOver
                glBindTexture(GL_TEXTURE_2D, ButtonOver_TextureHandle);
                glBlendFunc(GL_ONE, GL_ONE);   //A=1.0
              end;
            end else begin
              // not onMouseOver
              glBindTexture(GL_TEXTURE_2D, Button_TextureHandle);
              glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
              glColor4f(0,0,0, 0.3);           //A=0.7
            end;
            glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_ADD);
            glBegin(GL_QUADS);
              glTexCoord2f(0,1);  glVertex2f(bx,  by);
              glTexCoord2f(1,1);  glVertex2f(bxw, by);
              glTexCoord2f(1,0);  glVertex2f(bxw, byh);
              glTexCoord2f(0,0);  glVertex2f(bx,  byh);
            glEnd;
          end;

      end;
    end;
  end;
//--
  procedure RenderRadar(var aHUD: TEntityHUD);
  const MarginTop=10;  MarginRight=10;  IndicatorLength=8;
  var X,Y, CX,CY: integer;
      V: TVector;
  begin
    if Radar_TextureHandle = 0 then Exit;

    if not aHUD.GFX_Radar.Visible then Exit;


    bx := aHUD.GFX_Radar.pxLeft + aHUD.OffsetX;
    bxw := aHUD.GFX_Radar.pxRight + aHUD.OffsetX;
    by := OGL.Height - aHUD.GFX_Radar.pxTop - aHUD.OffsetY;
    byh := OGL.Height - aHUD.GFX_Radar.pxBottom - aHUD.OffsetY;

    // 2D tekenen..
    {OGL.SetupFor2D;}
    X := bx;
    Y := byh;

  //!  glScissor(X, Y, 96, 96);
  //!  glEnable(GL_SCISSOR_TEST);

    // het plaatje om de rand van de radar
    glFrontFace(GL_CCW);
    {glCullFace(GL_FRONT);}
    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_COLOR);  //one,one
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, Radar_TextureHandle);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
    glBegin(GL_QUADS);
      glTexCoord2f(0,0);  glVertex2f(X,Y+c_RADARWIDTHHEIGHT);
      glTexCoord2f(0,1);  glVertex2f(X,Y);
      glTexCoord2f(1,1);  glVertex2f(X+c_RADARWIDTHHEIGHT,Y);
      glTexCoord2f(1,0);  glVertex2f(X+c_RADARWIDTHHEIGHT,Y+c_RADARWIDTHHEIGHT);
    glEnd;
    glDisable(GL_TEXTURE_2D);

    // rest nog de camera/player-positie & -richting markeren..
    // De player staat relatief gezien altijd in het midden van het radarbeeld.
    // Een punt zetten in het midden van het radarbeeld..
    glPointSize(3);
    CX := X + (c_RADARWIDTHHEIGHT div 2);
    CY := Y + (c_RADARWIDTHHEIGHT div 2);
    glColor3f(1.0, 1.0, 0.0);
    glBegin(GL_POINTS);
      glVertex2f(CX, CY);
    glEnd;
    // Een lijntje tekenen in de richting waarin de camera kijkt
    // De negatieve Z-as wijst in de richting van het noorden op het radar.
    V := OGL.Camera.LineOfSight;
    glBegin(GL_LINES);
      glVertex2f(CX, CY);
      glVertex2f(CX+(V.X*IndicatorLength), CY+(V.Z*IndicatorLength));
    glEnd;
    //

  //!  glDisable(GL_SCISSOR_TEST);
//    glDisable(GL_BLEND);
  end;
//--
begin
  for h:=Low(Game.HUD) to High(Game.HUD) do begin
    if not Game.HUD[h].Visible then Continue;
    glFrontFace(GL_CCW);
    glCullFace(GL_BACK);
    glEnable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    //glEnable(GL_BLEND);
    {OGL.SetupFor2D;}
    with Game.HUD[h] do begin
      CheckOverButtons := Game.HUD[h].OverHUD(MouseX, MouseY);

      // HUD achtergrond
      rec.Left := pxLeft + OffsetX;
      rec.Top := pxTop + OffsetY;
      rec.Right := pxRight + OffsetX;
      rec.Bottom := pxBottom + OffsetY;
//      OGL.AlphaRectangle2D(rec, ColorR,ColorG,ColorB,ColorA);
      OGL.TexturedRectangle2D(rec, ColorR,ColorG,ColorB,ColorA, RoundedHUD_TextureHandle, RoundedRectMask_TextureHandle, Angle0);
OGL.SetupFor2D;//hmm elke keer weer opnieuw instellen..:S /fixit
      // HUD titelbalk
      rec.Left := pxLeft + OffsetX;
      rec.Top := pxTop + OffsetY;
      rec.Right := pxRight + OffsetX;
      rec.Bottom := pxTop+c_HUDTITLEBARHEIGHT + OffsetY;
      OGL.AlphaRectangle2D(rec, 0,0,0,0.2);   // ik zie nog een overlappig op de hoekjes :( /fixit
OGL.SetupFor2D;//hmm elke keer weer opnieuw instellen..:S /fixit

      // de Radar-grafiek
      if (GFX_Radar<>nil) then begin
        if GFX_Radar.Visible then
          with GFX_Radar do begin
            RenderRadar(Game.HUD[h]);
          end;
      end;

      // de vaste knoppen onder de radar
      if Length(ButtonSets)>0 then
        for b:=Low(ButtonSets[0].Buttons) to High(ButtonSets[0].Buttons) do
          RenderButton(Game.HUD[h],ButtonSets[0].Buttons[b]); //de root komt altijd in de eerste buttonset
        {for bs:=Low(ButtonSets) to High(ButtonSets) do
          if ButtonSets[bs].AssignedClassType = cltEntityRoot then begin
            for b:=Low(ButtonSets[bs].Buttons) to High(ButtonSets[bs].Buttons) do
              RenderButton(Game.HUD[h],ButtonSets[bs].Buttons[b]);
            break;
          end;}
      // HUD sysmenu knoppen afbeelden
      for b:=Low(SysButtons) to High(SysButtons) do
        RenderButton(Game.HUD[h],SysButtons[b]);
      // HUD entity knoppen afbeelden
      if ButtonsPtr<>nil then
        for b:=Low(ButtonsPtr.Buttons) to High(ButtonsPtr.Buttons) do
          RenderButton(Game.HUD[h],ButtonsPtr.Buttons[b]);

      // de FPS-grafiek
      if (GFX_FPS<>nil) then begin
        if GFX_FPS.Visible then
          with GFX_FPS do begin
            glDisable(GL_BLEND);
            glDisable(GL_TEXTURE_2D);
            glLineWidth(1);
            glColor3f(0,1,0);

            by := OGL.Height - pxTop - OffsetY - Height - 2;

            // teken de lijnen in de grafiek.
            // de huidige aan de rechterkant, en de oudere naar links aanvullen.
            // ..dan scrollt de grafiek.
            glBegin(GL_LINES);
              for i:=FPSIndex-1 downto 1 do begin   // 1 lijntje overslaan..spleetje maken
                bx := pxLeft + OffsetX + NFPS - (FPSIndex-i);
                f := aFPS[i];
                if f>100{maxfps} then f:=100;
                byh := by + Round((Height-2)/100*f);
                // 1 verticale lijn per FPS-meting
                glVertex2f(bx,by);
                glVertex2f(bx,byh);
              end;
            glEnd;

            bx := pxLeft + OffsetX + NFPS - (FPSIndex-1);
            f := aFPS[0];
            if f>100{maxfps} then f:=100;
            byh := by + Round((Height-2)/100*f);
            glColor3f(1,1,0);
            glBegin(GL_LINES);
              glVertex2f(bx,by);
              glVertex2f(bx,byh);
            glEnd;
            glColor3f(0,1,0);

            glBegin(GL_LINES);
              for i:=-1 downto -(NFPS-FPSIndex)+1+2 do begin
                bx := pxLeft + OffsetX + NFPS - (FPSIndex-i);
                f := aFPS[NFPS+i];
                if f>100{maxfps} then f:=100;
                byh := by + Round((Height-2)/100*f);
                // 1 verticale lijn per FPS-meting
                glVertex2f(bx,by);
                glVertex2f(bx,byh);
              end;
            glEnd;

            bx := pxLeft + OffsetX;
            bxw := pxRight + OffsetX;
            byh := by + Height-2;
            // de stippellijnen als markering voor 0, 50 en 100 FPS
            glColor3f(1,0.3,0);
            glLineWidth(1);
            glEnable(GL_LINE_STIPPLE);
            glLineStipple(2,$AAAA);
            glBegin(GL_LINES);
              glVertex2f(bx,byh); //+cFPSTEXTWIDTH
              glVertex2f(bxw,byh);
            glEnd;
            glLineStipple(2,$5454);
            glBegin(GL_LINES);
              glVertex2f(bx,(by+byh)/2);
              glVertex2f(bxw,(by+byh)/2);
            glEnd;
            glLineStipple(2,$AAAA);
            glBegin(GL_LINES);
              glVertex2f(bx,by);
              glVertex2f(bxw,by);
            glEnd;
            glDisable(GL_LINE_STIPPLE);

            // knop blingbling
            bx := pxLeft + OffsetX;
            bxw := pxRight + OffsetX;
            byh := by + Height;
            glEnable(GL_BLEND);
            glEnable(GL_TEXTURE_2D);
            glBindTexture(GL_TEXTURE_2D, Button_TextureHandle);
            glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE); //GL_BLEND GL_REPLACE
  //          glBlendFunc(GL_ONE, GL_ONE);
  //          glBindTexture(GL_TEXTURE_2D, Button_TextureHandle);
  //          glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
            glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
            glColor4f(1,1,1,0.4);            //A=0.7
            glBegin(GL_QUADS);
              glTexCoord2f(0,1);  glVertex2f(bx,  byh);
              glTexCoord2f(1,1);  glVertex2f(bxw, byh);
              glTexCoord2f(1,0);  glVertex2f(bxw, by);
              glTexCoord2f(0,0);  glVertex2f(bx,  by);
            glEnd;
          end;

        with GFX_FPS do begin
          // tekst
          rec.Left := bx;
          rec.Top := pxBottom - 14;//(Height-by)+c_BUTTONSPACING+3;
          rec.Right := rec.Left + cFPSTEXTWIDTH;
          rec.Bottom := pxBottom;
          OGL.AlphaRectangle2D(rec,0,0,0,0.7);
          OGL.PrintXY(rec.Left+3,OGL.Height-(rec.Bottom-3), IntToStr(OGL.GetFPS), 0.8,0.65,0.3);
        end;

        OGL.SetupFor2D;  //herstellen op 2D..:S
      end;
    end;

    //glActiveTextureARB(GL_TEXTURE0_ARB);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
    glColor4f(0,0,0,1);

    // offsets bepalen..
    Game.HUD[h].HandleMiniMaxmize;
    // caption
    with Game.HUD[h] do
      if HUDButtonsSwappedX then begin
        ButtonsWidth := TotalSysButtons*(c_HUDBUTTONWIDTH+c_HUDBUTTONSPACING);
        OGL.PrintXY(1, pxLeft+c_HUDMARGIN+ButtonsWidth+(2*c_HUDMARGIN)+OffsetX,OGL.Height-pxTop-12-OffsetY, Name, 1,1,0);
      end else
        OGL.PrintXY(1, pxLeft+4+OffsetX,OGL.Height-pxTop-12-OffsetY, Name, 1,1,0);
    //OGL.SetupProjection;
    
  end;
end;




//-----------------------------------------------
// 3D
procedure TDisplay.RenderDestination(const Entity: TEntity;
                                     const Size: single);
begin
  if not Game.ShowRoutes then Exit;
  glPushAttrib(GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT or GL_LINE_BIT);
  glEnable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glLineWidth(Size);
  glBegin(GL_LINES);
    glColor3f(0,0.5,0);
    glVertex3fv(@Entity.Location.X);
    glVertex3fv(@Entity.Destination.X);
  glEnd;
  glPopAttrib;
end;

procedure TDisplay.RenderUnit(Index: integer);
var Entity: TEntity;
    V1,V2,V3,V4,V5,V6,V7,V8: TVector;
    f: single;
    WFobject: TWavefront;
begin
  if (Index<0) or (Index>Length(Game.Map.Units)-1) then Exit;
  Entity := Game.Map.Units[Index];

  // een simpele/snelle occlusion-test uitvoeren..
//  if not OGL.Frustum.PointInside(Entity.Location) then Exit;
  if not OGL.Frustum.AABBInside(Entity.Location, WF.ObjectByName('harvester').BoundingBox) then Exit;
//  if not OGL.Frustum.AABBInside(Entity.Location, WF.Objects[1].BoundingBox) then Exit;

  Inc(NVisibleUnits);

  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
  glPushMatrix;
  try
    case Entity.ClassType_ of
      cltEntityOreMiner: begin
        WFobject := WF.ObjectByName('harvester');
        WFobject.Render(Entity.Location, Entity.Pitch,Entity.Yaw,Entity.Roll);
        // geselecteerd??
        if Entity.Selected then begin
          RenderSelectionMark(Entity, 10.0);  // teken en selectie marker om de unit
          RenderDestination(Entity,3);
          RenderBoundingBox(WFobject.BoundingBox, Entity);
(*
          // teken een health-indicator boven de unit
          BillBoard(AddVector(Entity.Location, Vector(0,5,0)), OGL.Camera.LineOfSight, Entity.Health/50,0.2, V1,V2,V3,V4);
          glDisable(GL_TEXTURE_2D);
          glDisable(GL_BLEND);
          glColor3f(0,1,0);
          glBegin(GL_QUADS);
            with V1 do glVertex3f(X,Y,Z);
            with V2 do glVertex3f(X,Y,Z);
            with V3 do glVertex3f(X,Y,Z);
            with V4 do glVertex3f(X,Y,Z);
          glEnd;
*)
        end;
      end; //cltEntityOreMiner
    end; //case
  finally
    glPopMatrix;
  end;
end;

procedure TDisplay.RenderUnits;
var u: integer;
begin
  NVisibleUnits := 0;
  for u:=Low(Game.Map.Units) to High(Game.Map.Units) do RenderUnit(u);
end;




//-----------------------------------------------
procedure TDisplay.RenderSelectionRect;
begin
  if not Selection.AllowSelecting then Exit;  // selecteren mag niet??
  if Selection.IsPixelSelectionRect then Exit;
  OGL.AlphaRectangle2D(Selection.SelectionRect, 0.3,0.4,0.3, 0.35);
end;

procedure TDisplay.RenderSelectionMark(const Entity: TEntity; const Size: single);
var s2: single;
begin
  if not Game.ShowSelectionMarks then Exit;
  if Selected_TextureHandle=0 then Exit;
  s2 := Size / 2;
  glPushMatrix;
    //
    with Entity.Location do glTranslatef(X, Y, Z);
//    glRotatef(Entity.Pitch, 1,0,0);
    glRotatef(Entity.Yaw, 0,1,0);
//    glRotatef(Entity.Roll, 0,0,1);
    //
    glFrontFace(GL_CCW);
    glCullFace(GL_BACK);
    glEnable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    //*glColor3f(R,G,B);
    with Game.Teams[Entity.Team] do glBlendColorEXT(ColorR,ColorG,ColorB,1{ColorA});
    glBlendFunc(GL_CONSTANT_COLOR_EXT, GL_ONE);

    //
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, Selected_TextureHandle);
    glBegin(GL_QUADS);
      glTexCoord2f(0,0);  glVertex3f(-s2, 1, s2);
      glTexCoord2f(0,1);  glVertex3f(s2,  1, s2);
      glTexCoord2f(1,1);  glVertex3f(s2,  1, -s2);
      glTexCoord2f(1,0);  glVertex3f(-s2, 1, -s2);
    glEnd;
  glPopMatrix;
end;




//-----------------------------------------------
procedure TDisplay.RenderConstructing;
var Objwm: TWavefront;
    objX,objY,objZ: GLdouble;
    V: TVector;
    TileX,TileZ: integer;
begin
  if not Game.isConstructing then Exit;
  OGL.ScreenToObjectCoords(MouseX,MouseY, objX,objY,objZ);
  V := Vector(objX,objY,objZ);
  if not Game.Map.IsOnTerrain(V) then Exit;

  // teamkleur instellen
  with Game.Teams[0] do glBlendColorEXT(ColorR,ColorG,ColorB, 1.0{ColorA});

  // snap het gebouw op een tegel van het terrein
  with Game.Map.Terrain do begin
    TileX := Floor(V.X / c_TILESIZE);
    TileZ := Floor(V.Z / c_TILESIZE);
    V.X := Tiles[TileX,TileZ].CenterX;
    V.Z := Tiles[TileX,TileZ].CenterY;
    V.Y := GetHeightAt(V.X,V.Z);
  end;

  // tekenen
  case Game.ConstructionClassType of
    cltEntitySpawnpoint: begin
      Objwm := WF.ObjectByName('windmill');
      Objwm.RenderConstructing(V);
    end;
    cltEntityPowerpylon: begin
      Objwm := WF.ObjectByName('electro-pole-1');
      Objwm.RenderConstructing(V);
    end;
    cltEntityPowerline: begin
//      (Game.ConstructingEntity as TEntityPowerline).RenderConstructing(Game.ConstructingEntity.Location, V);
      glDisable(GL_DEPTH_TEST);
      glColor3f(1.0, 1.0, 1.0);
      glBegin(GL_LINES);
        glVertex3fv(@Game.ConstructingEntity^.Location);
        glVertex3fv(@V);
      glEnd;
      glEnable(GL_DEPTH_TEST);
    end;
  end;
end;




//-----------------------------------------------
procedure TDisplay.UnitGoto(const ScreenX, ScreenY: integer);
var objX,objY,objZ: GLdouble;
    Len, u: integer;
    Entity: TEntity;
begin
  // is er een unit geselecteerd??
  Len := Length(Selection.SelectedUnits.Indexes);
  if Len = 0 then Exit;

  OGL.ScreenToObjectCoords(ScreenX,ScreenY, objX,objY,objZ);

  // de nieuwe bestemming voor de units instellen
  for u:=Low(Selection.SelectedUnits.Indexes) to High(Selection.SelectedUnits.Indexes) do begin
    Entity := Game.Map.Units[Selection.SelectedUnits.Indexes[u]];
    if Entity.Team<>0 then Continue; //alleen eigen units zijn te besturen
    case Entity.ClassType_ of
      cltEntityOreMiner: begin
        Entity.Destination := Vector(objX,objY,objZ);
        (Entity as TEntityOreMiner).isReturning := false;
      end;
    end;
  end;
end;



//-----------------------------------------------
procedure TDisplay.RenderFrame;
var s,s2: string;
    i,u: integer;
    b: boolean;
    rec: TRect;
begin
  // RenderContext actief maken..
  {wglMakeCurrent(fOpenGL.OGL.DC, fOpenGL.OGL.RC);}

  glEnable(GL_DEPTH_TEST);

  //--- scherm wissen ----------------------------------------------------------
  if OGL.SkyBox.Active and (not OGL.SkyBox.Paused) then
    glClear(GL_DEPTH_BUFFER_BIT)
  else
    glClear(GL_COLOR_BUFFER_BIT OR GL_DEPTH_BUFFER_BIT);

  //--- MODELVIEW instellen
  glLoadIdentity;
  //--- camera plaatsen --------------------------------------------------------
  with OGL.Camera^ do gluLookAt(Position.X,Position.Y,Position.Z, Target.X,Target.Y,Target.Z, UpY.X,UpY.Y,UpY.Z);
  // de frustum clipping-planes berekenen
  OGL.Frustum.Calculate_glFrustumPlanes;
  //---


  //--- 3D tekenen vanaf hier... -----------------------------------------------

  // teken de map/terrein
  RenderMap;
  // teken de spawnpunten (met windmolens)
  RenderSpawnpoints;
  // teken de powerpylons
  RenderPowerpylons;
  // teken de powerlines
  RenderPowerlines;
  // teken units
  RenderUnits;
  // teken onder-constructie
  RenderConstructing;
  
  //--- de achtergrond ---------------------------------------------------------
  // altijd eerst de virtuele wereld tekenen, en daarna de skybox,
  // zodat de Z-buffer al is gevuld ten tijde tekenen skybox.
  OGL.SkyBox.Render(OGL.Camera^);


  //--- 2D tekenen vanaf hier... -----------------------------------------------
  glPushMatrix;
    OGL.SetupFor2D;

    if OGL.Camera.MouseControlled then RenderCrosshair;
    {RenderRadar;}
    RenderHUD;
    RenderSelectionRect; // de rechthoek van de eventuele selectie (LMB-down,drag,LMB-up)

    //--- de teksten -------------------------------------------------------------
    OGL.PrintLine(0, '', laTop, 1,1,1); //DUMMY!!!!!DEBUG!!!!!
    //
    if fMain.HelpText.Count > 0 then begin
      // Help afbeelden ipv. enig andere tekst
      rec.Left := 0;
      rec.Top := 0;
      rec.Right := OGL.Width;
      rec.Bottom := fMain.HelpText.Count*14 + 4;
      OGL.AlphaRectangle2D(rec, 0,0,0,0.6);      //helptekst op een donkere/transparante achtergrond
      for i:=0 to fMain.HelpText.Count-1 do
        OGL.PrintLine(i, fMain.HelpText.Strings[i], laTop, 0.85,0.8,0.8);
      //
    end else begin
      {glPushMatrix;}
      //bovenaan
      with OGL.Camera.Position do begin
        s := AnsiReplaceStr( FloatToStrF(X, ffFixed, 6,0), ',','.');
        s := s +','+ AnsiReplaceStr( FloatToStrF(Y, ffFixed, 6,0), ',','.');
        s := s +','+ AnsiReplaceStr( FloatToStrF(Z, ffFixed, 6,0), ',','.');
        s := 'Camera['+ s + ']';
      end;
      if not OGL.GetVSync(b) then //vsync instelling opvragen
        s2:=' '
      else begin
        s2:='VSync:';
        if b then s2:=s2+'ON' else s2:=s2+'OFF';
      end;
      s2:=s2+' @'+ IntToStr(OGL.MonitorRefreshRate) +'Hz';
      s := s+'  TU:'+ IntToStr(OGL.GetMaxTextureUnits) +'  '+ s2;
      s := s+'  Units(N/V/S):'+ IntToStr(Length(Game.Map.Units)) +'/'+ IntToStr(NVisibleUnits) +'/'+ IntToStr(Length(Selection.SelectedUnits.Indexes));
      OGL.PrintLine(0, s, laTop, 1.0,1.0,0.5);
(*
      s := 'Selected Units: ';
      for u:=Low(SelectedUnits) to High(SelectedUnits) do begin
        s := s + IntToStr(SelectedUnits[u]);
        if u<High(SelectedUnits) then s := s+', ';
      end;
      OGL.PrintLine(1, s, laTop, 1.0,1.0,0.5);
*)
      s := 'Percentage = '+ IntToStr(Game.HUD[0].ButtonSets[0].Buttons[0].Percentage);
      OGL.PrintLine(1, s, laTop, 1.0,1.0,0.5);

      //onderaan
      //...
      //
      {glPopMatrix;}
    end;
  glPopMatrix;

  // Frame afhandelen ----------------------------------------------------------
  glFlush;
  // buffers wisselen
  OGL.DoBufferSwap;

  //FPS meting
  OGL.MeasureFPS;
  Game.StoreCurrentFPS;
end;



{---}
initialization
  Display := TDisplay.Create;
finalization
  Display.Free;
end.
