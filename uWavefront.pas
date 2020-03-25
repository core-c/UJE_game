// de Wavefront-materials file .MTL
// example .MTL-file:
//      # Exported from DeleD's Wavefront OBJ Exporter 1.0
//      # Visit http://www.delgine.com for more information.
//      newmtl Floor02
//      map_Kd C:\Program Files\DeleD LITE\Textures\Floor02.jpg
//      newmtl Roof03
//      map_Kd C:\Program Files\DeleD LITE\Textures\Roof03.jpg
//      newmtl Wall10
//      map_Kd C:\Program Files\DeleD LITE\Textures\Wall10.jpg
//
//
//
// de Wavefront file .OBJ
// example .OBJ file:
//      o cube1                                            // object name
//      v 128 64 -128                                      // vertex (x,y,z)
//      v 0 64 0
//      v 0 64 -1024
//      v 128 64 -1024
//      v 128 832 -128
//      v 0 832 0
//      v 0 832 -1024
//      v 128 832 -1024
//      vt 1 1                                             // texture coordinates (s,t)
//      vt 0 1
//      vt 0 0
//      vt 1 0
//      vt 0 0
//      vt 1 0
//      vt 1 1
//      vt 0 1
//      vt 0 0
//      vt 1 0
//      vt 1 1
//      vt 0 1
//      vt 1 1
//      vt 0 1
//      vt 0 0
//      vt 1 0
//      vt 1 1
//      vt 0 1
//      vt 0 0
//      vt 1 0
//      vt 1 1
//      vt 0 1
//      vt 0 0
//      vt 1 0
//      vn 1 0 0                                           // normals (x,y,z)
//      vn 0 -1 0
//      vn 0 1 0
//      vn 0.70710676908493 0 0.70710676908493
//      vn -1 0 0
//      vn 0 0 -1
//      usemtl Wall10                                      // material-name
//      f 8/1/1 5/2/2 1/3/3 4/4/4                          // faces (v/vt/vn)
//      f 3/5/5 4/6/6 1/7/7 2/8/8
//      f 6/9/9 5/10/10 8/11/11 7/12/12
//      f 5/13/13 6/14/14 2/15/15 1/16/16
//      f 6/17/17 7/18/18 3/19/19 2/20/20
//      f 7/21/21 8/22/22 4/23/23 3/24/24


unit uWavefront;
interface
uses u3DTypes, OpenGL;

type
  TWavefrontFace = record
    NormalIndexes: array of GLuint;
    TexCoordIndexes: array of GLuint;
    VertexIndexes: array of GLuint;
    TextureHandle, TextureHandleTeam: GLuint;
    R,G,B: single;
  end;

  TWavefront = class(TObject)
  private
    OGL: Pointer; //POGL;             // het OGL parent object

    Texture: array of TTexture;
    VertexArray: array of TVector;
    TexCoordArray: array of TTexCoords;
    NormalArray: array of TVector;
    FacesArray: array of TWavefrontFace;

    IndexArray: array of cardinal; // indexes van alle objects/groups/faces in 1 array (niet per face)

    DisplayList,
    DisplayListConstructing: GLuint;
    VBO_Vertex,
    VBO_TexCoords,
    VBO_Normal,
    VBO_Index: GLuint; //de Vertex-Buffer-Object handles naar buffer
    procedure CreateDisplayList; overload;
    procedure CreateDisplayList(const Scale: single); overload;
    procedure DeleteDisplayList;
    function TextureHandleFromName(Texturename: string) : GLuint;
    procedure TextureRGBFromName(Texturename: string; var R,G,B: single);
    function InitTextures(MaterialsFilename: string) : boolean;
  public
    Name: string;
    Rotation: TVector;
    BoundingBox: TBoundingBox;
    constructor Create(OGL_parent: Pointer); //POGL
    destructor Destroy; override;
    //
    procedure FreeTextures; //kan alleen als OpenGL actief is (geldige RC nodig)
    function Import(Filename: string) : boolean; overload;
    function Import(Filename: string; const Scale: single) : boolean; overload;
    procedure Render(const Position: TVector); overload;
    procedure Render(const Position: TVector; const Pitch,Yaw,Roll: single); overload;
    procedure RenderConstructing(const Position: TVector);
  end;


  TObjWavefront = class(TObject)
  private
    OGL: Pointer; //POGL;             // het OGL parent object
  public
    Objects: array of TWavefront;
    constructor Create(OGL_parent: Pointer); //POGL
    destructor Destroy; override;
    function LoadFromFile(const Filename: string) : boolean; overload;
    function LoadFromFile(const Filename: string; const Scale: single) : boolean; overload;
    function ObjectByName(const Name: string) : TWavefront;
  end;

var WF: TObjWavefront; //instantie

implementation
uses uOpenGL, StrUtils, SysUtils, uCalc;

{ TWavefront }
constructor TWavefront.Create(OGL_parent: Pointer); //POGL
begin
  inherited Create;
  OGL := OGL_parent;
end;

destructor TWavefront.Destroy;
begin
  FreeTextures;
  //DeleteDisplayList;
  inherited;
end;


function TWavefront.TextureHandleFromName(Texturename: string): GLuint;
var t: integer;
begin
  Result := 0;
  for t:=Low(Texture) to High(Texture) do
    if Texture[t].Name = Texturename then begin
      Result := Texture[t].Handle;
      Exit;
    end;
end;

procedure TWavefront.TextureRGBFromName(Texturename: string; var R,G,B: single);
var t: integer;
begin
  for t:=Low(Texture) to High(Texture) do
    if Texture[t].Name = Texturename then begin
      R := Texture[t].R;
      G := Texture[t].G;
      B := Texture[t].B;
      Exit;
    end;
end;



function TWavefront.InitTextures(MaterialsFilename: string): boolean;
var F: TextFile;
    s,sname: string;
    Len: integer;
    MaterialName, MaterialFilename: string;
    H: GLuint;
    KdR, KdG,KdB: single;
    p1,p2: integer;
begin
  if not POGL(OGL^).Active then begin
    Result := false;
    Exit;
  end;
//  AppPath := ExtractFilePath(Application.ExeName);
  AssignFile(F, 'models\'+MaterialsFilename);
  {$I-}
  Reset(F);
  {$I+}
  if IOResult <> 0 then Exit; //! geef foutmelding
  try
    MaterialName := '';
    MaterialFilename := '';
    repeat

      ReadLn(F,s);

      // commentaar regels
      if LeftStr(s,1) = '#' then Continue;

      // material name
      if LeftStr(s,7) = 'newmtl ' then begin
        Len := Length(s);
        MaterialName := Trim(MidStr(s,8,Len-7));
        Continue;
      end;

      // material filename
      if LeftStr(s,7) = 'map_Kd ' then begin
        Len := Length(s);
        MaterialFilename := Trim(MidStr(s,8,Len-7));
        Continue;
      end;

      // materiaal-kleur ??
      if LeftStr(s,3) = 'Kd ' then begin
        Len := Length(s);
        s := Trim(MidStr(s,4,Len-3));
        // splits RGB
        p1 := PosEx(' ', s, 1);
        KdR := StrToFloat(Trim(MidStr(s,1,p1-1)));
        p2 := PosEx(' ', s, p1+1);
        KdG := StrToFloat(Trim(MidStr(s,p1+1,p2-p1)));
        KdB := StrToFloat(Trim(MidStr(s,p2+1,Len-p2)));
        //Continue;
      end;

      // geldige strings gelezen?
      if (MaterialName='') or (MaterialFilename='') then Continue;  //! geef foutmelding

      try
        // texture aanmaken
        sname := ExtractFileName(MaterialFilename);
        H := POGL(OGL^).Textures.LoadTexture(sname, 1.0);;
        if H=0 then Result := false;
        Len := Length(Texture);
        SetLength(Texture, Len+1);
        with Texture[Len] do begin
          Name := MaterialName;
          Handle := H;
          R := KdR;
          G := KdG;
          B := KdB;
        end;

        // bestaat er een team-color
        sname := MaterialName + '_team';
        if POGL(OGL^).Textures.FindTexture(sname) then begin
          // texture aanmaken
          H := POGL(OGL^).Textures.LoadTexture(sname, 1.0);;
          if H=0 then Result := false;
          Len := Length(Texture);
          SetLength(Texture, Len+1);
          with Texture[Len] do begin
            Name := MaterialName+'_team';
            Handle := H;
          end;
        end;
      finally
        MaterialName := '';
        MaterialFilename := '';
      end;

    until EOF(F);
    Result := true;
  finally
    CloseFile(F);
  end;
end;

procedure TWavefront.FreeTextures;
var i: integer;
begin
  // VBO verwijderen
  if VBO_Vertex<>0 then
    if glIsBufferARB(VBO_Vertex) then
      glDeleteBuffersARB(1, @VBO_Vertex);
  if VBO_Normal<>0 then
    if glIsBufferARB(VBO_Normal) then
      glDeleteBuffersARB(1, @VBO_Normal);
  if VBO_TexCoords<>0 then
    if glIsBufferARB(VBO_TexCoords) then
      glDeleteBuffersARB(1, @VBO_TexCoords);
  if VBO_Index<>0 then
    if glIsBufferARB(VBO_Index) then
      glDeleteBuffersARB(1, @VBO_Index);

  // de OpenGL-displaylist verwijderen
  DeleteDisplayList;
  
  // de textures wissen
  for i:=Low(Texture) to High(Texture) do begin
    POGL(OGL^).Textures.DeleteTexture(Texture[i].Handle);
    Texture[i].Name := '';
  end;
end;


function TWavefront.Import(Filename: string; const Scale: single): boolean;
var F: TextFile;
    s, steam: string;
    Len,Len1, LenF,LenV,LenVT,LenVN,LenI, p1,p2,p3: integer;
    MaterialLib, ObjectName,
    VertexString, TexCoordString, NormalString, MaterialString, FacesString, Face1String: string;
    x,y,z, texS,texT: single;
    v,vt,vn: integer;
    hasV, hasVT, hasVN, hasF: boolean;
    xs,ys,zs: single;
begin
  if not POGL(OGL^).Active then begin
    Result := false;
    Exit;
  end;
  DecimalSeparator := '.';

  BoundingBox.Min := Vector(3.4E38,3.4E38,3.4E38);
  BoundingBox.Max := Vector(1.5E-45,1.5E-45,1.5E-45);

  SetLength(IndexArray, 0);

  AssignFile(F, Filename);
  {$I-}
  Reset(F);
  {$I+}
  if IOResult <> 0 then Exit; //! geef foutmelding
  try
    repeat
      ReadLn(F,s);
      Len := Length(s);

      // commentaar regels
      if LeftStr(s,1) = '#' then Continue;

      // material library
      if LeftStr(s,7) = 'mtllib ' then begin
        MaterialLib := Trim(MidStr(s,8,Len-7));
        // textures aanmaken
        if not InitTextures(MaterialLib) then begin
          Result := false;
          Exit;
        end;
        Continue;
      end;

      // group  (zonder groupname)
      if s = 'g' then begin
        ObjectName := Trim(MidStr(s,3,Len-2));
        MaterialString := ''; //reset de te gebruiken texture
        hasV := false;
        hasVT := false;
        hasVN := false;
        hasF := false;
        Continue;
      end;

      // Object name
      if LeftStr(s,2) = 'o ' then begin
        ObjectName := Trim(MidStr(s,3,Len-2));
        MaterialString := ''; //reset de te gebruiken texture
        hasV := false;
        hasVT := false;
        hasVN := false;
        hasF := false;
        Continue;
      end;

      // vertexes
      if LeftStr(s,2) = 'v ' then begin
        VertexString := Trim(MidStr(s,3,Len-2));
        Len := Length(VertexString);
        // splits coordinaten
        p1 := PosEx(' ', VertexString, 1);
        x := StrToFloat(Trim(MidStr(VertexString,1,p1-1)));
        p2 := PosEx(' ', VertexString, p1+1);
        y := StrToFloat(Trim(MidStr(VertexString,p1+1,p2-p1)));
        z := StrToFloat(Trim(MidStr(VertexString,p2+1,Len-p2)));
        // vertex toevoegen aan array
        Len := Length(VertexArray);
        SetLength(VertexArray, Len+1);
        VertexArray[Len].X := x;
        VertexArray[Len].Y := y;
        VertexArray[Len].Z := z;
        // de boundingbox
        xs := x * Scale;
        ys := y * Scale;
        zs := z * Scale;
        if xs < BoundingBox.Min.X then BoundingBox.Min.X := xs;
        if xs > BoundingBox.Max.X then BoundingBox.Max.X := xs;
        if ys < BoundingBox.Min.Y then BoundingBox.Min.Y := ys;
        if ys > BoundingBox.Max.Y then BoundingBox.Max.Y := ys;
        if zs < BoundingBox.Min.Z then BoundingBox.Min.Z := zs;
        if zs > BoundingBox.Max.Z then BoundingBox.Max.Z := zs;
        //
        hasV := true;
        Continue;
      end;

      // texture coordinaten
      if LeftStr(s,3) = 'vt ' then begin
        TexCoordString := Trim(MidStr(s,4,Len-3));
        Len := Length(TexCoordString);
        // splits coordinaten
        p1 := PosEx(' ', TexCoordString, 1);
        texS := StrToFloat(Trim(MidStr(TexCoordString,1,p1-1)));
        // soms staan er 3 texture-coords...??
        p2 := PosEx(' ', TexCoordString, p1+1);
        if p2=0 then texT := StrToFloat(Trim(MidStr(TexCoordString,p1+1,Len-p1)))
                else texT := StrToFloat(Trim(MidStr(TexCoordString,p1+1,p2-p1)));
        // texture-coordinaten toevoegen aan array
        Len := Length(TexCoordArray);
        SetLength(TexCoordArray, Len+1);
        TexCoordArray[Len].U := texS;
        TexCoordArray[Len].V := texT;
        hasVT := true;
        Continue;
      end;

      // normalen
      if LeftStr(s,3) = 'vn ' then begin
        NormalString := Trim(MidStr(s,4,Len-3));
        Len := Length(NormalString);
        // splits coordinaten
        p1 := PosEx(' ', NormalString, 1);
        x := StrToFloat(Trim(MidStr(NormalString,1,p1-1)));
        p2 := PosEx(' ', NormalString, p1+1);
        y := StrToFloat(Trim(MidStr(NormalString,p1+1,p2-p1)));
        z := StrToFloat(Trim(MidStr(NormalString,p2+1,Len-p2)));
        // texture-coordinaten toevoegen aan array
        Len := Length(NormalArray);
        SetLength(NormalArray, Len+1);
        NormalArray[Len].X := x;
        NormalArray[Len].Y := y;
        NormalArray[Len].Z := z;
        hasVN := false;
        Continue;
      end;

      // material
      if LeftStr(s,7) = 'usemtl ' then begin
        MaterialString := Trim(MidStr(s,8,Len-7));

        // !!! face toevoegen aan array
        LenF := Length(FacesArray);
        SetLength(FacesArray, LenF+1);
        FacesArray[LenF].TextureHandle := TextureHandleFromName(MaterialString);
        FacesArray[LenF].TextureHandleTeam := TextureHandleFromName(MaterialString+'_team');
        // !!!

        Continue;
      end;

      // faces
      if LeftStr(s,2) = 'f ' then begin
        FacesString := Trim(MidStr(s,3,Len-2));
        Len := Length(FacesString);
(* !!!
        // face toevoegen aan array
        LenF := Length(FacesArray);
        SetLength(FacesArray, LenF+1);
        FacesArray[LenF].TextureHandle := TextureHandleFromName(MaterialString);
        FacesArray[LenF].TextureHandleTeam := TextureHandleFromName(MaterialString+'_team');
!!! *)
        with FacesArray[LenF] do TextureRGBFromName(MaterialString, R,G,B);
        // split v, vt & vn
        repeat
          p3 := PosEx(' ', FacesString, 1);
          if p3=0 then Face1String := FacesString
                  else Face1String := Trim(MidStr(FacesString,1,p3-1));
          Len1 := Length(Face1String);
          if hasV and hasVT and hasVN then begin
            p1 := PosEx('/', Face1String, 1);
            v := StrToInt(Trim(MidStr(Face1String,1,p1-1)));
            p2 := PosEx('/', Face1String, p1+1);
            vt := StrToInt(Trim(MidStr(Face1String,p1+1,p2-p1-1)));
            vn := StrToInt(Trim(MidStr(Face1String,p2+1,Len-p2)));
          end else
          if hasV and hasVT and (not hasVN) then begin
            p1 := PosEx('/', Face1String, 1);
            v := StrToInt(Trim(MidStr(Face1String,1,p1-1)));
            p2 := PosEx('/', Face1String, p1+1);
            if p2>0 then begin
              vt := StrToInt(Trim(MidStr(Face1String,p1+1,p2-p1-1)));
              vn := StrToInt(Trim(MidStr(Face1String,p2+1,Len-p2)));
            end else begin
              vt := StrToInt(Trim(MidStr(Face1String,p1+1,Len-p1)));
            end;
          end;

          if hasV then begin
            LenV := Length(FacesArray[LenF].VertexIndexes);
            SetLength(FacesArray[LenF].VertexIndexes, LenV+1);
            FacesArray[LenF].VertexIndexes[LenV] := v-1;              // offset 0 in array (ipv 1)

            // de indexes van alle faces bewaren in 1 gemeenschappelijke array
            LenI := Length(IndexArray);
            SetLength(IndexArray, LenI+1);
            IndexArray[LenI] := v-1;

          end;
          if hasVT then begin
            LenVT := Length(FacesArray[LenF].TexCoordIndexes);
            SetLength(FacesArray[LenF].TexCoordIndexes, LenVT+1);
            FacesArray[LenF].TexCoordIndexes[LenVT] := vt-1;          // offset 0
          end;
          if hasVN then begin
            LenVN := Length(FacesArray[LenF].NormalIndexes);
            SetLength(FacesArray[LenF].NormalIndexes, LenVN+1);
            FacesArray[LenF].NormalIndexes[LenVN] := vn-1;             // offset 0
          end;
          // volgende groepje op deze regel
          FacesString := Trim(MidStr(FacesString,p3+1,Len-p3));
        until p3=0;
        hasF := true;
        //Continue;
      end;

    until EOF(F);

    // OpenGL-displaylist maken van de data
    CreateDisplayList(Scale);

    // de data kan weer weg..
    SetLength(VertexArray,0);
    SetLength(TexCoordArray,0);
    SetLength(NormalArray,0);
    SetLength(FacesArray,0);
    SetLength(IndexArray, 0);

    s := ExtractFilename(Filename);
    Name := MidStr(s,1,Length(s)-4);

    Result := true;
  finally
    CloseFile(F);
  end;
end;


function TWavefront.Import(Filename: string): boolean;
begin
  Result := Import(Filename, 1.0);
end;


procedure TWavefront.CreateDisplayList(const Scale: single);
var v,t,n,f: integer;
    LastTextureHandle: GLuint;
    useVBO, useNormals, useTextureCoords: boolean;
    Index: integer;
begin
  Index := 0;  // IndexArray

  // VBO te gebruiken??
  useVBO := POGL(OGL^).supports_VBO;

  useNormals := (Length(NormalArray)>0);
  useTextureCoords := (Length(TexCoordArray)>0);

  glClientActiveTextureARB(GL_TEXTURE0);
  if useVBO then begin
{
    // index buffer object
    glGenBuffersARB(1, @VBO_Index);
    glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, VBO_Index);
    glBufferDataARB(GL_ELEMENT_ARRAY_BUFFER_ARB, Length(IndexArray)*SizeOf(GLuint), @IndexArray[0], GL_STATIC_DRAW_ARB);
}
    // vertex buffer object
    glGenBuffersARB(1, @VBO_Vertex);
    glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_Vertex);
    glBufferDataARB(GL_ARRAY_BUFFER_ARB, Length(VertexArray)*SizeOf(TVector), @VertexArray[0].X, GL_STATIC_DRAW_ARB);
    glEnableClientState(GL_VERTEX_ARRAY); //er zijn altijd vertex

    // normal buffer object
    if useNormals then begin
      glGenBuffersARB(1, @VBO_Normal);
      glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_Normal);
      glBufferDataARB(GL_ARRAY_BUFFER_ARB, Length(NormalArray)*SizeOf(TVector), @NormalArray[0].X, GL_STATIC_DRAW_ARB);
    end;

    // texcoords buffer object
    if useTextureCoords then begin
      glGenBuffersARB(1, @VBO_TexCoords);
      glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_TexCoords);
      glBufferDataARB(GL_ARRAY_BUFFER_ARB, Length(TexCoordArray)*SizeOf(TTexCoords), @TexCoordArray[0].U, GL_STATIC_DRAW_ARB);
    end;

    // tekenen (voorbereiden)
    glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_Vertex);
    glVertexPointer(3, GL_FLOAT, 0, 0); //offset ipv adres
    if useNormals then begin
      glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_Normal);
      glNormalPointer(GL_FLOAT, 0, 0);
    end;
    if useTextureCoords then begin
      glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_TexCoords);
      glTexCoordPointer(2, GL_FLOAT, 0, 0);
    end;
{glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, VBO_Index);}
  end else begin

    // pointers instellen op array voor vertex
    glVertexPointer(3, GL_FLOAT, 0, @VertexArray[0].X);
    glEnableClientState(GL_VERTEX_ARRAY); //er zijn altijd vertex
    // pointers instellen op array voor normals
    if Length(NormalArray)>0 then glNormalPointer(GL_FLOAT, 0, @NormalArray[0].X);  //adres ipv offset
    // pointers instellen op array voor texcoords
    if Length(TexCoordArray)>0 then glTexCoordPointer(2, GL_FLOAT, 0, @TexCoordArray[0].U);
  end;

  // de displaylist maken voor dit "textured" object ---------------------------
  LastTextureHandle := $FFFFFFFF;
  DisplayList := glGenLists(1);
  if DisplayList <> 0 then begin
    glNewList(DisplayList, GL_COMPILE);
      if Scale<>1 then glScale(Scale,Scale,Scale);
      glEnable(GL_DEPTH_TEST);
      glDepthFunc(GL_LESS);
      {glDepthRange(0.0, 12.0);}
      {glDepthMask(GL_FALSE);}
      glFrontFace(GL_CCW);
      glCullFace(GL_BACK);
      glEnable(GL_CULL_FACE);
      glPolygonMode(GL_FRONT, GL_FILL);
//      glDisable(GL_FOG);
      glDisable(GL_LIGHTING);
      glDisable(GL_BLEND);
      glEnable(GL_TEXTURE_2D);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

      for f:=Low(FacesArray) to High(Facesarray) do begin
        // normals & texcoords per face-group
        useNormals := (Length(NormalArray)>0);
        useTextureCoords := (Length(TexCoordArray)>0);

        // als er teamcolor-textures zijn, dan poly-offset instellen..
        // ..om het mogelijk te maken om 2 textures precies op elkaar te mappen.
        if FacesArray[f].TextureHandleTeam<>0 then begin
          glEnable(GL_POLYGON_OFFSET_FILL);
          glPolygonOffset(1.0, 1.0);
        end;

        // teken het gewone object
        with FacesArray[f] do begin

          if TextureHandle<>0 then begin// is er een texture?
            if (TextureHandle<>LastTextureHandle) then
              if useTextureCoords then begin
                glEnable(GL_TEXTURE_2D);
                glBindTexture(GL_TEXTURE_2D, TextureHandle);
                glEnableClientState(GL_TEXTURE_COORD_ARRAY);//texcoord-gebruik inschakelen
              end;
          end else begin
            if (TextureHandle<>LastTextureHandle) then begin
              glDisable(GL_TEXTURE_2D);
              glColor3f(R,G,B);
              glDisableClientState(GL_TEXTURE_COORD_ARRAY);//texcoord-gebruik uitschakelen
            end;
          end;

          //normal-gebruik in/uit-schakelen
          if useNormals then glEnableClientState(GL_NORMAL_ARRAY)
                        else glDisableClientState(GL_NORMAL_ARRAY);

          // tekenen
(*
          glBegin(GL_TRIANGLES);
            for v:=Low(VertexIndexes) to High(VertexIndexes) do begin
              if Length(NormalIndexes)>0 then glNormal3fv(@NormalArray[NormalIndexes[v]]);
              if (Length(TexCoordIndexes)>0) and (TextureHandle<>0) then glTexCoord2fv(@TexCoordArray[TexCoordIndexes[v]]);
              glVertex3fv(@VertexArray[VertexIndexes[v]]);
            end;
          glEnd;
*)
          if useVBO then begin
(* orgineel
            glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_Vertex);
            glVertexPointer(3, GL_FLOAT, 0, 0); //offset ipv adres
            if useNormals then begin
              glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_Normal);
              glNormalPointer(GL_FLOAT, 0, 0);
            end;
            if useTextureCoords then begin
              glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_TexCoords);
              glTexCoordPointer(2, GL_FLOAT, 0, 0);
            end;
*)
(* test
            // index buffer object
            glGenBuffersARB(1, @VBO_Index);
            glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, VBO_Index);
            glBufferDataARB(GL_ELEMENT_ARRAY_BUFFER_ARB, Length(VertexIndexes)*4{SizeOf(GLuint)}, @VertexIndexes[0], GL_STATIC_DRAW_ARB);
            glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, VBO_Index);
            glDrawElements(GL_TRIANGLES, Length(VertexIndexes), GL_UNSIGNED_INT, 0);
            glDeleteBuffersARB(1, @VBO_Index);
*)
(* test
//glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, VBO_Index);
//            glDrawRangeElements(GL_TRIANGLES, Index,Index+Length(VertexIndexes),Length(VertexIndexes), GL_UNSIGNED_INT, @IndexArray[0]); //??!! 1 triangle te weinig getekend
//            glDrawRangeElements(GL_TRIANGLES, Index,Index+Length(VertexIndexes),Length(VertexIndexes), GL_UNSIGNED_INT, @IndexArray[Index]);
            glDrawElements(GL_TRIANGLES, Length(VertexIndexes), GL_UNSIGNED_INT, @IndexArray[Index]);
            Index := Index + Length(VertexIndexes);
*)
{
glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, VBO_Index);
glDrawElements(GL_TRIANGLES, Length(VertexIndexes), GL_UNSIGNED_INT, nil);
}
            glDrawElements(GL_TRIANGLES, Length(VertexIndexes), GL_UNSIGNED_INT, @VertexIndexes[0]);
          end else
            glDrawElements(GL_TRIANGLES, Length(VertexIndexes), GL_UNSIGNED_INT, @VertexIndexes[0]);
          end;

          // teken er een eventueel aanwezige team-color-texture overheen..
          if FacesArray[f].TextureHandleTeam<>0 then begin
            // poly-offset uit, en blending met een constant-color inschakelen
            glDisable(GL_POLYGON_OFFSET_FILL);
            glEnable(GL_BLEND);
            glBlendFunc(GL_CONSTANT_COLOR_EXT, GL_ONE);
            //glBlendColorEXT(1,0,0,1); //teamkleur vooraf instellen..
            //
            with FacesArray[f] do begin
              glBindTexture(GL_TEXTURE_2D, TextureHandleTeam);
              // tekenen
              if useVBO then begin
{ // is al gebeurd..
                glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_Vertex);
                glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_Normal);
                glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_TexCoords);
(*
glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, VBO_Index);
glDrawElements(GL_TRIANGLES, Length(IndexArray), GL_UNSIGNED_INT, 0);
*)
}
                glDrawElements(GL_TRIANGLES, Length(VertexIndexes), GL_UNSIGNED_INT, @VertexIndexes[0]);
              end else
                glDrawElements(GL_TRIANGLES, Length(VertexIndexes), GL_UNSIGNED_INT, @VertexIndexes[0]);
            end;
            glDisable(GL_BLEND); //blending weer uit
          end;

      end;
      if Scale<>1 then glScale(1,1,1);
    glEndList;
  end;


  // de displaylist maken voor dit object "under construction" -----------------
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  DisplayListConstructing := glGenLists(1);
  if DisplayListConstructing <> 0 then begin
    glNewList(DisplayListConstructing, GL_COMPILE);
      if Scale<>1 then glScale(Scale,Scale,Scale);
      glColor3f(1,1,1);
      glEnable(GL_DEPTH_TEST);
      glDepthFunc(GL_LESS);
      glFrontFace(GL_CCW);
      glCullFace(GL_BACK);
      glEnable(GL_CULL_FACE);
      glPolygonMode(GL_FRONT, GL_FILL);
      glDisable(GL_LIGHTING);
      glDisable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
      glBlendFunc(GL_CONSTANT_COLOR_EXT, GL_ONE);
      for f:=Low(FacesArray) to High(Facesarray) do begin
        // teken het gewone object
        with FacesArray[f] do begin
          //normal-gebruik in/uit-schakelen
          if Length(NormalIndexes)>0 then glEnableClientState(GL_NORMAL_ARRAY)
                                     else glDisableClientState(GL_NORMAL_ARRAY);
          // tekenen
          if useVBO then begin
            glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_Vertex);
            glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBO_Normal);
            glDrawElements(GL_TRIANGLES, Length(VertexIndexes), GL_UNSIGNED_INT, @VertexIndexes[0]);
          end else
            glDrawElements(GL_TRIANGLES, Length(VertexIndexes), GL_UNSIGNED_INT, @VertexIndexes[0]);
          end;
      end;
      if Scale<>1 then glScale(1,1,1);
    glEndList;
  end;
  //----------------------------------------------------------------------------

  // array-gebruik uitschakelen
  glDisableClientState(GL_NORMAL_ARRAY);
  {glDisableClientState(GL_TEXTURE_COORD_ARRAY);}   //al gedaan..
  glDisableClientState(GL_VERTEX_ARRAY);
//  glDisableClientState(GL_INDEX_ARRAY);

  // VBO's uitschakelen
  if useVBO then begin
    glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
//    glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, 0);
  end;
end;


procedure TWavefront.CreateDisplayList;
begin
  CreateDisplayList(1.0);
end;

procedure TWavefront.DeleteDisplayList;
begin
  if DisplayList <> 0 then
    if glIsList(DisplayList) then
      glDeleteLists(DisplayList, 1);
  DisplayList := 0;

  if DisplayListConstructing <> 0 then
    if glIsList(DisplayListConstructing) then
      glDeleteLists(DisplayListConstructing, 1);
  DisplayListConstructing := 0;
end;


procedure TWavefront.Render(const Position: TVector);
begin
  glPushMatrix;
    //glLoadIdentity;
    with Position do glTranslatef(X, Y, Z);
    // de displaylist uitvoeren
    if DisplayList<>0 then glCallList(DisplayList);
  glPopMatrix;
end;

procedure TWavefront.Render(const Position: TVector; const Pitch,Yaw,Roll: single);
begin
  if DisplayList=0 then Exit;
  glPushMatrix;
    with Position do glTranslatef(X, Y, Z);
//    glRotatef(Pitch, 1,0,0);
    glRotatef(Yaw, 0,1,0);
//    glRotatef(Roll, 0,0,1);
    // de displaylist uitvoeren
    glCallList(DisplayList);
  glPopMatrix;
end;


procedure TWavefront.RenderConstructing(const Position: TVector);
begin
  glPushMatrix;
    //glLoadIdentity;
    with Position do glTranslatef(X, Y, Z);
    // de displaylist uitvoeren
    if DisplayListConstructing<>0 then glCallList(DisplayListConstructing);
  glPopMatrix;
end;



{--- TObjWavefront ------------------------------}
constructor TObjWavefront.Create(OGL_parent: Pointer); //POGL
begin
  inherited Create;
  OGL := OGL_parent;
  SetLength(Objects,0);
end;

destructor TObjWavefront.Destroy;
begin
  SetLength(Objects,0);
  inherited;
end;

function TObjWavefront.LoadFromFile(const Filename: string): boolean;
begin
  Result := LoadFromFile(Filename, 1.0);
end;

function TObjWavefront.LoadFromFile(const Filename: string; const Scale: single): boolean;
var Len: integer;
begin
  // maak een nieuw object aan in de array
  Len := Length(Objects);
  SetLength(Objects, Len+1);
  Objects[Len] := TWavefront.Create(@OGL);
  Result := Objects[Len].Import(Filename, Scale);
  if Result then Exit;
  SetLength(Objects, Len);
end;


function TObjWavefront.ObjectByName(const Name: string): TWavefront;
var o: integer;
begin
  Result := nil;
  for o:=Low(Objects) to High(Objects) do begin
    if Objects[o].Name <> Name then Continue;
    Result := Objects[o];
    Exit;
  end;
end;


(*
initialization
  WF := TObjWavefront.Create(@OGL);
finalization
  WF.Free;
*)
end.
