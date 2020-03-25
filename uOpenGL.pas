unit uOpenGL;
interface
uses Windows, OpenGL, uConst, uFrustum, uTexture, uFont, uCamera, uSkyBox;

const
  {===== ERROR CONSTANTS =====}
  error_GetDC = -1;
  error_ChoosePixelFormat = -2;   //describePixelFormat erbij nog ?..
  error_SetPixelFormat = -3;
  error_MakeRC = -4;
  error_MakeCurrent = -5;
  error_CreatingTexture = -6;
  error_GenList = -7;


//  DefaultFogColor : TGLArrayf4 = (0.1, 0.1, 0.1, 1.0);
  DefaultFogColor : TGLArrayf4 = (1.0, 1.0, 1.0, 1.0);

const
  // Projectie Typen
  ptOrthogonal  = 1;
  ptPerspective = 2;
  ptFrustum     = 3;

type
  TRightAngle = (Angle0=0,Angle90=90,Angle180=180,Angle270=270);


  // een rectangle met floating point boundries (in bereik[0.0 .. 1.0])
  TFRect = record
    // padding (percentages van de gehele venster-breedte & -hoogte)
    top, left, bottom, right: Single;
    // vaste padding waarden (in pixels)
    pxTop, pxLeft, pxBottom, pxRight: Integer;
  end;

  TProjection = record
    ProjectionType: integer;
    Width, Height: integer;
    Center: TPoint;
    // De padding (tussen venster en viewport)
    // (waarden in bereik[0..1])
    // Dit zijn percentages van de gehele viewport-breedte & -hoogte.
    // Een waarde van 0.5 voor .left zorgt ervoor dat er aan de linkerkant
    // langs de viewport een ruimte open is, die de helft van het venster breed is.
    ViewPortRect: TFRect;
    //
    Left,Right,
    Bottom,Top,
    zNear,zFar: Extended;
    FOVY: Extended;
  end;

  POGL = ^TOGL;
  TOGL = class(TObject)
    private
      // Camera's
      local_Camera: TCamera;                       // de camera voor intern OpenGL gebruik
      // tbv. de properties
      local_WinHandle: HWND;                       // Windows handle Form
      local_DC: HDC;                               // Windows handle Device-Context
      local_RC: HGLRC;                             // OpenGL handle Render-Context
      local_Width,                                 // huidige form breedte
      local_Height: integer;                       // huidige form hoogte
      local_Active,                                // actief
      local_Paused,                                // gepauzeerd
      local_FullScreen,                            // volledig scherm
      local_Fog: boolean;                          // mist
      local_Projection: TProjection;               // projectie
      local_FPS,                                   // het aantal beelden per seconde
      local_FPSCount: integer;                     //
      local_LastFrameTime: single;                 // de tijdsduur die nodig was om het laatste beeld te renderen
      PerformanceFreq, PerformanceCount, LastPerformanceCount: Int64;
      IsSetupFor2D: boolean;                       // ingesteld op 2D??
      // de camera(-rotatie) met de muis besturen
      local_MouseLook: boolean;
      // supported features
      local_Multitexture,
      local_FogCoord,
      local_BlendColor,
      local_VertexBufferObject,
      local_ShaderObjects: boolean;
      //-- /properties
      Palette : hPalette;           //OpenGL palette
      local_VSync,
      OldVSync: GLint;              // de instelling van de VSync opvragen om te herstellen na afloop App.
      N_TextureUnits: Integer;      //aantal texture units aanwezig..
      procedure SetupFullScreen(State: boolean);    // van/naar volledig scherm omschakelen
      procedure SetupFog(State: boolean); overload; // mist in/uitschakelen
      procedure Init;                               // OpenGL instellingen
      // tbv. de properties
      {procedure SetWinHandle(const Value: HWND);}
      function GetActive : Boolean;
      {procedure SetActive(const Value: boolean);}
      procedure SetPaused(const Value: boolean);
      procedure SetFullScreen(const Value: boolean);
      procedure SetMouseLook(const Value: boolean);
      procedure SetFog(const Value: boolean);
      function GetCenter: TPoint;
      function GetHeight: integer;
      function GetWidth: integer;
      // OpenGL EXTENSIONS
      procedure GetExtensionProcs;
      //
    published
      property WinHandle: HWND                 read local_WinHandle;
      property DC: HDC                         read local_DC;
      property RC: HGLRC                       read local_RC;
      property Active: boolean                 read GetActive;
      property Paused: boolean                 read local_Paused      write SetPaused;
      property FullScreen: boolean             read local_FullScreen  write SetFullScreen;
      property MouseLook: boolean              read local_MouseLook   write SetMouseLook;
      property Fog: boolean                    read local_Fog         write SetFog;
      property Width: integer                  read GetWidth;   // breedte van het hele grafische scherm
      property Height: integer                 read GetHeight;  // hoogte van het hele grafische scherm
      property Center: TPoint                  read GetCenter;  // centrum van het hele grafische scherm
      property supports_Multitexture: boolean  read local_Multitexture;
      property supports_FogCoord: boolean      read local_FogCoord;
      property supports_BlendColor: boolean    read local_BlendColor;
      property supports_VBO: boolean           read local_VertexBufferObject;
      property supports_ShaderObjects: boolean read local_ShaderObjects;
    public
      // objecten
      Frustum: TFrustum;                            // het frustum
      Textures: TObjTexture;                        // textures
      Fonts: TGLFonts;                              // lettertypen
      Camera: PCamera;                              // Een pointer naar de camera
      SkyBox: TSkyBox;                              // de SkyBox
      // this.object
      constructor Create;
      destructor Destroy; override;
      // reset camera naar de lokale camera
      procedure ResetCamera;
      // projectie
      procedure SetupViewPort(aProjection: TProjection);
      procedure Resize(NewWidth, NewHeight: integer);
      procedure SetupProjection; overload;
      procedure SetupProjection(Width,Height: integer; fLeft,fRight,fBottom,fTop,fNear,fFar: Extended); overload; //frustum
      procedure SetupProjection(Width,Height: integer; FOVY, pNear,pFar: Extended); overload; //perspective
      procedure SetupProjection(Width,Height: integer; oLeft,oRight,oBottom,oTop: Extended); overload; //ortho
      procedure SetupProjectionPicking(const X,Y, W,H: integer);
      procedure SetupFor2D;
      // omzetten van 2D scherm-coordinaten (/form-coords) naar 3D object-coordinaten.
      procedure ScreenToObjectCoords(const ScreenX, ScreenY: integer; var X,Y,Z: GLdouble);
      // een ray-richting berekenen van vector door muis-cursorpos van near-plane naar far-plane
      // (tbv. selecteren van dingen in de 3D-wereld)
      procedure CalcMouseRay(const ScreenX,ScreenY: integer; var X,Y,Z: GLdouble);
      // fullscreen
      procedure ToggleFullScreen;
      //de camera(-rotatie) met de muis besturen
      procedure ToggleMouseLook;
      // fog
      procedure SetupFog(Mode: Cardinal; Density, StartDepth,EndDepth: Single; const FogColor: TGLArrayf4; Hint: Cardinal); overload;
      procedure SetupFog(Density, StartDepth, EndDepth: Single; const FogColor: TGLArrayf4); overload;
      procedure ToggleFog;

      // OpenGL in-/uit-schakelen
      procedure Enable(WindowHandle: HWND);         // OpenGL inschakelen
      procedure Disable;                            // OpenGL uitschakelen
      procedure DoBufferSwap;                       // doublebuffer swap
      procedure ReportErrorAndQuit(ErrorCode: integer);
      // OpenGL EXTENSIONS
      function ExtensionSupported(Ext: string) : boolean;

      procedure PushMatrices;  // push de projection- & modelview-matrices
      procedure PopMatrices;   // pop  de modelview- & projection-matrices

      // Windows fonts 3D & 2D
      procedure PrintXY(X,Y: integer; const Value: string; R,G,B: Single); overload;
      procedure PrintXY(FontIndex:integer; X,Y: integer; const Value: string; R,G,B: Single); overload;
      procedure PrintLine(Row: integer; const Value: string; VAlign: TLineVAlignment; R,G,B: Single); overload;
      procedure PrintLine(FontIndex:integer; Row: integer; const Value: string; VAlign: TLineVAlignment; R,G,B: Single); overload;

      // monitor refresh rate control
      function MonitorRefreshRate: Integer;
      function GetVSync(var OnOff: boolean) : boolean;          //resultaat = true als het instellen is gelukt, anders false
      function SetVSync(OnOff: boolean) : boolean; overload;    //resultaat = true als het instellen is gelukt, anders false
      function SetVSync(OnOff: integer) : boolean; overload;    //resultaat = true als het instellen is gelukt, anders false
      procedure ToggleVSync;
      //
      function GetMaxTextureUnits : GLint;
      // FPS meting
      procedure FPSTimer;    // de routine die elke seconde dient te worden aangeroepen
      procedure MeasureFPS;  // de routine die na elke frame-render dient te worden aangeroepen
      function GetFPS : integer;
      function GetLastFrameTime : single;

      //=== sub-routines die werken met 2D scherm-coördinaten
      // 2D vlakken tekenen (met alpha-blending)
      procedure AlphaRectangle2D(Rectangle: TRect; R,G,B,A: Single); overload;
      procedure AlphaRectangle2D(Rectangle: TRect; R,G,B,A: Single; BlendSRC,BlendDST: Cardinal); overload;
      // 2D vlakken tekenen (met texture)
      procedure TexturedRectangle2D(Rectangle: TRect; R,G,B,A: Single; TextureHandle: GLuint; AngleCCW: TRightAngle); overload;
      // 2D vlakken tekenen (met texture en een masker)
      procedure TexturedRectangle2D(Rectangle: TRect; R,G,B,A: Single; TextureHandle, MaskHandle: GLuint; AngleCCW: TRightAngle); overload;
      // 2D punt tekenen
      procedure Point2D(ScrPx_X, ScrPx_Y: integer; R,G,B,A: Single; PointSize: single);
      // 2D lijn tekenen
      procedure Line2D(ScrP1,ScrP2: TPoint; R,G,B,A: Single; LineWidth: single);
    end;


// OpenGL 1.2, 1.4, 1.5 & 2.0 constanten, functies & procedures
type
  PGLvoid = Pointer;
  GLintptrARB = Integer;
  PGLintptrARB = ^GLintptrARB;
  GLsizeiptrARB = Integer;
  PGLsizeiptrARB = ^GLsizeiptrARB;

  GLUquadric = record end;
  PGLUquadric = ^GLUquadric;
  GLUquadricObj = GLUquadric;
  PGLUquadricObj = PGLUquadric; // backwards compatibility:

  GLhandleARB = Cardinal;       // shader language
  PGLhandleARB = ^GLhandleARB;

  GLcharARB = Char;
  PGLcharARB = ^GLcharARB;
  GLchar = GLcharARB;
  PGLchar = PGLcharARB;
  PGLenum = ^GLenum;

const
  // Multi-texturing
  GL_MAX_TEXTURE_UNITS_ARB        = $84E2;
  GL_TEXTURE0_ARB                 = $84C0;
  GL_TEXTURE0                     = $84C0;
  GL_TEXTURE1_ARB                 = $84C1;
  GL_TEXTURE1                     = $84C1;
  GL_TEXTURE2_ARB                 = $84C2;
  GL_TEXTURE2                     = $84C2;
  GL_TEXTURE3_ARB                 = $84C3;
  GL_TEXTURE3                     = $84C3;

  // Fog extensions
  GL_FOG_COORDINATE_SOURCE_EXT    = $8450;
  GL_FOG_COORDINATE_SOURCE        = $8450;
  GL_FOG_COORD_SOURCE             = $8450;
  GL_FOG_COORD_SRC                = $8450;
  GL_FOG_COORDINATE_EXT           = $8451;
  GL_FOG_COORDINATE               = $8451;
  GL_FOG_COORD                    = $8451;
  GL_FRAGMENT_DEPTH               = $8452;
  GL_CURRENT_FOG_COORDINATE       = $8453;
  GL_CURRENT_FOG_COORD            = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE    = $8454;
  GL_FOG_COORD_ARRAY_TYPE         = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE  = $8455;
  GL_FOG_COORD_ARRAY_STRIDE       = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER = $8456;
  GL_FOG_COORD_ARRAY_POINTER      = $8456;
  GL_FOG_COORDINATE_ARRAY         = $8457;
  GL_FOG_COORD_ARRAY              = $8457;


  GL_VERTEX_ARRAY                 = $8074;
  GL_NORMAL_ARRAY                 = $8075;
  GL_COLOR_ARRAY                  = $8076;
  GL_INDEX_ARRAY                  = $8077;
  GL_TEXTURE_COORD_ARRAY          = $8078;

  GL_CLAMP_TO_EDGE                = $812F; // GL 1.2

  GL_POLYGON_OFFSET_UNITS         = $2A00;
  GL_POLYGON_OFFSET_POINT         = $2A01;
  GL_POLYGON_OFFSET_LINE          = $2A02;
  GL_POLYGON_OFFSET_FILL          = $8037;
  GL_POLYGON_OFFSET_FACTOR        = $8038;

  GL_COMBINE                      = $8570;
  GL_COMBINE_RGB                  = $8571;
  GL_COMBINE_ALPHA                = $8572;
  GL_RGB_SCALE                    = $8573;
  GL_ADD_SIGNED                   = $8574;
  GL_INTERPOLATE                  = $8575;
  GL_CONSTANT                     = $8576;
  GL_PRIMARY_COLOR                = $8577;
  GL_PREVIOUS                     = $8578;
  GL_SOURCE0_RGB                  = $8580;
  GL_SOURCE1_RGB                  = $8581;
  GL_SOURCE2_RGB                  = $8582;
  GL_SOURCE0_ALPHA                = $8588;
  GL_SOURCE1_ALPHA                = $8589;
  GL_SOURCE2_ALPHA                = $858A;
  GL_OPERAND0_RGB                 = $8590;
  GL_OPERAND1_RGB                 = $8591;
  GL_OPERAND2_RGB                 = $8592;
  GL_OPERAND0_ALPHA               = $8598;
  GL_OPERAND1_ALPHA               = $8599;
  GL_OPERAND2_ALPHA               = $859A;
  GL_SUBTRACT                     = $84E7;
  //
  GL_COMBINE_EXT                  = $8570;
  GL_COMBINE_RGB_EXT              = $8571;
  GL_COMBINE_ALPHA_EXT            = $8572;
  GL_RGB_SCALE_EXT                = $8573;
  GL_ADD_SIGNED_EXT               = $8574;
  GL_INTERPOLATE_EXT              = $8575;
  GL_CONSTANT_EXT                 = $8576;
  GL_PRIMARY_COLOR_EXT            = $8577;
  GL_PREVIOUS_EXT                 = $8578;
  GL_SOURCE0_RGB_EXT              = $8580;
  GL_SOURCE1_RGB_EXT              = $8581;
  GL_SOURCE2_RGB_EXT              = $8582;
  GL_SOURCE0_ALPHA_EXT            = $8588;
  GL_SOURCE1_ALPHA_EXT            = $8589;
  GL_SOURCE2_ALPHA_EXT            = $858A;
  GL_OPERAND0_RGB_EXT             = $8590;
  GL_OPERAND1_RGB_EXT             = $8591;
  GL_OPERAND2_RGB_EXT             = $8592;
  GL_OPERAND0_ALPHA_EXT           = $8598;
  GL_OPERAND1_ALPHA_EXT           = $8599;
  GL_OPERAND2_ALPHA_EXT           = $859A;
  //
  GL_COMBINE_ARB                  = $8570;
  GL_COMBINE_RGB_ARB              = $8571;
  GL_COMBINE_ALPHA_ARB            = $8572;
  GL_SOURCE0_RGB_ARB              = $8580;
  GL_SOURCE1_RGB_ARB              = $8581;
  GL_SOURCE2_RGB_ARB              = $8582;
  GL_SOURCE0_ALPHA_ARB            = $8588;
  GL_SOURCE1_ALPHA_ARB            = $8589;
  GL_SOURCE2_ALPHA_ARB            = $858A;
  GL_OPERAND0_RGB_ARB             = $8590;
  GL_OPERAND1_RGB_ARB             = $8591;
  GL_OPERAND2_RGB_ARB             = $8592;
  GL_OPERAND0_ALPHA_ARB           = $8598;
  GL_OPERAND1_ALPHA_ARB           = $8599;
  GL_OPERAND2_ALPHA_ARB           = $859A;
  GL_RGB_SCALE_ARB                = $8573;
  GL_ADD_SIGNED_ARB               = $8574;
  GL_INTERPOLATE_ARB              = $8575;
  GL_SUBTRACT_ARB                 = $84E7;
  GL_CONSTANT_ARB                 = $8576;
  GL_PRIMARY_COLOR_ARB            = $8577;
  GL_PREVIOUS_ARB                 = $8578;

  GL_CONSTANT_COLOR_EXT           = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR_EXT = $8002;
  GL_CONSTANT_ALPHA               = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA     = $8004;

  GL_CONSTANT_ALPHA_EXT           = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA_EXT = $8004;


  // cube maps
  NORMAL_MAP_ARB                  = $8511;
  REFLECTION_MAP_ARB              = $8512;
  TEXTURE_CUBE_MAP_ARB            = $8513;
  TEXTURE_BINDING_CUBE_MAP_ARB    = $8514;
  TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $8515;
  TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $8516;
  TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $8517;
  TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $8518;
  TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $8519;
  TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $851A;
  PROXY_TEXTURE_CUBE_MAP_ARB      = $851B;
  MAX_CUBE_MAP_TEXTURE_SIZE_ARB   = $851C;


  // vertex buffer objects
  GL_BUFFER_SIZE_ARB                           = $8764;
  GL_BUFFER_USAGE_ARB                          = $8765;
  GL_ARRAY_BUFFER_ARB                          = $8892;
  GL_ELEMENT_ARRAY_BUFFER_ARB                  = $8893;
  GL_ARRAY_BUFFER_BINDING_ARB                  = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB          = $8895;
  GL_VERTEX_ARRAY_BUFFER_BINDING_ARB           = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING_ARB           = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING_ARB            = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING_ARB            = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB    = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB        = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB  = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB   = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB           = $889E;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB    = $889F;
  GL_READ_ONLY_ARB                             = $88B8;
  GL_WRITE_ONLY_ARB                            = $88B9;
  GL_READ_WRITE_ARB                            = $88BA;
  GL_BUFFER_ACCESS_ARB                         = $88BB;
  GL_BUFFER_MAPPED_ARB                         = $88BC;
  GL_BUFFER_MAP_POINTER_ARB                    = $88BD;
  GL_STREAM_DRAW_ARB                           = $88E0;
  GL_STREAM_READ_ARB                           = $88E1;
  GL_STREAM_COPY_ARB                           = $88E2;
  GL_STATIC_DRAW_ARB                           = $88E4;
  GL_STATIC_READ_ARB                           = $88E5;
  GL_STATIC_COPY_ARB                           = $88E6;
  GL_DYNAMIC_DRAW_ARB                          = $88E8;
  GL_DYNAMIC_READ_ARB                          = $88E9;
  GL_DYNAMIC_COPY_ARB                          = $88EA;

  //ifndef GL_VERSION_2_0
  GL_BLEND_EQUATION_RGB                        = $8009;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED               = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE                  = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE                = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE                  = $8625;
  GL_CURRENT_VERTEX_ATTRIB                     = $8626;
  GL_VERTEX_PROGRAM_POINT_SIZE                 = $8642;
  GL_VERTEX_ATTRIB_ARRAY_POINTER               = $8645;
  GL_STENCIL_BACK_FUNC                         = $8800;
  GL_STENCIL_BACK_FAIL                         = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL              = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS              = $8803;
  GL_MAX_DRAW_BUFFERS                          = $8824;
  GL_DRAW_BUFFER0                              = $8825;
  GL_DRAW_BUFFER1                              = $8826;
  GL_DRAW_BUFFER2                              = $8827;
  GL_DRAW_BUFFER3                              = $8828;
  GL_DRAW_BUFFER4                              = $8829;
  GL_DRAW_BUFFER5                              = $882A;
  GL_DRAW_BUFFER6                              = $882B;
  GL_DRAW_BUFFER7                              = $882C;
  GL_DRAW_BUFFER8                              = $882D;
  GL_DRAW_BUFFER9                              = $882E;
  GL_DRAW_BUFFER10                             = $882F;
  GL_DRAW_BUFFER11                             = $8830;
  GL_DRAW_BUFFER12                             = $8831;
  GL_DRAW_BUFFER13                             = $8832;
  GL_DRAW_BUFFER14                             = $8833;
  GL_DRAW_BUFFER15                             = $8834;
  GL_BLEND_EQUATION_ALPHA                      = $883D;
  GL_MAX_VERTEX_ATTRIBS                        = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED            = $886A;
  GL_MAX_TEXTURE_IMAGE_UNITS                   = $8872;
  GL_FRAGMENT_SHADER                           = $8B30;
  GL_VERTEX_SHADER                             = $8B31;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS           = $8B49;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS             = $8B4A;
  GL_MAX_VARYING_FLOATS                        = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS            = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS          = $8B4D;
  GL_SHADER_TYPE                               = $8B4F;
  GL_FLOAT_VEC2                                = $8B50;
  GL_FLOAT_VEC3                                = $8B51;
  GL_FLOAT_VEC4                                = $8B52;
  GL_INT_VEC2                                  = $8B53;
  GL_INT_VEC3                                  = $8B54;
  GL_INT_VEC4                                  = $8B55;
  GL_BOOL                                      = $8B56;
  GL_BOOL_VEC2                                 = $8B57;
  GL_BOOL_VEC3                                 = $8B58;
  GL_BOOL_VEC4                                 = $8B59;
  GL_FLOAT_MAT2                                = $8B5A;
  GL_FLOAT_MAT3                                = $8B5B;
  GL_FLOAT_MAT4                                = $8B5C;
  GL_SAMPLER_1D                                = $8B5D;
  GL_SAMPLER_2D                                = $8B5E;
  GL_SAMPLER_3D                                = $8B5F;
  GL_SAMPLER_CUBE                              = $8B60;
  GL_SAMPLER_1D_SHADOW                         = $8B61;
  GL_SAMPLER_2D_SHADOW                         = $8B62;
  GL_DELETE_STATUS                             = $8B80;
  GL_COMPILE_STATUS                            = $8B81;
  GL_LINK_STATUS                               = $8B82;
  GL_VALIDATE_STATUS                           = $8B83;
  GL_INFO_LOG_LENGTH                           = $8B84;
  GL_ATTACHED_SHADERS                          = $8B85;
  GL_ACTIVE_UNIFORMS                           = $8B86;
  GL_ACTIVE_UNIFORM_MAX_LENGTH                 = $8B87;
  GL_SHADER_SOURCE_LENGTH                      = $8B88;
  GL_ACTIVE_ATTRIBUTES                         = $8B89;
  GL_ACTIVE_ATTRIBUTE_MAX_LENGTH               = $8B8A;
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT           = $8B8B;
  GL_SHADING_LANGUAGE_VERSION                  = $8B8C;
  GL_CURRENT_PROGRAM                           = $8B8D;
  GL_POINT_SPRITE_COORD_ORIGIN                 = $8CA0;
  GL_LOWER_LEFT                                = $8CA1;
  GL_UPPER_LEFT                                = $8CA2;
  GL_STENCIL_BACK_REF                          = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK                   = $8CA4;
  GL_STENCIL_BACK_WRITEMASK                    = $8CA5;

  //ifndef GL_ARB_vertex_program
  GL_COLOR_SUM_ARB                             = $8458;
  GL_VERTEX_PROGRAM_ARB                        = $8620;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB           = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB              = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB            = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB              = $8625;
  GL_CURRENT_VERTEX_ATTRIB_ARB                 = $8626;
  GL_PROGRAM_LENGTH_ARB                        = $8627;
  GL_PROGRAM_STRING_ARB                        = $8628;
  GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB        = $862E;
  GL_MAX_PROGRAM_MATRICES_ARB                  = $862F;
  GL_CURRENT_MATRIX_STACK_DEPTH_ARB            = $8640;
  GL_CURRENT_MATRIX_ARB                        = $8641;
  GL_VERTEX_PROGRAM_POINT_SIZE_ARB             = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_ARB               = $8643;
  GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB           = $8645;
  GL_PROGRAM_ERROR_POSITION_ARB                = $864B;
  GL_PROGRAM_BINDING_ARB                       = $8677;
  GL_MAX_VERTEX_ATTRIBS_ARB                    = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB        = $886A;
  GL_PROGRAM_ERROR_STRING_ARB                  = $8874;
  GL_PROGRAM_FORMAT_ASCII_ARB                  = $8875;
  GL_PROGRAM_FORMAT_ARB                        = $8876;
  GL_PROGRAM_INSTRUCTIONS_ARB                  = $88A0;
  GL_MAX_PROGRAM_INSTRUCTIONS_ARB              = $88A1;
  GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB           = $88A2;
  GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB       = $88A3;
  GL_PROGRAM_TEMPORARIES_ARB                   = $88A4;
  GL_MAX_PROGRAM_TEMPORARIES_ARB               = $88A5;
  GL_PROGRAM_NATIVE_TEMPORARIES_ARB            = $88A6;
  GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB        = $88A7;
  GL_PROGRAM_PARAMETERS_ARB                    = $88A8;
  GL_MAX_PROGRAM_PARAMETERS_ARB                = $88A9;
  GL_PROGRAM_NATIVE_PARAMETERS_ARB             = $88AA;
  GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB         = $88AB;
  GL_PROGRAM_ATTRIBS_ARB                       = $88AC;
  GL_MAX_PROGRAM_ATTRIBS_ARB                   = $88AD;
  GL_PROGRAM_NATIVE_ATTRIBS_ARB                = $88AE;
  GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB            = $88AF;
  GL_PROGRAM_ADDRESS_REGISTERS_ARB             = $88B0;
  GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB         = $88B1;
  GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB      = $88B2;
  GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB  = $88B3;
  GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB          = $88B4;
  GL_MAX_PROGRAM_ENV_PARAMETERS_ARB            = $88B5;
  GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB           = $88B6;
  GL_TRANSPOSE_CURRENT_MATRIX_ARB              = $88B7;
  GL_MATRIX0_ARB                               = $88C0;
  GL_MATRIX1_ARB                               = $88C1;
  GL_MATRIX2_ARB                               = $88C2;
  GL_MATRIX3_ARB                               = $88C3;
  GL_MATRIX4_ARB                               = $88C4;
  GL_MATRIX5_ARB                               = $88C5;
  GL_MATRIX6_ARB                               = $88C6;
  GL_MATRIX7_ARB                               = $88C7;
  GL_MATRIX8_ARB                               = $88C8;
  GL_MATRIX9_ARB                               = $88C9;
  GL_MATRIX10_ARB                              = $88CA;
  GL_MATRIX11_ARB                              = $88CB;
  GL_MATRIX12_ARB                              = $88CC;
  GL_MATRIX13_ARB                              = $88CD;
  GL_MATRIX14_ARB                              = $88CE;
  GL_MATRIX15_ARB                              = $88CF;
  GL_MATRIX16_ARB                              = $88D0;
  GL_MATRIX17_ARB                              = $88D1;
  GL_MATRIX18_ARB                              = $88D2;
  GL_MATRIX19_ARB                              = $88D3;
  GL_MATRIX20_ARB                              = $88D4;
  GL_MATRIX21_ARB                              = $88D5;
  GL_MATRIX22_ARB                              = $88D6;
  GL_MATRIX23_ARB                              = $88D7;
  GL_MATRIX24_ARB                              = $88D8;
  GL_MATRIX25_ARB                              = $88D9;
  GL_MATRIX26_ARB                              = $88DA;
  GL_MATRIX27_ARB                              = $88DB;
  GL_MATRIX28_ARB                              = $88DC;
  GL_MATRIX29_ARB                              = $88DD;
  GL_MATRIX30_ARB                              = $88DE;
  GL_MATRIX31_ARB                              = $88DF;

  //ifndef GL_ARB_fragment_program
  GL_FRAGMENT_PROGRAM_ARB                      = $8804;
  GL_PROGRAM_ALU_INSTRUCTIONS_ARB              = $8805;
  GL_PROGRAM_TEX_INSTRUCTIONS_ARB              = $8806;
  GL_PROGRAM_TEX_INDIRECTIONS_ARB              = $8807;
  GL_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB       = $8808;
  GL_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB       = $8809;
  GL_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB       = $880A;
  GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB          = $880B;
  GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB          = $880C;
  GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB          = $880D;
  GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB   = $880E;
  GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB   = $880F;
  GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB   = $8810;
  GL_MAX_TEXTURE_COORDS_ARB                    = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_ARB               = $8872;

  //ifndef GL_ARB_shader_objects
  GL_PROGRAM_OBJECT_ARB                        = $8B40;
  GL_SHADER_OBJECT_ARB                         = $8B48;
  GL_OBJECT_TYPE_ARB                           = $8B4E;
  GL_OBJECT_SUBTYPE_ARB                        = $8B4F;
  GL_FLOAT_VEC2_ARB                            = $8B50;
  GL_FLOAT_VEC3_ARB                            = $8B51;
  GL_FLOAT_VEC4_ARB                            = $8B52;
  GL_INT_VEC2_ARB                              = $8B53;
  GL_INT_VEC3_ARB                              = $8B54;
  GL_INT_VEC4_ARB                              = $8B55;
  GL_BOOL_ARB                                  = $8B56;
  GL_BOOL_VEC2_ARB                             = $8B57;
  GL_BOOL_VEC3_ARB                             = $8B58;
  GL_BOOL_VEC4_ARB                             = $8B59;
  GL_FLOAT_MAT2_ARB                            = $8B5A;
  GL_FLOAT_MAT3_ARB                            = $8B5B;
  GL_FLOAT_MAT4_ARB                            = $8B5C;
  GL_SAMPLER_1D_ARB                            = $8B5D;
  GL_SAMPLER_2D_ARB                            = $8B5E;
  GL_SAMPLER_3D_ARB                            = $8B5F;
  GL_SAMPLER_CUBE_ARB                          = $8B60;
  GL_SAMPLER_1D_SHADOW_ARB                     = $8B61;
  GL_SAMPLER_2D_SHADOW_ARB                     = $8B62;
  GL_SAMPLER_2D_RECT_ARB                       = $8B63;
  GL_SAMPLER_2D_RECT_SHADOW_ARB                = $8B64;
  GL_OBJECT_DELETE_STATUS_ARB                  = $8B80;
  GL_OBJECT_COMPILE_STATUS_ARB                 = $8B81;
  GL_OBJECT_LINK_STATUS_ARB                    = $8B82;
  GL_OBJECT_VALIDATE_STATUS_ARB                = $8B83;
  GL_OBJECT_INFO_LOG_LENGTH_ARB                = $8B84;
  GL_OBJECT_ATTACHED_OBJECTS_ARB               = $8B85;
  GL_OBJECT_ACTIVE_UNIFORMS_ARB                = $8B86;
  GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB      = $8B87;
  GL_OBJECT_SHADER_SOURCE_LENGTH_ARB           = $8B88;

  //ifndef GL_ARB_vertex_shader
  GL_VERTEX_SHADER_ARB                         = $8B31;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB         = $8B4A;
  GL_MAX_VARYING_FLOATS_ARB                    = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB        = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB      = $8B4D;
  GL_OBJECT_ACTIVE_ATTRIBUTES_ARB              = $8B89;
  GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB    = $8B8A;

  //ifndef GL_ARB_fragment_shader
  GL_FRAGMENT_SHADER_ARB                       = $8B30;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB       = $8B49;
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB       = $8B8B;

  //ifndef GL_ARB_shading_language_100
  GL_SHADING_LANGUAGE_VERSION_ARB              = $8B8C;


  procedure glPolygonOffset(factor, units: GLfloat); stdcall; external 'opengl32.dll';

  procedure glArrayElement(i: GLint); stdcall; external 'opengl32.dll';
//  procedure glDrawRangeElements(mode: GLenum; start: GLuint; _end: GLuint; count: GLsizei; _type: GLenum; const indices: PGLvoid); stdcall; external 'opengl32.dll';
  procedure glDrawElements(mode: GLenum; count: GLsizei; atype: GLenum; indices: Pointer); stdcall; external 'opengl32.dll'; overload; // standard
  procedure glDrawElements(mode: GLenum; count: GLsizei; atype: GLenum; var indicesOffset: integer); stdcall; external 'opengl32.dll'; overload;   // VBO
  procedure glDrawArrays(mode: GLenum; first: GLint; count: GLsizei); stdcall; external 'opengl32.dll';
  procedure glVertexPointer(size: GLint; atype: GLenum; stride : GLsizei; const pointer: Pointer); stdcall; external 'opengl32.dll';
  procedure glNormalPointer(atype: GLenum; stride: GLsizei; data: pointer); stdcall; external 'opengl32.dll';
  procedure glIndexPointer(atype: GLenum; stride: GLsizei; data: pointer); stdcall; external 'opengl32.dll';
  procedure glColorPointer(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); stdcall; external 'opengl32.dll';
  procedure glTexCoordPointer(size: GLint; atype: GLenum; stride : GLsizei; const pointer: Pointer); stdcall; external 'opengl32.dll';
  procedure glEnableClientState(id: GLuint); stdcall; external 'opengl32.dll';
  procedure glDisableClientState(id: GLuint); stdcall; external 'opengl32.dll';

  procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external 'opengl32.dll';
  procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external 'opengl32.dll';
  function glIsTexture(texture: GLuint): GLboolean; stdcall; external 'opengl32.dll';
  procedure glDeleteTextures(n: GLsizei; textures: PGLuint); stdcall; external 'opengl32.dll';
  function gluBuild2DMipmaps(Target: GLenum; Components, Width, Height: GLint; Format, atype: GLenum; Data: Pointer): GLint; stdcall; external 'glu32.dll';
(*
  function gluNewQuadric : PGLUquadric; stdcall; external 'glu32.dll';
  procedure gluDeleteQuadric(state: PGLUquadric); stdcall; external 'glu32.dll';
  procedure gluPartialDisk(quadObject: PGLUquadric; innerRadius, outerRadius: GLdouble; slices, loops: GLint; startAngle, sweepAngle : GLdouble); stdcall; external 'glu32.dll';
*)
var // Extensions
  glFogCoordfEXT               : procedure(coord: GLfloat); stdcall = nil;
  // WGL_EXT_swap_control
  wglSwapIntervalEXT           : function(interval: GLint) : Boolean; stdcall = nil;
  wglGetSwapIntervalEXT        : function : GLint; stdcall = nil;
  // multi-texturing
  glClientActiveTextureARB     : procedure(target: GLenum); stdcall = nil;
  glActiveTextureARB           : procedure(target: GLenum); stdcall = nil;
  glMultiTexCoord2f            : procedure(target: GLenum; s: GLfloat; t: GLfloat); stdcall = nil;
  glMultiTexCoord2fv           : procedure(target: GLenum; const v: PGLfloat); stdcall = nil;
  //
  glDrawRangeElements          : procedure(mode: GLenum; start: GLuint; _end: GLuint; count: GLsizei; _type: GLenum; const indices: PGLvoid); stdcall = nil;
  glBlendColorEXT              : procedure(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); stdcall = nil;
  // vertex buffer objects
  glGenBuffersARB              : procedure(n: GLsizei; buffers: PGLuint); stdcall = nil;
  glIsBufferARB                : function(buffer: GLuint): GLboolean; stdcall = nil;
  glBindBufferARB              : procedure(target: GLenum; buffer: GLuint); stdcall = nil;
  glDeleteBuffersARB           : procedure(n: GLsizei; const buffers: PGLuint); stdcall = nil;
  glBufferDataARB              : procedure(target: GLenum; size: GLsizeiptrARB; const data: PGLvoid; usage: GLenum); stdcall = nil;
  glBufferSubDataARB           : procedure(target: GLenum; offset: GLintptrARB; size: GLsizeiptrARB; const data: PGLvoid); stdcall = nil;
  glGetBufferSubDataARB        : procedure(target: GLenum; offset: GLintptrARB; size: GLsizeiptrARB; data: PGLvoid); stdcall = nil;
  glMapBufferARB               : function(target: GLenum; access: GLenum): PGLvoid; stdcall = nil;
  glUnmapBufferARB             : function(target: GLenum): GLboolean; stdcall = nil;
  glGetBufferParameterivARB    : procedure(target: GLenum; pname: GLenum; params: PGLint); stdcall = nil;
  glGetBufferPointervARB       : procedure(target: GLenum; pname: GLenum; params: PGLvoid); stdcall = nil;
  // GL_ARB_shader_objects
  glCreateShaderObject         : function(shaderType: GLenum) : GLhandleARB; stdcall = nil;
  glShaderSource               : procedure(shader: GLhandleARB; nstrings: GLuint; const strings: PGLcharARB; lengths: PGLint); stdcall = nil;
  glCompileShader              : procedure(shader: GLhandleARB); stdcall = nil;
  glCreateProgramObject        : function : GLhandleARB; stdcall = nil;
  glAttachObject               : procedure(prg: GLhandleARB; shader: GLhandleARB); stdcall = nil;
  glLinkProgram                : procedure(prg: GLhandleARB); stdcall = nil;
  glUseProgramObject           : procedure(prg: GLhandleARB); stdcall = nil;
  glDeleteObject               : procedure(obj: GLhandleARB); stdcall = nil;
  glDetachObject               : procedure(prg: GLhandleARB; shader: GLhandleARB); stdcall = nil;
  glGetObjectParameterfv       : procedure(obj: GLhandleARB; pname: GLenum; params: PGLfloat); stdcall = nil;
  glGetObjectParameteriv       : procedure(obj: GLhandleARB; pname: GLenum; params: PGLint); stdcall = nil;
  glGetShaderSource            : procedure(shader: GLhandleARB; maxLength: GLsizei; length: PGLsizei; source: PGLcharARB); stdcall = nil;
  glGetInfoLog                 : procedure(obj: GLhandleARB; maxLength: GLsizei; length: PGLsizei; infoLog: PGLcharARB); stdcall = nil;
  glGetHandle                  : function(pname: GLenum) : GLhandleARB; stdcall = nil;
  glGetAttachedObjects         : procedure(prg: GLhandleARB; maxCount: GLsizei; count: PGLsizei; objects: PGLhandleARB); stdcall = nil;
  //GL_ARB_vertex_program
  glVertexAttrib1d             : procedure(index: GLuint; x: GLdouble); stdcall = nil;
  glVertexAttrib1dv            : procedure(index: GLuint; const v: PGLdouble); stdcall = nil;
  glVertexAttrib1f             : procedure(index: GLuint; x: GLfloat); stdcall = nil;
  glVertexAttrib1fv            : procedure(index: GLuint; const v: PGLfloat); stdcall = nil;
  glVertexAttrib1s             : procedure(index: GLuint; x: GLshort); stdcall = nil;
  glVertexAttrib1sv            : procedure(index: GLuint; const v: PGLshort); stdcall = nil;
  glVertexAttrib2d             : procedure(index: GLuint; x: GLdouble; y: GLdouble); stdcall = nil;
  glVertexAttrib2dv            : procedure(index: GLuint; const v: PGLdouble); stdcall = nil;
  glVertexAttrib2f             : procedure(index: GLuint; x: GLfloat; y: GLfloat); stdcall = nil;
  glVertexAttrib2fv            : procedure(index: GLuint; const v: PGLfloat); stdcall = nil;
  glVertexAttrib2s             : procedure(index: GLuint; x: GLshort; y: GLshort); stdcall = nil;
  glVertexAttrib2sv            : procedure(index: GLuint; const v: PGLshort); stdcall = nil;
  glVertexAttrib3d             : procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); stdcall = nil;
  glVertexAttrib3dv            : procedure(index: GLuint; const v: PGLdouble); stdcall = nil;
  glVertexAttrib3f             : procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); stdcall = nil;
  glVertexAttrib3fv            : procedure(index: GLuint; const v: PGLfloat); stdcall = nil;
  glVertexAttrib3s             : procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); stdcall = nil;
  glVertexAttrib3sv            : procedure(index: GLuint; const v: PGLshort); stdcall = nil;
  glVertexAttrib4Nbv           : procedure(index: GLuint; const v: PGLbyte); stdcall = nil;
  glVertexAttrib4Niv           : procedure(index: GLuint; const v: PGLint); stdcall = nil;
  glVertexAttrib4Nsv           : procedure(index: GLuint; const v: PGLshort); stdcall = nil;
  glVertexAttrib4Nub           : procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); stdcall = nil;
  glVertexAttrib4Nubv          : procedure(index: GLuint; const v: PGLubyte); stdcall = nil;
  glVertexAttrib4Nuiv          : procedure(index: GLuint; const v: PGLuint); stdcall = nil;
  glVertexAttrib4Nusv          : procedure(index: GLuint; const v: PGLushort); stdcall = nil;
  glVertexAttrib4bv            : procedure(index: GLuint; const v: PGLbyte); stdcall = nil;
  glVertexAttrib4d             : procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); stdcall = nil;
  glVertexAttrib4dv            : procedure(index: GLuint; const v: PGLdouble); stdcall = nil;
  glVertexAttrib4f             : procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); stdcall = nil;
  glVertexAttrib4fv            : procedure(index: GLuint; const v: PGLfloat); stdcall = nil;
  glVertexAttrib4iv            : procedure(index: GLuint; const v: PGLint); stdcall = nil;
  glVertexAttrib4s             : procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); stdcall = nil;
  glVertexAttrib4sv            : procedure(index: GLuint; const v: PGLshort); stdcall = nil;
  glVertexAttrib4ubv           : procedure(index: GLuint; const v: PGLubyte); stdcall = nil;
  glVertexAttrib4uiv           : procedure(index: GLuint; const v: PGLuint); stdcall = nil;
  glVertexAttrib4usv           : procedure(index: GLuint; const v: PGLushort); stdcall = nil;
  glVertexAttribPointer        : procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const pointer: PGLvoid);
  glVertexAttribIPointer       : procedure(index: GLuint; size: GLint; _type: GLenum; stride: GLsizei; const pointer: PGLvoid);
  glGetVertexAttribIiv         : procedure(index: GLuint; pname: GLenum; params: PGLenum);
  glGetVertexAttribIuiv        : procedure(index: GLuint; pname: GLenum; params: PGLuint);
  glVertexAttribI1i            : procedure(index: GLuint; x: GLint);
  glVertexAttribI2i            : procedure(index: GLuint; x: GLint; y: GLint);
  glVertexAttribI3i            : procedure(index: GLuint; x: GLint; y: GLint; z: GLint);
  glVertexAttribI4i            : procedure(index: GLuint; x: GLint; y: GLint; z: GLint; w: GLint);
  glVertexAttribI1ui           : procedure(index: GLuint; x: GLuint);
  glVertexAttribI2ui           : procedure(index: GLuint; x: GLuint; y: GLuint);
  glVertexAttribI3ui           : procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint);
  glVertexAttribI4ui           : procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint);
  glVertexAttribI1iv           : procedure(index: GLuint; const v: PGLint);
  glVertexAttribI2iv           : procedure(index: GLuint; const v: PGLint);
  glVertexAttribI3iv           : procedure(index: GLuint; const v: PGLint);
  glVertexAttribI4iv           : procedure(index: GLuint; const v: PGLint);
  glVertexAttribI1uiv          : procedure(index: GLuint; const v: PGLuint);
  glVertexAttribI2uiv          : procedure(index: GLuint; const v: PGLint);
  glVertexAttribI3uiv          : procedure(index: GLuint; const v: PGLint);
  glVertexAttribI4uiv          : procedure(index: GLuint; const v: PGLint);
  glVertexAttribI4bv           : procedure(index: GLuint; const v: PGLbyte);
  glVertexAttribI4sv           : procedure(index: GLuint; const v: PGLshort);
  glVertexAttribI4ubv          : procedure(index: GLuint; const v: PGLubyte);
  glVertexAttribI4usv          : procedure(index: GLuint; const v: PGLushort);
  glGetUniformuiv              : procedure(prg: GLuint; location: GLint; params: PGLuint);
  glBindFragDataLocation       : procedure(prg: GLuint; color: GLuint; const name: PGLchar);
  glGetFragDataLocation        : function(prg: GLuint; const name: PGLchar) : GLint;
  glUniform1ui                 : procedure(location: GLint; v0: GLuint);
  glUniform2ui                 : procedure(location: GLint; v0: GLuint; v1: GLuint);
  glUniform3ui                 : procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint);
  glUniform4ui                 : procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint);
  glUniform1uiv                : procedure(location: GLint; count: GLsizei; const value: PGLuint);
  glUniform2uiv                : procedure(location: GLint; count: GLsizei; const value: PGLuint);
  glUniform3uiv                : procedure(location: GLint; count: GLsizei; const value: PGLuint);
  glUniform4uiv                : procedure(location: GLint; count: GLsizei; const value: PGLuint);
  glEnableVertexAttribArray    : procedure(index: GLuint);
  glDisableVertexAttribArray   : procedure(index: GLuint);
  glProgramString              : procedure(target: GLenum; format: GLenum; len: GLsizei; const str: PGLvoid);
  glBindProgram                : procedure(target: GLenum; prg: GLuint);
  glDeletePrograms             : procedure(n: GLsizei; const prgs: PGLuint);
  glGenPrograms                : procedure(n: GLsizei; const prgs: PGLuint);
  glProgramEnvParameter4d      : procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble);
  glProgramEnvParameter4dv     : procedure(target: GLenum; index: GLuint; const params: PGLdouble);
  glProgramEnvParameter4f      : procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat);
  glProgramEnvParameter4fv     : procedure(target: GLenum; index: GLuint; const params: PGLfloat);
  glProgramLocalParameter4d    : procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble);
  glProgramLocalParameter4dv   : procedure(target: GLenum; index: GLuint; const params: PGLdouble);
  glProgramLocalParameter4f    : procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat);
  glProgramLocalParameter4fv   : procedure(target: GLenum; index: GLuint; const params: PGLfloat);
  glGetProgramEnvParameterdv   : procedure(target: GLenum; index: GLuint; const params: PGLdouble);
  glGetProgramEnvParameterfv   : procedure(target: GLenum; index: GLuint; const params: PGLfloat);
  glGetProgramLocalParameterdv : procedure(target: GLenum; index: GLuint; const params: PGLdouble);
  glGetProgramLocalParameterfv : procedure(target: GLenum; index: GLuint; const params: PGLfloat);
  glGetProgramiv               : procedure(target: GLenum; pname: GLenum; const params: PGLint);
  glGetProgramString           : procedure(target: GLenum; pname: GLenum; const str: PGLvoid);
  glGetVertexAttribdv          : procedure(index: GLuint; pname: GLenum; const params: PGLdouble);
  glGetVertexAttribfv          : procedure(index: GLuint; pname: GLenum; const params: PGLfloat);
  glGetVertexAttribiv          : procedure(index: GLuint; pname: GLenum; const params: PGLint);
  glGetVertexAttribPointerv    : procedure(index: GLuint; pname: GLenum; const pointer: PGLvoid);
  glIsProgram                  : function(prg: GLuint) : GLboolean;
  // GL_ARB_vertex_shader
  glBindAttribLocation         : procedure(programObj: GLhandleARB; index: GLuint; const name: PGLcharARB);
  glGetActiveAttrib            : procedure(programObj: GLhandleARB; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLcharARB);
  glGetAttribLocation          : function(programObj: GLhandleARB; const name: PGLcharARB) : GLint;

var OGL: TOGL;


implementation
uses Forms, Types, uCalc;


{ TOGL }
//-- properties lezen en aanpassen/schijven
{procedure TOGL.SetWinHandle(const Value: HWND);
begin
  if Value <> local_WinHandle then begin
    if Active then begin
      Disable;                     // OpenGL uitschakelen..
      FullScreen := false;         // evt. de volledige scherm modus uitschakelen..
    end;
    local_WinHandle := Value;
  end;
end;}

function TOGL.GetActive: Boolean;
begin
  local_Active := ((local_DC<>0) and (local_RC<>0));
  Result := local_Active;
end;
{procedure TOGL.SetActive(const Value: boolean);
begin
  if Value <> local_Active then
    local_Active := Value;
end;}

procedure TOGL.SetPaused(const Value: boolean);
begin
  if Value <> local_Paused then
    local_Paused := Value;
end;

procedure TOGL.SetFullScreen(const Value: boolean);
begin
  if not Active then Exit;
  if Value <> local_FullScreen then begin
    local_FullScreen := Value;
    SetupFullScreen(local_FullScreen);
  end;
end;

procedure TOGL.SetMouseLook(const Value: boolean);
begin
  if Value <> local_MouseLook then local_MouseLook := Value;
end;
procedure TOGL.ToggleMouseLook;
begin
  local_MouseLook := not local_MouseLook;
end;

procedure TOGL.SetFog(const Value: boolean);
begin
  if not Active then Exit;
  if Value <> local_Fog then begin
    local_Fog := Value;
    SetupFog(local_Fog);
  end;
end;
function TOGL.GetWidth: integer;
begin
  Result := local_Projection.Width;
end;
function TOGL.GetHeight: integer;
begin
  Result := local_Projection.Height;
end;
function TOGL.GetCenter: TPoint;
begin
  Result := local_Projection.Center;
end;
//-- /properties



constructor TOGL.Create;
begin
  // Object initiëren
  inherited;
  // Data initialiseren
  local_WinHandle := 0;                // nog geen Window-handle toegewezen..
  local_DC := 0;                       // nog geen Device-Context toegewezen..
  local_RC := 0;                       // nog geen RenderContext toegewezen..
  local_Width := 0;                    // form breedte
  local_Height := 0;                   // form hoogte
  local_Active := false;               // OpenGL niet actief
  local_Paused := false;               // niet gepauzeerd
  local_FullScreen := false;           // windowed
  local_Fog := false;                  // mist uitschakelen
  local_Multitexture := false;         // supports EXT/ARB
  local_FogCoord := false;             // supports EXT/ARB
  local_BlendColor := false;           // supports EXT/ARB
  local_VertexBufferObject := false;   // supports EXT/ARB
  local_ShaderObjects := false;        // supports EXT/ARB

  local_VSync := 1;                    // standaard Vertical Retrace Sync aan
  OldVSync := local_VSync;

  local_FPS := 0;                      // 0 beelden per seconde
  local_FPSCount := 0;                 //
  local_LastFrameTime := 0.0;          // de tijdsduur die nodig was om het laatste beeld te renderen
  PerformanceCount := 0;
  QueryPerformanceFrequency(PerformanceFreq);

  local_MouseLook := false;

  with local_Projection do begin       // Projectie
    ProjectionType := ptPerspective;
    Width := local_Width;
    Height := local_Height;

    //FOVY := 65.0;
    FOVY := GetAngleFOV(MaxViewDistance, MaxViewDistance);

    zNear := 1.0;
    zFar := MaxViewDistance;
    Center := Point(0,0);
    // De padding (tussen venster en viewport)
    // (waarden in bereik[0..1])
    // Dit zijn percentages van de gehele viewport-breedte & -hoogte.
    // Een waarde van 0.5 voor .left zorgt ervoor dat er aan de linkerkant
    // langs de viewport een ruimte open is, die de helft van het venster breed is.
    ViewPortRect.top := 0.0;
    ViewPortRect.left := 0.0;
    ViewPortRect.bottom := 0.0;
    ViewPortRect.right := 0.0;
    // de vaste padding waarden (in pixels)
    ViewPortRect.pxTop := 0;
    ViewPortRect.pxLeft := 0;
    ViewPortRect.pxBottom := 0;
    ViewPortRect.pxRight := 0;
  end;
  N_TextureUnits := -1; //ongeldige waarde
  IsSetupFor2D := false; //niet in 2D-mode
  // objecten instantieren
  Frustum := TFrustum.Create;          // het frustum-object
  Textures := TObjTexture.Create;      // textures-object
  Fonts := TGLFonts.Create;            // lettertypen-object
  local_Camera := TCamera.Create;      // het camera-object
  Camera := @local_Camera;             // gebruik local_Camera als de huidige camera
  SkyBox := TSkyBox.Create(@OGL);      // SkyBox-object
(*
  // Extensions
  glFogCoordfEXT := nil;
  wglSwapIntervalEXT := nil;
  wglGetSwapIntervalEXT := nil;
  glClientActiveTextureARB := nil;
  glActiveTextureARB := nil;
  glMultiTexCoord2f := nil;
  glMultiTexCoord2fv := nil;
  glBlendColorEXT := nil;
*)
end;

destructor TOGL.Destroy;
begin
  // Data finaliseren
  SkyBox.Free;                         // SkyBox-object
  if local_Camera<>nil then begin
    local_Camera.Free;                 // het local_camera-object
    local_Camera := nil;
  end;
  if Camera<>nil then begin
//    Camera.Free;                       // het camera-object
    Camera := nil;
  end;
  Fonts.Free;                          // lettertypen
  Textures.Free;                       // het texture-object
  Frustum.Free;                        // het frustum-object
  Disable;                             // evt. OpenGL uitschakelen..
  // Object finaliseren
  inherited;
end;



procedure TOGL.ResetCamera;
begin
  Camera := @local_Camera;             // gebruik local_Camera als de huidige camera
end;






procedure TOGL.Enable(WindowHandle: HWND);
var pfd: PIXELFORMATDESCRIPTOR;
    iFormat: integer;
    f,c: integer;
begin
  if Active then Exit;  //al actief? dan niet nog eens activeren..
  // Zoek het form met handle WindowHandle, en bepaal de dimensies..
  for f:=0 to Screen.FormCount-1 do
    if Screen.Forms[f].Handle = WindowHandle then begin
      local_Width := Screen.Forms[f].Width;
      local_Height := Screen.Forms[f].Height;
      Break;
    end;
  // De DeviceContext (DC) opvragen
  local_WinHandle := WindowHandle;
  local_DC := GetDC(local_WinHandle);
  if (local_DC=0) then ReportErrorAndQuit(error_GetDC);
  // Het pixelformat voor de DC instellen
  ZeroMemory(@pfd, sizeof(pfd));
  pfd.nSize := sizeof(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := (PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER);
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.cColorBits := 32;   //8 bits per bitplane
  pfd.cDepthBits := 24;   //24-bits Z-buffer
  pfd.iLayerType := PFD_MAIN_PLANE;
  iFormat := ChoosePixelFormat(DC,@pfd);
  if (iFormat=0) then ReportErrorAndQuit(error_ChoosePixelFormat);
  if (not SetPixelFormat(local_DC, iFormat, @pfd)) then ReportErrorAndQuit(error_SetPixelFormat);
  // Het palette instellen
  {setupPalette(DC);}
  // De RenderContext instellen
  local_RC := wglCreateContext(local_DC);
  if (local_RC=0) then ReportErrorAndQuit(error_MakeRC);
  if (not wglMakeCurrent(local_DC,local_RC)) then ReportErrorAndQuit(error_MakeCurrent);

  // De OpenGL-DLL entry-points opvragen voor de extensions
  GetExtensionProcs;
  
  // OpenGL instellingen
  Init;
end;

procedure TOGL.Disable;
begin
  if Active then begin // OpenGL actief? dan uitschakelen..
    // evt. aangemaakte textures wissen..
    Textures.DeleteTextures;
    // evt. aangemaakte font displaylists wissen..
{    Fonts.DeleteFont;}
    // OpenGL RC vrijgeven
    wglMakeCurrent(0,0);
    wglDeleteContext(local_RC);
    // het palette vrijgeven
    if (Palette<>0) then DeleteObject(Palette);
    // De DeviceContext vrijgeven
    ReleaseDC(WinHandle, local_DC);
  end;
  FullScreen := false; // evt. de volledige scherm modus uitschakelen..
  local_DC := 0;
  local_RC := 0;
  // VSync herstellen
  if ExtensionSupported('WGL_EXT_swap_control') then wglSwapIntervalEXT(OldVSync);
  local_VSync := OldVSync;
end;

procedure TOGL.DoBufferSwap;
begin
  // Ook een bufferswap als OpenGL is gepauzeerd..
  if Active then SwapBuffers(local_DC);
end;


function TOGL.ExtensionSupported(Ext: string): boolean;
var s: string;
begin
  s := glGetString(GL_EXTENSIONS);
  Result := (Pos(Ext, s)>0);
end;

procedure TOGL.GetExtensionProcs;
begin
  glDrawRangeElements            := wglGetProcAddress('glDrawRangeElements');

  // de OpenGL-DLL procedure entry-points opvragen voor de extensions
  if ExtensionSupported('EXT_fog_coord') then begin
    glFogCoordfEXT               := wglGetProcAddress('glFogCoordfEXT');
    local_FogCoord := true; // supports EXT/ARB
  end;

  if ExtensionSupported('WGL_EXT_swap_control') then begin
    wglSwapIntervalEXT           := wglGetProcAddress('wglSwapIntervalEXT');
    wglGetSwapIntervalEXT        := wglGetProcAddress('wglGetSwapIntervalEXT');
    // de huidige VSync instelling onthouden
    OldVSync := wglGetSwapIntervalEXT;
  end;

  // GL_ARB_multitexture
  if ExtensionSupported('GL_ARB_multitexture') then begin
    glClientActiveTextureARB     := wglGetProcAddress('glClientActiveTextureARB');
    glActiveTextureARB           := wglGetProcAddress('glActiveTextureARB');
    glMultiTexCoord2f            := wglGetProcAddress('glMultiTexCoord2f');
    glMultiTexCoord2fv          := wglGetProcAddress('glMultiTexCoord2fv');
    local_Multitexture := true; // supports EXT/ARB
  end;

  // GL_EXT_blend_color
  if ExtensionSupported('GL_EXT_blend_color') then begin
    glBlendColorEXT              := wglGetProcAddress('glBlendColorEXT');
    local_BlendColor := true; // supports EXT/ARB
  end;

  // GL_ARB_vertex_buffer_object
  if ExtensionSupported('GL_ARB_vertex_buffer_object') then begin
    glGenBuffersARB              := wglGetProcAddress('glGenBuffersARB');
    glIsBufferARB                := wglGetProcAddress('glIsBufferARB');
    glBindBufferARB              := wglGetProcAddress('glBindBufferARB');
    glDeleteBuffersARB           := wglGetProcAddress('glDeleteBuffersARB');
    glBufferDataARB              := wglGetProcAddress('glBufferDataARB');
    glBufferSubDataARB           := wglGetProcAddress('glBufferSubDataARB');
    glGetBufferSubDataARB        := wglGetProcAddress('glGetBufferSubDataARB');
    glMapBufferARB               := wglGetProcAddress('glMapBufferARB');
    glUnmapBufferARB             := wglGetProcAddress('glUnmapBufferARB');
    glGetBufferParameterivARB    := wglGetProcAddress('glGetBufferParameterivARB');
    glGetBufferPointervARB       := wglGetProcAddress('glGetBufferPointervARB');
    local_VertexBufferObject := true; // supports EXT/ARB
  end;

  // shader language
  if ExtensionSupported('GL_ARB_shader_objects') and
     ExtensionSupported('GL_ARB_vertex_shader') and
     ExtensionSupported('GL_ARB_fragment_shader') then begin
    glCreateShaderObject         := wglGetProcAddress('glCreateShaderObjectARB');
    glShaderSource               := wglGetProcAddress('glShaderSourceARB');
    glCompileShader              := wglGetProcAddress('glCompileShaderARB');
    glCreateProgramObject        := wglGetProcAddress('glCreateProgramObjectARB');
    glAttachObject               := wglGetProcAddress('glAttachObjectARB');
    glLinkProgram                := wglGetProcAddress('glLinkProgramARB');
    glUseProgramObject           := wglGetProcAddress('glUseProgramObjectARB');
    glDeleteObject               := wglGetProcAddress('glDeleteObjectARB');
    glDetachObject               := wglGetProcAddress('glDetachObjectARB');
    glVertexAttrib1d             := wglGetProcAddress('glVertexAttrib1d');
    glVertexAttrib1dv            := wglGetProcAddress('glVertexAttrib1dv');
    glVertexAttrib1f             := wglGetProcAddress('glVertexAttrib1f');
    glVertexAttrib1fv            := wglGetProcAddress('glVertexAttrib1fv');
    glVertexAttrib1s             := wglGetProcAddress('glVertexAttrib1s');
    glVertexAttrib1sv            := wglGetProcAddress('glVertexAttrib1sv');
    glVertexAttrib2d             := wglGetProcAddress('glVertexAttrib2d');
    glVertexAttrib2dv            := wglGetProcAddress('glVertexAttrib2dv');
    glVertexAttrib2f             := wglGetProcAddress('glVertexAttrib2f');
    glVertexAttrib2fv            := wglGetProcAddress('glVertexAttrib2fv');
    glVertexAttrib2s             := wglGetProcAddress('glVertexAttrib2s');
    glVertexAttrib2sv            := wglGetProcAddress('glVertexAttrib2sv');
    glVertexAttrib3d             := wglGetProcAddress('glVertexAttrib3d');
    glVertexAttrib3dv            := wglGetProcAddress('glVertexAttrib3dv');
    glVertexAttrib3f             := wglGetProcAddress('glVertexAttrib3f');
    glVertexAttrib3fv            := wglGetProcAddress('glVertexAttrib3fv');
    glVertexAttrib3s             := wglGetProcAddress('glVertexAttrib3s');
    glVertexAttrib3sv            := wglGetProcAddress('glVertexAttrib3sv');
    glVertexAttrib4Nbv           := wglGetProcAddress('glVertexAttrib4Nbv');
    glVertexAttrib4Niv           := wglGetProcAddress('glVertexAttrib4Niv');
    glVertexAttrib4Nsv           := wglGetProcAddress('glVertexAttrib4Nsv');
    glVertexAttrib4Nub           := wglGetProcAddress('glVertexAttrib4Nub');
    glVertexAttrib4Nubv          := wglGetProcAddress('glVertexAttrib4Nubv');
    glVertexAttrib4Nuiv          := wglGetProcAddress('glVertexAttrib4Nuiv');
    glVertexAttrib4Nusv          := wglGetProcAddress('glVertexAttrib4Nusv');
    glVertexAttrib4bv            := wglGetProcAddress('glVertexAttrib4bv');
    glVertexAttrib4d             := wglGetProcAddress('glVertexAttrib4d');
    glVertexAttrib4dv            := wglGetProcAddress('glVertexAttrib4dv');
    glVertexAttrib4f             := wglGetProcAddress('glVertexAttrib4f');
    glVertexAttrib4fv            := wglGetProcAddress('glVertexAttrib4fv');
    glVertexAttrib4iv            := wglGetProcAddress('glVertexAttrib4iv');
    glVertexAttrib4s             := wglGetProcAddress('glVertexAttrib4s');
    glVertexAttrib4sv            := wglGetProcAddress('glVertexAttrib4sv');
    glVertexAttrib4ubv           := wglGetProcAddress('glVertexAttrib4ubv');
    glVertexAttrib4uiv           := wglGetProcAddress('glVertexAttrib4uiv');
    glVertexAttrib4usv           := wglGetProcAddress('glVertexAttrib4usv');
    glVertexAttribPointer        := wglGetProcAddress('glVertexAttribPointer');
    glVertexAttribIPointer       := wglGetProcAddress('glVertexAttribIPointer');
    glGetVertexAttribIiv         := wglGetProcAddress('glGetVertexAttribIiv');
    glGetVertexAttribIuiv        := wglGetProcAddress('glGetVertexAttribIuiv');
    glVertexAttribI1i            := wglGetProcAddress('glVertexAttribI1i');
    glVertexAttribI2i            := wglGetProcAddress('glVertexAttribI2i');
    glVertexAttribI3i            := wglGetProcAddress('glVertexAttribI3i');
    glVertexAttribI4i            := wglGetProcAddress('glVertexAttribI4i');
    glVertexAttribI1ui           := wglGetProcAddress('glVertexAttribI1ui');
    glVertexAttribI2ui           := wglGetProcAddress('glVertexAttribI2ui');
    glVertexAttribI3ui           := wglGetProcAddress('glVertexAttribI3ui');
    glVertexAttribI4ui           := wglGetProcAddress('glVertexAttribI4ui');
    glVertexAttribI1iv           := wglGetProcAddress('glVertexAttribI1iv');
    glVertexAttribI2iv           := wglGetProcAddress('glVertexAttribI2iv');
    glVertexAttribI3iv           := wglGetProcAddress('glVertexAttribI3iv');
    glVertexAttribI4iv           := wglGetProcAddress('glVertexAttribI4iv');
    glVertexAttribI1uiv          := wglGetProcAddress('glVertexAttribI1uiv');
    glVertexAttribI2uiv          := wglGetProcAddress('glVertexAttribI2uiv');
    glVertexAttribI3uiv          := wglGetProcAddress('glVertexAttribI3uiv');
    glVertexAttribI4uiv          := wglGetProcAddress('glVertexAttribI4uiv');
    glVertexAttribI4bv           := wglGetProcAddress('glVertexAttribI4bv');
    glVertexAttribI4sv           := wglGetProcAddress('glVertexAttribI4sv');
    glVertexAttribI4ubv          := wglGetProcAddress('glVertexAttribI4ubv');
    glVertexAttribI4usv          := wglGetProcAddress('glVertexAttribI4usv');
    glGetUniformuiv              := wglGetProcAddress('glGetUniformuiv');
    glBindFragDataLocation       := wglGetProcAddress('glBindFragDataLocation');
    glGetFragDataLocation        := wglGetProcAddress('glGetFragDataLocation');
    glUniform1ui                 := wglGetProcAddress('glUniform1ui');
    glUniform2ui                 := wglGetProcAddress('glUniform2ui');
    glUniform3ui                 := wglGetProcAddress('glUniform3ui');
    glUniform4ui                 := wglGetProcAddress('glUniform4ui');
    glUniform1uiv                := wglGetProcAddress('glUniform1uiv');
    glUniform2uiv                := wglGetProcAddress('glUniform2uiv');
    glUniform3uiv                := wglGetProcAddress('glUniform3uiv');
    glUniform4uiv                := wglGetProcAddress('glUniform4uiv');
    glEnableVertexAttribArray    := wglGetProcAddress('glEnableVertexAttribArrayARB');
    glDisableVertexAttribArray   := wglGetProcAddress('glDisableVertexAttribArrayARB');
    glProgramString              := wglGetProcAddress('glProgramStringARB');
    glBindProgram                := wglGetProcAddress('glBindProgramARB');
    glDeletePrograms             := wglGetProcAddress('glDeleteProgramsARB');
    glGenPrograms                := wglGetProcAddress('glGenProgramsARB');
    glProgramEnvParameter4d      := wglGetProcAddress('glProgramEnvParameter4dARB');
    glProgramEnvParameter4dv     := wglGetProcAddress('glProgramEnvParameter4dvARB');
    glProgramEnvParameter4f      := wglGetProcAddress('glProgramEnvParameter4fARB');
    glProgramEnvParameter4fv     := wglGetProcAddress('glProgramEnvParameter4fvARB');
    glProgramLocalParameter4d    := wglGetProcAddress('glProgramLocalParameter4dARB');
    glProgramLocalParameter4dv   := wglGetProcAddress('glProgramLocalParameter4dvARB');
    glProgramLocalParameter4f    := wglGetProcAddress('glProgramLocalParameter4fARB');
    glProgramLocalParameter4fv   := wglGetProcAddress('glProgramLocalParameter4fvARB');
    glGetProgramEnvParameterdv   := wglGetProcAddress('glGetProgramEnvParameterdvARB');
    glGetProgramEnvParameterfv   := wglGetProcAddress('glGetProgramEnvParameterfvARB');
    glGetProgramLocalParameterdv := wglGetProcAddress('glGetProgramLocalParameterdvARB');
    glGetProgramLocalParameterfv := wglGetProcAddress('glGetProgramLocalParameterfvARB');
    glGetProgramiv               := wglGetProcAddress('glGetProgramivARB');
    glGetProgramString           := wglGetProcAddress('glGetProgramStringARB');
    glGetVertexAttribdv          := wglGetProcAddress('glGetVertexAttribdvARB');
    glGetVertexAttribfv          := wglGetProcAddress('glGetVertexAttribfvARB');
    glGetVertexAttribiv          := wglGetProcAddress('glGetVertexAttribivARB');
    glGetVertexAttribPointerv    := wglGetProcAddress('glGetVertexAttribPointervARB');
    glIsProgram                  := wglGetProcAddress('glIsProgramARB');
    glBindAttribLocation         := wglGetProcAddress('glBindAttribLocationARB');
    glGetActiveAttrib            := wglGetProcAddress('glGetActiveAttribARB');
    glGetAttribLocation          := wglGetProcAddress('glGetAttribLocationARB');
    local_ShaderObjects := true; // supports EXT/ARB
  end;
end;






procedure TOGL.Init;
begin
  if not Active then Exit;

  // Verticale beeldscherm refresh uitschakelen
  SetVSync(local_VSync);

  //het aantal texture units bepalen
  GetMaxTextureUnits;

  // projectie instellen
  SetupProjection;

  // mist standaard inschakelen..
//  SetupFog(GL_EXP, 0.0001, MaxViewDistance/2,MaxViewDistance, DefaultFogColor, GL_FASTEST);
//  SetupFog(GL_LINEAR, 1.0, 0.0,1.0, DefaultFogColor, GL_FASTEST);
  SetupFog(GL_LINEAR, 0.3, 1000.0,5000.0, DefaultFogColor, GL_FASTEST);
  SetupFog(local_Fog);

  // fonts
//  Fonts.CreateFontDLs(local_DC, local_RC, 'Arial', 12, false); //2D-Arial 12px
//  Fonts.CreateFontDLs(local_DC, local_RC, 'Courier New', 14, false); //2D-Arial 12px
  Fonts.AddFont(local_DC, local_RC, 'Courier New', 14, false);
  Fonts.AddFont(local_DC, local_RC, 'Arial', 12, false);

  // Z-Buffer (diepte)
  glDepthFunc(GL_LESS);
  glDepthRange(0.0, 1.0);       // near far
  glDepthMask(GL_TRUE);         // depth-buffer is writable
  glEnable(GL_DEPTH_TEST);

  // culling
  glFrontFace(GL_CCW);
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);

  // form achtergrond
  glPolygonMode(GL_FRONT, GL_FILL);
  {glPolygonOffset(1.0, 1.0);}
  glDisable(GL_POLYGON_OFFSET_FILL);
  glClearDepth(1.0);
  glClearColor(0.0, 0.0, 0.0,  0.0);
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_FALSE);
  glDrawBuffer(GL_FRONT_AND_BACK);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT {or GL_STENCIL_BUFFER_BIT} );
  glDrawBuffer(GL_BACK);

  // render-performance
  glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);  //GL_FASTEST, GL_NICEST, GL_DONT_CARE
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  {glHint(GL_FOG_HINT, GL_NICEST);}
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);  //moet zijn GL_NICEST in-game..
  glDisable(GL_NORMALIZE); // normalen gebruik ik zelf
  glShadeModel(GL_SMOOTH);
  glDisable(GL_DITHER);
  glDisable(GL_BLEND);     // blending uit

  // environment-mapping
  glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
  glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);

  //
  DoBufferSwap;
end;



procedure TOGL.ReportErrorAndQuit(ErrorCode: integer);
var s : PChar;
begin
  case ErrorCode of
    error_GetDC : s:='The device-context could not be determined';
    error_ChoosePixelFormat : s:='The pixel-format could not be determined.';
    error_SetPixelFormat : s:='The pixel-format could not be set.';
    error_MakeRC : s:='The render-context could not be made.';
    error_MakeCurrent : s:='The render-context could not be made current.';
    error_CreatingTexture : s:='A texture could not be created.';
    error_GenList : s:='A compiled list could not be created';
  else
    s := 'OpenGL Error';
  end;
  MessageBox(WindowFromDC(DC), s, 'OpenGL Error', MB_ICONERROR or MB_OK);
  if ErrorCode<>error_CreatingTexture then PostQuitMessage(ErrorCode);
end;


//--- VSync --------------------------------------------------------------------
function TOGL.MonitorRefreshRate: Integer;
var Desktop: HDC;
begin
  Desktop := GetDC(0);
  Result := GetDeviceCaps(Desktop, VREFRESH);
  ReleaseDC(0, Desktop);
end;

function TOGL.GetVSync(var OnOff: boolean): boolean;
begin
  Result := ExtensionSupported('WGL_EXT_swap_control');
  if Result then OnOff := (local_VSync=1);
end;

function TOGL.SetVSync(OnOff: boolean): boolean;
begin
  Result := ExtensionSupported('WGL_EXT_swap_control');
  if Result then begin
    if OnOff then begin
      local_VSync := 1;
      wglSwapIntervalEXT(1);
    end else begin
      local_VSync := 0;
      wglSwapIntervalEXT(0);
    end;
  end;
end;

function TOGL.SetVSync(OnOff: integer): boolean;
begin
  Result := SetVSync(OnOff<>0);
end;


procedure TOGL.ToggleVSync;
begin
  if ExtensionSupported('WGL_EXT_swap_control') then begin
    // de huidige VSync instelling onthouden
    if local_VSync = 0 then begin
      local_VSync := 1;
      wglSwapIntervalEXT(1);
    end else begin
      local_VSync := 0;
      wglSwapIntervalEXT(0);
    end;
  end;
end;



//-- FPS meting ----------------------------------------------------------------
procedure TOGL.FPSTimer;
begin
  local_FPS := local_FPSCount; //gemiddelde FPS de afgelopen seconde
  local_FPSCount := 0; //opnieuw tellen..
end;

procedure TOGL.MeasureFPS;
var Interval: Int64;
begin
  //het gemiddeld aantal FPS berekenen (per seconde)
  Inc(local_FPSCount);
  //tijdsduur laatste frame-render (in seconden)
  QueryPerformanceCounter(PerformanceCount);
  Interval := PerformanceCount - LastPerformanceCount;
  local_LastFrameTime := 1/(PerformanceFreq / Interval);
  //timing
  LastPerformanceCount := PerformanceCount;
end;

function TOGL.GetFPS: integer;
begin
  Result := local_FPS;
end;

function TOGL.GetLastFrameTime: single;
begin
  Result := local_LastFrameTime;
end;
//--



function TOGL.GetMaxTextureUnits: GLint;
begin
  if not Active then
    Result := -1
  else begin
    glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @N_TextureUnits);
    Result := N_TextureUnits;
  end;
end;




//--- Projection ---------------------------------------------------------------
procedure TOGL.SetupViewPort(aProjection: TProjection);
var Left,Top, Right,Bottom, W,H: Integer;
begin
  // viewport berekenen
  Left := Round(aProjection.ViewPortRect.left * aProjection.Width);
  Bottom := Round(aProjection.ViewPortRect.top * aProjection.Height); //!
  Right := Round(aProjection.ViewPortRect.right * aProjection.Width);
  Top := Round(aProjection.ViewPortRect.bottom * aProjection.Height); //!
  W := aProjection.Width - Right - Left;
  H := aProjection.Height - Bottom - Top;

  W := W - aProjection.ViewPortRect.pxLeft - aProjection.ViewPortRect.pxRight;
  H := H - aProjection.ViewPortRect.pxTop - aProjection.ViewPortRect.pxBottom;

  Left := Left + aProjection.ViewPortRect.pxLeft;
  Top := Top + aProjection.ViewPortRect.pxTop;

  // en instellen
  glViewPort(Left,Top, W,H);
end;

procedure TOGL.Resize(NewWidth, NewHeight: integer);
begin
  if (NewWidth=local_Projection.Width) and (NewHeight=local_Projection.Height) then Exit;
  //projectie
  local_Projection.Width := NewWidth;
  local_Projection.Height := NewHeight;
  local_Projection.Center := Point(NewWidth div 2, NewHeight div 2);
  //form
  local_Width := NewWidth;
  local_Height := NewHeight;
  SetupProjection;
end;

procedure TOGL.SetupProjection;
begin
  if not Active then Exit;
  // De huidige projectie instellen/herstellen
  with local_Projection do begin
    case ProjectionType of
      ptOrthogonal: SetupProjection(Width,Height, Left,Right, Bottom,Top);
      ptPerspective: SetupProjection(Width,Height, FOVY, zNear,zFar);
      ptFrustum: SetupProjection(Width,Height, Left,Right, Bottom,Top, zNear,zFar);
    end;
  end;
(*
  // de frustum clip-planes herberekenen
  Frustum.ClipPlanesFromFrustum;
*)  
  IsSetupFor2D := false;
end;

procedure TOGL.SetupProjection(Width, Height: integer; oLeft,oRight,  oBottom,oTop: Extended);
begin
  if not Active then Exit;
  // viewport berekenen
  SetupViewPort(local_Projection);
  // Orthogonaal
  {glViewPort(0,0, Width,Height);}
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(oLeft,oRight, oBottom,oTop);  //Z-coördinaten moeten in bereik [-1..1] liggen !
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  IsSetupFor2D := false;
end;

procedure TOGL.SetupProjection(Width, Height: integer; FOVY, pNear,pFar: Extended);
begin
  if not Active then Exit;
  // Perspective
  // viewport berekenen
  SetupViewPort(local_Projection);
  {glViewPort(0,0, Width,Height);}
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  {aspect := (Width * (local_Projection.ViewPortRect.right-local_Projection.ViewPortRect.left)) / (Height * (local_Projection.ViewPortRect.top-local_Projection.ViewPortRect.bottom));}
  gluPerspective(FOVY, Width/Height, pNear,pFar);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  IsSetupFor2D := false;
end;

procedure TOGL.SetupProjection(Width,Height: integer; fLeft,fRight, fBottom,fTop, fNear,fFar: Extended);
begin
  if not Active then Exit;
  // Frustum
  // viewport berekenen
  SetupViewPort(local_Projection);
  {glViewPort(0,0, Width,Height);}
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glFrustum(fLeft,fRight, fBottom,fTop, fNear,fFar);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  IsSetupFor2D := false;
end;

procedure TOGL.SetupFor2D;
begin
  if not Active then Exit;
  // orthogonaal afbeelden (geen perspectief)
  // !NB. De huidige projectie instellingen niet aanpassen..
  // viewport berekenen
  SetupViewPort(local_Projection);
  {glViewPort(0,0, local_Projection.Width,local_Projection.Height);}
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  
  glOrtho(0,local_Projection.Width, 0,local_Projection.Height, -1,1); //org.
//glOrtho(0,local_Projection.Width, local_Projection.Height,0, -1,1);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  IsSetupFor2D := true;
end;

procedure TOGL.SetupProjectionPicking(const X, Y, W,H: integer);
var viewport: array[0..3] of GLint;
begin
  if not Active then Exit;
  // viewport berekenen
  SetupViewPort(local_Projection);
  glGetIntegerv(GL_VIEWPORT, @viewport);

  {glViewPort(0,0, Width,Height);}
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPickMatrix(X,viewport[3]-Y, W,H, @viewport);
  with local_Projection do gluPerspective(FOVY, Width/Height, zNear,zFar);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  with Camera^ do gluLookAt(Position.X,Position.Y,Position.Z, Target.X,Target.Y,Target.Z, UpY.X,UpY.Y,UpY.Z);
  Frustum.Calculate_glFrustumPlanes;
end;

procedure TOGL.ScreenToObjectCoords(const ScreenX,ScreenY: integer;  var X,Y,Z: GLdouble);
var vp: array[0..3] of GLint;
    mv: array[0..15] of GLdouble;
    pr: array[0..15] of GLdouble;
    iX,iY,iZ: GLint;
    fX,fY,fZ: GLfloat;
    dX,dY,dZ: GLdouble;
    objX,objY,objZ: GLdouble;
begin
  if not Active then Exit;
  glGetIntegerv(GL_VIEWPORT, @vp);
	glGetDoublev(GL_MODELVIEW_MATRIX, @mv);
	glGetDoublev(GL_PROJECTION_MATRIX, @pr);
  iX := ScreenX;
  iY := vp[3]-ScreenY;
  glReadPixels(iX, iY, 1,1, GL_DEPTH_COMPONENT, GL_FLOAT, @fZ);
  dX := iX;
  dY := iY;
  dZ := fZ;
	gluUnProject(dX,dY,dZ, @mv,@pr,@vp, X,Y,Z);
end;

procedure TOGL.CalcMouseRay(const ScreenX,ScreenY: integer; var X,Y,Z: GLdouble);
var vp: array[0..3] of GLint;
    mv: array[0..15] of GLdouble;
    pr: array[0..15] of GLdouble;
    iX,iY,iZ: GLint;
    dX,dY,dZ1,dZ2: GLdouble;
    X1,Y1,Z1, X2,Y2,Z2: GLdouble;
begin
  if not Active then Exit;
  glGetIntegerv(GL_VIEWPORT, @vp);
	glGetDoublev(GL_MODELVIEW_MATRIX, @mv);
	glGetDoublev(GL_PROJECTION_MATRIX, @pr);

  dX := ScreenX;
  dY := vp[3]-ScreenY;
  dZ1 := 0.0;    // near-plane
	gluUnProject(dX,dY,dZ1, @mv,@pr,@vp, X1,Y1,Z1);

  dZ2 := 1.0;    // far-plane
	gluUnProject(dX,dY,dZ2, @mv,@pr,@vp, X2,Y2,Z2);

  // de richting van de "ray" door muispos. en near- & far-plane
  X := X2-X1;
  Y := Y2-Y1;
  Z := Z2-Z1;
end;




procedure TOGL.PushMatrices;
begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
end;

procedure TOGL.PopMatrices;
begin
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;
end;





//--- FullScreen ---------------------------------------------------------------
procedure TOGL.SetupFullScreen(State: boolean);
const ENUM_CURRENT_SETTINGS  = $FFFFFFFF; //-1;
      ENUM_REGISTRY_SETTINGS = $FFFFFFFE; //-2;
var DM: TDeviceMode;
begin
  if not State then begin
    //volledig scherm uitschakelen
    ChangeDisplaySettings(TDeviceMode(nil^), 0);
  end else begin
    //volledig scherm inschakelen
    ZeroMemory(@DM, sizeof(DM));
    DM.dmSize := sizeof(DM);
    if not EnumDisplaySettings(nil, ENUM_CURRENT_SETTINGS, DM) then begin
      //ShowMessage('Volledig scherm modus is niet mogelijk');
      Exit;
    end;
    DM.dmPelsWidth := Screen.Width;
    DM.dmPelsHeight := Screen.Height;
    DM.dmFields := (DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT or DM_DISPLAYFREQUENCY);
    if ChangeDisplaySettings(DM, CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL then begin
      //ShowMessage('Volledig scherm modus is niet mogelijk');
      Exit;
    end;
  end;
end;

procedure TOGL.ToggleFullScreen;
begin
  FullScreen := not FullScreen;
end;




//--- Fog ----------------------------------------------------------------------
procedure TOGL.SetupFog(State: boolean);
begin
  if not Active then Exit;
  if State then glEnable(GL_FOG)
           else glDisable(GL_FOG);
end;

procedure TOGL.SetupFog(Mode: Cardinal; Density, StartDepth,EndDepth: Single; const FogColor: TGLArrayf4; Hint: Cardinal);
begin
  if not Active then Exit;
  glHint(GL_FOG_HINT, Hint);         //GL_NICEST, GL_FASTEST
  glFogf(GL_FOG_DENSITY, Density);
  glFogf(GL_FOG_START, StartDepth);
  glFogf(GL_FOG_END, EndDepth);
  glFogfv(GL_FOG_COLOR, @FogColor);
  glFog(GL_FOG_MODE, Mode);
  // extension
//  glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FOG_COORDINATE_EXT);
end;

procedure TOGL.SetupFog(Density, StartDepth, EndDepth: Single; const FogColor: TGLArrayf4);
begin
  if not Active then Exit;
  glFogf(GL_FOG_DENSITY, Density);
  glFogf(GL_FOG_START, StartDepth);
  glFogf(GL_FOG_END, EndDepth);
  glFogfv(GL_FOG_COLOR, @FogColor);
  // extension
//  glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FOG_COORDINATE_EXT);
end;

procedure TOGL.ToggleFog;
begin
  Fog := not Fog;
end;





//--- Fonts --------------------------------------------------------------------
procedure TOGL.PrintXY(FontIndex, X,Y: integer; const Value: string; R,G,B: Single);
begin
  if (not Active) or Paused then Exit;
  if (FontIndex<0) or (FontIndex>Fonts.FontCount-1) then Exit;
  if not Fonts.Font[FontIndex].IsFontLoaded then Exit;
  glColor3f(R,G,B);
  Fonts.Font[FontIndex].PrintXY(X,Y, Value, local_Width,local_Height);
  SetupProjection; //projectie herstellen met de laatst gebruikte lokale instellingen..
end;

procedure TOGL.PrintXY(X, Y: integer; const Value: string; R,G,B: Single);
begin
  PrintXY(0, X,Y, Value, R,G,B);
end;


procedure TOGL.PrintLine(FontIndex, Row: integer; const Value: string; VAlign: TLineVAlignment; R,G,B: Single);
begin
  if (not Active) or Paused then Exit;
  if (FontIndex<0) or (FontIndex>Fonts.FontCount-1) then Exit;
  if not Fonts.Font[FontIndex].IsFontLoaded then Exit;
  glColor3f(R,G,B);
  Fonts.Font[FontIndex].PrintLine(Row, Value, Valign, local_Width,local_Height);
  SetupProjection; //projectie herstellen met de laatst gebruikte lokale instellingen..
end;

procedure TOGL.PrintLine(Row: integer; const Value: string; VAlign: TLineVAlignment; R, G, B: Single);
begin
  PrintLine(0, Row, Value, VAlign, R,G,B);
end;





//--- 2D Vlakken tekenen -------------------------------------------------------
procedure TOGL.AlphaRectangle2D(Rectangle: TRect; R,G,B,A: Single);
begin
  AlphaRectangle2D(Rectangle, R,G,B,A, GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
end;

procedure TOGL.AlphaRectangle2D(Rectangle: TRect; R, G, B, A: Single;  BlendSRC, BlendDST: Cardinal);
var rec: TRect;
begin
  rec := Rectangle;
  with Rectangle do begin
    if Right<Left then begin rec.Right:=Left; rec.Left:=Right; end;
    if Bottom<Top then begin rec.Top:=Bottom; rec.Bottom:=Top; end;
  end;

  PushMatrices;
  if not IsSetupFor2D then SetupFor2D;

  glFrontFace(GL_CW);
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);
  glPolygonMode(GL_FRONT, GL_FILL);
  glDisable(GL_DEPTH_TEST);

  glDisable(GL_TEXTURE_2D);

  glEnable(GL_BLEND);
  glBlendFunc(BlendSRC, BlendDST);

  glBegin(GL_QUADS);
    glColor4f(R, G, B, A);
    with rec{Rectangle} do begin
      glVertex2i(Left,  local_Projection.Height - Bottom);
      glVertex2i(Left,  local_Projection.Height - Top);
      glVertex2i(Right, local_Projection.Height - Top);
      glVertex2i(Right, local_Projection.Height - Bottom);
    end;
  glEnd;
  glDisable(GL_BLEND);

  PopMatrices;

end;


procedure TOGL.TexturedRectangle2D(Rectangle: TRect; R,G,B,A: Single; TextureHandle: GLuint; AngleCCW: TRightAngle);
begin
  PushMatrices;
  if not IsSetupFor2D then SetupFor2D;

  glActiveTextureARB(GL_TEXTURE0_ARB);

  glFrontFace(GL_CW);
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);
  glPolygonMode(GL_FRONT, GL_FILL);
  glDisable(GL_DEPTH_TEST);
  // alpha waarde
  if A = 1 then
    glDisable(GL_BLEND)
  else begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_DST_ALPHA);
  end;
  // de texture
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, TextureHandle);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE); // de standaard vertex kleur vervangen..
  //
  glBegin(GL_QUADS);
    glColor4f(R, G, B, A);
    with Rectangle do begin
     {glTexCoord2f(0,0);  //90° CCW volgorde: (0,0),(1,0),(1,1),(0,1)}
      case AngleCCW of
        Angle0:   glTexCoord2f(0,0);
        Angle90:  glTexCoord2f(1,0);
        Angle180: glTexCoord2f(1,1);
        Angle270: glTexCoord2f(0,1);
      end;
      glVertex2i(Left,  local_Projection.Height - Bottom);
      {glTexCoord2f(0,1);  //90° CCW volgorde: (0,1),(0,0),(1,0),(1,1)}
      case AngleCCW of
        Angle0:   glTexCoord2f(0,1);
        Angle90:  glTexCoord2f(0,0);
        Angle180: glTexCoord2f(1,0);
        Angle270: glTexCoord2f(1,1);
      end;
      glVertex2i(Left,  local_Projection.Height - Top);
      {glTexCoord2f(1,1);  //90° CCW volgorde: (1,1),(0,1),(0,0),(1,0)}
      case AngleCCW of
        Angle0:   glTexCoord2f(1,1);
        Angle90:  glTexCoord2f(0,1);
        Angle180: glTexCoord2f(0,0);
        Angle270: glTexCoord2f(1,0);
      end;
      glVertex2i(Right, local_Projection.Height - Top);
      {glTexCoord2f(1,0);  //90° CCW volgorde: (1,0),(1,1),(0,1),(0,0)}
      case AngleCCW of
        Angle0:   glTexCoord2f(1,0);
        Angle90:  glTexCoord2f(1,1);
        Angle180: glTexCoord2f(0,1);
        Angle270: glTexCoord2f(0,0);
      end;
      glVertex2i(Right, local_Projection.Height - Bottom);
    end;
  glEnd;
  glDisable(GL_BLEND);

  PopMatrices;
end;


procedure TOGL.TexturedRectangle2D(Rectangle: TRect; R,G,B,A: Single; TextureHandle,MaskHandle: GLuint; AngleCCW: TRightAngle);
begin
  PushMatrices;
  if not IsSetupFor2D then SetupFor2D;

  glActiveTextureARB(GL_TEXTURE0_ARB);

  glFrontFace(GL_CW);
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);
  glPolygonMode(GL_FRONT, GL_FILL);
  glDisable(GL_DEPTH_TEST);
  // alpha waarde
  if A = 1 then
    glDisable(GL_BLEND)
  else begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_DST_ALPHA);
  end;

//  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, MaskHandle);
  glBlendFunc(GL_DST_COLOR,GL_ZERO);
  glBegin(GL_QUADS);
//    glColor4f(R, G, B, A);
    with Rectangle do begin
     {glTexCoord2f(0,0);  //90° CCW volgorde: (0,0),(1,0),(1,1),(0,1)}
      case AngleCCW of
        Angle0:   glTexCoord2f(0,0);
        Angle90:  glTexCoord2f(1,0);
        Angle180: glTexCoord2f(1,1);
        Angle270: glTexCoord2f(0,1);
      end;
      glVertex2i(Left,  local_Projection.Height - Bottom);
      {glTexCoord2f(0,1);  //90° CCW volgorde: (0,1),(0,0),(1,0),(1,1)}
      case AngleCCW of
        Angle0:   glTexCoord2f(0,1);
        Angle90:  glTexCoord2f(0,0);
        Angle180: glTexCoord2f(1,0);
        Angle270: glTexCoord2f(1,1);
      end;
      glVertex2i(Left,  local_Projection.Height - Top);
      {glTexCoord2f(1,1);  //90° CCW volgorde: (1,1),(0,1),(0,0),(1,0)}
      case AngleCCW of
        Angle0:   glTexCoord2f(1,1);
        Angle90:  glTexCoord2f(0,1);
        Angle180: glTexCoord2f(0,0);
        Angle270: glTexCoord2f(1,0);
      end;
      glVertex2i(Right, local_Projection.Height - Top);
      {glTexCoord2f(1,0);  //90° CCW volgorde: (1,0),(1,1),(0,1),(0,0)}
      case AngleCCW of
        Angle0:   glTexCoord2f(1,0);
        Angle90:  glTexCoord2f(1,1);
        Angle180: glTexCoord2f(0,1);
        Angle270: glTexCoord2f(0,0);
      end;
      glVertex2i(Right, local_Projection.Height - Bottom);
    end;
  glEnd;

//  glEnable(GL_DEPTH_TEST);
  glBlendFunc(GL_ONE,GL_ONE);
  glBindTexture(GL_TEXTURE_2D, TextureHandle);
  glBegin(GL_QUADS);
//    glColor4f(R, G, B, A);
    with Rectangle do begin
     {glTexCoord2f(0,0);  //90° CCW volgorde: (0,0),(1,0),(1,1),(0,1)}
      case AngleCCW of
        Angle0:   glTexCoord2f(0,0);
        Angle90:  glTexCoord2f(1,0);
        Angle180: glTexCoord2f(1,1);
        Angle270: glTexCoord2f(0,1);
      end;
      glVertex2i(Left,  local_Projection.Height - Bottom);
      {glTexCoord2f(0,1);  //90° CCW volgorde: (0,1),(0,0),(1,0),(1,1)}
      case AngleCCW of
        Angle0:   glTexCoord2f(0,1);
        Angle90:  glTexCoord2f(0,0);
        Angle180: glTexCoord2f(1,0);
        Angle270: glTexCoord2f(1,1);
      end;
      glVertex2i(Left,  local_Projection.Height - Top);
      {glTexCoord2f(1,1);  //90° CCW volgorde: (1,1),(0,1),(0,0),(1,0)}
      case AngleCCW of
        Angle0:   glTexCoord2f(1,1);
        Angle90:  glTexCoord2f(0,1);
        Angle180: glTexCoord2f(0,0);
        Angle270: glTexCoord2f(1,0);
      end;
      glVertex2i(Right, local_Projection.Height - Top);
      {glTexCoord2f(1,0);  //90° CCW volgorde: (1,0),(1,1),(0,1),(0,0)}
      case AngleCCW of
        Angle0:   glTexCoord2f(1,0);
        Angle90:  glTexCoord2f(1,1);
        Angle180: glTexCoord2f(0,1);
        Angle270: glTexCoord2f(0,0);
      end;
      glVertex2i(Right, local_Projection.Height - Bottom);
    end;
  glEnd;

(*
glEnable(GL_BLEND);
  // de texture
  glActiveTextureARB(GL_TEXTURE0);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, MaskHandle);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);
  glTexEnvf(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_REPLACE);
  // masker
  glActiveTextureARB(GL_TEXTURE1);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, TextureHandle);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);
  glTexEnvf(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_ADD);
  //
  glActiveTextureARB(GL_TEXTURE0_ARB);
  glBegin(GL_QUADS);
//    glColor4f(R, G, B, A);
    with Rectangle do begin
     {glTexCoord2f(0,0);  //90° CCW volgorde: (0,0),(1,0),(1,1),(0,1)}
      case AngleCCW of
        Angle0:   begin
                    glMultiTexCoord2f(GL_TEXTURE0, 0,0);
                    glMultiTexCoord2f(GL_TEXTURE1, 0,0);
                  end;
        Angle90:  begin
                    glMultiTexCoord2f(GL_TEXTURE0, 1,0);
                    glMultiTexCoord2f(GL_TEXTURE1, 1,0);
                  end;
        Angle180: begin
                    glMultiTexCoord2f(GL_TEXTURE0, 1,1);
                    glMultiTexCoord2f(GL_TEXTURE1, 1,1);
                  end;
        Angle270: begin
                    glMultiTexCoord2f(GL_TEXTURE0, 0,1);
                    glMultiTexCoord2f(GL_TEXTURE1, 0,1);
                  end;
      end;
      glVertex2i(Left,  local_Projection.Height - Bottom);
      {glTexCoord2f(0,1);  //90° CCW volgorde: (0,1),(0,0),(1,0),(1,1)}
      case AngleCCW of
        Angle0:   begin
                    glMultiTexCoord2f(GL_TEXTURE0, 0,1);
                    glMultiTexCoord2f(GL_TEXTURE1, 0,1);
                  end;
        Angle90:  begin
                    glMultiTexCoord2f(GL_TEXTURE0, 0,0);
                    glMultiTexCoord2f(GL_TEXTURE1, 0,0);
                  end;
        Angle180: begin
                    glMultiTexCoord2f(GL_TEXTURE0, 1,0);
                    glMultiTexCoord2f(GL_TEXTURE1, 1,0);
                  end;
        Angle270: begin
                    glMultiTexCoord2f(GL_TEXTURE0, 1,1);
                    glMultiTexCoord2f(GL_TEXTURE1, 1,1);
                  end;
      end;
      glVertex2i(Left,  local_Projection.Height - Top);
      {glTexCoord2f(1,1);  //90° CCW volgorde: (1,1),(0,1),(0,0),(1,0)}
      case AngleCCW of
        Angle0:   begin
                    glMultiTexCoord2f(GL_TEXTURE0, 1,1);
                    glMultiTexCoord2f(GL_TEXTURE1, 1,1);
                  end;
        Angle90:  begin
                    glMultiTexCoord2f(GL_TEXTURE0, 0,1);
                    glMultiTexCoord2f(GL_TEXTURE1, 0,1);
                  end;
        Angle180: begin
                    glMultiTexCoord2f(GL_TEXTURE0, 0,0);
                    glMultiTexCoord2f(GL_TEXTURE1, 0,0);
                  end;
        Angle270: begin
                    glMultiTexCoord2f(GL_TEXTURE0, 1,0);
                    glMultiTexCoord2f(GL_TEXTURE1, 1,0);
                  end;
      end;
      glVertex2i(Right, local_Projection.Height - Top);
      {glTexCoord2f(1,0);  //90° CCW volgorde: (1,0),(1,1),(0,1),(0,0)}
      case AngleCCW of
        Angle0:   begin
                    glMultiTexCoord2f(GL_TEXTURE0, 1,0);
                    glMultiTexCoord2f(GL_TEXTURE1, 1,0);
                  end;
        Angle90:  begin
                    glMultiTexCoord2f(GL_TEXTURE0, 1,1);
                    glMultiTexCoord2f(GL_TEXTURE1, 1,1);
                  end;
        Angle180: begin
                    glMultiTexCoord2f(GL_TEXTURE0, 0,1);
                    glMultiTexCoord2f(GL_TEXTURE1, 0,1);
                  end;
        Angle270: begin
                    glMultiTexCoord2f(GL_TEXTURE0, 0,0);
                    glMultiTexCoord2f(GL_TEXTURE1, 0,0);
                  end;
      end;
      glVertex2i(Right, local_Projection.Height - Bottom);
    end;
  glEnd;

  glActiveTextureARB(GL_TEXTURE1_ARB);
  glDisable(GL_TEXTURE_2D);
  glActiveTextureARB(GL_TEXTURE0_ARB);
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glDisable(GL_DEPTH_TEST);
*)
  PopMatrices;
end;


procedure TOGL.Point2D(ScrPx_X, ScrPx_Y: integer; R,G,B,A: Single; PointSize: single);
var noAlpha: boolean;
begin
  PushMatrices;
  if not IsSetupFor2D then SetupFor2D;

  glActiveTextureARB(GL_TEXTURE0_ARB);

  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  // alpha waarde
  noAlpha := (A = 1);
  if noAlpha then
    glDisable(GL_BLEND)
  else begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
//    glBlendFunc(GL_ZERO, GL_ONE);
  end;
  //
  glPointSize(PointSize);
  glBegin(GL_POINTS);
    glColor4f(R, G, B, A);
    glVertex2i(ScrPx_X,  local_Projection.Height - ScrPx_Y);
  glEnd;

  PopMatrices;
end;

procedure TOGL.Line2D(ScrP1, ScrP2: TPoint; R, G, B, A, LineWidth: single);
var noAlpha: boolean;
begin
  PushMatrices;
  if not IsSetupFor2D then SetupFor2D;

  glActiveTextureARB(GL_TEXTURE0_ARB);

  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  // alpha waarde
  noAlpha := (A = 1);
  if noAlpha then
    glDisable(GL_BLEND)
  else begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
//    glBlendFunc(GL_ZERO, GL_ONE);
  end;
  //
  glLineWidth(LineWidth);
  glBegin(GL_LINES);
    glColor4f(R, G, B, A);
    glVertex2i(ScrP1.X,  local_Projection.Height - ScrP1.Y);
    glVertex2i(ScrP2.X,  local_Projection.Height - ScrP2.Y);
  glEnd;

  PopMatrices;
end;







(*
initialization
  OGL := TOGL.Create;

finalization
  //OGL.Disable;
  OGL.Free;
*)

end.
