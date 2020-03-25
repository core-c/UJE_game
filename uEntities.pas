unit uEntities;
interface
uses Graphics, SysUtils, ExtCtrls, u3DTypes, uCamera, Classes, OpenGL;

const
  //-- classtypes
  cltEntityNone                     = $00000000;      // een dummy entity-class
  cltEntityRoot                     = cltEntityNone;  // een dummy entity-class tbv de knoppen die altijd aanwezig zijn.
  cltEntity                         = $00010000;
  cltEntityTile                     = $00020000;   
  cltEntityButton                   = $00030000;
  cltEntityButtons                  = $00040000;
  cltEntityGame                     = $00050000;
  cltEntityGFX_FPS                  = $00060000;
  cltEntityGFX_Radar                = $00080000;
  cltEntityHUD                      = $00090000;
  cltEntityMap                      = $000A0000;
  cltEntityMover                    = $000B0000;   // een entity welke kan bewegen
  cltEntityOreMiner                 = $000C0000;   // een TEntityMover
    cltEntityOreMinerUpgrade1       = $000C0001;
    cltEntityOreMinerUpgrade2       = $000C0002;
    cltEntityOreMinerUpgrade3       = $000C0003;
    cltEntityOreMinerUpgrade4       = $000C0004;
    cltEntityOreMinerUpgrade5       = $000C0005;
  cltEntityPlayer                   = $000D0000;
  cltEntityQuadTree                 = $000E0000;
  cltEntityResource                 = $000F0000;
  cltEntitySpawnpoint               = $00100000;
    cltEntitySpawnpointUpgrade1     = $00100001;
    cltEntitySpawnpointUpgrade2     = $00100002;
    cltEntitySpawnpointUpgrade3     = $00100003;
  cltEntityTeam                     = $00110000;
  cltEntityTerrain                  = $00110000;
  cltEntityPowerpylon               = $00200000;
  cltEntityPowerline                = $00400000;
  //--

  c_TILESIZE = 50.0;         // het aantal units per tile

  c_NROFRESOURCES = 3;       // het aantal gedefiniëerde resources
  c_MAXRESOURCEPERAREA = 100;// max. hoeveelheid resource-ore per area

  // teams
  c_NOTEAM = -1;             // geen team, van niemand, voor iedereen..
  c_TEAM0 = 0;
  c_TEAM1 = 1;
  c_TEAM2 = 2;
  c_TEAM3 = 3;
  c_TEAM4 = 4;
  c_TEAM5 = 5;
  c_TEAM6 = 6;
  c_TEAM7 = 7;

  c_POWERLINE_HEIGHT = 20.0; // de hoogte van de draden boven de grond
  c_POWERLINE_MAXDISTANCE = 5 * c_TILESIZE;

  // units
  c_UNIT_OREMINER = 1;       // een ore-miner
  c_MAXSPEED_OREMINER = 1.0; // maximale snelheid voor een ore-miner

  // HUD
  c_BUTTONWIDTH = 32;        // pixel-breedte van de knoppen in de HUD
  c_BUTTONHEIGHT = 32;       // pixel-hoogte van de knoppen in de HUD
  c_BUTTONSPACING = 2;       // ruimte tussen de verschillende knoppen
  c_HUDBUTTONWIDTH = 8;      // pixel-breedte van de knoppen in de HUD
  c_HUDBUTTONHEIGHT = 8;     // pixel-hoogte van de knoppen in de HUD
  c_HUDBUTTONSPACING = 1;    // ruimte tussen de verschillende knoppen
  c_HUDMARGIN = 4;           // de marges voor objecten in de HUD
  c_HUDTITLEBARHEIGHT = 16;  // de titelbalk-hoogte van de HUD
  c_HUDWIDTH = 214;          // pixel-breedte van de main-HUD
  c_HUDHEIGHT = 188;         // pixel-hoogte van de main-HUD
  c_RADARWIDTHHEIGHT = 96;   // pixel-breedte van het Radar-beeld.. (breedte=hoogte)
  // align HUD met het scherm
  hudAlignNone         = $00;
  hudAlignLeft         = $01;                             // links (met behoud van HUD-breedte)
  hudAlignRight        = $02;                             // rechts (met behoud van HUD-breedte)
  hudAlignTop          = $04;                             // boven (met behoud van HUD-hoogte)
  hudAlignBottom       = $08;                             // onder (met behoud van HUD-hoogte)
  hudAlignTopLeft      = hudAlignTop + hudAlignLeft;      // linker-boven hoek (met behoud van HUD-breedte & -hoogte)
  hudAlignTopRight     = hudAlignTop + hudAlignRight;
  hudAlignBottomLeft   = hudAlignBottom + hudAlignLeft;
  hudAlignBottomRight  = hudAlignBottom + hudAlignRight;
  hudAlignClientLeft   = hudAlignLeft + hudAlignTop + hudAlignBottom;    // met behoud van HUD-breedte
  hudAlignClientRight  = hudAlignRight + hudAlignTop + hudAlignBottom;   //         "           "
  hudAlignClientTop    = hudAlignTop + hudAlignLeft + hudAlignRight;     // met behoud van HUD-hoogte
  hudAlignClientBottom = hudAlignBottom + hudAlignLeft + hudAlignRight;  //         "           "
  hudAlignClient       = hudAlignTop + hudAlignBottom + hudAlignLeft + hudAlignRight;

type
  // tbv. terrain quadtree
  qtTypeConst = (qtNode,qtLeaf);


type
  {----------------------------------------------}
  PEntity = ^TEntity;
  TEntity = class(TObject)
  private
    PerformanceFreq, LastPerformanceCount: Int64;
  public
    Classname_: string;     // de naam van deze entity class
    ClassType_: cardinal;   // het type van deze entity class
    Camera: PCamera;        // Een pointer naar de camera

    Name: string;           // de naam van deze entity instantie
    Team: integer;          // het team van deze entity
    Color: TColor;          // de in-game kleur voor deze entity
    ColorR,ColorG,ColorB,ColorA: single; // dezelfde kleur in een ander formaat

    Selected: boolean;      // is deze entity geselecteerd??

    Location,               // de huidige positie in de wereld
    Destination,            // de huidige bestemming
    Movement: TVector;      // tbv. interpoleren naar destination
    Pitch, Yaw, Roll: single; // tbv afbeelden

    Health,                 // de gezondheid
    Shield: single;         // verdedigings-schild
    {Speed,                  // de huidige snelheid
    MaxSpeed: single;       // de maximale snelheid voor deze entity}

    DefendRatio,            // verdediging sterkte verhouding.. (1.0 = standaard)
    AttackRatio: single;    // aanval sterkte.. (1.0 = standaard)

    isStatic,               // bv. een berg, barricade, mijn...
    isIdle,                 // heeft deze entity nu niks te doen?

    canMove,                // kan deze entity verplaatsen?
    {isMoving,              // is deze entity nu aan het bewegen?
    hasAI_move,             // heeft deze entity AI om te bewegen?
    useAI_move,             // .. is de AI ingeschakeld?}

    canCollect,             // kan deze entity resources verzamelen?
    isCollecting,           // is deze entity nu aan het verzamelen?
    hasAI_collect,          // is er AI om resources te verzamelen?
    useAI_collect,          // .. is de AI ingeschakeld?
    canStore,               // kan deze entity resources opslaan?
    isStoring,              // is deze entity nu aan het opslaan?
    hasAI_store,            // is er AI om resources op te slaan?
    useAI_store,            // .. is de AI ingeschakeld?
    canDistribute,          // kan deze entity resources verspreiden/verdelen onder (bepaalde) andere entities?
    isDistributing,         // is deze entity nu aan het verdelen?
    hasAI_distribute,       // is er AI om resources te verspreiden?
    useAI_distribute,       // .. is de AI ingeschakeld?

    canDefend,              // verdedigings-mogelijkheden?
    isDefending,            // is deze entity nu aan het verdedigen?
    hasAI_defend,           // is er AI om te verdedigen?
    useAI_defend,           // .. is de AI ingeschakeld?
    canAttack,              // aanvals-mogelijkheden?
    isAttacking,            // is deze entity nu aan het aanvallen?
    hasAI_attack,           // is er AI om aan te vallen?
    useAI_attack: boolean;  // .. is de AI ingeschakeld?

    constructor Create;
    destructor Destroy; override;

  end;


  {----------------------------------------------}
  TEntityResource = class(TEntity)
  private
  public
    Image: TImage;          // een afbeelding van deze resource
    Icon: TIcon;            // het pictogram voor deze resource
    Price: single;          // de prijs van deze resource
    constructor Create(const aName: string;
                       const aColor:Tcolor;
                       const aImageFilename: string;
                       const aIconFilename: string;
                       const aPrice: single);
    destructor Destroy; override;
  end;


  {----------------------------------------------}
  TEntityPlayer = class(TEntity)
  private
  public
    Score: integer;
    Credits: single;
    constructor Create(const aNickname: string;
                       const aColor:Tcolor);
(*
    destructor Destroy;
*)
  end;


  {----------------------------------------------}
  TEntityTeam = class(TEntity)
  private
  public
    Players: array of TEntityPlayer;
    constructor Create(const aTeamname: string;
                       const aColor:Tcolor);
    destructor Destroy; override;
    procedure AddPlayer(const aNickname: string;
                        const aColor:Tcolor);
    procedure DeletePlayer(const aNickname: string); overload;
    procedure DeletePlayer(const aIndex: integer); overload;
  end;


  {----------------------------------------------}
  PEntityMap = ^TEntityMap;
  PEntityTerrain = ^TEntityTerrain;
  PEntityTile = ^TEntityTile;


  {----------------------------------------------}
  // units
  PEntityMover = ^TEntityMover;
  TEntityMover = class(TEntity)
  private
    ParentMap: PEntityMap;
    NextLocation, NextDestination: TVector;
    localCamera: TCamera;   // nodig om een instantie van TCamera te maken.
  public
    Speed,                  // de huidige snelheid
    MaxSpeed: single;       // de maximale snelheid voor deze entity
    isMoving,               // is deze entity nu aan het bewegen?
    hasAI_move,             // heeft deze entity AI om te bewegen?
    useAI_move: boolean;    // .. is de AI ingeschakeld?
    //
    constructor Create;
    destructor Destroy; override;
    //
    procedure CalcNextRoute; virtual;  //override in descendant class
    procedure AI_move;
  end;


  {----------------------------------------------}
  PEntityOreMiner = ^TEntityOreMiner;
  TEntityOreMiner = class(TEntityMover) //TEntity
  private
    ResourcesLoaded: array[0..c_NROFRESOURCES-1] of integer; // hoeveelheid lading aan boord
    canLoadResource: array[0..c_NROFRESOURCES-1] of boolean; // de mogelijke ladingen
    fInstalledUpgrades: integer;
  published
    property InstalledUpgrades: integer read fInstalledUpgrades;
  public
    isReturning: boolean;
    //
    constructor Create(aParentMap: PEntityMap;
                       const aTeam: integer;
                       const aResourcesBitMask: cardinal;  // mogelijke resources te laden (bit0 = resouce[0], etc...)
                       const aMaxLoad: integer;        // de maximale lading voor een/elk resource
                       const aMaxSpeed: single;
                       const aLocation: TVector);
    destructor Destroy; override;
    //
    procedure CalcNextRoute; override;
    {procedure AI_move;}
    procedure AI_collect;
    procedure AI_store;
    procedure AI_distribute;
    //
    procedure Upgrade;
  end;


  {----------------------------------------------}
  PEntityPowerline = ^TEntityPowerline;
  TEntityPowerline = class(TEntity)
  private
    ParentMap: PEntityMap;
  public
    TileX, TileY: integer;                            // index in TEntityMap.TEntityTerrain.TEntityTile-array
    ConnectedTo1,                                     // a powerline is connected to these entities,
    ConnectedTo2: PEntity;                            // it can be a powerpylon or spawnpoint.
    Height1, Height2: single;                         // de hoogte van de powerline op connectedTo1 & 2
    constructor Create(aParentMap: PEntityMap;
                       const aName: string;
                       const aTeam: integer;          // het team van deze entity
                       const aTileX, aTileY: integer);
    procedure RenderConstructing(const FromPosition, ToPosition: TVector);
    procedure Render;
  end;


  {----------------------------------------------}
  PEntityPowerpylon = ^TEntityPowerpylon;
  TEntityPowerpylon = class(TEntity)
  private
    ParentMap: PEntityMap;
  public
    TileX, TileY: integer;                            // index in TEntityMap.TEntityTerrain.TEntityTile-array
    ConnectedTo1,                                     // a powerline can be connected to these entities,..
    ConnectedTo2: PEntity;                            // it can be a powerpylon or spawnpoint.
    constructor Create(aParentMap: PEntityMap;
                       const aName: string;
                       const aTeam: integer;          // het team van deze entity
                       const aTileX, aTileY: integer);
  end;


  {----------------------------------------------}
  PEntitySpawnPoint = ^TEntitySpawnPoint;
  TEntitySpawnPoint = class(TEntity)
  private
    ParentMap: PEntityMap;
  public
    TileX, TileY: integer;                            // index in TEntityMap.TEntityTerrain.TEntityTile-array
    ConnectedTo: PEntityPowerline;                    // a spawnpoint is connected to this entity
    constructor Create(aParentMap: PEntityMap;
                       const aName: string;
                       const aTeam: integer;          // het team van deze entity
                       const aTileX, aTileY: integer);
  end;


  {----------------------------------------------}
  TEntityTile = class(TEntity)
  private
    ParentTerrain: PEntityTerrain;
  public
    TileX, TileY: integer;                 // index in TEntityMap.TEntityTerrain.TEntityTile-array
    MinX,MaxX, MinY,MaxY, CenterX,CenterY: single;     // wereld-coordinaten
    ResourcesAmount: array[0..c_NROFRESOURCES-1] of integer; // hoeveelheid erts in deze area
    constructor Create(aParentTerrain: PEntityTerrain;
                       const aTileX, aTileY: integer;
                       const aOwnedByTeam: integer);
    function MinLocationX : single;
    function MaxLocationX : single;
    function MinLocationY : single;
    function MaxLocationY : single;
    procedure SetOwner(const aTeam: integer);
    procedure SetColor(const aColor: TColor);
    procedure SetColorToResource(const ResourceIndex: integer);
    procedure SetColorToResources;
    procedure SetResourceAmount(const ResourceIndex: integer;
                                const Amount: integer);
    procedure AddResourceAmount(const ResourceIndex: integer;
                                const Amount: integer);
  end;


  {----------------------------------------------}
  // quadtree
  PQuadTreeNode = ^TQuadTreeNode;
  TQuadTreeNode = record
    qtType: qtTypeConst;                         // leaf of node
    PosX,PosZ, DeltaX,DeltaZ,
    MinHeight,MaxHeight,DeltaHeight: Single;     // world-coords.
    BoundingBox: TBoundingBox;                   // world-coords
    Parent: PQuadTreeNode;
    Children: array[0..3] of PQuadTreeNode;
    TrisW,TrisWCount, TrisZ,TrisZCount: integer; // tbv tekenen mbv glDrawElements
  end;
  {----------------------------------------------}
  TEntityQuadTree = class(TEntity)
  private
    SubdivideLimit: integer;
  public
    ParentTerrain: PEntityTerrain;
    Nodes: array of TQuadTreeNode;
    constructor Create(aParentTerrain: PEntityTerrain);
    destructor Destroy; override;
    //
    procedure CreateTree(const aSubDivideLimit: integer);      // recursief opdelen tot de limit is bereikt
                                                               // (waarde 4 geeft cells van 4x4 triangles)
    procedure CalcHeights(var aNode: TQuadTreeNode);           // tbv boundingbox bij afbeelden
    procedure SubDivideNode(var aNode: TQuadTreeNode;
                            const CurrentSubdivide: integer);
    procedure Render(var aNode: TQuadTreeNode);
  end;
  {----------------------------------------------}
  // terrain
  //PEntityTerrain = ^TEntityTerrain;
  TEntityTerrain = class(TEntity)
  private
    IndexArray: array of array of cardinal;
    PlaneNormalArray: array[0..1] of array of array of TVector;      // [0..1,w,h]
    QuadTree: TEntityQuadTree;
  public
    ParentMap: PEntityMap;
    Width, Height: integer;                                    // heightmap breedte/hoogte
    DeltaX, DeltaZ, DeltaHeight: single;                       // map breedte/diepte/hoogte
    PosX, PosZ, PosHeight: single;                             // map(0,0) "origin"
    VertexArray: array of TTerrainVertex;
    Tiles: array of array of TEntityTile;                            // [w,h]
    constructor Create(aParentMap: PEntityMap;
                       const HeightmapBMP: string;             // 8b grayscale-image
                       const minHeight,maxHeight: single);     // world-coords
    destructor Destroy; override;
    // de hoogte (world-coords) bepalen van een positie(X,?,Z)
    function GetHeightAt(const mapX, mapZ: single) : single; overload;
    function GetHeightAt(const Position: TVector) : single; overload;
    //
    procedure Render; overload;
    procedure Render(const TrisW,TrisWCount, TrisZ,TrisZCount: integer); overload;
  end;





  {----------------------------------------------}
//PEntityMap = ^TEntityMap;
  TEntityMap = class(TEntity)
  private
  public
    MaxX, MaxY: single;
    Spawnpoints: array of TEntitySpawnPoint;
    Powerpylons: array of TEntityPowerpylon;
    Powerlines: array of TEntityPowerline;
    Units: array of TEntity;                              // alle units in de map
    Terrain: TEntityTerrain;
    constructor Create(const aName: string);
    destructor Destroy; override;
    // hulp-functies
    procedure LocationToTile(const aLocation: TVector;
                             var vTileX, vTileY: integer);
    function RandomLocation(const MinLocation, MaxLocation: TVector): TVector; overload; // een willekeurige plek op de map tussen Min..Max
    function RandomLocation: TVector; overload; // een willekeurige plek op de map
    function IsOnTerrain(const Position: TVector) : boolean;
    // terrein
    procedure CreateTerrain(const HeightmapBMP: string;
                            const minHeight,maxHeight: single);
    // spawnpoints
    procedure AddSpawnpoint(const aName: string;
                            const aTeam: integer;          // het team van deze entity
                            const aTileX, aTileY: integer);    // de area in de wereld
    procedure DeleteSpawnpoint(const aName: string); overload;
    procedure DeleteSpawnpoint(const aIndex: integer); overload;
    // units
    procedure AddUnit(const aUnitType: integer;      // het type unit
                      const aTeam: integer;          // het team van deze entity
                      const aLocation: TVector);     // de huidige positie in de wereld
    // powerpylons
    procedure AddPowerpylon(const aName: string;
                            const aTeam: integer;          // het team van deze entity
                            const aTileX, aTileY: integer);    // de area in de wereld
    procedure DeletePowerpylon(const aName: string); overload;
    procedure DeletePowerpylon(const aIndex: integer); overload;
    // powerlines
    procedure AddPowerline(const aName: string;
                           const aTeam: integer;          // het team van deze entity
                           const aTileX, aTileY: integer;     // de area in de wereld
                           const aConnection1, aConnection2: PEntity); // verbonden aan deze entities (powerpylon, powerline of spawnpoint)
    procedure DeletePowerline(const aName: string); overload;
    procedure DeletePowerline(const aIndex: integer); overload;
  end;


  {----------------------------------------------}
  PEntityHUD = ^TEntityHUD;


  {----------------------------------------------}
  // een knop om op te drukken.
  TEntityButton = class(TEntity)
  private
    fOnUp: TNotifyEvent;
    fOnDown: TNotifyEvent;
    fVisible: boolean;              // deze knop zichtbaar??
    fEnabled: boolean;              // deze knop ingeschakeld??
    fBlinking: boolean;             // is de knop aan het knipperen??
    fBlinkingOn: boolean;           // is de (knipperende) knop nu aan??
    fToggle: boolean;               // is dit een toggle-knop??
    fToggledOn: boolean;            // is de knop nu ingedrukt??
    fOnce: boolean;                 // kan deze knop maar 1 x worden gebruikt??
    fPercentage: integer;           // percentage klaar (met bouwen)  bereik[0..100]
    fDone: boolean;                 // deze knop klaar met maken?? niks meer te doen..
    fReady: integer;                // aantal constructies klaar?? klaar om constructie te plaatsen..
    fQueueCount: integer;           // het aantal extra te maken entities (0=geen queue voor deze knop)
    fQueued: integer;               // het aantal extra te maken entities in de queue
    procedure SetVisible(const Value: boolean);
    procedure SetEnabled(const Value: boolean);
    procedure SetPercentage(const Value: integer);
    procedure SetDone(const Value: boolean);
    procedure SetReady(const Value: integer);
    procedure SetToggledOn(const Value: boolean);
    procedure SetBlinking(const Value: boolean);
    procedure SetBlinkingOn(const Value: boolean);
    procedure SetQueueCount(const Value: integer);
    procedure SetQueued(const Value: integer);
  published
    property Visible: boolean          read fVisible           write SetVisible;
    property Enabled: boolean          read fEnabled           write SetEnabled;
    property Toggle: boolean           read fToggle;
    property ToggledOn: boolean        read fToggledOn         write SetToggledOn;
    property Once: boolean             read fOnce;
    property Blinking: boolean         read fBlinking          write SetBlinking;
    property BlinkingOn: boolean       read fBlinkingOn        write SetBlinkingOn;
    property Percentage: integer       read fPercentage        write SetPercentage;
    property Done: boolean             read fDone              write SetDone;
    property Ready: integer            read fReady             write SetReady;
    property QueueCount: integer       read fQueueCount        write SetQueueCount;
    property Queued: integer           read fQueued            write SetQueued;
    property OnUp: TNotifyEvent        read fOnUp              write fOnUp;
    property OnDown: TNotifyEvent      read fOnDown            write fOnDown;
  public
    ParentHUD: TEntity; //PEntityHUD;
    TechIndex: integer; // de index in de TechTree voor de technologie onder deze knop
    TextureHandle: GLuint;
    Width, Height: integer;
    Left,Right,Top,Bottom: integer; // client-coords op de HUD
    //
    constructor Create(aParentHUD: TEntity;     // TEntityHUD
                       const aCaption: string;  // tekst op de knop
                       const X,Y,W,H: integer;  // positie op de HUD
                       const r,g,b,a: single;   // de basis kleur
                       const aImage: string);   // plaatje
    destructor Destroy; override;
    //
    function OverButton(const X, Y: integer): boolean;
    procedure DoClick;
    //
    function pxLeft : integer;
    function pxRight : integer;
    function pxTop : integer;
    function pxBottom : integer;
  end;



  {----------------------------------------------}
  PEntityButtons = ^TEntityButtons;
  TEntityButtons = class(TEntity)
  private
    fTotalButtons: integer;
    fThisIndex: integer;
  published
    property TotalButtons: integer  read fTotalButtons;
    property ThisIndex: integer     read fThisIndex;
  public
    ParentHUD: TEntity; //PEntityHUD;
    AssignedClassType: cardinal; // de knoppen zijn voor dit soort entity
    AssignedEntity: PEntity;
    Buttons: array of TEntityButton;
    //
    constructor Create(aParentHUD: TEntity;                 // TEntityHUD
                       const aButtonSetIndex: integer;
                       const aName: string;                 // naam voor de knoppen-verzameling
                       const AssignedClassType_: cardinal); // de knoppen zijn voor dit soort entity
    destructor Destroy; override;
    // knoppen toevoegen/verwijderen..
    procedure AddButton(const aCaption: string;
                        const X,Y,W,H: integer;
                        const r,g,b,a: single;
                        const aImage: string); overload;
    procedure AddButton(const aCaption: string;
                        const r,g,b,a: single;
                        const aImage: string); overload;
    procedure AddButton(const aCaption: string;
                        const aImage: string); overload;
    procedure RemoveButton(const aIndex: integer); overload;
    procedure RemoveButton(const aCaption: string); overload;
    procedure RemoveButton(const X,Y: integer); overload;
  end;


  {----------------------------------------------}
  // een FPS-grafiek
  TEntityGFX_FPS = class(TEntity)
  private
    fVisible: boolean;
    procedure SetVisible(const Value: boolean); //tbv property
  published
    property Visible: boolean     read fVisible write SetVisible;
  public
    ParentHUD: TEntity; //PEntityHUD;
    Width, Height: integer;
    Left,Right,Top,Bottom: integer; // client-coords op de HUD
    //tbv. FPS metingen
    NFPS, FPSIndex, NRecorded: integer;
    aFPS: array of integer; // een array met FPS-waarden
    //
    constructor Create(aParentHUD: TEntity;     // TEntityHUD
                       const aCaption: string;
                       const X,Y,W,H: integer;
                       const r,g,b,a: single);
    destructor Destroy; override;
    //
    function OverGFX(const X, Y: integer): boolean;
    //
    function pxLeft : integer;
    function pxRight : integer;
    function pxTop : integer;
    function pxBottom : integer;
    //
    procedure StoreCurrentFPS;
  end;


  {----------------------------------------------}
  // een FPS-grafiek
  TEntityGFX_Radar = class(TEntity)
  private
    fVisible: boolean;
    procedure SetVisible(const Value: boolean); //tbv property
  published
    property Visible: boolean     read fVisible write SetVisible;
  public
    ParentHUD: TEntity; //PEntityHUD;
    Width, Height: integer;
    Left,Right,Top,Bottom: integer; // client-coords op de HUD
    //
    constructor Create(aParentHUD: TEntity;     // TEntityHUD
                       const aCaption: string;
                       const X,Y,W,H: integer;
                       const r,g,b,a: single);
    destructor Destroy; override;
    //
    function OverGFX(const X, Y: integer): boolean;
    //
    function pxLeft : integer;
    function pxRight : integer;
    function pxTop : integer;
    function pxBottom : integer;
  end;



  {----------------------------------------------}
  // een soort van Panel, tbv. knoppen en aanduidingen.
  TEntityHUD = class(TEntity)
  private
    TextureHandle: GLuint;
    Alignment: Byte;
    fThisIndex: integer;  // de HUDIndex
    fHUDButtonsSwappedX: boolean;
    fTotalSysButtons: integer;
    fVisible: boolean;
    procedure SetVisible(const Value: boolean);
  published
    property Visible: boolean             read fVisible             write SetVisible;
    property TotalSysButtons: integer     read fTotalSysButtons;
    property HUDButtonsSwappedX: boolean  read fHUDButtonsSwappedX;
    property ThisIndex: integer           read fThisIndex;
  public
    Minimizing,Maximizing, Minimized,Maximized: boolean;
    OffsetX,OffsetY: integer;            // offset tbv. minimize/maximize
    DeltaOffsetX,DeltaOffsetY: integer;  // delta-offset tbv. minimize/maximize
    EndOffsetX,EndOffsetY: integer;      // offset tbv. minimize/maximize
    //
    pxLeft,pxRight,pxTop,pxBottom: integer; // ware coords op de viewport (na alignment)
    Width, Height: integer;
    Left,Right,Top,Bottom: integer;      // coords op de viewport
    SysButtons: array of TEntityButton;  // de vaste HUD-(Sys)buttons
    ButtonSets: array of TEntityButtons; // verzamelingen van knoppen (per entity)
    ButtonsPtr: PEntityButtons;          // een pointer naar de huidige knoppen-verzameling (indien entity is geselecteerd)
    GFX_FPS: TEntityGFX_FPS;             // een evt. FPS-grafiekje
    GFX_Radar: TEntityGFX_Radar;         // een radar
    //
    constructor Create(const aHUDIndex: integer; //fThisHUDIndex
                       const aName: string;
                       const aAlignment: Byte;   //bitmask
                       const X,Y,W,H: integer;
                       const r,g,b,a: single;
                       const aImage: string);
    destructor Destroy; override;
    // hulpfuncties
    procedure ReAlign(const NewWidth, NewHeight: integer);  //tbv uitlijnen na een form-resize
    function OverHUD(const X,Y: integer) : boolean;
    function OverSysButton(const X,Y: integer) : integer; // -1 = niet over een knop in deze HUD, anders de index in SysButtons
    function OverButton(const X,Y: integer) : integer; // -1 = niet over een knop in deze HUD, anders de index in Buttons
    function OverFixedButton(const X,Y: integer) : integer; // -1 = niet over een knop in deze HUD, anders de index in ButtonSets[0].Buttons
    function ButtonByCaption(const aCaption: string) : TEntityButton;
    procedure HandleMiniMaxmize;
    procedure Minimize(Sender: TObject);
    procedure Maximize(Sender: TObject);
    // knoppen..
    procedure AddSysButton(const aCaption: string;
                           const X,Y,W,H: integer;
                           const r,g,b,a: single;
                           const aImage: string); overload;
    procedure CreateButtonSet(const aName: string;                 // naam voor de knoppen-verzameling
                              const AssignedClassType_: cardinal); // de knoppen zijn voor dit soort entity
    // indicators..
    procedure AddGFXFPS(const aCaption: string;
                        const X,Y,W,H: integer;
                        const r,g,b,a: single;
                        const NrOfFrames: integer); overload;
    procedure AddGFXFPS(const aCaption: string); overload;         // auto-positioned
    procedure AddGFXRadar(const aCaption: string;
                          const X,Y,W,H: integer;
                          const r,g,b,a: single); overload;
    procedure AddGFXRadar(const aCaption: string); overload;       // auto-positioned
    // knoppen aan-/uit-schakelen voor een opgegeven entity.
    // Niet alle knoppen zijn nl. al enabled in het begin (voor een entity)
    procedure AssignEntity(Entity: PEntity);
  end;


  {----------------------------------------------}
  PEntityGame = ^TEntityGame;
  TEntityGame = class(TEntity)
  private
    Camera: PCamera;                     // Een pointer naar de camera
  public
    ShowSelectionMarks,                  // een selectie-markering tekenen??
    ShowBoundingBoxes,                   // boundingboxes tekenen om geselecteerde objecten??
    ShowRoutes,                          // lijnen tekenen naar Destination??
    ShowTiles,                           // tiles in (ore)kleur afbeelden??
    ShowTerrain,                         // terrein afbeelden??
    ShowFog: boolean;                    // mist afbeelden??
    //
    isConstructing: boolean;             // speler bezig met bouwen??               (tijdens gebouw plaatsen)
    ConstructionClassType: cardinal;     // de classtype van de (te bouwen) entity  (tijdens gebouw plaatsen)
    ConstructingEntity: PEntity;         // de entity die iets bouwt
    //
    Teams: array of TEntityTeam;         // alle teams (met spelers)
    Resources: array of TEntityResource; // alle resources bekend in dit spel
    Map: TEntityMap;                     // de map
    HUD: array of TEntityHUD;            // alle HUDs
    constructor Create(const aGamename: string);
    destructor Destroy; override;
    //
    procedure AddResource(const aName: string;
                          const aColor:Tcolor;
                          const aImageFilename: string;
                          const aIconFilename: string;
                          const aPrice: single);
    //
    procedure AddTeam(const aTeamname: string;
                      const aColor:Tcolor);
    procedure DeleteTeam(const aTeamname: string); overload;
    procedure DeleteTeam(const aIndex: integer); overload;
    procedure DeleteTeam(const aColor: Tcolor); overload;
    //
    procedure AddHUD(const aHUDIndex: integer; //fThisHUDIndex
                     const aName: string;
                     const aAlignment: Byte;   //bitmask
                     const X,Y,W,H: integer;
                     const r,g,b,a: single;
                     const aImage: string);
    procedure RealignHUDs(const NewWidth, NewHeight: integer);
    function OverHUD(const X,Y: integer) : integer; // resultaat = -1 indien over geen enkele HUD, anders HUD-index in array
    procedure StoreCurrentFPS;
    //
    procedure CreateMap(const Mapname: string);
    procedure FreeMap;
    procedure SetMap(const aMap: PEntityMap);
    //
    procedure StartConstructing(const aClassType: cardinal);
    procedure StopConstructing;
    procedure ConstructBuilding(const MouseX,MouseY: integer);
    //
    procedure ServerFrame;
  end;

var
  // de instantie van een spel
  Game : TEntityGame;

//------------------------------------------------------------------------------
//--- object-definities & instanties
implementation
uses uConst, uCalc, uOpenGL, uSelection, uGameTech,
     Math, Types, Windows, MMsystem, Unit1;

{-- TEntity -------------------------------------}
constructor TEntity.Create;
begin
  Camera := nil;
  QueryPerformanceFrequency(PerformanceFreq);
  QueryPerformanceCounter(LastPerformanceCount);

  Classname_ := 'clEntity';
  ClassType_ := cltEntity;
  Location := Vector(0,0,0);
  Destination := Vector(0,0,0);
  Pitch := 0.0;
  Yaw := 0.0;
  Roll := 0.0;
  Name := '';
  Team := -1;    // nvt.
  Health := -1;
  Shield := -1;
  {Speed := -1;
  MaxSpeed := -1;}
  DefendRatio := 1.0;
  AttackRatio := 1.0;
  isStatic := true;
  isIdle := true;
  canMove := false;
  {isMoving := false;}
  canCollect := false;
  isCollecting := false;
  canStore := false;
  isStoring := false;
  canDistribute := false;
  isDistributing := false;
  canDefend := false;
  isDefending := false;
  canAttack := false;
  isAttacking := false;
  {hasAI_move := false;
  useAI_move := false;}
  hasAI_collect := false;
  useAI_collect := false;
  hasAI_store := false;
  useAI_store := false;
  hasAI_distribute := false;
  useAI_distribute := false;
  hasAI_defend := false;
  useAI_defend := false;
  hasAI_attack := false;
  useAI_attack := false;
end;

destructor TEntity.Destroy;
begin
  if Camera <> nil then begin
    Camera.Free;
    Camera := nil;
  end;
end;



{--- TResourceEntity ----------------------------}
constructor TEntityResource.Create(const aName: string;
                                   const aColor: Tcolor;
                                   const aImageFilename, aIconFilename: string;
                                   const aPrice: single);
begin
  // TEntity properties
  Classname_ := 'clEntityResource';
  ClassType_ := cltEntityResource;
  Name := aName;
  Color := aColor;
  TColorToRGB(Color, ColorR,ColorG,ColorB);
  // TResourceEntity properties
  Image := TImage.Create(nil);    // een TImage instantiëren
  if FileExists(aImageFilename) then Image.Picture.LoadFromFile(aImageFilename);
  Icon := TIcon.Create;           // een TIcon instantiëren
  if FileExists(aIconFilename) then Icon.LoadFromFile(aIconFilename);
  Price := aPrice;
end;

destructor TEntityResource.Destroy;
begin
  Image.Free;
  Icon.Free;
end;



{--- TEntityPlayer ------------------------------}
constructor TEntityPlayer.Create(const aNickname: string; const aColor: Tcolor);
begin
  // TEntity properties..
  Classname_ := 'clEntityPlayer';
  ClassType_ := cltEntityPlayer;
  Name := aNickname;
  Color := aColor;
  TColorToRGB(Color, ColorR,ColorG,ColorB);
  // TEntityPlayer properties..
  Score := 0;
  Credits := 0.00;
end;


{--- TEntityTeam --------------------------------}
constructor TEntityTeam.Create(const aTeamname: string;
                               const aColor: Tcolor);
begin
  // TEntity properties..
  Classname_ := 'clEntityTeam';
  ClassType_ := cltEntityTeam;
  Name := aTeamname;
  Color := aColor;
  TColorToRGB(Color, ColorR,ColorG,ColorB);
  // TEntityTeam properties..
  SetLength(Players,0); // array of TEntityPlayer;
end;

destructor TEntityTeam.Destroy;
begin
  SetLength(Players,0); // array of TEntityPlayer;
end;

procedure TEntityTeam.AddPlayer(const aNickname: string;
                                const aColor: Tcolor);
var Len: integer;
begin
  Len := Length(Players);
  SetLength(Players,Len+1); // array of TEntityPlayer;
  Players[Len] := TEntityPlayer.Create(aNickname,aColor);
end;

procedure TEntityTeam.DeletePlayer(const aNickname: string);
var Len,
    Index,
    i: integer;
begin
  Len := Length(Players);
  Index := -1;
  for i:=0 to Len-1 do
    if Players[i].Name = aNickname then Index := i;
  // verwissel Index met de laatste..dan de laatste verwijderen.
  if Index = -1 then Exit;
  Players[Index] := Players[Len];
  SetLength(Players,Len-1); // array of TEntityPlayer;
end;

procedure TEntityTeam.DeletePlayer(const aIndex: integer);
var Len: integer;
begin
  Len := Length(Players);
  if aIndex = -1 then Exit;
  if aIndex >= Len then Exit;
  // verwissel Index met de laatste..dan de laatste verwijderen.
  Players[aIndex] := Players[Len];
  SetLength(Players,Len-1); // array of TEntityPlayer;
end;


{--- TEntityMover -------------------------------}
constructor TEntityMover.Create;
begin
  inherited;
  //
  localCamera := TCamera.Create;     // een camera-object instantieren
  localCamera.Floating := false;     // zodat deze cam overal kan kijken /volgen van de entity-bewegingen
  Camera := @localCamera;            // TEntity PCamera instellen op de TCamera-instantie
  //
  isMoving := false;
  hasAI_move := false;
  useAI_move := false;
end;

destructor TEntityMover.Destroy;
begin
  localCamera.Free;
  localCamera := nil;
  Camera := nil;
  inherited;
end;

procedure TEntityMover.CalcNextRoute;
begin
  // override deze virual procedure
end;

procedure TEntityMover.AI_move;
var route, V,V1,V2: TVector;
    d, rad, Yaw2, DeltaYaw, ispeed, ft: single;
    Interval: Int64;
    Duration: single;
    PerformanceCount: Int64;
begin
  QueryPerformanceCounter(PerformanceCount);
  Interval := PerformanceCount - LastPerformanceCount;
  Duration := (Interval/PerformanceFreq);   //tijdsduur in seconden
//!!
(*
ft := OGL.GetLastFrameTime;
overtime := ft-Duration; //de sleep in de thread
if overtime<0 then overtime:=0;
*)
  LastPerformanceCount := PerformanceCount; //next

  // de af te leggen weg van huidige locatie naar doelbestemming
  route := SubVector(Destination, Location);
  d := VectorLength(route);

  // interpoleer de huidige movement naar de doel-bestemming
  V1 := Movement; //is al genormaliseerd
  V2 := UnitVector(route);
  Movement := VectorLERP(V1,V2, Duration*2 ); //0.05
  Movement := UnitVector(Movement);

  if Movement.X=0 then Movement.X := 0.00001;

  // de stand van de entity
  rad := ATan2(Movement.Z, Movement.X);
  Yaw := 180 - rad*constRadToDeg; //entity.yaw
  //rad := ATan2(route.Z, route.X);
  //Roll := 180 - rad*constRadToDeg; //entity.yaw

  ispeed := Speed * ((Duration {- OGL.GetLastFrameTime})*50);

  // de afstand daartussen
  if d<c_TILESIZE/2 then
    // afremmen
    Speed := MaxSpeed * (d/(c_TILESIZE/2)) + (MaxSpeed/100)
  else begin
    // optrekken
    if Speed=0 then Speed := 0.001;
    if Speed*1.05<MaxSpeed then Speed := Speed * 1.05;
  end;

  if d < 2 then begin
    // gearriveerd.. kies een nieuwe bestemming
    CalcNextRoute();
  end else begin
    // verder rijden..
//    if ispeed>MaxSpeed then ispeed := MaxSpeed;
    V := ScaleVector(Movement, ispeed);
    Location := AddVector(Location, V);
    Location.Y := ParentMap.Terrain.GetHeightAt(Location);
  end;

  // camera aanpassen
  {if Camera<>nil then begin}
    Camera.Target := AddVector(Camera.Position,ScaleVector(Movement,50));
    V := ScaleVector(InverseVector(Camera.LineOfSight),15); //camera wat achter de entity
    V := AddVector(V, Vector(0,6,0));
    Camera.Position := AddVector(Location, V); // .SetPosition(Location);
  {end;}
end;


{--- TOreMiner ----------------------------------}
constructor TEntityOreMiner.Create(aParentMap: PEntityMap;
                                   const aTeam: integer;
                                   const aResourcesBitMask: cardinal;
                                   const aMaxLoad: integer;
                                   const aMaxSpeed: single;
                                   const aLocation: TVector);
var r: integer;
    b: byte;
begin
  inherited Create();

  // TEntity properties..
  Classname_ := 'clEntityOreMiner';
  ClassType_ := cltEntityOreMiner;
  Team := aTeam;
  Color := Game.Teams[aTeam].Color;
  TColorToRGB(Color, ColorR,ColorG,ColorB);
//  Yaw := 1;  //graden
  //
  Location := aLocation;         //wereld-coordinaten
  Destination := Vector(c_TILESIZE,1,c_TILESIZE);  //wereld-coordinaten
  Movement := Vector(0,0,0);
  Health := 100;
  Speed := 0.1;
  MaxSpeed := aMaxSpeed;//c_MAXSPEED_OREMINER;
  DefendRatio := 1.0;
  AttackRatio := 1.0;

  canMove := true;
  isMoving := false;     //TEntityMover
  hasAI_move := true;    //TEntityMover
  useAI_move := true;    //TEntityMover

  isStatic := false;
  isIdle := true;
  canCollect := true;
  isCollecting := false;
  canStore := true;
  isStoring := false;
  canDistribute := true;
  isDistributing := false;
  canDefend := false;
  isDefending := false;
  canAttack := false;
  isAttacking := false;
  hasAI_collect := true;
  useAI_collect := true;
  hasAI_store := true;
  useAI_store := true;
  hasAI_distribute := true;
  useAI_distribute := true;
  hasAI_defend := false;
  useAI_defend := false;
  hasAI_attack := false;
  useAI_attack := false;
  //
  ParentMap := aParentMap;
  isReturning := false;
  //
  b := $01;
  for r:=0 to c_NROFRESOURCES-1 do begin
    canLoadResource[r] := ((aResourcesBitMask and b)>0);
    b := b shl 1;
    //
    ResourcesLoaded[r] := 0;
  end;
  //
  fInstalledUpgrades := 0;            // nog geen upgrades
end;

destructor TEntityOreMiner.Destroy;
begin
  inherited;
end;

procedure TEntityOreMiner.CalcNextRoute;
var s, closestS: integer;
    d, closestD: single;
    Vs,Vd: TVector;
    xx,yy,zz: single;
begin
  // bereken de volgende route
  if not isReturning then begin
    // zoek het dichtstbijzijnde spawnpoint om naar terug te rijden..
    closestS := -1;
    closestD := 10000000.0;
    for s:=Low(Game.Map.Spawnpoints) to High(Game.Map.Spawnpoints) do begin
      if Game.Map.Spawnpoints[s].Team <> Team then Continue;
      Vs := Game.Map.Spawnpoints[s].Location;
      Vd := Destination;
      d := VectorLength(SubVector(Vs,Vd));
      if d < closestD then begin
        closestS := s;
        closestD := d;
      end;
    end;
    // stel die spawn in als nieuwe bestemming
    Destination := Game.Map.Spawnpoints[closestS].Location;
    isReturning := true;
  end else begin
    // zoek een ore-field..
//test!!!!!DEBUG!!!!! kiez zolang een willekeurige plek op de map
//Randomize;
//ParentMap.RandomLocation
    xx := Random(100)/100*c_TILESIZE*ParentMap.Terrain.Width;
    zz := Random(100)/100*c_TILESIZE*ParentMap.Terrain.Height;
    yy := ParentMap.Terrain.GetHeightAt(xx,zz);
    Destination := Vector(xx,yy,zz);
    isReturning := false;
  end;
end;

procedure TEntityOreMiner.AI_collect;
begin
  //
end;

procedure TEntityOreMiner.AI_store;
begin
  //
end;

procedure TEntityOreMiner.AI_distribute;
begin
  //
end;


procedure TEntityOreMiner.Upgrade;   //!!!!!DEBUG!!!!!
var u: integer;
begin
  u := fInstalledUpgrades + 1;
  if u > 5 then Exit;
  fInstalledUpgrades := u;
end;


{--- TEntitySpawnPoint --------------------------}
constructor TEntitySpawnPoint.Create(aParentMap: PEntityMap;
                                     const aName: string;
                                     const aTeam: integer;
                                     const aTileX, aTileY: integer);
var h: single;
    Tile: TEntityTile;
begin
  if not Assigned(aParentMap) then Exit;
  // TEntity properties
  Classname_ := 'clEntitySpawnPoint';
  ClassType_ := cltEntitySpawnpoint;
  Name := aName;
  Team := aTeam;
  ConnectedTo := nil;
  //
  ParentMap := aParentMap;
  TileX := aTileX;
  TileY := aTileY;
  Tile := ParentMap.Terrain.Tiles[TileX,TileY];
  h := ParentMap.Terrain.GetHeightAt(Tile.CenterX, Tile.CenterY);
  Location := Vector(Tile.CenterX, h, Tile.CenterY);
  // de grondbezitter instellen
  Tile.SetOwner(aTeam);
end;


{--- TEntityPowerpylon --------------------------}
constructor TEntityPowerpylon.Create(aParentMap: PEntityMap;
                                     const aName: string;
                                     const aTeam,
                                           aTileX, aTileY: integer);
var h: single;
    Tile: TEntityTile;
begin
  if not Assigned(aParentMap) then Exit;
  // TEntity properties
  Classname_ := 'clEntityPowerpylon';
  ClassType_ := cltEntityPowerpylon;
  Name := aName;
  Team := aTeam;
  ConnectedTo1 := nil;
  ConnectedTo2 := nil;
  //
  ParentMap := aParentMap;
  TileX := aTileX;
  TileY := aTileY;
  Tile := ParentMap.Terrain.Tiles[TileX,TileY];
  h := ParentMap.Terrain.GetHeightAt(Tile.CenterX, Tile.CenterY);
  Location := Vector(Tile.CenterX, h, Tile.CenterY);
  // de grondbezitter instellen
  Tile.SetOwner(aTeam);
end;


{--- TEntityPowerline ---------------------------}
constructor TEntityPowerline.Create(aParentMap: PEntityMap; const aName: string; const aTeam, aTileX, aTileY: integer);
var h: single;
    Tile: TEntityTile;
begin
  if not Assigned(aParentMap) then Exit;
  // TEntity properties
  Classname_ := 'clEntityPowerline';
  ClassType_ := cltEntityPowerline;
  Name := aName;
  Team := aTeam;
  ConnectedTo1 := nil;
  ConnectedTo2 := nil;
  //
  ParentMap := aParentMap;
  TileX := aTileX;
  TileY := aTileY;
  Tile := ParentMap.Terrain.Tiles[TileX,TileY];
  h := ParentMap.Terrain.GetHeightAt(Tile.CenterX, Tile.CenterY);
  Location := Vector(Tile.CenterX, h, Tile.CenterY);
  // de grondbezitter instellen
  Tile.SetOwner(aTeam);
end;


{--- TEntityArea --------------------------------}
constructor TEntityTile.Create(aParentTerrain: PEntityTerrain;
                               const aTileX, aTileY: integer;
                               const aOwnedByTeam: integer);
var r: integer;
begin
  Classname_ := 'clEntityTile';
  ClassType_ := cltEntityTile;
  TileX := aTileX;
  TileY := aTileY;
  MinX := TileX * c_TILESIZE;
  MaxX := (TileX+1) * c_TILESIZE;
  MinY := TileY * c_TILESIZE;
  MaxY := (TileY+1) * c_TILESIZE;
  CenterX := (MinX+MaxX)/2;
  CenterY := (MinY+MaxY)/2;
  ParentTerrain := aParentTerrain;
  Team := aOwnedByTeam;
  if Team = c_NOTEAM then Color := clBlack
                     else Color := Game.Teams[Team].Color;
  TColorToRGB(Color, ColorR,ColorG,ColorB);
  for r:=0 to c_NROFRESOURCES-1 do ResourcesAmount[r] := 0; // hoeveelheid erts in deze area
end;

function TEntityTile.MinLocationX: single;
begin
  Result := Floor(TileX / ParentTerrain.Width);       //!
end;

function TEntityTile.MaxLocationX: single;
begin
  Result := Floor((TileX+1) / ParentTerrain.Width);       //!
end;

function TEntityTile.MinLocationY: single;
begin
  Result := Floor(TileY / ParentTerrain.Height);
end;

function TEntityTile.MaxLocationY: single;
begin
  Result := Floor((TileY+1) / ParentTerrain.Height);
end;

procedure TEntityTile.SetOwner(const aTeam: integer);
begin
  if aTeam >= Length(Game.Teams) then Exit;
  Team := aTeam;
  if Team = c_NOTEAM then Color := clBlack
                     else Color := Game.Teams[Team].Color;
  TColorToRGB(Color, ColorR,ColorG,ColorB);
end;

procedure TEntityTile.SetColor(const aColor: TColor);
begin
  Color := aColor;
  TColorToRGB(Color, ColorR,ColorG,ColorB);
end;

procedure TEntityTile.SetColorToResource(const ResourceIndex: integer);
var r,g,b: cardinal;
    c: TColor;
begin
  // de kleur van de area
  c := Game.Resources[ResourceIndex].Color;
  r := (c and $000000FF);
  g := ((c and $0000FF00) shr 8);
  b := ((c and $00FF0000) shr 16);
  r := Round(r * ResourcesAmount[ResourceIndex]/c_MAXRESOURCEPERAREA);
  g := Round(g * ResourcesAmount[ResourceIndex]/c_MAXRESOURCEPERAREA);
  b := Round(b * ResourcesAmount[ResourceIndex]/c_MAXRESOURCEPERAREA);
  if r>255 then r:=255;
  if g>255 then g:=255;
  if b>255 then b:=255;
  Color := (b shl 16)+(g shl 8)+r;
  TColorToRGB(Color, ColorR,ColorG,ColorB);
end;

procedure TEntityTile.SetColorToResources;
var r,g,b: cardinal;
    fr,fg,fb: cardinal;
    c: TColor;
    res, Len: integer;
begin
  fr := 0;
  fg := 0;
  fb := 0;
  for res:=Low(Game.Resources) to High(Game.Resources) do begin
    // de kleur van de area
    c := Game.Resources[res].Color;
    r := (c and $000000FF);
    g := ((c and $0000FF00) shr 8);
    b := ((c and $00FF0000) shr 16);
    r := Round(r * ResourcesAmount[res]/c_MAXRESOURCEPERAREA);
    g := Round(g * ResourcesAmount[res]/c_MAXRESOURCEPERAREA);
    b := Round(b * ResourcesAmount[res]/c_MAXRESOURCEPERAREA);
    if r<0 then r:=0;
    if g<0 then g:=0;
    if b<0 then b:=0;
    if r>255 then r:=255;
    if g>255 then g:=255;
    if b>255 then b:=255;
    fr := fr + r;
    fg := fg + g;
    fb := fb + b;
//    Color := (b shl 16)+(g shl 8)+r;
  end;
  //Len := Length(Game.Resources);
  //fr := fr div Len;
  //fg := fg div Len;
  //fb := fb div Len;
  Color := (fb shl 16)+(fg shl 8)+fr;
  TColorToRGB(Color, ColorR,ColorG,ColorB);
end;


procedure TEntityTile.SetResourceAmount(const ResourceIndex, Amount: integer);
var a: integer;
begin
  if (ResourceIndex>c_NROFRESOURCES) or (ResourceIndex<0) then Exit;
  a := Amount;
  if a < 0 then a := 0;
  if a > c_MAXRESOURCEPERAREA then a := c_MAXRESOURCEPERAREA;
  ResourcesAmount[ResourceIndex] := a;
  SetColorToResource(ResourceIndex);
end;

procedure TEntityTile.AddResourceAmount(const ResourceIndex,
                                              Amount: integer);
var a: integer;
begin
  if (ResourceIndex>c_NROFRESOURCES) or (ResourceIndex<0) then Exit;
  a := ResourcesAmount[ResourceIndex] + Amount;
  if a < 0 then a := 0;
  if a > c_MAXRESOURCEPERAREA then a := c_MAXRESOURCEPERAREA;
  ResourcesAmount[ResourceIndex] := a;
  SetColorToResources;
end;



{--- TEntityMap ---------------------------------}
constructor TEntityMap.Create(const aName: string);
var x,y: integer;
begin
  // TEntity properties
  Classname_ := 'clEntityMap';
  ClassType_ := cltEntityMap;
  Name := aName;
  // TEntityMap properties
  SetLength(Spawnpoints,0);
  SetLength(Powerpylons,0);
  SetLength(Powerlines,0);
  // terrein
  Terrain := nil;
end;

destructor TEntityMap.Destroy;
begin
  SetLength(Spawnpoints,0);
  SetLength(Powerpylons,0);
  SetLength(Powerlines,0);
  if Assigned(Terrain) then Terrain.Free;
end;


procedure TEntityMap.LocationToTile(const aLocation: TVector;
                                    var vTileX, vTileY: integer);
begin
  vTileX := Floor(aLocation.X / c_TILESIZE);
  vTileY := Floor(aLocation.Z / c_TILESIZE);
end;

function TEntityMap.RandomLocation(const MinLocation,MaxLocation: TVector): TVector;
begin
  Result.X := MinLocation.X + Random(Round((MaxLocation.X - MinLocation.X)*10000))/10000;
  Result.Y := MinLocation.Y + Random(Round((MaxLocation.Y - MinLocation.Y)*10000))/10000;
  Result.X := MinLocation.Z + Random(Round((MaxLocation.Z - MinLocation.Z)*10000))/10000;
end;

function TEntityMap.RandomLocation: TVector;
var minLoc,maxLoc: TVector;
begin
  minLoc := Vector(0,0,0);
  maxLoc := Vector(Terrain.Width*c_TILESIZE,0,Terrain.Height*c_TILESIZE);
  Result := RandomLocation(minLoc,maxLoc);
end;

function TEntityMap.IsOnTerrain(const Position: TVector): boolean;
var TileX,TileY: integer;
    h: single;
begin
  Result := false;
  LocationToTile(Position, TileX,TileY);
  if (TileX < 0) or (TileX >= Terrain.Width) then Exit;
  if (TileY < 0) or (TileY >= Terrain.Height) then Exit;
  h := Terrain.GetHeightAt(Position);
  if h - Position.Y > 1.0 then Exit;   //hoogteverschil > 1 unit
  Result := true;
end;





procedure TEntityMap.CreateTerrain(const HeightmapBMP: string;
                                   const minHeight, maxHeight: single);
begin
  Terrain := TEntityTerrain.Create(@Game.Map, HeightmapBMP, minHeight, maxHeight);
  // de quadtree opbouwen
  Terrain.QuadTree := TEntityQuadTree.Create(@Terrain);
  Terrain.QuadTree.CreateTree(16); // heightmap breedte moet door de waarde deelbaar zijn (geen rest dus..)
end;




procedure TEntityMap.AddSpawnpoint(const aName: string;
                                   const aTeam: integer;
                                   const aTileX, aTileY: integer);
var Len: integer;
begin
  Len := Length(Spawnpoints);
  SetLength(Spawnpoints,Len+1); // array of TEntitySpawnPoint;
  Spawnpoints[Len] := TEntitySpawnPoint.Create(@Game.Map,aName,aTeam,aTileX,aTileY);
  //Terrain.Tiles[aTileX,aTileY].SetOwner(aTeam);
end;

procedure TEntityMap.DeleteSpawnpoint(const aName: string);
var Len,
    Index,
    i: integer;
begin
  Len := Length(Spawnpoints);
  Index := -1;
  for i:=Low(Spawnpoints) to High(Spawnpoints) do
    if Spawnpoints[i].Name = aName then Index := i;
  if Index = -1 then Exit;
  // eigenaar van de area wissen..
  Terrain.Tiles[Spawnpoints[Index].TileX, Spawnpoints[Index].TileY].SetOwner(c_NOTEAM);
  // verwissel Index met de laatste..dan de laatste verwijderen.
  Spawnpoints[Index] := Spawnpoints[Len];
  SetLength(Spawnpoints,Len-1); // array of TEntitySpawnPoint;
end;

procedure TEntityMap.DeleteSpawnpoint(const aIndex: integer);
var Len: integer;
begin
  Len := Length(Spawnpoints);
  if aIndex = -1 then Exit;
  if aIndex >= Len then Exit;
  // eigenaar van de area wissen..
  Terrain.Tiles[Spawnpoints[aIndex].TileX, Spawnpoints[aIndex].TileY].SetOwner(c_NOTEAM);
  // verwissel Index met de laatste..dan de laatste verwijderen.
  Spawnpoints[aIndex] := Spawnpoints[Len];
  SetLength(Spawnpoints,Len-1); // array of TEntitySpawnPoint;
end;

procedure TEntityMap.AddUnit(const aUnitType,
                                   aTeam: integer;
                             const aLocation: TVector);
var Len: integer;
    OreMiner: TEntityOreMiner;
begin
  Len := Length(Units);
  SetLength(Units, Len+1);
  case aUnitType of
    c_UNIT_OREMINER: begin
      OreMiner := TEntityOreMiner.Create(@Game.Map,aTeam,$07,100,c_MAXSPEED_OREMINER,aLocation);
      //OreMiner.Destination := Vector(UnitsPerArea,0,UnitsPerArea);
      OreMiner.isReturning := true;
      OreMiner.CalcNextRoute;
      OreMiner.isIdle := false;
      OreMiner.isMoving := false;
      Units[Len] := OreMiner;
    end;
  end;
end;




procedure TEntityMap.AddPowerpylon(const aName: string; const aTeam, aTileX, aTileY: integer);
var Len: integer;
begin
  Len := Length(Powerpylons);
  SetLength(Powerpylons,Len+1); // array of TEntityPowerpylon;
  Powerpylons[Len] := TEntityPowerpylon.Create(@Game.Map,aName,aTeam,aTileX,aTileY);
end;

procedure TEntityMap.DeletePowerpylon(const aName: string);
var Len,
    Index,
    i: integer;
begin
  Len := Length(Powerpylons);
  Index := -1;
  for i:=Low(Powerpylons) to High(Powerpylons) do
    if Powerpylons[i].Name = aName then Index := i;
  if Index = -1 then Exit;
  // eigenaar van de area wissen..
  Terrain.Tiles[Powerpylons[Index].TileX, Powerpylons[Index].TileY].SetOwner(c_NOTEAM);
  // verwissel Index met de laatste..dan de laatste verwijderen.
  Powerpylons[Index] := Powerpylons[Len];
  SetLength(Powerpylons,Len-1); // array of TEntityPowerpylon;
end;

procedure TEntityMap.DeletePowerpylon(const aIndex: integer);
var Len: integer;
begin
  Len := Length(Powerpylons);
  if aIndex = -1 then Exit;
  if aIndex >= Len then Exit;
  // eigenaar van de area wissen..
  Terrain.Tiles[Powerpylons[aIndex].TileX, Powerpylons[aIndex].TileY].SetOwner(c_NOTEAM);
  // verwissel Index met de laatste..dan de laatste verwijderen.
  Powerpylons[aIndex] := Powerpylons[Len];
  SetLength(Powerpylons,Len-1); // array of TEntityPowerpylon;
end;





procedure TEntityMap.AddPowerline(const aName: string; const aTeam, aTileX, aTileY: integer;
                                  const aConnection1, aConnection2: PEntity);
var Len: integer;
    Powerline: PEntity;
begin
  if (aConnection1=nil) or (aConnection2=nil) then Exit;
  //
  Len := Length(Powerlines);
  SetLength(Powerlines,Len+1); // array of TEntityPowerline;
  Powerlines[Len] := TEntityPowerline.Create(@Game.Map,aName,aTeam,aTileX,aTileY);
  Powerline := @Powerlines[Len];

  // deze powerline is verbonden met 2 andere entities
  with PEntityPowerline(Powerline)^ do begin
    ConnectedTo1 := aConnection1;
    ConnectedTo2 := aConnection2;
    Height1 := Terrain.GetHeightAt(aConnection1^.Location) + c_POWERLINE_HEIGHT;
    Height2 := Terrain.GetHeightAt(aConnection2^.Location) + c_POWERLINE_HEIGHT;
  end;

  // de 2 andere entities zijn verbonden met deze powerline
  case aConnection1^.ClassType_ of
    cltEntityPowerpylon: PEntityPowerpylon(aConnection1)^.ConnectedTo1 := Powerline;
    //cltEntitySpawnpoint: PEntitySpawnpoint(aConnection1)^.ConnectedTo := Powerline;
  end;
  case aConnection2^.ClassType_ of
    cltEntityPowerpylon: PEntityPowerpylon(aConnection2)^.ConnectedTo2 := Powerline;
    cltEntitySpawnpoint: PEntitySpawnpoint(aConnection2)^.ConnectedTo := PEntityPowerline(Powerline);
  end;
end;

procedure TEntityMap.DeletePowerline(const aName: string);
var Len,
    Index,
    i: integer;
begin
  Len := Length(Powerlines);
  Index := -1;
  for i:=Low(Powerlines) to High(Powerlines) do
    if Powerlines[i].Name = aName then Index := i;
  if Index = -1 then Exit;
  // eigenaar van de area wissen..
  Terrain.Tiles[Powerlines[Index].TileX, Powerlines[Index].TileY].SetOwner(c_NOTEAM);
  // verwissel Index met de laatste..dan de laatste verwijderen.
  Powerlines[Index] := Powerlines[Len];
  SetLength(Powerlines,Len-1); // array of TEntityPowerline;
end;

procedure TEntityMap.DeletePowerline(const aIndex: integer);
var Len: integer;
begin
  Len := Length(Powerlines);
  if aIndex = -1 then Exit;
  if aIndex >= Len then Exit;
  // eigenaar van de area wissen..
  Terrain.Tiles[Powerlines[aIndex].TileX, Powerlines[aIndex].TileY].SetOwner(c_NOTEAM);
  // verwissel Index met de laatste..dan de laatste verwijderen.
  Powerlines[aIndex] := Powerlines[Len];
  SetLength(Powerlines,Len-1); // array of TEntityPowerline;
end;

procedure TEntityPowerline.RenderConstructing(const FromPosition, ToPosition: TVector);
begin
  //if Game.ConstructionClassType <> cltEntityPowerline then Exit;
  glDisable(GL_DEPTH_TEST);
  glColor3f(1.0, 1.0, 1.0);
  glBegin(GL_LINES);
    glVertex3fv(@FromPosition);
    glVertex3fv(@ToPosition);
  glEnd;
  glEnable(GL_DEPTH_TEST);
end;

procedure TEntityPowerline.Render;
begin
  if (ConnectedTo1=nil) or (ConnectedTo2=nil) then Exit;

  // team-kleur instellen
  with Game.Teams[Team] do glColor3f(ColorR,ColorG,ColorB);

  glDisable(GL_DEPTH_TEST);
  glBegin(GL_LINES);
    glVertex3fv(@ConnectedTo1^.Location);
    glVertex3fv(@ConnectedTo2^.Location);
  glEnd;
  glEnable(GL_DEPTH_TEST);
end;







{--- TQuadTree ----------------------------------}
constructor TEntityQuadTree.Create(aParentTerrain: PEntityTerrain);
begin
  Classname_ := 'clEntityQuadTree';
  ClassType_ := cltEntityQuadTree;
  ParentTerrain := aParentTerrain;
  SetLength(Nodes, 0);
end;

destructor TEntityQuadTree.Destroy;
begin
  SetLength(Nodes, 0);
  inherited;
end;

procedure TEntityQuadTree.CreateTree(const aSubDivideLimit: integer);
var Len, c: integer;
begin
  // recursief de quadtree opbouwen..
  SubDivideLimit := aSubDivideLimit;
  // een node toevoegen
  Len := Length(Nodes);
  SetLength(Nodes, Len+1);
  with Nodes[0] do begin
    qtType := qtNode;
    Parent := nil;
    for c:=0 to 3 do Children[c] := nil;
    PosX := ParentTerrain^.PosX;
    PosZ := ParentTerrain^.PosZ;
    DeltaX := ParentTerrain^.DeltaX;
    DeltaZ := ParentTerrain^.DeltaZ;
    //for i:=TrisW to TrisWCount do
    //  glDrawElements(GL_TRIANGLE_STRIP, TrisZCount, GL_UNSIGNED_INT, @IndexArray[TrisW][TrisZ]);
    TrisW := 0;
    TrisWCount := ParentTerrain^.Width;
    TrisZ := 0;
    TrisZCount := ParentTerrain^.Height*2;
    // de hoogte-waarden invullen tbv. bounding-box check bij afbeelden
    CalcHeights(Nodes[0]);
  end;
  SubDivideNode(Nodes[0], ParentTerrain^.Width);  // breedte en hoogte van de map moeten gelijk zijn :-(
end;

procedure TEntityQuadTree.CalcHeights(var aNode: TQuadTreeNode);
var x,z: integer;
    minH,maxH: single;
    Index: integer;
    V: TVector;
begin
//  for i:=TrisW to Min(Width-1{-1}, TrisW+TrisWCount) do
//    glDrawElements(GL_TRIANGLE_STRIP, TrisZCount+2, GL_UNSIGNED_INT, @IndexArray[i][TrisZ]);
  minH := 3.4E38;
  maxH := 1.5E-45;
  with aNode do begin
    for x:=TrisW to TrisWCount-1 do
      for z:= TrisZ to (TrisZCount div 2) do begin
        Index := ParentTerrain^.IndexArray[x][z*2];
        with ParentTerrain^.VertexArray[Index] do V := Vector(X,Y,Z);
        if V.Y < minH then minH := V.Y;
        if V.Y > maxH then maxH := V.Y;
      end;
    MinHeight := minH;
    MaxHeight := maxH;
    DeltaHeight := maxH - minH;
    BoundingBox.Min := Vector(PosX,MinHeight,PosZ);
    BoundingBox.Max := Vector(PosX+DeltaX,MaxHeight,PosZ+DeltaZ);
  end;
end;

//   3 2
//   1 0
procedure TEntityQuadTree.SubDivideNode(var aNode: TQuadTreeNode;
                                        const CurrentSubdivide: integer);
var ThisSubdivide, Len, c: integer;
    dx,dz: single;
    tw,twc, tz,tzc: integer;
begin
  if CurrentSubdivide <= SubDivideLimit then Exit; //klaar
  ThisSubdivide := CurrentSubdivide div 2;
  dx := aNode.DeltaX / 2;
  dz := aNode.DeltaZ / 2;
  twc := aNode.TrisWCount div 2;
  tzc := aNode.TrisZCount div 2;

  aNode.qtType := qtNode;

  // node #0 toevoegen
  Len := Length(Nodes);
  SetLength(Nodes, Len+1);
  with Nodes[Len] do begin
    qtType := qtLeaf;
    Parent := @aNode;
    for c:=0 to 3 do Children[c] := nil;
    DeltaX := dx;
    DeltaZ := dz;
    PosX := aNode.PosX;
    PosZ := aNode.PosZ;
    TrisW := aNode.TrisW + 0;//Trunc(PosX/(ParentTerrain as TEntityTerrain).ScaleX)+0;
    TrisWCount := twc;
    TrisZ := 2*Trunc(PosZ/c_TILESIZE)+0;
    TrisZCount := tzc;
  end;
  CalcHeights(Nodes[Len]);  // de hoogte-waarden invullen tbv. bounding-box check bij afbeelden
  aNode.Children[0] := @Nodes[Len];

  // node #1 toevoegen
  Len := Length(Nodes);
  SetLength(Nodes, Len+1);
  with Nodes[Len] do begin
    qtType := qtLeaf;
    Parent := @aNode;
    for c:=0 to 3 do Children[c] := nil;
    DeltaX := dx;
    DeltaZ := dz;
    PosX := aNode.PosX + dx;
    PosZ := aNode.PosZ;
    TrisW := aNode.TrisW + twc;// Trunc(PosX/(ParentTerrain as TEntityTerrain).ScaleX){+twc};
    TrisWCount := twc;
    TrisZ := 2*Trunc(PosZ/c_TILESIZE)+0;
    TrisZCount := tzc;
  end;
  CalcHeights(Nodes[Len]);  // de hoogte-waarden invullen tbv. bounding-box check bij afbeelden
  aNode.Children[1] := @Nodes[Len];

  // node #2 toevoegen
  Len := Length(Nodes);
  SetLength(Nodes, Len+1);
  with Nodes[Len] do begin
    qtType := qtLeaf;
    Parent := @aNode;
    for c:=0 to 3 do Children[c] := nil;
    DeltaX := dx;
    DeltaZ := dz;
    PosX := aNode.PosX;
    PosZ := aNode.PosZ + dz;
    TrisW := aNode.TrisW + 0;//  Trunc(PosX/(ParentTerrain as TEntityTerrain).ScaleX)+0;
    TrisWCount := twc;
    TrisZ := 2*Trunc( PosZ/c_TILESIZE){+tzc};
    TrisZCount := tzc;
  end;
  CalcHeights(Nodes[Len]);  // de hoogte-waarden invullen tbv. bounding-box check bij afbeelden
  aNode.Children[2] := @Nodes[Len];

  // node #3 toevoegen
  Len := Length(Nodes);
  SetLength(Nodes, Len+1);
  with Nodes[Len] do begin
    qtType := qtLeaf;
    Parent := @aNode;
    for c:=0 to 3 do Children[c] := nil;
    DeltaX := dx;
    DeltaZ := dz;
    PosX := aNode.PosX + dx;
    PosZ := aNode.PosZ + dz;
    TrisW :=  aNode.TrisW + twc;// Trunc(PosX/(ParentTerrain as TEntityTerrain).ScaleX){+twc};
    TrisWCount := twc;
    TrisZ := 2*Trunc(PosZ/c_TILESIZE){+tzc};
    TrisZCount := tzc;
  end;
  CalcHeights(Nodes[Len]);  // de hoogte-waarden invullen tbv. bounding-box check bij afbeelden
  aNode.Children[3] := @Nodes[Len];

  // alle children van deze node behandelen..
  for c:=0 to 3 do SubDivideNode(aNode.Children[c]^, ThisSubdivide);
end;

procedure TEntityQuadTree.Render(var aNode: TQuadTreeNode);
var V1,V2,V3,V4,V5,V6,V7,V8: TVector;
    Visible: boolean;
    c: integer;
    Factor: single;
begin
//  // indien aNode==nil dan beginnen met controleren van de root-node
//  if aNode=nil then aNode := Nodes[0];

  // check de node op zichtbaarheid
  Factor := MaxViewDistance / (SubDivideLimit*10);   // *10
  if (aNode.DeltaX<Factor) and (aNode.DeltaZ<Factor) and (aNode.DeltaHeight<Factor) then begin
    Visible := OGL.Frustum.AABBInside({NullVector,}aNode.BoundingBox); // boxes staan al op world-coords
    if not Visible then Exit;
  end;

  // de node is (gedeeltelijk) zichtbaar.
  // is dit een end-node?? (zonder children).. dan tekenen.
  if aNode.qtType=qtNode then begin //if aNode.Children[0]<>nil then
    // dit is geen end-node,
    // in dit geval alle children controleren..
    for c:=0 to 3 do Render(aNode.Children[c]^);
  end else begin
    // dit is een end-node...tekenen!
    with aNode do PEntityTerrain(ParentTerrain)^.Render(TrisW,TrisWCount, TrisZ,TrisZCount);
  end;

end;





{--- TEntityTerrain -----------------------------}
constructor TEntityTerrain.Create(aParentMap: PEntityMap;
                                  const HeightmapBMP: string;
                                  const minHeight,maxHeight: single);
var BMP: Graphics.TBitmap;
    xx,yy: integer;
    c: single;
    V1,V2,V3, N1,N2: TVector;
begin
  Classname_ := 'clEntityTerrain';
  ClassType_ := cltEntityTerrain;

  ParentMap := aParentMap;
  PosX := 0;
  PosZ := 0;
  PosHeight := minHeight;
  DeltaHeight := maxHeight - minHeight;

//  QuadTree := TEntityQuadTree.Create(@aParentMap^.Terrain);

  BMP := Graphics.TBitmap.Create;
  try
    // lees de hoogtemap
    BMP.LoadFromFile(HeightmapBMP);
    Width := BMP.Width;
    Height := BMP.Height;

    DeltaX := (Width * c_TILESIZE) - PosX;
    DeltaZ := (Height * c_TILESIZE) - PosZ;

    // vertexarray alloceren
    SetLength(VertexArray, Width*(Height+1));
    //
    for yy:=0 to Height{-1} do
      for xx:=0 to Width-1 do
        with Vertexarray[yy*Width+xx] do begin
          if yy=Height then c := (BMP.Canvas.Pixels[xx,yy-1] and $FF) / 255
                       else c := (BMP.Canvas.Pixels[xx,yy] and $FF) / 255;
          // grayscale-waarde naar hoogte omzetten
          Y := c * DeltaHeight + minHeight;
          // X & Z world-coords
          X := xx*c_TILESIZE + PosX;
          Z := yy*c_TILESIZE + PosZ;

          // vertexkleur
          if c>0.5 then R := c *0.3
                   else if c<0.1 then R := c *0.13
                                 else R := c *0.3;
          if c>0.1 then G := c *0.3
                   else G := c *0.2;
          if c>0.1 then B := c *0.3
                   else B := c *2.0;
          A := 1.0;
        end;
  finally
    BMP.Free;
  end;

  // de index-array
  SetLength(IndexArray, Width);
  for xx:=0 to Width-1 do begin
    SetLength(IndexArray[xx], Height*2+4);
    for yy:=0 to Height{-1} do begin
      IndexArray[xx,yy*2]   := yy*Height + xx + 1;
      IndexArray[xx,yy*2+1] := yy*Height + xx;
    end;

  end;

  // de normalen van alle triangles
  SetLength(PlaneNormalArray[0], Width, Height);
  SetLength(PlaneNormalArray[1], Width, Height);
  for xx:=0 to Width-2 do begin
    for yy:=0 to Height-1 do begin
      with Vertexarray[yy*Width+xx] do V1 := Vector(X,Y,Z);
      with Vertexarray[yy*Width+(xx+1)] do V2 := Vector(X,Y,Z);
      with Vertexarray[(yy+1)*Width+(xx+1)] do V3 := Vector(X,Y,Z);
      PlaneNormalArray[0,xx,yy] := PlaneNormal(V1,V2,V3);

    //V1 := V1; //with Vertexarray[yy*Width+xx] do V1 := Vector(X,Y,Z);
      V2 := V3; //with Vertexarray[(yy+1)*Width+(xx+1)] do V2 := Vector(X,Y,Z);
      with Vertexarray[(yy+1)*Width+xx] do V3 := Vector(X,Y,Z);
      PlaneNormalArray[1,xx,yy] := PlaneNormal(V1,V2,V3);
    end;
  end;

  // de quadtree opbouwen
//!  QuadTree.CreateTree(16); // heightmap breedte moet door de waarde deelbaar zijn (geen rest dus..)

  // de tiles met info over: resources, grondbezitter, bebouwing...
  SetLength(Tiles, Width, Height);
  for xx:=0 to Width-2 do begin
    for yy:=0 to Height-1 do begin
      //Tiles[xx,yy] := TEntityTile.Create(@Game.Map.Terrain,xx,yy,c_NOTEAM);
      Tiles[xx,yy] := TEntityTile.Create(@aParentMap^.Terrain,xx,yy,c_NOTEAM);
    end;
  end;
end;

destructor TEntityTerrain.Destroy;
begin
  QuadTree.Free;
  SetLength(VertexArray, 0);
  SetLength(IndexArray, 0);
  SetLength(PlaneNormalArray[0], 0);
  SetLength(PlaneNormalArray[1], 0);
  SetLength(Tiles, 0);
  inherited;
end;

function TEntityTerrain.GetHeightAt(const mapX, mapZ: single): single;
var TriX, TriZ: integer;
    RestX, RestZ: single;
    xx,yy: single;
    s1,s2,d: single;
    V1,V2,V3: TVector;
    Normal,u,w,V,P0: TVector;
begin
  Result := 0.0;
  // is de positie wel boven de map??
  if (mapX<PosX) or (mapX>PosX+DeltaX) then Exit;
  if (mapZ<PosZ) or (mapZ>PosZ+DeltaZ) then Exit;

  // bereken de triangle die bij de positie hoort
  xx := (mapX-PosX)/c_TILESIZE;
  yy := (mapZ-PosZ)/c_TILESIZE;
  TriX := Trunc(xx);  if (TriX<0) or (TriX>Width-1) then Exit;
  TriZ := Trunc(yy);  if (TriZ<0) or (TriZ>Height-2) then Exit;
  RestX := 1-Frac(xx);   //triangle-strip vlakken liggen andersom..
  RestZ := Frac(yy);

  with Vertexarray[TriZ*Width+TriX] do V1 := Vector(X,Y,Z); //!
  if RestZ*RestX < 0.25 then begin
    //!with Vertexarray[TriZ*Width+TriX] do V1 := Vector(X,Y,Z);
    //!with Vertexarray[TriZ*Width+(TriX+1)] do V2 := Vector(X,Y,Z);
    //!with Vertexarray[(TriZ+1)*Width+(TriX+1)] do V3 := Vector(X,Y,Z);
    Normal := PlaneNormalArray[0,TriX,TriZ]; //!
(*
    glDrawBuffer(GL_FRONT);
    glDisable(GL_CULL_FACE);
    glDisable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glBegin(GL_TRIANGLES);
      glColor3f(1,0,0);
      with V1 do glVertex3f(X,Y,Z);
      with V2 do glVertex3f(X,Y,Z);
      with V3 do glVertex3f(X,Y,Z);
    glEnd;
    glEnable(GL_DEPTH_TEST);
    glDrawBuffer(GL_BACK);
*)
  end else begin
    //!with Vertexarray[TriZ*Width+TriX] do V1 := Vector(X,Y,Z);
    //!with Vertexarray[(TriZ+1)*Width+(TriX+1)] do V2 := Vector(X,Y,Z);
    //!with Vertexarray[(TriZ+1)*Width+TriX] do V3 := Vector(X,Y,Z);
    Normal := PlaneNormalArray[1,TriX,TriZ]; //!
(*
    glDrawBuffer(GL_FRONT);
    glDisable(GL_CULL_FACE);
    glDisable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glBegin(GL_TRIANGLES);
      glColor3f(0,0,1);
        with V1 do glVertex3f(X,Y,Z);
        with V2 do glVertex3f(X,Y,Z);
        with V3 do glVertex3f(X,Y,Z);
    glEnd;
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glDrawBuffer(GL_BACK);
*)
  end;
//!  Normal := PlaneNormal(V1,V2,V3);
  P0 := Vector(mapX,0,mapZ);    // lijn door P0 met richtingsvector u
//  u := Vector(0,1,0);         //
  w := SubVector(P0,V1);
//  s1 := -DotProduct(Normal,w) / DotProduct(Normal,u);
  d := Normal.Y; //d := DotProduct(Normal,u); //== Normal.X*0 + Normal.Y*1 + Normal.Z*0 = Normal.Y
  if d=0 then s1:=0 else s1 := -DotProduct(Normal,w) / d;
  Result := s1; //  V:=ScaleVector(u,s1);  Result:=V.Y;
end;

function TEntityTerrain.GetHeightAt(const Position: TVector): single;
begin
  Result := GetHeightAt(Position.X, Position.Z);
end;


procedure TEntityTerrain.Render;
var i: Integer;
begin
  QuadTree.Render(QuadTree.Nodes[0]);
(*
  glFrontFace(GL_CCW);
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);

  glClientActiveTextureARB(GL_TEXTURE0);

  glEnableClientState(GL_COLOR_ARRAY);
  glColorPointer(4, GL_FLOAT, SizeOf(TTerrainVertex), @VertexArray[0].R);
{
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glTexCoordPointer(2, GL_FLOAT, SizeOf(TTerrainVertex), @VertexArray[0].U1);
}
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TTerrainVertex), @VertexArray[0].X);

//glPolygonMode(GL_FRONT, GL_LINE);
  for i:=0 to Width-2 do
    glDrawElements(GL_TRIANGLE_STRIP, Height*2, GL_UNSIGNED_INT, @IndexArray[i][0]);
//glPolygonMode(GL_FRONT, GL_FILL);

  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_VERTEX_ARRAY);
*)
end;

procedure TEntityTerrain.Render(const TrisW,TrisWCount, TrisZ,TrisZCount: integer);
var i: Integer;
begin
  glFrontFace(GL_CCW);
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);

  glClientActiveTextureARB(GL_TEXTURE0);

  glEnableClientState(GL_COLOR_ARRAY);
  glColorPointer(4, GL_FLOAT, SizeOf(TTerrainVertex), @VertexArray[0].R);
(*
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glTexCoordPointer(2, GL_FLOAT, SizeOf(TTerrainVertex), @VertexArray[0].U1);
*)
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TTerrainVertex), @VertexArray[0].X);

  try
//glPolygonMode(GL_FRONT, GL_LINE);
    for i:=TrisW to Min(Width-2, TrisW+TrisWCount) do
      glDrawElements(GL_TRIANGLE_STRIP, TrisZCount+2, GL_UNSIGNED_INT, @IndexArray[i][TrisZ]);
//glPolygonMode(GL_FRONT, GL_FILL);
  except
    //
  end;

  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_VERTEX_ARRAY);
end;





{--- TEntityButton ------------------------------}
constructor TEntityButton.Create(aParentHUD: TEntity;       //PEntityHUD
                                 const aCaption: string;
                                 const X, Y, W, H: integer;
                                 const r, g, b, a: single;
                                 const aImage: string);
var bw, bh: integer;
begin
  Classname_ := 'clEntityButton';
  ClassType_ := cltEntityButton;

  ParentHUD := (aParentHUD as TEntityHUD);
  Name := aCaption;
  if W = -1 then bw := c_BUTTONWIDTH else bw := W;
  if H = -1 then bh := c_BUTTONHEIGHT else bh := H;
  Left := X;
  Right := Left + bw;
  Top := Y;
  Bottom := Top + bh;
  Width := bw;
  Height := bh;
  ColorR := r;
  ColorG := g;
  ColorB := b;
  ColorA := a;
  if aImage<>'' then TextureHandle := OGL.Textures.LoadTexture(aImage);
  fVisible := true;
  fEnabled := true;
  fDone := false;
  fOnce := false;
  fToggle := false;
  fToggledOn := false;
  fPercentage := 0;
  fQueueCount := 10;
  fReady := 0;
end;

destructor TEntityButton.Destroy;
begin
//  if TextureHandle<>0 then
  //
  inherited;
end;

procedure TEntityButton.SetEnabled(const Value: boolean);
begin
  if Value = fEnabled then Exit;
  fEnabled := Value;
end;

procedure TEntityButton.SetVisible(const Value: boolean);
begin
  if Value = fVisible then Exit;
  fVisible := Value;
end;

procedure TEntityButton.SetPercentage(const Value: integer);
begin
  if Value = fPercentage then Exit;
  fPercentage := Value;
end;

procedure TEntityButton.SetDone(const Value: boolean);
begin
  if Value = fDone then Exit;
  fDone := Value;
end;

procedure TEntityButton.SetReady(const Value: integer);
begin
  if Value = fReady then Exit;
  fReady := Value;
end;

procedure TEntityButton.SetToggledOn(const Value: boolean);
begin
  if Value = fToggledOn then Exit;
  fToggledOn := Value;
end;

procedure TEntityButton.SetBlinking(const Value: boolean);
begin
  if Value = fBlinking then Exit;
  fBlinking := Value;
end;

procedure TEntityButton.SetBlinkingOn(const Value: boolean);
begin
  if Value = fBlinkingOn then Exit;
  fBlinkingOn := Value;
end;

procedure TEntityButton.SetQueueCount(const Value: integer);
begin
  if Value = fQueueCount then Exit;
  fQueueCount := Value;
end;

procedure TEntityButton.SetQueued(const Value: integer);
begin
  if Value = fQueued then Exit;
  fQueued := Value;
end;

function TEntityButton.pxLeft: integer;
begin
  Result := (ParentHUD as TEntityHUD).pxLeft + c_HUDMARGIN + Left;
end;

function TEntityButton.pxTop: integer;
begin
  Result := (ParentHUD as TEntityHUD).pxTop + c_HUDMARGIN + Top;
end;

function TEntityButton.pxRight: integer;
begin
  Result := (ParentHUD as TEntityHUD).pxLeft + c_HUDMARGIN + Right;
end;

function TEntityButton.pxBottom: integer;
begin
  Result := (ParentHUD as TEntityHUD).pxTop + c_HUDMARGIN + Bottom;
end;


procedure TEntityButton.DoClick;
begin
  // Game.ConstructingEntity bevat de entity die iets bouwt

  if Assigned(OnUp) then begin
    PlaySound(SoundClick,0,SND_ASYNC+SND_NODEFAULT);
    OnUp(self);
  end;
end;

function TEntityButton.OverButton(const X, Y: integer) : boolean;
var pLeft,pTop,pRight,pBottom: integer;
    hitX, hitY: boolean;
begin
  pLeft := pxLeft + (ParentHUD as TEntityHUD).OffsetX;
  pTop := pxTop + (ParentHUD as TEntityHUD).OffsetY;
  pRight := pxRight + (ParentHUD as TEntityHUD).OffsetX;
  pBottom := pxBottom + (ParentHUD as TEntityHUD).OffsetY;
  hitX := ((X>=pLeft) and (X<=pRight));
  hitY := ((Y>=pTop) and (Y<=pBottom));
  Result := (hitX and hitY);
end;




{--- TEntityButtons -----------------------------}
constructor TEntityButtons.Create(aParentHUD: TEntity;
                                  const aButtonSetIndex: integer;
                                  const aName: string;
                                  const AssignedClassType_: cardinal);
begin
  Classname_ := 'clEntityButtons';
  ClassType_ := cltEntityButtons;

  ParentHUD := (aParentHUD as TEntityHUD);
  fThisIndex := aButtonSetIndex;
  AssignedClassType := AssignedClassType_;
  AssignedEntity := nil;
  Name := aName;
  SetLength(Buttons, 0);
  fTotalButtons := 0;
end;

destructor TEntityButtons.Destroy;
var b: integer;
begin
  for b:=Low(Buttons) to High(Buttons) do Buttons[b].Free;
  SetLength(Buttons, 0);
  inherited;
end;


procedure TEntityButtons.AddButton(const aCaption: string;
                                   const X,Y, W,H: integer;
                                   const r,g,b,a: single;
                                   const aImage: string);
var Len: integer;
begin
  Len := Length(Buttons);
  SetLength(Buttons, Len+1);
  fTotalButtons := (Len+1);
  Buttons[Len] := TEntityButton.Create(ParentHUD, aCaption, X,Y,W,H, r,g,b,a, aImage);
end;

procedure TEntityButtons.AddButton(const aCaption: string;
                                   const r,g,b,a: single;
                                   const aImage: string);
var x,y: integer;
    leftX, leftY: integer;
    hasRadar, isRoot: boolean;
begin
  hasRadar := ((ParentHUD as TEntityHUD).GFX_Radar<>nil);  // een radar op deze HUD??
  isRoot := (AssignedClassType = cltEntityRoot); // een root-set met vaste knoppen

  if isRoot then begin
    leftX := c_HUDMARGIN;
    if hasRadar then
      leftY := c_HUDTITLEBARHEIGHT + c_HUDMARGIN + c_RADARWIDTHHEIGHT + c_HUDMARGIN
    else
      leftY := c_HUDTITLEBARHEIGHT + c_HUDMARGIN;
  end else begin
    if hasRadar then leftX := c_HUDMARGIN + c_RADARWIDTHHEIGHT + c_HUDMARGIN
                else leftX := c_HUDMARGIN;
    leftY := c_HUDTITLEBARHEIGHT + c_HUDMARGIN;
  end;


  // voeg de knop toe, naast de vorige knop in de array:
  // ..eerst de rij vol maken, dan de volgende rij.
  // gebruik de standaard knop-afmetingen.
  if fTotalButtons=0 then begin
    x := leftX;
    y := leftY;
  end else begin
    x := Buttons[fTotalButtons-1].Left;
    y := Buttons[fTotalButtons-1].Top;
    x := x + c_BUTTONWIDTH + c_BUTTONSPACING;
    if x > (ParentHUD as TEntityHUD).Width-c_HUDMARGIN-c_BUTTONWIDTH then begin //op de volgende rij plaatsen..
      x := leftX;
      y := y + c_BUTTONHEIGHT + c_BUTTONSPACING;
      if y > (ParentHUD as TEntityHUD).Height-c_HUDMARGIN-c_BUTTONHEIGHT then //op de volgende rij plaatsen..
        raise ERangeError.Create('De knop "'+ aCaption +'" past niet meer op de HUD');
        //Exit; // past niet meer..
    end;
  end;
  AddButton(aCaption, x,y,c_BUTTONWIDTH,c_BUTTONHEIGHT, r,g,b,a, aImage);
end;

procedure TEntityButtons.AddButton(const aCaption, aImage: string);
begin
  AddButton(aCaption, 0,0,0,1, aImage);
end;


procedure TEntityButtons.RemoveButton(const aIndex: integer);
var tb: integer;
begin
  tb := fTotalButtons - 1;
  if aIndex < tb then Buttons[aIndex] := Buttons[tb];
  Buttons[tb].Free;
  SetLength(Buttons, tb);
  fTotalButtons := tb;
end;

procedure TEntityButtons.RemoveButton(const aCaption: string);
var Index, b: integer;
begin
  Index := -1;
  for b:=Low(Buttons) to High(Buttons) do
    if Buttons[b].Name = aCaption then begin
      Index := b;
      Break;
    end;
  if Index = -1 then Exit;
  RemoveButton(Index);
end;

procedure TEntityButtons.RemoveButton(const X, Y: integer);
var Index, b: integer;
begin
  Index := -1;
  for b:=Low(Buttons) to High(Buttons) do
    if (Buttons[b].Left = X) and (Buttons[b].Top = Y) then begin
      Index := b;
      Break;
    end;
  if Index = -1 then Exit;
  RemoveButton(Index);
end;






{--- TEntityGFX_FPS -----------------------------}
constructor TEntityGFX_FPS.Create(aParentHUD: TEntity;
                                  const aCaption: string;
                                  const X, Y, W, H: integer;
                                  const r, g, b, a: single);
begin
  Classname_ := 'clEntityGFX_FPS';
  ClassType_ := cltEntityGFX_FPS;

  ParentHUD := (aParentHUD as TEntityHUD);
  fVisible := true;
  Name := aCaption;
  Left := X;
  Right := Left + W;
  Top := Y;
  Bottom := Top + H;
  Width := W;
  Height := H;
  ColorR := r;
  ColorG := g;
  ColorB := b;
  ColorA := a;
  //
  NFPS := W-4;
  SetLength(aFPS, NFPS);
  FPSIndex := 0;
  NRecorded := 0;
end;

destructor TEntityGFX_FPS.Destroy;
begin
  SetLength(aFPS, 0);
  //
  inherited;
end;

function TEntityGFX_FPS.OverGFX(const X, Y: integer): boolean;
var pLeft,pTop,pRight,pBottom: integer;
    hitX, hitY: boolean;
begin
  pLeft := pxLeft + (ParentHUD as TEntityHUD).OffsetX;
  pTop := pxTop + (ParentHUD as TEntityHUD).OffsetY;
  pRight := pxRight + (ParentHUD as TEntityHUD).OffsetX;
  pBottom := pxBottom + (ParentHUD as TEntityHUD).OffsetY;
  hitX := ((X>=pLeft) and (X<=pRight));
  hitY := ((Y>=pTop) and (Y<=pBottom));
  Result := (hitX and hitY);
end;

procedure TEntityGFX_FPS.SetVisible(const Value: boolean);
begin
  if Value<>fVisible then fVisible := Value;
end;

function TEntityGFX_FPS.pxTop: integer;
begin
  Result := (ParentHUD as TEntityHUD).pxTop + c_HUDMARGIN + Top;
end;

function TEntityGFX_FPS.pxLeft: integer;
begin
  Result := (ParentHUD as TEntityHUD).pxLeft + c_HUDMARGIN + Left;
end;

function TEntityGFX_FPS.pxBottom: integer;
begin
  Result := (ParentHUD as TEntityHUD).pxTop + c_HUDMARGIN + Bottom;
end;

function TEntityGFX_FPS.pxRight: integer;
begin
  Result := (ParentHUD as TEntityHUD).pxLeft + c_HUDMARGIN + Right;
end;

procedure TEntityGFX_FPS.StoreCurrentFPS;
var Len: integer;
begin
  aFPS[FPSIndex] := OGL.GetFPS;
  Inc(FPSIndex);
  if FPSIndex > NFPS-1 then FPSIndex := 0;
  if NRecorded < NFPS then Inc(NRecorded);
end;




{--- TEntityGFX_Radar ---------------------------}
constructor TEntityGFX_Radar.Create(aParentHUD: TEntity;
                                    const aCaption: string;
                                    const X,Y, W,H: integer;
                                    const r,g,b,a: single);
begin
  Classname_ := 'clEntityGFX_Radar';
  ClassType_ := cltEntityGFX_Radar;

  ParentHUD := (aParentHUD as TEntityHUD);
  fVisible := true;
  Name := aCaption;
  Left := X;
  Right := Left + W;
  Top := Y;
  Bottom := Top + H;
  Width := W;
  Height := H;
  ColorR := r;
  ColorG := g;
  ColorB := b;
  ColorA := a;
  //
end;

destructor TEntityGFX_Radar.Destroy;
begin
  //
  inherited;
end;

procedure TEntityGFX_Radar.SetVisible(const Value: boolean);
begin
  if Value<>fVisible then fVisible := Value;
end;

function TEntityGFX_Radar.OverGFX(const X, Y: integer): boolean;
var pLeft,pTop,pRight,pBottom: integer;
    hitX, hitY: boolean;
begin
  pLeft := pxLeft + (ParentHUD as TEntityHUD).OffsetX;
  pTop := pxTop + (ParentHUD as TEntityHUD).OffsetY;
  pRight := pxRight + (ParentHUD as TEntityHUD).OffsetX;
  pBottom := pxBottom + (ParentHUD as TEntityHUD).OffsetY;
  hitX := ((X>=pLeft) and (X<=pRight));
  hitY := ((Y>=pTop) and (Y<=pBottom));
  Result := (hitX and hitY);
end;

function TEntityGFX_Radar.pxTop: integer;
begin
  Result := (ParentHUD as TEntityHUD).pxTop + c_HUDMARGIN + Top;
end;

function TEntityGFX_Radar.pxLeft: integer;
begin
  Result := (ParentHUD as TEntityHUD).pxLeft + c_HUDMARGIN + Left;
end;

function TEntityGFX_Radar.pxBottom: integer;
begin
  Result := (ParentHUD as TEntityHUD).pxTop + c_HUDMARGIN + Bottom;
end;

function TEntityGFX_Radar.pxRight: integer;
begin
  Result := (ParentHUD as TEntityHUD).pxLeft + c_HUDMARGIN + Right;
end;







{--- TEntityHUD ---------------------------------}
constructor TEntityHUD.Create(const aHUDIndex: integer; //fThisHUDIndex
                              const aName: string;
                              const aAlignment: Byte;   //bitmask
                              const X, Y, W, H: integer;
                              const r, g, b, a: single;
                              const aImage: string);
var bx: integer;
    al,ar: boolean;
begin
  Classname_ := 'clEntityHUD';
  ClassType_ := cltEntityHUD;

  GFX_FPS := nil;           // TEntityGFX_FPS
  GFX_Radar := nil;         // TEntityGFX_Radar
  SetLength(SysButtons, 0); // de array met vast HUD-sysmenu-knoppen
  SetLength(ButtonSets, 0); // de array met button-sets (voor elke entity een set)
  ButtonsPtr := nil;
  Name := aName;
  //
  Alignment := aAlignment;
  al := (Alignment and hudAlignLeft)>0;
  ar := (Alignment and hudAlignRight)>0;
  fHUDButtonsSwappedX := (ar and (not al));
  fThisIndex := aHUDIndex;
  //
  OffsetX := 0;        // offset tbv. minimize/maximize
  OffsetY := 0;
  DeltaOffsetX := 0;        // delta-offset tbv. minimize/maximize
  DeltaOffsetY := 0;
  EndOffsetX := 0;
  EndOffsetY := 0;
  Minimizing := false;
  Maximizing := false;
  Minimized := false;
  Maximized := true;
  //
  Left := X;
  Right := Left + W;
  Top := Y;
  Bottom := Top + H;
  Width := W;
  Height := H;
  ReAlign(W,H); //px-waarden invullen..(form-coordinaten in pixels)
  //
  ColorR := r;
  ColorG := g;
  ColorB := b;
  ColorA := a;
  //
  fVisible := true;
  // knoppen
  fTotalSysButtons := 0;
  // minimize/maximize Button[0]
  if fHUDButtonsSwappedX then bx := pxLeft+c_HUDMARGIN
                         else bx := Width-c_HUDMARGIN-(2*c_HUDBUTTONWIDTH);
  AddSysButton('', bx,0,c_HUDBUTTONWIDTH,c_HUDBUTTONHEIGHT, 0,0,0,1, 'button_minimize.tga');
  SysButtons[0].OnUp := Minimize;
(*
    // Button[1]
  if fHUDButtonsSwappedX then bx := pxLeft+c_HUDMARGIN+(c_HUDBUTTONWIDTH+c_HUDBUTTONSPACING)
                         else bx := Width-c_HUDMARGIN-(c_HUDBUTTONWIDTH+c_HUDBUTTONSPACING);
  AddButton('', bx,0,c_HUDBUTTONWIDTH,c_HUDBUTTONHEIGHT, 0,0,0,1, 'button_maximize.tga');
  Buttons[1].OnUp := Maximize;
  Inc(fTotalHUDButtons);
*)
end;

destructor TEntityHUD.Destroy;
var b: integer;
begin
  if GFX_FPS<>nil then GFX_FPS.Free;     // TEntityGFX_FPS;
  if GFX_Radar<>nil then GFX_Radar.Free; // TEntityGFX_Radar
  //
  for b:=Low(ButtonSets) to High(ButtonSets) do ButtonSets[b].Free;
  SetLength(ButtonSets, 0); // de array met button-sets (voor elke entity een set)
  //
  for b:=Low(SysButtons) to High(SysButtons) do SysButtons[b].Free;
  SetLength(SysButtons, 0); // de array met HUD-sysmenu-knoppen
  //
  inherited;
end;

//property
procedure TEntityHUD.SetVisible(const Value: boolean);
begin
  if Value=fVisible then Exit;
  fVisible := Value;
end;

procedure TEntityHUD.ReAlign(const NewWidth, NewHeight: integer);
var al,at,ar,ab, w,h: boolean;
    ButtonsWidth: integer;
begin
  al := (Alignment and hudAlignLeft)>0;
  ar := (Alignment and hudAlignRight)>0;
  at := (Alignment and hudAlignTop)>0;
  ab := (Alignment and hudAlignBottom)>0;
  w := ((NewWidth = -1) or (NewWidth = 0));
  h := ((NewHeight = -1) or (NewHeight = 0));
  ButtonsWidth := fTotalSysButtons * (c_HUDBUTTONWIDTH+c_HUDBUTTONSPACING);

  if al and ar then
    if w then Width := OGL.Width     //!! heeft waarde 0 !! :S
         else Width := NewWidth;
  if al then begin
    pxLeft := 0;
    pxRight := pxLeft + Width;
    EndOffsetX := -Width + (8 + c_HUDMARGIN + ButtonsWidth){24};
  end;
  if ar then begin
    pxRight := NewWidth;
    pxLeft := pxRight - Width;
    EndOffsetX := Width - (8 + c_HUDMARGIN + ButtonsWidth);
  end;
  // left + right
  if not (al or ar) then begin
    pxLeft := Left;
    pxRight := Right;
    EndOffsetX := 0;
  end;

  if at and ab then
    if h then Height := OGL.Height
         else Height := NewHeight;
  if at then begin
    pxTop := 0;
    pxBottom := pxTop + Height;
    EndOffsetY := -Height + c_HUDTITLEBARHEIGHT;
  end;
  if ab then begin
    pxBottom := NewHeight;
    pxTop := pxBottom - Height;
    EndOffsetY := Height - c_HUDTITLEBARHEIGHT;
  end;
  // top + bottom
  if not (at or ab) then begin
    pxTop := Top;
    pxBottom := Bottom;
    EndOffsetY := Height - 16;
  end;
end;


function TEntityHUD.OverHUD(const X, Y: integer): boolean;
begin
  // test of de coordinaten in de HUD vallen
  Result := ((X>=pxLeft) and (X<=pxRight) and (Y>=pxTop) and (Y<=pxBottom));
end;

function TEntityHUD.OverSysButton(const X, Y: integer): integer;
var b: integer;
begin
  Result := -1;
  if not OverHUD(X,Y) then Exit;
  // controleer de HUD sysmenu knoppen
  for b:=Low(SysButtons) to High(SysButtons) do
    if SysButtons[b].OverButton(X,Y) then begin
      Result := b;
      Exit;
    end;
end;

function TEntityHUD.OverButton(const X, Y: integer): integer;
var b: integer;
begin
  Result := -1;
  if not OverHUD(X,Y) then Exit;
  if ButtonsPtr=nil then Exit;
  // controleer de HUD entity-specifieke knoppen
  for b:=Low(ButtonsPtr.Buttons) to High(ButtonsPtr.Buttons) do
    if ButtonsPtr.Buttons[b].OverButton(X,Y) then begin
      Result := b;
      Exit;
    end;
end;

function TEntityHUD.OverFixedButton(const X, Y: integer): integer;
var b: integer;
begin
  Result := -1;
  if not OverHUD(X,Y) then Exit;
  if Length(ButtonSets)=0 then Exit;
  // controleer de HUD entity-specifieke knoppen
  for b:=Low(ButtonSets[0].Buttons) to High(ButtonSets[0].Buttons) do
    if ButtonSets[0].Buttons[b].OverButton(X,Y) then begin
      Result := b;
      Exit;
    end;
end;


//!!!!! SysButton... /oude code  /fixit
function TEntityHUD.ButtonByCaption(const aCaption: string): TEntityButton;
var b: integer;
begin
  Result := nil;
  for b:=Low(SysButtons) to High(SysButtons) do
    if SysButtons[b].Name = aCaption then begin
      Result := SysButtons[b];
      Exit;
    end;
end;

procedure TEntityHUD.Minimize;
var al,at,ar,ab: boolean;
begin
  if Minimized or Minimizing or Maximizing then Exit;

  al := (Alignment and hudAlignLeft)>0;
  ar := (Alignment and hudAlignRight)>0;
  at := (Alignment and hudAlignTop)>0;
  ab := (Alignment and hudAlignBottom)>0;
  //
  if al then DeltaOffsetX := -1 else DeltaOffsetX := 0;
  if ar then DeltaOffsetX := DeltaOffsetX+1;
  if at then DeltaOffsetY := -1 else DeltaOffsetY := 0;
  if ab then DeltaOffsetY := DeltaOffsetY+1;
  //
  if (DeltaOffsetX=0) and (DeltaOffsetY=0) then DeltaOffsetY := 1;
  Minimizing := true;
(*
  // een eventuele GFX_FPS onzichtbaar maken
  if GFX_FPS <> nil then GFX_FPS.Visible := false;
*)
end;

procedure TEntityHUD.Maximize;
var al,at,ar,ab: boolean;
begin
  if Maximized or Minimizing or Maximizing then Exit;
  al := (Alignment and hudAlignLeft)>0;
  ar := (Alignment and hudAlignRight)>0;
  at := (Alignment and hudAlignTop)>0;
  ab := (Alignment and hudAlignBottom)>0;
  //
  if al then DeltaOffsetX := 1 else DeltaOffsetX := 0;
  if ar then DeltaOffsetX := DeltaOffsetX-1;
  if at then DeltaOffsetY := 1 else DeltaOffsetY := 0;
  if ab then DeltaOffsetY := DeltaOffsetY-1;
  //
  if (DeltaOffsetX=0) and (DeltaOffsetY=0) then DeltaOffsetY := 1;
  Maximizing := true;

  // een eventuele GFX_* zichtbaar maken
  if GFX_FPS <> nil then GFX_FPS.Visible := true;
  if GFX_Radar <> nil then GFX_Radar.Visible := true;
end;

procedure TEntityHUD.HandleMiniMaxmize;
begin
  if DeltaOffsetX<>0 then begin
    OffsetX := OffsetX + DeltaOffsetX;
    if Minimizing then begin
      if OffsetX=EndOffsetX then begin
        DeltaOffsetX:=0;
        Minimizing := false;
        Minimized := true;
        Maximized := false;
        SysButtons[0].OnUp := Maximize;
        // een eventuele GFX_* onzichtbaar maken
        if GFX_FPS <> nil then GFX_FPS.Visible := false;
        if GFX_Radar <> nil then GFX_Radar.Visible := false;
      end;
    end else
    if Maximizing then
      if OffsetX=0 then begin
        DeltaOffsetX:=0;
        Maximizing := false;
        Minimized := false;
        Maximized := true;
        SysButtons[0].OnUp := Minimize;
      end;
  end;
  if DeltaOffsetY<>0 then begin
    OffsetY := OffsetY + DeltaOffsetY;
    if Minimizing then begin
      if OffsetY=EndOffsetY then begin
        DeltaOffsetY:=0;
        Minimizing := false;
        Minimized := true;
        Maximized := false;
        SysButtons[0].OnUp := Maximize;
        // een eventuele GFX_* onzichtbaar maken
        if GFX_FPS <> nil then GFX_FPS.Visible := false;
        if GFX_Radar <> nil then GFX_Radar.Visible := false;
      end;
    end else
    if Maximizing then
      if OffsetY=0 then begin
        DeltaOffsetY:=0;
        Maximizing := false;
        Minimized := false;
        Maximized := true;
        SysButtons[0].OnUp := Minimize;
      end;
  end;
end;



procedure TEntityHUD.AddSysButton(const aCaption: string;
                                  const X, Y, W, H: integer;
                                  const r, g, b, a: single;
                                  const aImage: string);
var Len: integer;
begin
  Len := Length(SysButtons);
  SetLength(SysButtons, Len+1);
  SysButtons[Len] := TEntityButton.Create(self, aCaption, X,Y,W,H, r,g,b,a, aImage);
  Inc(fTotalSysButtons);
end;

procedure TEntityHUD.CreateButtonSet(const aName: string;
                                     const AssignedClassType_: cardinal);
var Len: integer;
begin
  // maak een nieuwe buttonset
  Len := Length(ButtonSets);
  SetLength(ButtonSets, Len+1);
  ButtonSets[Len] := TEntityButtons.Create(self, Len, aName, AssignedClassType_);
end;

procedure TEntityHUD.AssignEntity(Entity: PEntity);
var bs, b,u, N: integer;
begin
  // ontkoppel de HUD-buttons (niks geselecteerd)
  if Entity=nil then begin
    ButtonsPtr := nil;
    Exit;
  end;

  if Entity^.Team<>0 then Exit; // alleen eigen entities HUD-buttons afbeelden

  // zoek de bijbehorende buttonset, en activeer die in deze HUD
  for bs:=Low(ButtonSets) to High(ButtonSets) do begin
    if ButtonSets[bs].AssignedClassType <> Entity^.ClassType_ then Continue;
    ButtonsPtr := @ButtonSets[bs];
    ButtonsPtr.AssignedEntity := Entity;

    // Game.ConstructingEntity bevat de entity welke iets bouwt
    Game.ConstructingEntity := Entity;

    // nu nog de knoppen aan/uit-schakelen voor deze entity..
    case Entity^.ClassType_ of
      cltEntityOreMiner: begin
        // Enabled & Done
        N := Length(ButtonsPtr.Buttons);
        u := (Entity^ as TEntityOreMiner).InstalledUpgrades;
        for b:=0 to N-1 do ButtonsPtr.Buttons[b].Enabled := (u >= b);
        for b:=0 to u-1 do ButtonsPtr.Buttons[b].Done := true;
        // Percentage
//!!!!!DEBUG!!!!! hudindex ophalen
        for b:=0 to N-1 do ButtonsPtr.Buttons[b].Percentage := fMain.thrConstruct.GetPercentage(0,bs,b,Entity);
      end;

      cltEntityPowerpylon: begin
        // Enabled & Done
        N := Length(ButtonsPtr.Buttons);
        if PEntityPowerpylon(Entity)^.ConnectedTo1 <> nil then begin
          for b:=0 to N-1 do ButtonsPtr.Buttons[b].Enabled := false;
          for b:=0 to N-1 do ButtonsPtr.Buttons[b].Done := true;
//          Game.ConstructingEntity := nil; // bouwen is nu ongeldig
        end else
          for b:=0 to N-1 do ButtonsPtr.Buttons[b].Enabled := true;
        // Percentage
//!!!!!DEBUG!!!!! hudindex ophalen
        for b:=0 to N-1 do ButtonsPtr.Buttons[b].Percentage := fMain.thrConstruct.GetPercentage(0,bs,b,Entity);
      end;
    end;

    Exit;
  end;
end;


procedure TEntityHUD.AddGFXFPS(const aCaption: string;
                               const X, Y, W, H: integer;
                               const r, g, b, a: single;
                               const NrOfFrames: integer);
begin
  // instantie aanmaken van een FPS-grafiek
  GFX_FPS := TEntityGFX_FPS.Create(self, aCaption, X,Y,W,H, r,g,b,a);
end;

procedure TEntityHUD.AddGFXFPS(const aCaption: string);
var Len: integer;
    x,y,h: integer;
begin
  // voeg de FPS-grafiek toe ONDER de laagste rij knoppen
  if ButtonsPtr=nil then Len:=0
                    else Len := ButtonsPtr.TotalButtons;
  if Len>0 then begin
    x := c_HUDMARGIN;
    y := ButtonsPtr.Buttons[Len-1].Top;
    y := y + c_BUTTONHEIGHT + c_BUTTONSPACING; //op de volgende rij plaatsen..
    if y > Height-c_HUDMARGIN-c_BUTTONHEIGHT then Exit; // past niet meer..
  end else begin
    x := c_HUDMARGIN;
    y := c_HUDTITLEBARHEIGHT + c_HUDMARGIN;
  end;
  h := Min(2*c_BUTTONHEIGHT, Height-2*c_HUDMARGIN-y);
  AddGFXFPS(aCaption, x,y,100,h, 0,0,0,1, 100);
end;



procedure TEntityHUD.AddGFXRadar(const aCaption: string;
                                 const X,Y, W,H: integer;
                                 const r,g,b,a: single);
begin
  // instantie aanmaken van een Radar-beeld
  GFX_Radar := TEntityGFX_Radar.Create(self, aCaption, X,Y,W,H, r,g,b,a);
end;

procedure TEntityHUD.AddGFXRadar(const aCaption: string);
var Len: integer;
    x,y: integer;
begin
(*
  // voeg de FPS-grafiek toe ONDER de laagste rij knoppen
  if ButtonsPtr=nil then Len:=0
                    else Len := ButtonsPtr.TotalButtons;
  if Len>0 then begin
    x := c_HUDMARGIN;
    y := ButtonsPtr.Buttons[Len-1].Top;
    y := y + c_BUTTONHEIGHT + c_BUTTONSPACING; //op de volgende rij plaatsen..
    if y > Height-c_HUDMARGIN-c_BUTTONHEIGHT then Exit; // past niet meer..
  end else begin
*)
    x := c_HUDMARGIN;
    y := c_HUDTITLEBARHEIGHT + c_HUDMARGIN;
(*
  end;
*)  
  AddGFXRadar(aCaption, x,y,96,96, 0,0,0,1);
end;








{--- TEntityGame --------------------------------}
constructor TEntityGame.Create(const aGamename: string);
begin
  Classname_ := 'clEntityGame';
  ClassType_ := cltEntityGame;

  SetLength(Teams, 0);
  SetLength(Resources, 0);
  SetLength(HUD, 0);
  ShowSelectionMarks := true;
  ShowBoundingBoxes := true;
  ShowRoutes := false;
  ShowTiles := false;
  ShowTerrain := true;
  ShowFog := false;

  isConstructing := false;
end;

destructor TEntityGame.Destroy;
begin
  SetLength(Resources, 0);
  SetLength(Teams, 0);
  //
  if Assigned(Map) then Map.Free;
  Map := nil;
  //
  SetLength(HUD, 0);
end;

procedure TEntityGame.AddTeam(const aTeamname: string;
                              const aColor: Tcolor);
var Len: integer;
begin
  Len := Length(Teams);
  SetLength(Teams,Len+1); // array of TEntitySpawnPoint;
  Teams[Len] := TEntityTeam.Create(aTeamname,aColor);
end;

procedure TEntityGame.DeleteTeam(const aTeamname: string);
var Len,
    Index,
    i: integer;
begin
  Len := Length(Teams);
  Index := -1;
  for i:=Low(Teams) to High(Teams) do
    if Teams[i].Name = aTeamname then begin
      Index := i;
      Break;
    end;
  if Index = -1 then Exit;
  // verwissel Index met de laatste..dan de laatste verwijderen.
  Teams[Index] := Teams[Len];
  SetLength(Teams,Len-1); // array of TEntityTeam;
end;

procedure TEntityGame.DeleteTeam(const aColor: Tcolor);
var Len,
    Index,
    i: integer;
begin
  Len := Length(Teams);
  Index := -1;
  for i:=Low(Teams) to High(Teams) do
    if Teams[i].Color = aColor then begin
      Index := i;
      Break;
    end;
  if Index = -1 then Exit;
  // verwissel Index met de laatste..dan de laatste verwijderen.
  Teams[Index] := Teams[Len];
  SetLength(Teams,Len-1); // array of TEntityTeam
end;

procedure TEntityGame.DeleteTeam(const aIndex: integer);
var Len: integer;
begin
  if aIndex = -1 then Exit;
  Len := Length(Teams);
  if Len <= 0 then Exit;
  if aIndex >= Len then Exit;
  // verwissel Index met de laatste..dan de laatste verwijderen.
  Teams[aIndex] := Teams[Len];
  SetLength(Teams,Len-1); // array of TEntityTeam
end;


procedure TEntityGame.CreateMap(const Mapname: string);
begin
  FreeMap;
  Map := TEntityMap.Create('<MAP NAME>');
end;

procedure TEntityGame.FreeMap;
begin
  Map.Free;
  Map := nil;
end;

procedure TEntityGame.SetMap(const aMap: PEntityMap);
begin
  if not Assigned(aMap) then Exit;
  Map := aMap^;
end;


procedure TEntityGame.AddHUD(const aHUDIndex: integer; //fThisHUDIndex
                             const aName: string;
                             const aAlignment: Byte;   //bitmask
                             const X, Y, W, H: integer;
                             const r, g, b, a: single;
                             const aImage: string);
var Len: integer;
begin
  Len := Length(HUD);
  SetLength(HUD, Len+1);
  HUD[Len] := TEntityHUD.Create(aHUDIndex, aName,aAlignment,X,Y,W,H,r,g,b,a,aImage);
end;

procedure TEntityGame.ReAlignHUDs(const NewWidth, NewHeight: integer);
var h: integer;
begin
//  if Length(HUD)=0 then Exit;
  for h:=Low(HUD) to High(HUD) do
    Game.HUD[h].ReAlign(NewWidth, NewHeight);
end;

function TEntityGame.OverHUD(const X, Y: integer): integer;
var h: integer;
begin
  Result := -1;
  for h:=Low(HUD) to High(HUD) do
    if HUD[h].OverHUD(X,Y) then begin
      Result := h;
      Exit;
    end;
end;

procedure TEntityGame.StoreCurrentFPS;
var h: integer;
begin
  // alle HUDs doorlopen en huidige FPS laten opslaan tbv de FPS-grafiek.
  for h:=0 to Length(HUD)-1 do
    if HUD[h].GFX_FPS<>nil then
      HUD[h].GFX_FPS.StoreCurrentFPS;
end;


procedure TEntityGame.AddResource(const aName: string;
                                  const aColor: Tcolor;
                                  const aImageFilename, aIconFilename: string;
                                  const aPrice: single);
var Len: integer;
begin
  // een resource toevoegen..
  Len := Length(Resources);
  SetLength(Resources, Len+1);
  Resources[Len] := TEntityResource.Create(aName,aColor,aImageFilename,aIconFilename,aPrice);
end;

procedure TEntityGame.ServerFrame;
var u: integer;
    Entity: TEntity;
begin
  // elke timertick komt dit event aan de orde..
  for u:=Low(Map.Units) to High(Map.Units) do begin
    Entity := Map.Units[u];
    if Entity.canMove then  // is het een Mover?
      if not (Entity as TEntityMover).isMoving then
        if (Entity as TEntityMover).hasAI_move then
          if (Entity as TEntityMover).useAI_move then begin
            // laat deze entity bewegen..
            (Entity as TEntityMover).AI_move;
            {case Entity.ClassType_ of
              cltEntityOreMiner: (Entity as TEntityOreMiner).AI_move;
            end;}
          end;
  end;
end;


procedure TEntityGame.StartConstructing(const aClassType: cardinal);
begin
  // al bezig met bouwen??
  if isConstructing then StopConstructing;

  // niks selecteren
  Selection.InvalidateSelectionRect;
  Selection.NoSelectedUnits;
  Selection.NoSelectedSpawnpoints;
  Selection.NoSelectedPowerpylons;
  HUD[0].ButtonsPtr := nil;

  isConstructing := true;
  ConstructionClassType := aClassType;
  Selection.AllowSelecting := false;
end;

procedure TEntityGame.StopConstructing;
begin
  isConstructing := false;
  Selection.InvalidateSelectionRect;
  Selection.AllowSelecting := true;
end;

procedure TEntityGame.ConstructBuilding(const MouseX,MouseY: integer);
begin
  // plaats een gebouw
  case ConstructionClassType of
    cltEntitySpawnpoint: GameTech.SpawnNewSpawnpoint(MouseX,MouseY);
    cltEntityPowerpylon: GameTech.SpawnNewPowerpylon(MouseX,MouseY);
    cltEntityPowerline:  GameTech.SpawnNewPowerline(MouseX,MouseY);
  end;
end;



initialization
{--- }
finalization
  Game.Free;
  Game := nil;

end.
