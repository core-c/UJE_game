unit uShader;
interface
uses OpenGL, uOpenGL;

type
  TObjShader = class(TObject)
  private
    OGL: Pointer; //POGL;             // het OGL parent object
    // test
    my_vertex_shader_source,
    my_fragment_shader_source: PGLcharARB;
    my_program,
    my_vertex_shader,
    my_fragment_shader: GLenum;
  public
    constructor Create(OGL_parent: Pointer); //POGL
    destructor Destroy; override;
    //
    procedure Init;
    procedure test_on;
    procedure test_off;
  end;

var ShaderObj: TObjShader;

implementation
uses Classes, SysUtils;

{ TObjShader }
constructor TObjShader.Create(OGL_parent: Pointer);
begin
  inherited Create;
  OGL := OGL_parent;
end;

destructor TObjShader.Destroy;
begin
  //
  inherited;
end;

procedure TObjShader.Init;
var Fv,Ff: TFileStream;
    BufferV, BufferF: array[0..4095] of GLcharARB;
begin
  Fv := TFileStream.Create('E:\Program Files\Borland\Delphi7\C\UJE_game\shaders\ambient.v', fmOpenRead);
  Ff := TFileStream.Create('E:\Program Files\Borland\Delphi7\C\UJE_game\shaders\ambient.f', fmOpenRead);
  //Fv := TFileStream.Create('E:\Program Files\Borland\Delphi7\C\UJE_game\shaders\diffuse.v', fmOpenRead);
  //Ff := TFileStream.Create('E:\Program Files\Borland\Delphi7\C\UJE_game\shaders\diffuse.f', fmOpenRead);
  try
    // Get Vertex Shader Sources
    Fv.ReadBuffer(BufferV, Fv.Size);
    my_vertex_shader_source := @BufferV[0];

    // Get Fragment Shader Sources
    Ff.ReadBuffer(BufferF, Ff.Size);
    my_fragment_shader_source := @BufferF[0];

    // Create Shader And Program Objects
    my_program := glCreateProgramObject;
    my_vertex_shader := glCreateShaderObject(GL_VERTEX_SHADER_ARB);
    my_fragment_shader := glCreateShaderObject(GL_FRAGMENT_SHADER_ARB);

    // Load Shader Sources
    glShaderSource(my_vertex_shader, 1, @my_vertex_shader_source, nil);
    glShaderSource(my_fragment_shader, 1, @my_fragment_shader_source, nil);

    // Compile The Shaders
    glCompileShader(my_vertex_shader);
    glCompileShader(my_fragment_shader);

    // Attach The Shader Objects To The Program Object
    glAttachObject(my_program, my_vertex_shader);
    glAttachObject(my_program, my_fragment_shader);

    // Link The Program Object
    glLinkProgram(my_program);
(*
    // Use The Program Object Instead Of Fixed Function OpenGL
    glUseProgramObject(my_program);
*)
  finally
    Fv.Free;
    Ff.Free;
  end;
end;

procedure TObjShader.test_on;
begin
  // Use The Program Object Instead Of Fixed Function OpenGL
  glUseProgramObject(my_program);
end;

procedure TObjShader.test_off;
begin
  // Use The Program Object Instead Of Fixed Function OpenGL
  glUseProgramObject(0);
end;


initialization
  ShaderObj := TObjShader.Create(@OGL);
finalization
  ShaderObj.Free;

end.
