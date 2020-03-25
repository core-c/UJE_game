program uje_game;
{%ToDo 'uje_game.todo'}
uses
  Forms,
  Unit1 in 'Unit1.pas' {fMain},
  uEntities in 'uEntities.pas',
  uDisplay in 'uDisplay.pas',
  uKeys in 'uKeys.pas',
  Windows,
  Messages,
  uOpenGL,
  uWavefront in 'uWavefront.pas',
  uThreadMove in 'uThreadMove.pas',
  uSelection,
  uGameTech in 'uGameTech.pas',
  uThreadConstruct in 'uThreadConstruct.pas',
  uShader in 'uShader.pas';

{$A8}
{$R *.res}
var Msg: TMsg;
    AppTerminated: Boolean;

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  //  Application.Run;

  fMain.Visible := True; // Zelf het MainForm zichtbaar maken
  AppTerminated := false;
  repeat
    if (PeekMessage(Msg,0,0,0,PM_REMOVE)) then begin //een msg voor dit venster?
      if (Msg.message = WM_QUIT) then                //stoppen bij een WM_QUIT msg..
        AppTerminated := true
      else begin                                     //de msg verwerken
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end else begin
      // De routines die ik in de mainloop wil uitvoeren..
//!      if Application.Active then begin
        if OGL.Active and (not OGL.Paused) then begin
          // Een frame tekenen
          Display.RenderFrame;
          // speciale toetsen status testen..
          CheckSpecialKeys;
        end;
//!      end {else
//!        Application.ProcessMessages};


    end;
  until AppTerminated;
end.
