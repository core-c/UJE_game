unit uKeys;
interface

procedure CheckSpecialKeys;

implementation
uses windows, uOpenGL, uEntities, uDisplay;

procedure CheckSpecialKeys;
// Test de status van bepaalde toetsen (ingedrukt/uitgedrukt).
// De toetsen voor besturing in de 3D-wereld gaan nl. repeteren als ze even worden
// ingehouden. Besturing gaat dan niet zoals gewenst.
// Toetsen die niets met de besturing te maken hebben (bv. toggle-toetsen) worden
// in de procedure niet getest omdat het niet van belang is of deze repeteren.
// GetKeyState levert een byte terug waarvan het hoogste bit aangeeft of een toets
// is ingedrukt of uitgedrukt op het moment van aanroep van GetKeyState.
// Als bit 7 is gezet dan is de toets ingedrukt, anders uitgedrukt.
// Bit 0 geeft aan of er een toets-toggle is voorgevallen.
// !NB: hoofdletters gebruiken bij testen van toetsen (indien van toepassing)
var speed: Single;
//    CtrlPressed: boolean;
begin
  speed := OGL.Camera.Speed / c_TILESIZE;
  // niet op botsingen controleren..
  if (GetKeyState(Ord('W')) and $80)<>0 then OGL.Camera.Move(speed);
  if (GetKeyState(Ord('S')) and $80)<>0 then OGL.Camera.Move(-speed);
  if (GetKeyState(Ord('A')) and $80)<>0 then OGL.Camera.Strafe(speed);
  if (GetKeyState(Ord('D')) and $80)<>0 then OGL.Camera.Strafe(-speed);
(*
  // groepen maken en selecteren..
  CtrlPressed := (((GetKeyState(VK_LCONTROL) and $80)<>0) or ((GetKeyState(VK_RCONTROL) and $80)<>0));
  if CtrlPressed then begin
    if (GetKeyState(Ord('1')) and $80)<>0 then
  end;
*)  
end;

end.
 