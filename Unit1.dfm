object fMain: TfMain
  Left = 464
  Top = 117
  Width = 532
  Height = 424
  Caption = 'Main'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCanResize = FormCanResize
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TimerServerFrame: TTimer
    Enabled = False
    Interval = 50
    OnTimer = TimerServerFrameTimer
    Left = 8
    Top = 8
  end
  object TimerFPS: TTimer
    OnTimer = TimerFPSTimer
    Left = 40
    Top = 8
  end
  object ApplicationEvents1: TApplicationEvents
    OnMinimize = ApplicationEvents1Minimize
    OnRestore = ApplicationEvents1Restore
    Left = 8
    Top = 40
  end
end
