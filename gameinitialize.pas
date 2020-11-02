unit GameInitialize;

{$mode objfpc}{$H+}

interface

implementation

uses
  CastleWindow, CastleScene, CastleControls, CastleLog, 
  CastleApplicationProperties, MainGameUnit;

var
  Window: TCastleWindowBase;
  CastleApp: TCastleApp;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  CastleApp := TCastleApp.Create(Window);
  CastleApp.RunCGEApplication(Window);
  Window.onBeforeRender := @WindowBeforeRender;
  Window.onClose := @WindowClose;
  Window.onMotion := @WindowMotion;
  Window.onOpen := @WindowOpen;
  Window.onPress := @WindowPress;
  Window.onRelease := @WindowRelease;
  Window.onRender := @WindowRender;
  Window.onResize := @WindowResize;
  Window.onUpdate := @WindowUpdate;
  Window.Caption := 'Trivial CGE Standalone Application';
end;

initialization
  { Set ApplicationName early, as our log uses it.
    Optionally you could also set ApplicationProperties.Version here. }
  ApplicationProperties.ApplicationName := 'Trivial';

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization.

    For programs, InitializeLog is not called here.
    Instead InitializeLog is done by the program main file,
    after command-line parameters are parsed. }
  if IsLibrary then
    InitializeLog;

  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (because in case of non-desktop platforms,
    some necessary resources may not be prepared yet). }

end.

