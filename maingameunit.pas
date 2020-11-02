unit MainGameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$ifndef cgeapp}
  Forms, Controls, Graphics, Dialogs, CastleControl,
  {$else}
  CastleWindow,
  {$endif}
  CastleCameras, CastleApplicationProperties, CastleLog,
  CastleSceneCore, CastleVectors, CastleScene, CastleViewport,
  X3DNodes, CastleTimeUtils;

type

  { TCastleApp }

  {$ifndef cgeapp}
  TCastleApp = class(TForm)
    Window: TCastleControlBase;
    procedure WindowBeforeRender(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  {$else}
  TCastleApp = class(TWindowContainer)
  {$endif}
  private
  public
    procedure RunCGEApplication(Sender: TObject);
    procedure LoadScene(Sender: TObject; filename: String);
  end;

var
  {$ifndef cgeapp}
  CastleApp: TCastleApp;
  {$endif}
  Viewport: TCastleViewport;
  Scene: TCastleScene;

  {$ifdef cgeapp}
  procedure WindowBeforeRender(Sender: TUIContainer);
  {$endif}
implementation

{$R *.lfm}

{ TCastleApp }

{$ifdef cgeapp}
procedure WindowBeforeRender(Sender: TUIContainer);
{$else}
procedure TCastleApp.WindowBeforeRender(Sender: TObject);
{$endif}
const
  // How many seconds to take to rotate the scene
  SecsPerRot = 4;
var
  theta: Single;
begin
  // Set angle (theta) to revolve completely once every SecsPerRot
  theta := ((CastleGetTickCount64 mod
            (SecsPerRot * 1000)) /
            (SecsPerRot * 1000)) * (Pi * 2);

  // Rotate the scene in Y
  // Change to Vector4(1, 0, 0, theta); to rotate in X

  Scene.Rotation := Vector4(0, 1, 0, theta);
end;

procedure TCastleApp.LoadScene(Sender: TObject; filename: String);
begin
  // Set up the main viewport
  Viewport := TCastleViewport.Create(Application);
  // Use all the viewport
  Viewport.FullSize := true;
  // Automatically position the camera
  Viewport.AutoCamera := true;
  // Use default navigation keys
  Viewport.AutoNavigation := true;

  // Add the viewport to the CGE control
  {$ifndef cgeapp}
  Window.Controls.InsertFront(Viewport);
  {$else}
  TCastleWindowBase(Sender).Controls.InsertFront(Viewport);
  {$endif}

  Scene := TCastleScene.Create(Application);
  // Load a model into the scene
  Scene.load(filename);
  // Add the scene to the viewport
  Viewport.Items.Add(Scene);

  // Tell the control this is the main scene so it gets some lighting
  Viewport.Items.MainScene := Scene;
end;

procedure TCastleApp.RunCGEApplication(Sender: TObject);
begin
  // Textured cube designed for rotation in Y
  // Change the model to box_rotx.x3dv for the X rotation version
  LoadScene(Sender, 'castle-data:/box_roty.x3dv');
end;

{$ifndef cgeapp}
procedure TCastleApp.FormCreate(Sender: TObject);
begin
  Caption := 'Trivial CGE Lazarus Application';
  RunCGEApplication(Sender);
end;
{$endif}

end.

