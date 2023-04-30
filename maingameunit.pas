unit MainGameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$ifndef cgeapp}
  Forms, Controls, Graphics, Dialogs, CastleControl, CastleLCLUtils,
  {$else}
  CastleWindow,
  {$endif}
  CastleControls, CastleColors, CastleUIControls,
  CastleTriangles, CastleShapes, CastleVectors,
  CastleCameras, CastleApplicationProperties, CastleLog,
  CastleSceneCore, CastleScene, CastleViewport,
  X3DNodes, X3DFields, X3DTIme,
  CastleImages, CastleTimeUtils, CastleKeysMouse;

type

  TMyEventListener = class(TComponent)
    procedure ReceivedIsActive(const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  end;

  { TCastleApp }

  {$ifndef cgeapp}
  TCastleApp = class(TForm)
    Window: TCastleControlBase;
    procedure FormDestroy(Sender: TObject);
    procedure WindowBeforeRender(Sender: TObject);
    procedure WindowClose(Sender: TObject);
    procedure WindowMotion(Sender: TObject; const Event: TInputMotion);
    procedure WindowOpen(Sender: TObject);
    procedure WindowPress(Sender: TObject; const Event: TInputPressRelease);
    procedure WindowRelease(Sender: TObject; const Event: TInputPressRelease);
    procedure WindowRender(Sender: TObject);
    procedure WindowResize(Sender: TObject);
    procedure WindowUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  {$else}
  TCastleApp = class(TWindowContainer)
  {$endif}
  private
    Viewport: TCastleAutoNavigationViewport;
    Scene: TCastleScene;
    ColorChoice: Integer;
    MasterTexture: TRGBAlphaImage;
    MasterMetalTexture: TRGBAlphaImage;
    LabelSpare: TCastleLabel;
    LabelTimer: TCastleLabel;
    LabelLast: TCastleLabel;
    LabelFPS: TCastleLabel;
    LabelClick: TCastleLabel;
    LabelRender: TCastleLabel;
    DoingRecolor: Boolean;
    RecolorTime: Int64;
    LastTime: Int64;
    CacheTime: Int64;
    RecolorCount: Int64;
    GLIsReady: Boolean;
    NewColor: TVector4Byte;
    EventListener: TMyEventListener;
    LabelT1: TCastleLabel;
    LabelT2: TCastleLabel;
    evt_t2: Integer;
  public
    procedure RunCGEApplication(Sender: TObject);
    procedure KillCGEApplication(Sender: TObject);
    procedure LoadScene(Sender: TObject; filename: String);
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True);
    function ChangeTexture(const Node: TX3DRootNode; const Texture: TCastleImage): TVector3Cardinal;
    function RecolorImage(const ImageIn: TRGBAlphaImage; const NewRGB: TVector4Byte): TRGBAlphaImage;
    function LoadMasterTexture(filename: String): TRGBAlphaImage;
    procedure SetSensor(AScene: TCastleScene; AColor: TVector4Byte);
    procedure PaintJob;
  end;

{$ifndef cgeapp}
var
  CastleApp: TCastleApp;
{$endif}

{$ifdef cgeapp}
procedure WindowBeforeRender(Sender: TUIContainer);
procedure WindowClose(Sender: TUIContainer);
procedure WindowMotion(Sender: TUIContainer; const Event: TInputMotion);
procedure WindowOpen(Sender: TUIContainer);
procedure WindowPress(Sender: TUIContainer; const Event: TInputPressRelease);
procedure WindowRelease(Sender: TUIContainer; const Event: TInputPressRelease);
procedure WindowRender(Sender: TUIContainer);
procedure WindowResize(Sender: TUIContainer);
procedure WindowUpdate(Sender: TUIContainer);
{$endif}

function Vec4BtoInt(const AValue: TVector4Byte): Cardinal;

implementation
{$ifdef cgeapp}
uses GameInitialize;
{$endif}

{$ifndef cgeapp}
{$R *.lfm}
{$endif}
{
*** Start of sensor related code ***
}
{
Taken from example and slightly modified
}
procedure TMyEventListener.ReceivedIsActive(const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  AColor: Cardinal;
  Val: Boolean;
begin
  with Owner as TCastleApp do
    begin
      // Need to get to the owning Sensor object
      if Event.ParentNode is TTouchSensorNode then
        begin
        Val := (Value as TSFBool).Value;
        if Val then
          begin
          // Find out the currnt color of the model
          AColor := TTouchSensorNode(Event.ParentNode).MetadataInteger['Color'];
          LabelT2.Caption := Format('isActive %d : %s', [evt_t2, IntToHex(AColor, 6)]);
          Inc(evt_t2);
          // Change color
          PaintJob;
          // Update Meta with new color
          SetSensor(Scene, NewColor);
          end;
        end;
    end;
end;

{
SetSensor - Add a sensor to the model, set event handlers and attach a meta
            containing the current model color as a MetaInteger.
            If sensor already exists just update the MetaInteger.
}
procedure TCastleApp.SetSensor(AScene: TCastleScene; AColor: TVector4Byte);
var
  TransformNode: TTransformNode;
  TouchSensor: TTouchSensorNode;
begin
  if not(AScene = nil) then
    begin
      TransformNode := AScene.RootNode.TryFindNodeByName(TTransformNode, 'HoverRacer', false) as TTransformNode;
      if not (TransformNode = nil) then
        begin
          TouchSensor := TransformNode.TryFindNodeByName(TTouchSensorNode, 'TextureColor', false) as TTouchSensorNode;
          if TouchSensor = nil then
            begin
              TouchSensor := TTouchSensorNode.Create('TextureColor');
              TouchSensor.Enabled := true;
              TouchSensor.EventIsActive.AddNotification(@EventListener.ReceivedIsActive);
              TouchSensor.MetadataInteger['Color'] := Vec4BtoInt(NewColor);
              TransformNode.AddChildren(TouchSensor);
              LabelT1.Caption := 'Sensor set - ' + IntToHex(Vec4BtoInt(NewColor), 6);
            end
          else
            begin
              TouchSensor.MetadataInteger['Color'] := Vec4BtoInt(NewColor);
              LabelT1.Caption := 'Sensor set - ' + IntToHex(Vec4BtoInt(NewColor), 6);
            end;
        end
    else
      LabelT1.Caption := 'Sensor not set';
    end;
end;

{
*** End of sensor related code ***
}

{
RecolorImage - Change White to passed color
}
function TCastleApp.RecolorImage(const ImageIn: TRGBAlphaImage; const NewRGB: TVector4Byte): TRGBAlphaImage;
var
  ImageOut: TRGBAlphaImage;
  PSource, PDest: PVector4Byte;
  x, y: Integer;
  SeekRGB: TVector4Byte;
  ReplaceRGB: TVector4Byte;
begin
  ImageOut := nil;
  SeekRGB := Vector4Byte(255, 255, 255, 255);
  ReplaceRGB := NewRGB;
  if not(ImageIn = nil) then
    begin
      if not(ImageIn.Dimensions.IsZero) then
        begin
          ImageOut := TRGBAlphaImage.Create(ImageIn.Dimensions.X, ImageIn.Dimensions.Y);
          PSource := ImageIn.PixelPtr(0, 0);
          PDest := ImageOut.PixelPtr(0, 0);
          for y := 0 to ImageIn.Dimensions.Y -1 do
            begin
              for x := 0 to ImageIn.Dimensions.X -1 do
                begin
                  if TVector4Byte.Equals(PSource^, SeekRGB) then
                    PDest^ := ReplaceRGB
                  else
                    PDest^ := PSource^;
                  Inc(PSource);
                  Inc(PDest);
                end;
            end;
        end;
    end;
  Result := ImageOut;
end;

{
LoadMasterTexture - Load a texture that's going to be used multiple times
}
function TCastleApp.LoadMasterTexture(filename: String): TRGBAlphaImage;
var
  Texture: TRGBAlphaImage;
begin
  try
    Texture := LoadImage(filename, [TRGBAlphaImage]) as TRGBAlphaImage;
  except
    on E : Exception do
      begin
        {$ifndef cgeapp}
        ShowMessage('Exception' + LineEnding +
                    'Trying to load : ' + filename + LineEnding +
                     E.ClassName + LineEnding +
                     E.Message);
        {$endif}
        Texture := nil;
       end;
  end;
  Result := Texture;
end;

{
ChangeTexture - Changes the texture image used by the model to the recolored version
                Makes a copy of the roughness/metal maps and assigns it to new material
}
function TCastleApp.ChangeTexture(const Node: TX3DRootNode; const Texture: TCastleImage): TVector3Cardinal;
var
  PhysicalMaterialNode: TPhysicalMaterialNode;
  PixelTextureNode: TPixelTextureNode;
  MetalTextureNode: TPixelTextureNode;
  AppearanceNode: TAppearanceNode;
begin
  Result := TVector3Cardinal.Zero;
  AppearanceNode := Node.TryFindNodeByName(TAppearanceNode, 'Glass', false) as TAppearanceNode;
  if not (AppearanceNode = nil) then
  begin
    PixelTextureNode := TPixelTextureNode.Create;
    PixelTextureNode.FdImage.Value := Texture;
    MetalTextureNode := TPixelTextureNode.Create;
    MetalTextureNode.FdImage.Value := MasterMetalTexture.MakeCopy;
    if PixelTextureNode.IsTextureImage then
      begin
        PhysicalMaterialNode := TPhysicalMaterialNode.Create;
        PhysicalMaterialNode.baseTexture := PixelTextureNode;
        PhysicalMaterialNode.baseTextureMapping := 'TEXCOORD_0';
        PhysicalMaterialNode.metallicRoughnessTextureMapping := 'TEXCOORD_0';
        PhysicalMaterialNode.metallicRoughnessTexture := MetalTextureNode;
        AppearanceNode.Material := PhysicalMaterialNode;
        Result := PixelTextureNode.TextureImage.Dimensions;
      end;
  end;
end;

{
CreateLabel - Create a label, set it's position and style
}
procedure TCastleApp.CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True);
begin
  objLabel := TCastleLabel.Create(Application);
  objLabel.Padding := 5;
  objLabel.Color := White;
  objLabel.Frame := True;
  objLabel.FrameColor := Black;
  objLabel.Anchor(hpLeft, 10);
  if BottomUp then
    objLabel.Anchor(vpBottom, 10 + (Line * 35))
  else
    objLabel.Anchor(vpTop, -(10 + (Line * 35)));
  Window.Controls.InsertFront(objLabel);
end;

{
Vec4BtoInt - Convert TVector4Byte to Unsigned 32bit Integer
}
function Vec4BtoInt(const AValue: TVector4Byte): Cardinal;
begin
  Result := (AValue.X << 16) or (AValue.Y << 8) or AValue.Z;
end;

{
Paintjob - Create a new version of the master texture and recolor it
           then apply it to the model
           Report info to screen
}
procedure TCastleApp.PaintJob;
var
  TempImage: TRGBAlphaImage;
  ReColorTimer: Int64;
  CacheTimer: Int64;
begin
  if not (Scene = nil) and not(DoingRecolor) then
    begin
      DoingRecolor := True;
      ReColorTimer := CastleGetTickCount64;
      NewColor := Vector4Byte(random(256), random(256), random(256), 255);
      TempImage := RecolorImage(MasterTexture, NewColor);
      if not(TempImage = nil) then
        begin
          ReColorTimer := CastleGetTickCount64 - ReColorTimer;
          CacheTimer := CastleGetTickCount64;
          ChangeTexture(Scene.RootNode, TempImage);
          CacheTimer := CastleGetTickCount64 - CacheTimer;
        end;
      RecolorTime += ReColorTimer;
      CacheTime += CacheTimer;
      Inc(RecolorCount);
      LabelSpare.Caption := 'ReColor   = ' +
                           FormatFloat('####0.000', ReColorTimer / 1000) +
                           ' s' + LineEnding +
                           'Avg ReCol  = ' +
                           FormatFloat('####0.000', (RecolorTime / RecolorCount) / 1000) +
                           ' s (' + IntToStr(RecolorCount) + ')' +
                            LineEnding +'Apply Tex = ' +
                            FormatFloat('####0.000', CacheTimer / 1000) +
                            ' s' + LineEnding +
                            'Avg Apply  = ' +
                            FormatFloat('####0.000', (CacheTime / RecolorCount) / 1000) +
                            ' s (' + IntToStr(RecolorCount) + ')';

      DoingRecolor := False;
    end;

end;

{
LoadScene - Load the initial model, set a viewport and a load of other one-off
            things
}
procedure TCastleApp.LoadScene(Sender: TObject; filename: String);
var
  TempImage: TRGBAlphaImage;
begin
  // Set up the main viewport
  Viewport := TCastleAutoNavigationViewport.Create(Application);
  // Use all the viewport
  Viewport.FullSize := true;
  // Automatically position the camera
  Viewport.AutoCamera := True;
  // Use default navigation keys
  Viewport.AutoNavigation := False;

  MasterTexture := LoadMasterTexture('castle-data:/HoverRacerReColor.png');
  MasterMetalTexture := LoadMasterTexture('castle-data:/HoverRacerReColor_maps.png');

  // Add the viewport to the CGE control
  {$ifndef cgeapp}
  Window.Controls.InsertFront(Viewport);
  {$else}
  TCastleWindowBase(Sender).Controls.InsertFront(Viewport);
  {$endif}

  Scene := TCastleScene.Create(Application);
  // Load a model into the scene
  Scene.Load(filename);

  TempImage := RecolorImage(MasterTexture, NewColor); // Red at initialization
  ChangeTexture(Scene.RootNode, TempImage);
  Scene.ProcessEvents := true;
  Scene.Spatial := [ssRendering, ssDynamicCollisions];

  // Add the scene to the viewport
  Viewport.Items.Add(Scene);

  // Tell the control this is the main scene so it gets some lighting
  Viewport.Items.MainScene := Scene;

  CreateLabel(LabelTimer, 0, False);
  CreateLabel(LabelLast, 1, False);
  CreateLabel(LabelSpare, 2, False);
  CreateLabel(LabelT1, 4);
  CreateLabel(LabelT2, 3);
  CreateLabel(LabelClick, 2);
  CreateLabel(LabelFPS, 1);
  CreateLabel(LabelRender, 0);
end;

{
RunCGEApplication - Shared initialization
}
procedure TCastleApp.RunCGEApplication(Sender: TObject);
begin
  DoingRecolor := False;
  GLIsReady := False;
  RecolorTime := 0;
  CacheTime := 0;
  RecolorCount := 0;
  LastTime := 0;
  ColorChoice := 0;
  Scene := nil;
  evt_t2 := 0;
  MasterTexture := nil;
  MasterMetalTexture := nil;
  Window.Container.UIScaling := usDpiScale;
  EventListener := TMyEventListener.Create(Self);
  NewColor := Vector4Byte(255, 0, 0, 255); // Default to Red
  LoadScene(Sender, 'castle-data:/HoverRacer.gltf');
end;

{
KillCGEApplication - Shared tidy up
}
procedure TCastleApp.KillCGEApplication(Sender: TObject);
begin
  FreeAndNil(MasterTexture);
  FreeAndNil(MasterMetalTexture);
end;

{
Lazarus only code
}
{$ifndef cgeapp}
procedure TCastleApp.FormCreate(Sender: TObject);
begin
  Caption := 'Hover CGE Lazarus Application';
  RunCGEApplication(Sender);
end;

procedure TCastleApp.FormDestroy(Sender: TObject);
begin
  KillCGEApplication(Sender);
end;
{$endif}

{
Lazarus / Standalone versions of all event handlers
}
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
  {$ifdef cgeapp}with CastleApp do begin{$endif}
  if GLIsReady then
    begin
    LabelTimer.Caption  := 'Timer (ms) = ' + RightStr(IntToStr(CastleGetTickCount64), 6);
    if not(LastTime = 0) then // and ((CastleGetTickCount64 - LastTime) > 100)then
      LabelLast.Caption := 'Elapsed ms = ' + IntToStr(CastleGetTickCount64 - LastTime);
    LastTime := CastleGetTickCount64;

    LabelFPS.Caption := 'FPS = ' + FormatFloat('####0.00', Window.Fps.RealFps);
    LabelRender.Caption := 'Render = ' + FormatFloat('####0.00', Window.Fps.OnlyRenderFps);

    // Set angle (theta) to revolve completely once every SecsPerRot
    theta := ((CastleGetTickCount64 mod
              (SecsPerRot * 1000)) /
              (SecsPerRot * 1000)) * (Pi * 2);

    // Rotate the scene in Y
    // Change to Vector4(1, 0, 0, theta); to rotate in X

    Scene.Rotation := Vector4(0, 1, 0, theta);
    end;
  {$ifdef cgeapp}end;{$endif}
end;

{$ifdef cgeapp}
procedure WindowClose(Sender: TUIContainer);
{$else}
procedure TCastleApp.WindowClose(Sender: TObject);
{$endif}
begin
  {$ifdef cgeapp}with CastleApp do begin{$endif}
  {$ifdef cgeapp}end;{$endif}
end;

{$ifdef cgeapp}
procedure WindowMotion(Sender: TUIContainer; const Event: TInputMotion);
{$else}
procedure TCastleApp.WindowMotion(Sender: TObject; const Event: TInputMotion);
{$endif}
begin
  {$ifdef cgeapp}with CastleApp do begin{$endif}
  {$ifdef cgeapp}end;{$endif}
end;

{$ifdef cgeapp}
procedure WindowOpen(Sender: TUIContainer);
{$else}
procedure TCastleApp.WindowOpen(Sender: TObject);
{$endif}
begin
  {$ifdef cgeapp}with CastleApp do begin{$endif}
  GLIsReady := True;
  // TODO: There's a real test for GL being ready (can't remember the call)
  SetSensor(Scene, NewColor);
  // Calling SetSensor after GL is ready so it can display a message
  {$ifdef cgeapp}end;{$endif}
end;

{$ifdef cgeapp}
procedure WindowPress(Sender: TUIContainer;
  const Event: TInputPressRelease);
{$else}
procedure TCastleApp.WindowPress(Sender: TObject;
  const Event: TInputPressRelease);
{$endif}
var
  TempImage: TRGBAlphaImage;
begin
  {$ifdef cgeapp}with CastleApp do begin{$endif}
  {$ifndef darwin}
  if Event.IsKey(keyX) then
    begin
      TempImage := RecolorImage(MasterTexture, NewColor);
      Save3D(Scene.RootNode, 'data/exportedModels/scene_' + IntToStr(evt_t2) + '.x3dv', 'Hover', 'HoverRacer.gltf', xeClassic);
      FreeAndNil(TempImage);
    end;

    if Event.IsKey(keyT) then
      begin
        TempImage := RecolorImage(MasterTexture, NewColor);
        SaveImage(TempImage, 'castle-data:/exportedTextures/HoverRacer_' + IntToHex(Vec4BtoInt(NewColor), 6) + '.png');
        FreeAndNil(TempImage);
      end;
  {$endif}

  if Event.IsKey(keySpace) then
    begin
      PaintJob;
    end;
  {$ifdef cgeapp}end;{$endif}
end;

{$ifdef cgeapp}
procedure WindowRelease(Sender: TUIContainer;
  const Event: TInputPressRelease);
{$else}
procedure TCastleApp.WindowRelease(Sender: TObject;
  const Event: TInputPressRelease);
{$endif}
begin
  {$ifdef cgeapp}with CastleApp do begin{$endif}
  {$ifdef cgeapp}end;{$endif}
end;

{$ifdef cgeapp}
procedure WindowRender(Sender: TUIContainer);
{$else}
procedure TCastleApp.WindowRender(Sender: TObject);
{$endif}
begin
  {$ifdef cgeapp}with CastleApp do begin{$endif}
  {$ifdef cgeapp}end;{$endif}
end;

{$ifdef cgeapp}
procedure WindowResize(Sender: TUIContainer);
{$else}
procedure TCastleApp.WindowResize(Sender: TObject);
{$endif}
begin
  {$ifdef cgeapp}with CastleApp do begin{$endif}
  {$ifdef cgeapp}end;{$endif}
end;

{$ifdef cgeapp}
procedure WindowUpdate(Sender: TUIContainer);
{$else}
procedure TCastleApp.WindowUpdate(Sender: TObject);
{$endif}
begin
  {$ifdef cgeapp}with CastleApp do begin{$endif}
  {$ifdef cgeapp}end;{$endif}
end;

end.

