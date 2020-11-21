unit MainGameUnit;

{$mode objfpc}{$H+}
{$define useDiskImage}
// {$define useFloatRecolor}

interface

uses
  Classes, SysUtils,
  {$ifndef cgeapp}
  Forms, Controls, Graphics, Dialogs, CastleControl, CastleLCLUtils,
  {$else}
  CastleWindow,
  {$endif}
  CastleControls, CastleColors, CastleUIControls,
  CastleCameras, CastleApplicationProperties, CastleLog,
  CastleSceneCore, CastleVectors, CastleScene, CastleViewport,
  X3DNodes, CastleImages, CastleTimeUtils, CastleKeysMouse;

type

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
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    ColorChoice: Integer;
    MasterTexture: TRGBAlphaImage;
    LabelSpare: TCastleLabel;
    DoingRecolor: Boolean;
    RecolorTime: Int64;
    RecolorCount: Int64;
  public
    procedure RunCGEApplication(Sender: TObject);
    procedure KillCGEApplication(Sender: TObject);
    procedure LoadScene(Sender: TObject; filename: String);
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True);    function ChangeTexture(const Node: TX3DRootNode; const TextureUrl: String): TVector3Cardinal;
    function RecolorImage(const ImageIn: TRGBAlphaImage; const NewRGB: TVector4): TRGBAlphaImage;
    function RecolorImage(const ImageIn: TRGBAlphaImage; const NewRGB: TVector4Byte): TRGBAlphaImage;
    function LoadMasterTexture(filename: String): TRGBAlphaImage;
  end;

{$ifndef cgeapp}
var
  CastleApp: TCastleApp;
{$endif}

const
  InitialSet: array [0 .. 2] of String =
      ('HoverRacer_Red.png',
       'HoverRacer_Green.png',
       'HoverRacer_Blue.png');

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

implementation
{$ifdef cgeapp}
uses GameInitialize;
{$endif}

{$ifndef cgeapp}
{$R *.lfm}
{$endif}

{ TCastleApp }

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

function TCastleApp.RecolorImage(const ImageIn: TRGBAlphaImage; const NewRGB: TVector4): TRGBAlphaImage;
var
  ImageOut: TRGBAlphaImage;
  x, y: Integer;
  ImRGB: TVector4;
  SeekRGB: TVector4;
  ReplaceRGB: TVector4;
begin
  ImageOut := nil;
  SeekRGB := Vector4(1, 1, 1, 1);
  ReplaceRGB := Vector4(NewRGB.X / 255, NewRGB.Y / 255, NewRGB.Z / 255, 1);
  if not(ImageIn = nil) then
    begin
      if not(ImageIn.Dimensions.IsZero) then
        begin
          ImageOut := TRGBAlphaImage.Create(ImageIn.Dimensions.X, ImageIn.Dimensions.Y);
          for y := 0 to ImageIn.Dimensions.Y -1 do
            begin
              for x := 0 to ImageIn.Dimensions.X -1 do
                begin
                  ImRGB := ImageIn.Colors[x, y, 0];
                  if TVector4.Equals(ImRGB, SeekRGB) then
                    ImageOut.Colors[x, y, 0] := ReplaceRGB
                  else
                    ImageOut.Colors[x, y, 0] := ImRGB;
                end;
            end;
        end;
    end;
  Result := ImageOut;
end;

function TCastleApp.LoadMasterTexture(filename: String): TRGBAlphaImage;
begin
  try
    MasterTexture := LoadImage(filename, [TRGBAlphaImage]) as TRGBAlphaImage;
  except
    on E : Exception do
      begin
        {$ifndef cgeapp}
        ShowMessage('Exception' + LineEnding +
                    'Trying to load : ' + filename + LineEnding +
                     E.ClassName + LineEnding +
                     E.Message);
        {$endif}
        MasterTexture := nil;
       end;
  end;
  Result := MasterTexture;
end;

function TCastleApp.ChangeTexture(const Node: TX3DRootNode; const TextureUrl: String): TVector3Cardinal;
var
  TextureNode: TImageTextureNode;
  AppearanceNode: TAppearanceNode;
begin
  Result := TVector3Cardinal.Zero;
  AppearanceNode := Node.TryFindNodeByName(TAppearanceNode, 'Glass', false) as TAppearanceNode;
  if not (AppearanceNode = nil) then
  begin
    TextureNode := AppearanceNode.MainTexture as TImageTextureNode;
    if not (TextureNode = nil) then
      begin
        TextureNode.SetUrl(TextureUrl);
        if TextureNode.IsTextureImage then
          Result := TextureNode.TextureImage.Dimensions;
      end;
  end;
end;

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

  LoadMasterTexture('castle-data:/HoverRacer.png');
  // Add the viewport to the CGE control
  {$ifndef cgeapp}
  Window.Controls.InsertFront(Viewport);
  {$else}
  TCastleWindowBase(Sender).Controls.InsertFront(Viewport);
  {$endif}

  Scene := TCastleScene.Create(Application);
  // Load a model into the scene
  Scene.load(filename);
  ChangeTexture(Scene.RootNode, InitialSet[ColorChoice]);

  // Add the scene to the viewport
  Viewport.Items.Add(Scene);

  // Tell the control this is the main scene so it gets some lighting
  Viewport.Items.MainScene := Scene;

  CreateLabel(LabelSpare, 0, False);
end;

procedure TCastleApp.RunCGEApplication(Sender: TObject);
begin
  DoingRecolor := False;
  RecolorTime := 0;
  RecolorCount := 0;
  ColorChoice := 0;
  Scene := nil;
  MasterTexture := nil;
  LoadScene(Sender, 'castle-data:/HoverRacer.gltf');
end;

procedure TCastleApp.KillCGEApplication(Sender: TObject);
begin
  FreeAndNil(MasterTexture);
end;

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
  // Set angle (theta) to revolve completely once every SecsPerRot
  theta := ((CastleGetTickCount64 mod
            (SecsPerRot * 1000)) /
            (SecsPerRot * 1000)) * (Pi * 2);

  // Rotate the scene in Y
  // Change to Vector4(1, 0, 0, theta); to rotate in X

  Scene.Rotation := Vector4(0, 1, 0, theta);
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
  ReColorTimer: Int64;
begin
  {$ifdef cgeapp}with CastleApp do begin{$endif}
  if Event.IsKey(keySpace) then
    begin
      if not (Scene = nil) and not(DoingRecolor) then
        begin
          DoingRecolor := True;
          ReColorTimer := CastleGetTickCount64;
          {$ifndef useDiskImage}
          Inc(ColorChoice);
          if (ColorChoice >= Length(InitialSet)) then
            ColorChoice := 0;
          ChangeTexture(Scene.RootNode, InitialSet[ColorChoice]);
          {$else}
          {$ifdef useFloatRecolor}
          TempImage := RecolorImage(MasterTexture, Vector4(random(256), random(256), random(256), 255));
          {$else}
          TempImage := RecolorImage(MasterTexture, Vector4Byte(random(256), random(256), random(256), 255));
          {$endif}
          if not(TempImage = nil) then
            begin
              ReColorTimer := CastleGetTickCount64 - ReColorTimer;
              SaveImage(TempImage, 'castle-data:/HoverRacer_temp.png');
              ChangeTexture(Scene.RootNode, 'castle-data:/HoverRacer_temp.png');
              FreeAndNil(TempImage);
            end;
          {$endif}
          RecolorTime += ReColorTimer;
          Inc(RecolorCount);
          LabelSpare.Caption := 'ReColor = ' +
                               FormatFloat('####0.000', ReColorTimer / 1000) +
                               ' seconds' + LineEnding +
                               'Average = ' +
                               FormatFloat('####0.000', (RecolorTime / RecolorCount) / 1000) +
                               ' seconds (' + IntToStr(RecolorCount) + ' ReColors)';

          DoingRecolor := False;
        end;
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

