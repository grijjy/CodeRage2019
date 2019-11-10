unit BlurBehindControl;

interface

uses
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Graphics,
  FMX.Filter.Effects;

type
  { A control that blurs whatever is behind it. }
  TBlurBehindControl = class(TControl)
  {$REGION 'Internal Declarations'}
  private
    FBitmapOfControlBehind: TBitmap;
    FBitmapBlurred: TBitmap;
    FGaussianBlurEffect: TGaussianBlurEffect;
    FBlurAmount: Single;
    procedure SetBlurAmount(const AValue: Single);
  private
    procedure UpdateBitmapOfControlBehind;
    procedure UpdateBitmapBlurred;
  protected
    procedure ParentChanged; override;
    procedure Paint; override;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The blur amount. Defaults to 1.5 }
    property BlurAmount: Single read FBlurAmount write SetBlurAmount;

    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Locked;
    property Height;
    property HitTest default False;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property TabOrder;
    property TabStop;

    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

procedure Register;

implementation

uses
  System.Types;

procedure Register;
begin
  RegisterComponents('Grijjy', [TBlurBehindControl]);
end;

{ TBlurBehindControl }

constructor TBlurBehindControl.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
  FBitmapOfControlBehind := TBitmap.Create;
  FBitmapBlurred := TBitmap.Create;
  FGaussianBlurEffect := TGaussianBlurEffect.Create(Self);
  FBlurAmount := 1.5;
end;

destructor TBlurBehindControl.Destroy;
begin
  FBitmapBlurred.Free;
  FBitmapOfControlBehind.Free;
  inherited;
end;

procedure TBlurBehindControl.Paint;
begin
  { Copy the visual of the parent control to FBitmapOfControlBehind. }
  UpdateBitmapOfControlBehind;

  { Copy the part of FBitmapOfControlBehind that this control occupies to
    FBitmapBlurred and blur it. }
  UpdateBitmapBlurred;

  { Draw the blurred bitmap to the canvas. }
  Canvas.BeginScene;
  try
    Canvas.DrawBitmap(
      { The bitmap to draw }
      FBitmapBlurred,

      { The source rectangle. We use the entire blurred bitmap }
      RectF(0, 0, FBitmapBlurred.Width, FBitmapBlurred.Height),

      { The target rectangle. This is the local rectangle of our control. }
      LocalRect,

      { Opacity }
      1);
  finally
    Canvas.EndScene;
  end;
end;

procedure TBlurBehindControl.ParentChanged;
begin
  inherited;
  { Our code only works if the parent is a TControl (or nil).
    So you cannot place this control directly on a form. }
  if (Parent <> nil) and (not (Parent is TControl)) then
    raise EInvalidOperation.Create('A TBlurBehindControl can only be placed inside another control');
end;

procedure TBlurBehindControl.SetBlurAmount(const AValue: Single);
begin
  if (AValue <> FBlurAmount) then
  begin
    FBlurAmount := AValue;
    Repaint;
  end;
end;

procedure TBlurBehindControl.UpdateBitmapBlurred;
var
  TargetWidth, TargetHeight: Integer;
  AreaOfInterest: TRect;
begin
  { The FBitmapOfControlBehind was created at half the dimensions of the
    control. We need to copy the area of interest from this bitmap to
    FBitmapBlurred, so we can run a blur filter on it.
    So the dimensions of FBitmapBlurred should be half of this control. }
  TargetWidth := Round(0.5 * Width);
  TargetHeight := Round(0.5 * Height);
  FBitmapBlurred.SetSize(TargetWidth, TargetHeight);

  { Copy the area of interest to FBitmapBlurred.
    Since the source bitmap is scaled by 0.5, we need to set the area of
    interest accordingly. }
  AreaOfInterest.Left := Trunc(0.5 * Position.X);
  AreaOfInterest.Top := Trunc(0.5 * Position.Y);
  AreaOfInterest.Width := TargetWidth;
  AreaOfInterest.Height := TargetHeight;

  FBitmapBlurred.Canvas.BeginScene;
  try
    FBitmapBlurred.Canvas.Clear(0);
    FBitmapBlurred.Canvas.DrawBitmap(
      { Source bitmap to copy from }
      FBitmapOfControlBehind,

      { Source rectangle. This is the area of interest scaled by 0.5 }
      AreaOfInterest,

      { Target rectangle }
      RectF(0, 0, TargetWidth, TargetHeight),

      { Opacity }
      1,

      { Set HighSpeed to True for a faster blit. Since the source and target
        dimensions are the same, we don't need the slower image interpolation
        method.}
      True);
  finally
    FBitmapBlurred.Canvas.EndScene;
  end;

  { Apply blur }
  FGaussianBlurEffect.BlurAmount := FBlurAmount;
  FGaussianBlurEffect.ProcessEffect(
    { Canvas. Not used for GPU accelerated effects. }
    nil,

    { Bitmap to apply effect to. }
    FBitmapBlurred,

    { Any data to pass to effect. Not used. }
    0);

  { Blur a second time at a lower amount to reduce the "box blur" effect. }
  FGaussianBlurEffect.BlurAmount := FBlurAmount * 0.4;
  FGaussianBlurEffect.ProcessEffect(nil, FBitmapBlurred, 0);
end;

procedure TBlurBehindControl.UpdateBitmapOfControlBehind;
var
  CanvasBehind: TCanvas;
  ControlBehind: TControl;
  TargetWidth, TargetHeight: Integer;
begin
  { The parent should be a TControl. This is checked in ParentChanged. }
  Assert(Parent is TControl);
  ControlBehind := TControl(Parent);

  { To speed up the Gaussian blur, we draw to a reduced size bitmap. Since
    we are going to blur anyway, reducing the size of the bitmap has little
    effect on visual quality, but can decrease blurring time a lot. It also
    decreases memory requirements.

    We draw to a bitmap half the size of the control. This should speed up the
    blur by a factor of around 4. Also note that on retina/high-DPI displays, we
    do *not* capture at high-DPI, further reducing the bitmap size. }
  TargetWidth := Round(0.5 * ControlBehind.Width);
  TargetHeight := Round(0.5 * ControlBehind.Height);
  FBitmapOfControlBehind.SetSize(TargetWidth, TargetHeight);

  { Paint the parent control to the FBitmapOfControlBehind bitmap.
    Note: the parent control will also paint any child controls, including this
    control. This would paint the parent control again, resulting in an infinite
    loop. We prevent this by setting FDisablePaint to True (this is a protected
    field of the TControl class). }
  CanvasBehind := FBitmapOfControlBehind.Canvas;
  CanvasBehind.BeginScene;
  FDisablePaint := True;
  try
    { Note: if ControlBehind is not completely opaque, then part of the
      resulting bitmap will have transparent areas as well. This can result in
      unwanted blurring behavior. To avoid this, make sure that the parent of
      the TBlurBehindControl is completely opaque. }
    ControlBehind.PaintTo(CanvasBehind, RectF(0, 0, TargetWidth, TargetHeight));
  finally
    FDisablePaint := False;
    CanvasBehind.EndScene;
  end;
end;

end.
