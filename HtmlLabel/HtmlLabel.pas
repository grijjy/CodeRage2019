unit HtmlLabel;

{$SCOPEDENUMS ON}

interface

uses
  System.UITypes,
  System.Classes,
  System.Generics.Collections,
  FMX.StdCtrls,
  FMX.Graphics,
  FMX.TextLayout;

type
  { Event type for THtmlLabel.OnLinkClick }
  THtmlLabelLinkClickEvent = procedure(const ASender: TObject;
    const AHRef: String) of Object;

type
  { A text label that supports simple HTML markup. Only a couple of HTML tags
    are supported. Tags can be in uppercase or lowercase.

    A very simple HTML parsing algorithm is used that requires that the HTML
    follows these strict rules:
    * every start tag MUST have a matching end tag.
    * attributes must be in 'name="Value"' format (with double quotes for
      values).

    Behaviour will be undefined if the Text property does not follow these
    rules!

    Currently the following tags are supported:
    * <b></b>, <strong></strong>: renders bold
    * <i></i>, <em></em>: renders italics
    * <a href="ref">: renders using LinkStyle and LinkColor, and fires
      OnLinkClick when clicked
    * <font color="#rrggbb">: renders using alternate color. Other font
      attributes are not supported in this sample control. }
  THtmlLabel = class(TLabel)
  {$REGION 'Internal Declarations'}
  private type
    TLink = record
    public
      StartPos: Integer;
      EndPos: Integer;
      HRef: String;
    end;
  private type
    TAttribute = record
    public
      Name: String;
      Value: String;
    end;
  private type
    TTagKind = (Unsupported, Bold, Italic, Font, Link);
  private type
    TTag = record
    public
      IsStartTag: Boolean;
      Kind: TTagKind;
      Link: TLink;
      StartPos: Integer;
      Attributes: TArray<TAttribute>;
      TextAttribute: TTextAttribute;
    private
      function GetAttribute(const AName: String; out AValue: String): Boolean;
    end;
  private
    FLinkStyle: TFontStyles;
    FLinkColor: TAlphaColor;
    FLinks: TArray<TLink>;
    FFonts: TObjectList<TFont>;
    FCurFontStyle: TFontStyles;
    FOnLinkClick: THtmlLabelLinkClickEvent;
    procedure SetLinkColor(const AValue: TAlphaColor);
    procedure SetLinkStyle(const AValue: TFontStyles);
    procedure SetOnLinkClick(const AValue: THtmlLabelLinkClickEvent);
  private
    procedure ApplyMarkup;
    procedure ApplyStartTag(const ALayout: TTextLayout; var ATag: TTag);
    procedure ApplyEndTag(const AStartTag, AEndTag: TTag);
    procedure ApplyLink(const ALayout: TTextLayout; var ATag: TTag);
    procedure ApplyFont(const ALayout: TTextLayout; var ATag: TTag);
    procedure ApplyFontStyle(const ALayout: TTextLayout; var ATag: TTag;
      const AFontStyle: TFontStyles;
      const AFontColor: TAlphaColor = TAlphaColors.Null);
    function FindLink(const AX, AY: Single; out ALink: TLink): Boolean;
  private
    class function ParseTag(var AText: String; out ATag: TTag): Boolean; static;
  protected
    procedure Loaded; override;
    procedure DoChanged; override;
    procedure MouseMove(AShift: TShiftState; AX, AY: Single); override;
    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Single); override;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Font style to use to render links. Defaults to [fsUnderline] }
    property LinkStyle: TFontStyles read FLinkStyle write SetLinkStyle default [TFontStyle.fsUnderline];

    { Color to use to render links. Defaults to TAlphaColors.Blue }
    property LinkColor: TAlphaColor read FLinkColor write SetLinkColor default TAlphaColors.Blue;

    { Is fired when the user clicks on a link }
    property OnLinkClick: THtmlLabelLinkClickEvent read FOnLinkClick write SetOnLinkClick;
  end;

procedure Register;

implementation

uses
  System.Types,
  System.SysUtils,
  System.Character,
  FMX.Objects;

procedure Register;
begin
  RegisterComponents('Grijjy', [THtmlLabel]);
end;

type
  TTextAccess = class(TText);

{ THtmlLabel }

procedure THtmlLabel.ApplyEndTag(const AStartTag, AEndTag: TTag);
var
  Link: TLink;
begin
  { This method is called when an end-tag is encountered. It restores the
    current font style or adds a link to the FLinks collection (depending on
    the tag kind) }
  case AStartTag.Kind of
    TTagKind.Bold:
      FCurFontStyle := FCurFontStyle - [TFontStyle.fsBold];

    TTagKind.Italic:
      FCurFontStyle := FCurFontStyle - [TFontStyle.fsItalic];

    TTagKind.Link:
      begin
        Link := AStartTag.Link;

        { Set the end (character) position of the link to the start position of
          the end tag. }
        Link.EndPos := AEndTag.StartPos;
        FLinks := FLinks + [Link];
      end;
  end;
end;

procedure THtmlLabel.ApplyFont(const ALayout: TTextLayout; var ATag: TTag);
var
  Color: TAlphaColor;
  Value: String;
begin
  { Process the <font color="..."> tag. Convert the 'color' attribute to a
    color value and apply it to the current font. }
  if ATag.GetAttribute('color', Value) then
  begin
    Value := Value.Replace('#', '$');
    if (TryStrToUInt(Value, Cardinal(Color))) then
    begin
      Color := Color or $FF000000;
      ApplyFontStyle(ALayout, ATag, [], Color);
    end;
  end;
end;

procedure THtmlLabel.ApplyFontStyle(const ALayout: TTextLayout; var ATag: TTag;
  const AFontStyle: TFontStyles; const AFontColor: TAlphaColor);
var
  Font: TFont;
  Color: TAlphaColor;
begin
  { Creates a text attribute for a new font style.
    For this, we need to create a new font object. The TTextAttibute we apply
    the font to does *not* own the font. So we store the font in an object list
    so it will be destroyed when markup is reapplied or the label is destroyed. }
  Font := TFont.Create;
  FFonts.Add(Font);

  { Apply style to the font, and update the current font style accordingly. }
  Font.Assign(ALayout.Font);
  if (AFontStyle <> []) then
  begin
    FCurFontStyle := FCurFontStyle + AFontStyle;
    Font.Style := FCurFontStyle;
  end;

  if (AFontColor = TAlphaColors.Null) then
    Color := ALayout.Color
  else
    Color := AFontColor;

  { Create a text attribute with the font and color }
  ATag.TextAttribute := TTextAttribute.Create(Font, Color);
end;

procedure THtmlLabel.ApplyLink(const ALayout: TTextLayout; var ATag: TTag);
begin
  { Process the <a href="..."> tag. }
  if (not ATag.GetAttribute('href', ATag.Link.HRef)) then
    Exit;

  { Set the start (character) position of the link to the start position of the
    tag. The end position will be set later in ApplyEndTag (when the position
    of the end tag is known). }
  ATag.Link.StartPos := ATag.StartPos;

  { Apply the style and color for links. }
  ApplyFontStyle(ALayout, ATag, FLinkStyle, FLinkColor);
end;

procedure THtmlLabel.ApplyMarkup;
var
  Layout: TTextLayout;
  Text: String;
  Tag, StartTag: TTag;
  TagStack: TStack<TTag>;
  TextAttrStack: TObjectStack<TTextAttributedRange>;
  TextAttr: TTextAttributedRange;
  Range: TTextRange;
begin
  { This method is called when the Text property changes, or another property
    that requires applying markup. }
  FLinks := nil;

  { Every label should have a style with a TText object. This object is
    available through the TPresentedControl.TextObject property. }
  if (TextObject = nil) or (not (TextObject is TText)) then
    Exit;

  { We need access the protected TText.Layout field to apply attributes to the
    text. Since this field is protected, we use the common "local class hack"
    (TTextAccess) to get to this field. }
  Layout := TTextAccess(TextObject).Layout;
  Assert(Assigned(Layout));

  { We are going to parse the Text property for HTML tags. }
  Text := Self.Text;

  { To handle nested tags, we use a stack (TagStack). Every time we encounter a
    start tag, we push to this stack. And when we encounter an end tag we pop
    from the stack. }
  TextAttrStack := nil;
  TagStack := TStack<TTag>.Create;
  try
    { We also need a stack of text attributes. A text attribute contains font
      and color information that can be applied to a portion of the text.
      We first gather all attributes while parsing the HTML and push them to the
      stack. Then, when parsing has finished, we keep popping this stack to
      apply the attributes. }
    TextAttrStack := TObjectStack<TTextAttributedRange>.Create;

    { FFont contains a list of all fonts that are used by this label. Since
      the TTextLayout does *not* own any fonts, we need to keep track of the
      fonts ourselves to avoid a memory leak. }
    FFonts.Clear;
    FCurFontStyle := [];

    { Keep parsing tags until the end of the text has been reached. When
      ParseTag returns True, the tag is removed from Text and its properties
      are stored in the Tag record. }
    while ParseTag(Text, Tag) do
    begin
      if (Tag.IsStartTag) then
      begin
        { If this is a start tag, then create a text attribute for it.
          We cannot apply it to the text layout yet since we don't know yet when
          the tag ends. So we push it to the tag stack instead. }
        ApplyStartTag(Layout, Tag);
        TagStack.Push(Tag);
      end
      else
      begin
        { This is an end tag. There should be a matching start tag in the
          stack. }
        if (TagStack.Count = 0) then
          raise EInvalidOperation.Create('HTML end tag without start tag');

        StartTag := TagStack.Pop;
        if (StartTag.Kind <> Tag.Kind) then
          raise EInvalidOperation.Create('HTML end tag doesn''t match start tag');

        { Now that we have the end tag, we can calculate the length of the text
          fragment to which the tag applies. }
        Range.Pos := StartTag.StartPos;
        Range.Length := Tag.StartPos - StartTag.StartPos;

        { Push the attribute to a stack so we can apply it when we are done
          parsing. }
        TextAttr := TTextAttributedRange.Create(Range, StartTag.TextAttribute);
        TextAttrStack.Push(TextAttr);

        { Take any actions depending on the type of tag. For example, if it is
          a link (<a>) tag, then add the link to the FLinks collection. }
        ApplyEndTag(StartTag, Tag);
      end;
    end;

    { We are done parsing now. There should be no (start) tags left in the
      stack. }
    if (TagStack.Count > 0) then
      raise EInvalidOperation.Create('HTML start tag without end tag');

    { We can now finally apply the text attributes to the text layout. }
    Layout.BeginUpdate;
    try
      { Clear any attributes from a previous markup session. }
      Layout.ClearAttributes;

      { The Text variable now contains the text with all HTML markup removed. }
      Layout.Text := Text;

      { Add attributes in *reverse* order. This is needed for nested tags
        (like <b><i>text</i></b>) to work correctly. Because we are using a
        stack, this is automatic. }
      while (TextAttrStack.Count > 0) do
      begin
        { Use Extract instead of Pop since we don't want to destroy the object.
          The layout becomes owner of the object now. }
        TextAttr := TextAttrStack.Extract;
        Layout.AddAttribute(TextAttr);
      end;
    finally
      Layout.EndUpdate;
    end;
  finally
    { If there was an exception, then TextAttrStack contains unused objects.
      Since this is a TObjectStack, those objects will be destroyed here. }
    TextAttrStack.Free;

    TagStack.Free;
  end;
end;

procedure THtmlLabel.ApplyStartTag(const ALayout: TTextLayout; var ATag: TTag);
begin
  { This method is called to create a text attribute for the kind of tag. }
  case ATag.Kind of
    TTagKind.Bold:
      ApplyFontStyle(ALayout, ATag, [TFontStyle.fsBold]);

    TTagKind.Italic:
      ApplyFontStyle(ALayout, ATag, [TFontStyle.fsItalic]);

    TTagKind.Font:
      ApplyFont(ALayout, ATag);

    TTagKind.Link:
      ApplyLink(ALayout, ATag);
  end;
end;

constructor THtmlLabel.Create(AOwner: TComponent);
begin
  inherited;
  FFonts := TObjectList<TFont>.Create;
  FLinkStyle := [TFontStyle.fsUnderline];
  FLinkColor := TAlphaColors.Blue;
end;

destructor THtmlLabel.Destroy;
begin
  FFonts.Free;
  inherited;
end;

procedure THtmlLabel.DoChanged;
begin
  inherited;
  { Apply the markup when the text has changed. }
  ApplyMarkup;
end;

function THtmlLabel.FindLink(const AX, AY: Single; out ALink: TLink): Boolean;
var
  Pos: Integer;
  Link: TLink;
begin
  { This method is called to find a link at the given AX, AY position.
    We need to TText.Layout object for this. Also, this method is only
    relevant if we have any links and if the OnLinkClick event is assigned. }
  if (TextObject = nil) or (not (TextObject is TText))
    or (FLinks = nil) or (not Assigned(FOnLinkClick))
  then
    Exit(False);

  { TTextLayout.PositionAtPoint gives us the character position within the text
    at the given (mouse) location. }
  Pos := TTextAccess(TextObject).Layout.PositionAtPoint(PointF(AX, AY));

  { Check if there is any link where Pos falls between its start and end
    position. }
  for Link in FLinks do
  begin
    if ((Pos >= Link.StartPos) and (Pos < Link.EndPos)) then
    begin
      ALink := Link;
      Exit(True);
    end;
  end;

  Result := False;
end;

procedure THtmlLabel.Loaded;
begin
  inherited;
  if (AutoSize) then
    { Make sure AutoSize works with markup applied }
    Resize;
end;

procedure THtmlLabel.MouseMove(AShift: TShiftState; AX, AY: Single);
var
  Link: TLink;
begin
  inherited;
  { Update the cursor to a Hand cursor when the mouse is over a link. }
  if FindLink(AX, AY, Link) then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

procedure THtmlLabel.MouseUp(AButton: TMouseButton; AShift: TShiftState; AX,
  AY: Single);
var
  Link: TLink;
begin
  inherited;
  { Check if there is a link under the mouse cursor.
    If so, fire an OnLinkClick event. }
  if (AButton = TMouseButton.mbLeft) and FindLink(AX, AY, Link) then
  begin
    Assert(Assigned(FOnLinkClick));
    FOnLinkClick(Self, Link.HRef);
  end;
end;

class function THtmlLabel.ParseTag(var AText: String; out ATag: TTag): Boolean;
var
  I, StringStart: Integer;
  Attr: TAttribute;
  TagName: String;

  procedure SkipWhitespace;
  begin
    while (I < AText.Length) and (AText.Chars[I].IsWhiteSpace) do
      Inc(I);
  end;

begin
  { This is an (inefficient) way to parse the next tag in the given AText
    string. This method will do the following:
    * Look for any tag (starting with '<')
    * If a tag is found:
      * Parse it attributes (if any)
      * Store the properties and attributes inside the ATag record.
      * Strip the markup from the AText string.
      * Return True
    * If a tag is not found, or the markup is invalid, return False. }

  { Search for the start of a tag }
  ATag.IsStartTag := True;
  ATag.StartPos := AText.IndexOf('<');
  if (ATag.StartPos < 0) then
    { No tag found }
    Exit(False);

  I := ATag.StartPos + 1;
  if (I >= AText.Length) then
    { Unterminated tag. }
    Exit(False);

  { If the '<' is followed by '/', then we have an end tag.
    Note that we use the String.Chars[I] property instead of String[I], since
    the Chars[] property is zero-based on all platforms. }
  if (AText.Chars[I] = '/') then
  begin
    ATag.IsStartTag := False;
    Inc(I);
  end;

  { Parse the name of the tag. To do this, we keep parsing characters until
    a non-letter is read. }
  StringStart := I;
  while (I < AText.Length) do
  begin
    if (not AText.Chars[I].IsLetter) then
      Break;

    Inc(I);
  end;
  if (I = StringStart) then
    { No tag name }
    Exit(False);

  { Create a lower-case tag name and convert it to a TTagKind. }
  TagName := AText.Substring(StringStart, I - StringStart).ToLower;
  ATag.Kind := TTagKind.Unsupported;
  case TagName.Chars[0] of
    'a': if (TagName = 'a') then
           ATag.Kind := TTagKind.Link;
    'b': if (TagName = 'b') then
           ATag.Kind := TTagKind.Bold;
    'e': if (TagName = 'em') then
           ATag.Kind := TTagKind.Italic;
    'f': if (TagName = 'font') then
           ATag.Kind := TTagKind.Font;
    'i': if (TagName = 'i') then
           ATag.Kind := TTagKind.Italic;
    's': if (TagName = 'strong') then
           ATag.Kind := TTagKind.Bold;
  end;

  { If this is a start tag, then parse the attributes (if any) }
  ATag.Attributes := nil;
  SkipWhitespace;
  if (ATag.IsStartTag) and (I < AText.Length) then
  begin
    { Keep parsing attributes until a '>' is found. }
    while (I < AText.Length) and (AText.Chars[I] <> '>') do
    begin
      { Parse the attribute name, using the same algorithm used for parsing
        the tag name. }
      StringStart := I;
      while (I < AText.Length) do
      begin
        if (not AText.Chars[I].IsLetter) then
          Break;

        Inc(I);
      end;

      { Set the attribute name to lower case (since case is unimportant here). }
      Attr.Name := AText.Substring(StringStart, I - StringStart).ToLower;

      SkipWhitespace;
      if (I >= AText.Length) or (AText.Chars[I] <> '=') then
        { Invalid attribute. No '=' found. }
        Exit(False);

      Inc(I);
      SkipWhitespace;
      if (I >= AText.Length) or (AText.Chars[I] <> '"') then
        { Invalid attribute. Value must start with double quotes. }
        Exit(False);

      Inc(I);
      StringStart := I;
      I := AText.IndexOf('"', I);
      if (I < 0) then
        { Ending quotes not found }
        Exit(False);

      { Set the attribute value. Case is important here. }
      Attr.Value := AText.Substring(StringStart, I - StringStart);

      { Add the attribute and parse the next one }
      ATag.Attributes := ATag.Attributes + [Attr];

      Inc(I);
      SkipWhitespace;
    end;
  end;

  { We should be at a '>' now. If not, the markup is invalid. }
  if (I >= AText.Length) or (AText.Chars[I] <> '>') then
    Exit(False);

  { Remove the markup from the text. }
  AText := AText.Remove(ATag.StartPos, I - ATag.StartPos + 1);
  Result := True;
end;

procedure THtmlLabel.SetLinkColor(const AValue: TAlphaColor);
begin
  if (AValue <> FLinkColor) then
  begin
    FLinkColor := AValue;
    ApplyMarkup;
    Repaint;
  end;
end;

procedure THtmlLabel.SetLinkStyle(const AValue: TFontStyles);
begin
  if (AValue <> FLinkStyle) then
  begin
    FLinkStyle := AValue;
    ApplyMarkup;
    Repaint;
  end;
end;

procedure THtmlLabel.SetOnLinkClick(const AValue: THtmlLabelLinkClickEvent);
begin
  FOnLinkClick := AValue;
  HitTest := Assigned(AValue); { Required for capturing clicks }
end;

{ THtmlLabel.TTag }

function THtmlLabel.TTag.GetAttribute(const AName: String;
  out AValue: String): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(Attributes) - 1 do
  begin
    if (Attributes[I].Name = AName) then
    begin
      AValue := Attributes[I].Value;
      Exit(True);
    end;
  end;
  Result := False;
end;

end.
