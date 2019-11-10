unit Form.Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  HtmlLabel, FMX.Objects;

type
  TFormMain = class(TForm)
    LabelRegular1: TLabel;
    HtmlLabel1: THtmlLabel;
    HtmlLabel2: THtmlLabel;
    LabelRegular2: TLabel;
    Rectangle: TRectangle;
    Line1: TLine;
    Line2: TLine;
    Line3: TLine;
    procedure FormCreate(Sender: TObject);
    procedure HtmlLabel1LinkClick(const ASender: TObject; const AHRef: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TFormMain.HtmlLabel1LinkClick(const ASender: TObject;
  const AHRef: string);
begin
  ShowMessage('Clicked on link: ' + AHRef);
end;

end.
