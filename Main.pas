unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AOCBase, Vcl.ExtCtrls,
  System.Generics.Collections, uAOCTests, uAocConfig, uAocManager, typInfo, strUtils, uAocUtils, uAocTypes;

type
  TForm1 = class(TForm)
    btnSolve: TButton;
    cbbDay: TComboBox;
    btnTest: TButton;
    cbbYear: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnSolveClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbbYearChange(Sender: TObject);
  private
    FConfig: TAOCConfig;
    function SelectedYear: AocYear;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Year: AocYear;
begin
  AllocConsole;
  FConfig := TAOCConfig.Create;

  for Year := Low(AocYear) to High(AocYear) do
    cbbYear.Items.AddObject('Year ' + ord(Year).ToString, TObject(Year));

  cbbYear.ItemIndex := cbbYear.Items.Count - 1;
  cbbYearChange(nil);

  btnSolveClick(nil);
//  Application.Terminate;
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FConfig.Free;
end;

function TForm1.SelectedYear: AocYear;
begin
  Result := AocYear(cbbYear.Items.Objects[cbbYear.ItemIndex]);
end;

procedure TForm1.btnSolveClick(Sender: TObject);
var
  SelectedClass: AocClass;
  AdventOfCode: TAdventOfCode;
begin
  SelectedClass := AocClass(CbbDay.Items.Objects[CbbDay.ItemIndex]);

  AdventOfCode := SelectedClass.Create(FConfig, SelectedYear);
  try
    AdventOfCode.Solve;
  finally
    AdventOfCode.Free;
  end;
end;

procedure TForm1.btnTestClick(Sender: TObject);
begin
  AOCTests.RunTests(FConfig);
end;

procedure TForm1.cbbYearChange(Sender: TObject);
var
  Implementations: AocArray;
  i: integer;
begin
  Implementations := AocManager.GetImplementations(SelectedYear);

  cbbDay.Clear;
  for i := 0 to Length(Implementations) -1 do
    cbbDay.Items.AddObject('Day ' + AOCUtils.DayIndexFromClassName(Implementations[i].ClassName), TObject(Implementations[i]));
  cbbDay.ItemIndex := cbbDay.Items.Count - 1;
end;

end.
