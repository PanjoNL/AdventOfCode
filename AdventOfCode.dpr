program AdventOfCode;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  AOCBase in 'AOCBase.pas',
  AOC2015Solutions in '2015/AOC2015Solutions.pas',
  AOC2024Solutions in '2024/AOC2024Solutions.pas' {$R *.res},
  uAOCUtils in 'uAOCUtils.pas',
  uAOCTests in 'uAOCTests.pas',
  uAocConfig in 'uAocConfig.pas',
  PriorityQueues in 'PriorityQueue\PriorityQueues.pas',
  PriorityQueues.Detail in 'PriorityQueue\PriorityQueues.Detail.pas',
  AOCLetterReader in 'AOCLetterReader.pas',
  uAOCTimer in 'uAOCTimer.pas',
  uAocGrid in 'uAocGrid.pas',
  uAocManager in 'uAocManager.pas',
  uAocTypes in 'uAocTypes.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
