unit uAOCTests;

interface

uses
  System.SysUtils, Winapi.Windows, system.Classes,
  uAocUtils, AOCBase, uAOCConfig, uAocTimer, uAocTypes,

  AOC2015Solutions,
  AOC2024Solutions
  ;

type
  AOCTest = record
    Year: AocYear;
    AOCClass: AocClass;
    ExpectedSolutionA, ExpectedSolutionB: String;
    LoadOverridenTestData: TLoadOverridenTestData
end;

type AOCTests = class
public
  Class procedure RunTests(aConfig: TAOCConfig);
end;

Const AOCTestData: array[0..49] of AOCTest =
(
// 2015
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay1; ExpectedSolutionA: '74'; ExpectedSolutionB: '1795'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay2; ExpectedSolutionA: '1598415'; ExpectedSolutionB: '3812909'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay3; ExpectedSolutionA: '2081'; ExpectedSolutionB: '2341'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay4; ExpectedSolutionA: '282749'; ExpectedSolutionB: '9962624'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay5; ExpectedSolutionA: '255'; ExpectedSolutionB: '55'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay6; ExpectedSolutionA: '543903'; ExpectedSolutionB: '14687245'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay7; ExpectedSolutionA: '956'; ExpectedSolutionB: '40149'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay8; ExpectedSolutionA: '1371'; ExpectedSolutionB: '2117'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay9; ExpectedSolutionA: '207'; ExpectedSolutionB: '804'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay10; ExpectedSolutionA: '329356'; ExpectedSolutionB: '4666278'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay11; ExpectedSolutionA: 'hepxxyzz'; ExpectedSolutionB: 'heqaabcc'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay12; ExpectedSolutionA: '156366'; ExpectedSolutionB: '96852'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay13; ExpectedSolutionA: '733'; ExpectedSolutionB: '725'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay14; ExpectedSolutionA: '2696'; ExpectedSolutionB: '1084'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay15; ExpectedSolutionA: '21367368'; ExpectedSolutionB: '1766400'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay16; ExpectedSolutionA: '213'; ExpectedSolutionB: '323'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay17; ExpectedSolutionA: '1638'; ExpectedSolutionB: '17'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay18; ExpectedSolutionA: '768'; ExpectedSolutionB: '781'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay19; ExpectedSolutionA: '579'; ExpectedSolutionB: ''),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay20; ExpectedSolutionA: '831600'; ExpectedSolutionB: '884520'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay21; ExpectedSolutionA: '121'; ExpectedSolutionB: '201'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay22; ExpectedSolutionA: '1824'; ExpectedSolutionB: '1937'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay23; ExpectedSolutionA: '307'; ExpectedSolutionB: '160'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay24; ExpectedSolutionA: '11266889531'; ExpectedSolutionB: '77387711'),
 (Year: _2015; AOCClass: AOC2015Solutions.TAdventOfCodeDay25; ExpectedSolutionA: '9132360'; ExpectedSolutionB: ''),

// 2024
 (Year: _2024; AOCClass: TAdventOfCodeDay1; ExpectedSolutionA: '3714264'; ExpectedSolutionB: '18805872'),
 (Year: _2024; AOCClass: TAdventOfCodeDay2; ExpectedSolutionA: '670'; ExpectedSolutionB: '700'),
 (Year: _2024; AOCClass: TAdventOfCodeDay3; ExpectedSolutionA: '173529487'; ExpectedSolutionB: '99532691'),
 (Year: _2024; AOCClass: TAdventOfCodeDay4; ExpectedSolutionA: '2514'; ExpectedSolutionB: '1888'),
 (Year: _2024; AOCClass: TAdventOfCodeDay5; ExpectedSolutionA: '5651'; ExpectedSolutionB: '4743'),
 (Year: _2024; AOCClass: TAdventOfCodeDay6; ExpectedSolutionA: '5242'; ExpectedSolutionB: '1424'),
 (Year: _2024; AOCClass: TAdventOfCodeDay7; ExpectedSolutionA: '5540634308362'; ExpectedSolutionB: '472290821152397'),
 (Year: _2024; AOCClass: TAdventOfCodeDay8; ExpectedSolutionA: '359'; ExpectedSolutionB: '1293'),
 (Year: _2024; AOCClass: TAdventOfCodeDay9; ExpectedSolutionA: '6398252054886'; ExpectedSolutionB: '6415666220005'),
 (Year: _2024; AOCClass: TAdventOfCodeDay10;ExpectedSolutionA: '667'; ExpectedSolutionB: '1344'),
 (Year: _2024; AOCClass: TAdventOfCodeDay11;ExpectedSolutionA: '216042'; ExpectedSolutionB: '255758646442399'),
 (Year: _2024; AOCClass: TAdventOfCodeDay12;ExpectedSolutionA: '1375476'; ExpectedSolutionB: '821372'),
 (Year: _2024; AOCClass: TAdventOfCodeDay13;ExpectedSolutionA: '29187'; ExpectedSolutionB: '99968222587852'),
 (Year: _2024; AOCClass: TAdventOfCodeDay14;ExpectedSolutionA: '216027840'; ExpectedSolutionB: '6876'),
 (Year: _2024; AOCClass: TAdventOfCodeDay15;ExpectedSolutionA: '1451928'; ExpectedSolutionB: '1462788'),
 (Year: _2024; AOCClass: TAdventOfCodeDay16;ExpectedSolutionA: '94444'; ExpectedSolutionB: '502'),
 (Year: _2024; AOCClass: TAdventOfCodeDay17;ExpectedSolutionA: '1,5,3,0,2,5,2,5,3'; ExpectedSolutionB: '108107566389757'),
 (Year: _2024; AOCClass: TAdventOfCodeDay18;ExpectedSolutionA: '316'; ExpectedSolutionB: '45,18'),
 (Year: _2024; AOCClass: TAdventOfCodeDay19;ExpectedSolutionA: '358'; ExpectedSolutionB: '600639829400603'),
 (Year: _2024; AOCClass: TAdventOfCodeDay20;ExpectedSolutionA: '1426'; ExpectedSolutionB: '1000697'),
 (Year: _2024; AOCClass: TAdventOfCodeDay21;ExpectedSolutionA: '155252'; ExpectedSolutionB: '195664513288128'),
 (Year: _2024; AOCClass: TAdventOfCodeDay22;ExpectedSolutionA: '21147129593'; ExpectedSolutionB: '2445'),
 (Year: _2024; AOCClass: TAdventOfCodeDay23;ExpectedSolutionA: '1370'; ExpectedSolutionB: 'am,au,be,cm,fo,ha,hh,im,nt,os,qz,rr,so'),
 (Year: _2024; AOCClass: TAdventOfCodeDay24;ExpectedSolutionA: '69201640933606'; ExpectedSolutionB: 'dhq,hbs,jcp,kfp,pdg,z18,z22,z27'),
 (Year: _2024; AOCClass: TAdventOfCodeDay25;ExpectedSolutionA: '3162'; ExpectedSolutionB: '')
);

implementation
class procedure AOCTests.RunTests(aConfig: TAOCConfig);

  procedure _Check(const DisplayName, Expected, Actual: String);
  begin
    if Expected <> '' then
      if Expected <> Actual then
      begin
        WriteLn(Format('FAIL, %s Expected: %s, Actual: %s', [DisplayName, Expected, Actual]));
        Assert(False);
      end
  end;

Var
  Test: AOCTest;
  AdventOfCode: TAdventOfCode;
  TotalTime, TestTimer: AocTimer;
  SolutionA, SolutionB: string;
  Times: TStringList;
  ElapsedMicroSeconds: Integer;
  s: string;
begin
  Writeln('');

  Times := TStringList.Create;
  try
    TotalTime := AOCTimer.Start;
    for Test in AOCTestData do
    begin
      Writeln(Format('Running tests for %s', [Test.AOCClass.ClassName]));

      AdventOfCode := Test.AOCClass.Create(aConfig, Test.Year);

      TestTimer := AOCTimer.Start;
      AdventOfCode.Test(SolutionA, SolutionB, Test.LoadOverridenTestData);
      ElapsedMicroSeconds := TestTimer.ElapsedTime;
      Times.Add(Format('%s -> Time: %d %s', [Test.AOCClass.Classname, ElapsedMicroSeconds, TimeIndicator[MicroSeconds]]));
      AdventOfCode.Free;

      _Check('Part a', Test.ExpectedSolutionA, SolutionA);
      _Check('Part b', Test.ExpectedSolutionB, SolutionB);
      Writeln(FormAt('Total time %d %s', [ElapsedMicroSeconds, TimeIndicator[MicroSeconds]]));
      Writeln('');
    end;

    Writeln(Format('All tests done in %d %s', [TotalTime.ElapsedTime(MilliSeconds), TimeIndicator[MilliSeconds]]));
    for s in Times do
      WriteLn(s);
  finally
    Times.Free;
  end
end;

end.
