unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, RTTI,
  Generics.Defaults, System.Generics.Collections,
  System.Diagnostics, AOCBase, RegularExpressions, System.DateUtils,
  System.StrUtils,
  System.Math, uAOCUtils, System.Types, PriorityQueues, System.Json,
  AocLetterReader, uAOCTimer, uAocGrid,
  System.Threading, System.SyncObjs, system.Hash;

type
  IntegerArray = Array Of Integer;

  TAdventOfCodeDay1 = class(TAdventOfCode)
  private
    IdList1, IdList2: TList<integer>;
    Counts: TDictionary<integer, integer>;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay2 = class(TAdventOfCode)
  private
    function CheckReactor(TolerateBadLevel: Boolean): integer;
    function IsReportSave(aRepport: IntegerArray; idxToIgnore: integer = -1): Boolean;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay3 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay4 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay5 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay6 = class(TAdventOfCode)
  private
    type TGridElement = (geWall, geEmpty, geGaurd);
    var
      SolutionA, SolutionB: integer;
      function CharToGridElement(Const aChar: Char): TGridElement;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay7 = class(TAdventOfCode)
  private
    function Solve(aUseConcatenation: boolean): int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay8 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TFileInfo = class
    FileSize, FileId: int64;
    Prev, Next: TFileInfo;
    constructor Create(aFileSize, aFileId: Int64);
  end;

  TAdventOfCodeDay9 = class(TAdventOfCode)
  private
    function DefragmentDisk(aKeepFilesTogether: boolean): int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay10 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay11 = class(TAdventOfCode)
  private
    function ObservePebbles(const aRounds: integer): int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay12 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: integer;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay13 = class(TAdventOfCode)
  private
    function PlayOnClawContraption(AddToTarget: int64): int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay14 = class(TAdventOfCode)
  private
    const
      RoomWidth: integer = 101;
      RoomHeigth: integer = 103;
    var
      FStartPositions, FVelocities: Array of TPosition;
    function CalcPosition(aSeconds: integer; aStartPosition, aVelocity: TPosition): TPosition;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  type TWarehouseElement = (Wall, Empty, Robot, Box, BoxLeft, BoxRight);
  TAdventOfCodeDay15 = class(TAdventOfCode)
  private
    function CharToWareHouseElement(const aChar: Char): TWarehouseElement;
    function WareHouseElementToChar(const aElement: TWarehouseElement): Char;
    function ParseInput(out aCommands: string): TAocGrid<TWarehouseElement>;
    procedure MoveRobot(aWareHouse: TAocGrid<TWarehouseElement>; aRobotStartPosition: TPosition; aCommands: string);
    function CalcGPSCoordinates(aWareHouse: TAocGrid<TWarehouseElement>; aElement: TWarehouseElement): integer;
    const
      WareHouseLabels: Array[TWarehouseElement] of string = ('#', '.', '@', 'O', '[', ']');
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay16 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay17 = class(TAdventOfCode)
  private
    FProgram: string;
    FParsedProgram: IntegerArray;
    FRegisterA , FRegisterB, FRegisterC: int64;
    function RunProgram(InitalARegister: int64): string;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay18 = class(TAdventOfCode)
  private
    FParsedInput: TList<TPosition>;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay19 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay20 = class(TAdventOfCode)
  private
    FTimes: TAocGrid<Integer>;
    FTrackPieces: TList<TPosition>;
    function CalcNoOfCheaths(aCheatTime: integer): integer;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay21 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay22 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay23 = class(TAdventOfCode)
  private
    FComputers: TDictionary<String, TList<String>>;    
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TOperation = (tAnd, tXor, tOr);
  ROperation = Record
    Adress1, Adress2, TargetAdress: string;
    Operation: TOperation;
    class function CreateFromString(aString: String): ROperation; static;
    function Equals(aOperation: TOperation; Const aAdress1, aAdress2: string): Boolean;
    function OtherAdress(Const aAddress: string): string;
  end;

  TAdventOfCodeDay24 = class(TAdventOfCode)
  private
    FWires: TDictionary<string, Boolean>;
    FOperations: TDictionary<string, ROperation>;
    function WireName(Const aPrefix: string; aIndex: integer): string;
    function RunProgram(aWires: TDictionary<string, Boolean>): int64;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay25 = class(TAdventOfCode)
  private
    function IntToChar(const aInt: Integer): char;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

implementation

{$REGION 'TAdventOfCodeDay1'}
procedure TAdventOfCodeDay1.BeforeSolve;
var
  s: string;
  split: TStringDynArray;
  id2, cnt: integer;
begin
  inherited;

  IdList1 := TList<integer>.Create;
  IdList2 := TList<integer>.Create;
  Counts := TDictionary<integer, integer>.Create;

  for s in FInput do
  begin
    split := SplitString(s, ' ');

    IdList1.Add(split[0].ToInteger);
    id2 := split[length(split)-1].ToInteger;
    IdList2.Add(id2);
    Counts.TryGetValue(id2, cnt);
    Counts.AddOrSetValue(id2, cnt + 1);
  end;
end;

procedure TAdventOfCodeDay1.AfterSolve;
begin
  inherited;

  IdList1.Free;
  IdList2.Free;
  Counts.Free;
end;

function TAdventOfCodeDay1.SolveA: Variant;
var
  i: integer;
begin
  IdList1.Sort;
  IdList2.Sort;

  Result := 0;
  for i := 0 to IdList1.Count -1 do
    result := Result + Abs(IdList1[i] - IdList2[i]);
end;

function TAdventOfCodeDay1.SolveB: Variant;
var
  i, cnt: integer;
begin
  Result := 0;
  for i in idList1 do
  begin
    Counts.TryGetValue(i, cnt);
    result := Result + i * cnt;
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay2'}
function TAdventOfCodeDay2.CheckReactor(TolerateBadLevel: Boolean): integer;
var
  split: TStringDynArray;
  s: string;
  i: integer;
  Report: IntegerArray;
begin
  Result := 0;
  for s in FInput do
  begin
    split := SplitString(s, ' ');

    SetLength(Report, Length(split));
    for i := 0 to Length(split)-1 do
      Report[i] := Split[i].ToInteger;

    if (not TolerateBadLevel) and IsReportSave(Report) then
      Result := Result + 1
    else if TolerateBadLevel then
    begin
      for i := 0 to length(Report)-1 do
      begin
        if IsReportSave(Report, i) then
        begin
          Result := Result + 1;
          break;
        end;
      end;
    end;
  end;
end;

function TAdventOfCodeDay2.IsReportSave(aRepport: IntegerArray; idxToIgnore: integer = -1): Boolean;

  function _idx(i: integer): integer;
  begin
    Result := i;
    if (idxToIgnore >= 0) and (IdxToIgnore <= i) then
      Inc(Result);
  end;

var
  i, diff, CheckSign, CurrentSign: integer;
begin
  CheckSign := Sign(aRepport[_idx(1)] - aRepport[_idx(0)]);
  for i := 1 to Length(aRepport) -1 - ifthen(idxToIgnore >= 0, 1, 0) do
  begin
    CurrentSign := Sign(aRepport[_idx(i)] - aRepport[_idx(i-1)]);
    diff := abs(aRepport[_idx(i)] - aRepport[_idx(i-1)]);
    if (CheckSign <> CurrentSign) or (diff < 1) or (diff > 3) then
      Exit(False)
  end;

  result := True;
end;

function TAdventOfCodeDay2.SolveA: Variant;
begin
  Result := CheckReactor(False);
end;

function TAdventOfCodeDay2.SolveB: Variant;
begin
  Result := CheckReactor(True);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay3'}
procedure TAdventOfCodeDay3.BeforeSolve;
var
  mulResult: Int64;
  s: string;
  split: TStringDynArray;
  Regex: TRegEx;
  Matches: TMatchCollection;
  Match: TMatch;
  Enabled: Boolean;
begin
  SolutionA := 0;
  SolutionB := 0;

  Regex := TRegex.Create('mul\(\d*,\d*\)|do\(\)|don''t\(\)');

  Enabled := True;
  for s in FInput do
  begin
    Matches := RegEx.Matches(s);
    for match in Matches do
    begin
      if Match.Value = 'do()' then
        Enabled := True
      else if Match.Value = 'don''t()' then
        Enabled := False
      else
      begin
        Split := SplitString(Copy(Match.Value, 5, Length(Match.Value)-5), ',');
        mulResult := Split[0].ToInt64 * Split[1].ToInt64;
        Inc(SolutionA, mulResult);

        if Enabled then
          Inc(SolutionB, mulResult);
      end;
    end;
  end;
end;

function TAdventOfCodeDay3.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay3.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay4'}
procedure TAdventOfCodeDay4.BeforeSolve;
const
  OffSetsA: array[0..7] of TAOCDirections = ([North],[East],[South],[West],[North,East],[North,West],[South,East],[South,West]);
  OffSetsB: array[0..3] of TAOCDirections = ([North,East],[South,East],[South,West],[North,West]);
var
  Grid: TAocGrid<Char>;

  function _checkPos(aBasePos: TPosition; aDirs: TAOCDirections; aDelta: integer; Const aExpectedChar: string): Boolean;
  var
    Pos: TPosition;
    Dir: TAOCDirection;
    chr: Char;
  begin
    Pos := aBasePos.Clone();
    for Dir in aDirs do
      Pos := Pos.ApplyDirection(Dir, aDelta);
    Result := Grid.TryGetValue(Pos, chr) and (chr = aExpectedChar);
  end;

var
  Dirs: TAOCDirections;
  i: Integer;
  Pair: TPair<TPosition, char>;
begin
  Grid := TAocGridHelper.CreateCharGrid(FInput);

  SolutionA := 0;
  SolutionB := 0;

  for Pair in Grid do
  begin
    if Pair.Value = 'X' then
      for Dirs in OffSetsA do
        if _checkPos(Pair.Key, Dirs, 1, 'M') and _checkPos(Pair.Key, Dirs, 2, 'A') and _checkPos(Pair.Key, Dirs, 3, 'S') then
          inc(SolutionA);

    if Pair.Value = 'A' then
      for i := 0 to 3 do
        if _checkPos(Pair.Key, OffSetsB[(i+0) mod 4], 1, 'M') and
           _checkPos(Pair.Key, OffSetsB[(i+1) mod 4], 1, 'M') and
           _checkPos(Pair.Key, OffSetsB[(i+2) mod 4], 1, 'S') and
           _checkPos(Pair.Key, OffSetsB[(i+3) mod 4], 1, 'S') then
        begin
          inc(SolutionB);
          Break;
        end;
  end;
  Grid.Free;
end;

function TAdventOfCodeDay4.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay4.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay5'}
type
  TPageComparer = class (TInterfacedObject, IComparer<integer>)
  private
    FRules: TDictionary<integer, boolean{greatherThen}>;

    function GenerateKey(x, y: integer): integer;
    function Compare(const Left, Right: integer): Integer;

  public
    constructor create; reintroduce;
    destructor Destroy; override;

    procedure AddGreaterThenRule(x, y: integer);
    procedure AddSmallerThenRule(x, y: integer);
  end;

{ TPageComparer }

constructor TPageComparer.create;
begin
  FRules := TDictionary<integer, boolean{greatherThen}>.Create;
end;

destructor TPageComparer.Destroy;
begin
  FRules.Free;

  inherited;
end;

function TPageComparer.Compare(const Left, Right: integer): Integer;
begin
  Result := 1;

  if Left = Right then
    Exit(0);

  if not FRules[GenerateKey(Left, Right)] then
    Result := -1
end;

procedure TPageComparer.AddGreaterThenRule(x, y: integer);
begin
  FRules.Add(GenerateKey(x, y), true);
end;

procedure TPageComparer.AddSmallerThenRule(x, y: integer);
begin
  FRules.Add(GenerateKey(x, y), false);
end;

function TPageComparer.GenerateKey(x, y: integer): integer;
begin
  Result := (x shl 16) + y;
end;

procedure TAdventOfCodeDay5.BeforeSolve;
var
  i,j: integer;
  Split: TStringDynArray;
  Page: TList<integer>;
  x,y: integer;
  ReadingRules, Valid: Boolean;
  Comparer: TPageComparer;
begin
  ReadingRules := True;
  SolutionA := 0;
  SolutionB := 0;

  Comparer := TPageComparer.create;
  Page := TList<integer>.Create(Comparer);

  for i := 0 to FInput.Count -1 do
  begin
    if FInput[i] = '' then
    begin
      ReadingRules := False;
      continue;
    end;

    if ReadingRules then
    begin
      Split := SplitString(FInput[i], '|');
      x := Split[0].ToInteger;
      y := Split[1].ToInteger;

      Comparer.AddSmallerThenRule(x, y);
      Comparer.AddGreaterThenRule(y, x);
      Continue;
    end;

    Split := SplitString(FInput[i], ',');
    Valid := True;
    Page.Clear;

    for j := 0 to Length(split) - 1 do
      Page.Add(split[j].ToInteger);

    Page.Sort;
    for j := 0 to Length(split) - 1 do
    begin
      if Page[j] <> split[j].ToInteger then
      begin
        Valid := False;
        Break;
      end;
    end;

    if Valid then
      Inc(SolutionA, Page[Page.Count shr 1])
    else
      Inc(SolutionB, Page[Page.Count shr 1]);
  end;

  Page.Free;
end;

function TAdventOfCodeDay5.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay5.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay6'}
function TAdventOfCodeDay6.CharToGridElement(const aChar: Char): TGridElement;
begin
  Result := TGridElement(IndexStr(aChar, ['#','.','^']));
end;

procedure TAdventOfCodeDay6.BeforeSolve;
var
  Grid: TAocStaticGrid<TGridElement>;

  function _CreateTask(aGaurdPosition, aBlock: TPosition; aGaurdFacing: TAOCDirection): ITask;
  begin
    Result := TTask.Create(procedure()
      var
        Seen: TAocStaticGrid<integer>;
        GaurdPosition, Next: TPosition;
        GaurdFacing: TAOCDirection;
        GaurdFacings, intGaurdFacing: integer;
        chr: TGridElement;
      begin
        Seen := TAocStaticGrid<integer>.Create(Grid.MaxX, Grid.MaxY);
        GaurdPosition := aGaurdPosition;
        GaurdFacing := aGaurdFacing;

        try
          while True do
          begin
            next := GaurdPosition.Clone.ApplyDirection(GaurdFacing);
            if not Grid.TryGetValue(next.x, next.y, Chr) then
              Exit;

            if (chr = TGridElement.geWall) or (next.CacheKey = aBlock.CacheKey) then
            begin
              GaurdFacing := RotateDirection(GaurdFacing, 1);
              intGaurdFacing := 1 shl ord(GaurdFacing);

              if Seen.TryGetValue(GaurdPosition, GaurdFacings) then
              begin
                if (GaurdFacings and intGaurdFacing) = intGaurdFacing then
                begin
                  TInterlocked.Increment(SolutionB);
                  Exit;
                end;
              end;

              Seen.SetData(GaurdPosition, GaurdFacings or intGaurdFacing);
            end
            else
              GaurdPosition := next;
          end;
        finally
          Seen.Free;
        end;
      end);
    Result.Start;
  end;

var
  chr: TGridElement;
  GaurdPosition, Next: TPosition;
  GaurdFacing: TAOCDirection;
  Tasks: TList<ITask>;
  SeenA: TAocStaticGrid<Boolean>;
  Pair: TPair<TPosition, TGridElement>;
begin
  inherited;

  Tasks := TList<ITask>.Create;

  SolutionA := 1;
  SolutionB := 0;

  Grid := TAocStaticGrid<TGridElement>.Create(FInput, CharToGridElement, nil);
  SeenA := TAocStaticGrid<Boolean>.Create(Grid.MaxX, Grid.MaxY);

  for Pair in Grid do
    if Pair.Value = TGridElement.geGaurd then
      GaurdPosition := Pair.Key;

  GaurdFacing := TAOCDirection.North;
  SeenA.SetData(GaurdPosition, true);
  while True do
  begin
    next := GaurdPosition.Clone.ApplyDirection(GaurdFacing);
    if not Grid.TryGetValue(next.x, next.y, Chr) then
      Break; // Out of bounds

    if chr = TGridElement.geWall then
      GaurdFacing := RotateDirection(GaurdFacing, 1)
    else
    begin
      if not SeenA.GetValue(Next) then
      begin
        SeenA.SetData(Next, True);
        Inc(SolutionA);
        Tasks.Add(_CreateTask(GaurdPosition, Next, GaurdFacing));
      end;

      GaurdPosition := next;
    end;
  end;

  TTask.WaitForAll(tasks.ToArray);
  Tasks.Free;
  SeenA.Free;
  Grid.Free;
end;

function TAdventOfCodeDay6.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay6.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay7'}
function TAdventOfCodeDay7.Solve(aUseConcatenation: boolean): int64;

  function Calc(values: TStringDynArray; const aTarget, aIndex: int64): boolean;
  var
    strValue: string;
    value: int64;
  begin
    if aIndex = 2 then
      exit(aTarget = Values[2].ToInt64);

    strValue := values[aIndex];
    value := strValue.ToInt64;

    Result :=
      Calc(Values, aTarget - value, aIndex - 1) or
      ((aTarget mod value = 0) and Calc(Values, aTarget div value, aIndex - 1)) or
      (aUseConcatenation and aTarget.ToString.EndsWith(strValue) and Calc(Values, aTarget div round(power(10, Length(strValue))), aIndex - 1));
  end;

var
  target: int64;
  s: string;
  split: TStringDynArray;
begin
  Result := 0;

  for s in FInput do
  begin
    split := SplitString(s, ': ');
    target := Split[0].ToInt64;
    if Calc(split, target, Length(Split)-1) then
      Inc(result, target);
  end;
end;

function TAdventOfCodeDay7.SolveA: Variant;
begin
  Result := Solve(False);
end;

function TAdventOfCodeDay7.SolveB: Variant;
begin
  Result := Solve(True);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay8'}
procedure TAdventOfCodeDay8.BeforeSolve;
var
  Grid: TAocGrid<Char>;

  procedure CheckAntinode_A(Seen: TDictionary<int64, boolean>; LocI, LocJ, Start, Delta: TPosition);
  var
    Pos: TPosition;
  begin
    Pos := TPosition.Create(Start.x + Delta.X, Start.Y + Delta.Y);
    if (Pos.CacheKey <> LocI.CacheKey) and (Pos.CacheKey <> LocJ.CacheKey) and InRange(Pos.X, 0, Grid.MaxX-1) and InRange(pos.y, 0, Grid.MaxY-1) then
      Seen.AddOrSetValue(Pos.CacheKey, true);
  end;

  procedure CheckAntinode_B(Seen: TDictionary<int64, boolean>; Start, Delta: TPosition);
  var
    Pos: TPosition;
  begin
    Pos := TPosition.Create(Start.x, Start.Y);

    while InRange(Pos.X, 0, Grid.MaxX-1) and InRange(pos.y, 0, Grid.MaxY-1) do
    begin
      Seen.AddOrSetValue(Pos.CacheKey, True);
      Pos.AddDelta(Delta.X, Delta.Y);
    end;
  end;

var
  Antennas: TDictionary<char, TList<TPosition>>;
  lst: TList<TPosition>;
  x,y,i,j: integer;
  posI, posJ: TPosition;
  SeenA, SeenB: TDictionary<int64, boolean>;
  Pair: TPair<TPosition,char>;
begin
  Grid := TAocGridHelper.CreateCharGrid(FInput);
  Antennas := TObjectDictionary<char, TList<TPosition>>.Create([doOwnsValues]);
  SeenA := TDictionary<int64, boolean>.Create;
  SeenB := TDictionary<int64, boolean>.Create;

  for Pair in Grid do
  begin
    if Pair.Value = '.' then
      continue;

    if not Antennas.TryGetValue(Pair.Value, lst) then
    begin
      lst := TList<TPosition>.Create;
      Antennas.Add(Pair.Value, lst);
    end;

    lst.Add(Pair.Key);
  end;

  for lst in Antennas.Values do
  begin
    for i := 0 to lst.Count - 2 do
      for j := i + 1 to lst.Count -1 do
      begin
        posI := lst[i];
        posJ := lst[j];

        x := posI.x - posJ.x;
        y := posI.y - posJ.y;

        CheckAntinode_A(SeenA, PosI, PosJ, PosI, TPosition.Create(x, y));
        CheckAntinode_A(SeenA, PosI, PosJ, PosI, TPosition.Create(-x, -y));
        CheckAntinode_A(SeenA, PosI, PosJ, PosJ, TPosition.Create(x, y));
        CheckAntinode_A(SeenA, PosI, PosJ, PosJ, TPosition.Create(-x, -y));

        CheckAntinode_B(SeenB, PosI, TPosition.Create(x, y));
        CheckAntinode_B(SeenB, PosI, TPosition.Create(-x, -y));
      end;
  end;

  SolutionA := SeenA.Count;
  SolutionB := SeenB.Count;

  SeenA.Free;
  SeenB.Free;
  Grid.Free;
  Antennas.Free;
end;

function TAdventOfCodeDay8.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay8.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay9'}
constructor TFileInfo.Create(aFileSize, aFileId: Int64);
begin
  FileSize := aFileSize;
  FileId := aFileId;
end;

function TAdventOfCodeDay9.DefragmentDisk(aKeepFilesTogether: boolean): int64;
var
  FileInfoList: TList<TFileInfo>;

  function CreateFileInfo(aFileSize, aFileId: int64): TFileInfo;
  begin
    Result := TFileInfo.Create(aFileSize, aFileId);
    FileInfoList.Add(Result);
  end;

var
  FileId, FileSize, TargetIdx: int64;
  i, MaxLengthToFind, SearchHeadIdx: integer;
  Head, LastFile, Target, SearchHead, Inserted, Prev: TFileInfo;
  SearchHeads: Array[1..9] of TFileInfo;
  isEmpty, MoveLastFilePointer: Boolean;
begin
  Head := nil;
  Prev := nil;
  LastFile := nil;
  FileId := 0;
  isEmpty := False;
  FileInfoList := TObjectList<TFileInfo>.Create(true);

  for i := 1 to length(FInput[0]) do
  begin
    FileSize := StrToInt(FInput[0][i]);
    if isEmpty then
      LastFile := CreateFileInfo(FileSize, -1)
    else
      LastFile := CreateFileInfo(FileSize, FileId);

    if not isEmpty then
      Inc(FileId);

    isEmpty := not isEmpty;
    if not Assigned(Head) then
      Head := LastFile;
    LastFile.Prev := Prev;
    if Assigned(Prev) then
      Prev.Next := LastFile;
    Prev := LastFile;
  end;

  MaxLengthToFind := 9;
  for I := 1 to 9 do
    SearchHeadS[i] := Head;

  while Assigned(LastFile.Prev) do
  begin
    if (LastFile.FileId = -1) {not a file} or (aKeepFilesTogether and (MaxLengthToFind < LastFile.FileSize)) {already to big} then
    begin
      LastFile := LastFile.Prev;
      Continue;
    end;

    Target := nil;
    SearchHeadIdx := 1;
    if aKeepFilesTogether then
      SearchHeadIdx := LastFile.FileSize;

    SearchHead := SearchHeads[SearchHeadIdx];
    while Assigned(SearchHead) and (SearchHead <> LastFile) do
    begin
      if (SearchHead.FileId = -1) and ((SearchHead.FileSize >= LastFile.FileSize) or not aKeepFilesTogether) then
      begin
        Target := SearchHead;
        break
      end;

      SearchHead := SearchHead.Next;
    end;
    SearchHeads[SearchHeadIdx] := Target;

    MoveLastFilePointer := True;
    if Assigned(Target) then
    begin
      if Target.FileSize = LastFile.FileSize then
      begin
        Target.FileId := LastFile.FileId; // target has the same length as the file, update the reference in target
        LastFile.FileId := -1; // Mark this spot as free
      end
      else if Target.FileSize > LastFile.FileSize then
      begin // Found spot is bigger then the file, adjust size and make a new file entry
        Target.FileSize := Target.FileSize - LastFile.FileSize;

        Inserted := CreateFileInfo(LastFile.FileSize, LastFile.FileId);
        Inserted.Next := Target;
        Inserted.Prev := Target.Prev;
        Target.Prev.Next := Inserted;
        Target.Prev := Inserted;
        LastFile.FileId := -1; // Mark this spot as free
      end
      else // Found spot is not big enough
      begin
        Target.FileId := LastFile.FileId;
        LastFile.FileSize := LastFile.FileSize - Target.FileSize;
        MoveLastFilePointer := False; // Dont move pointer in this case, there is still a part of this file to move
      end;
    end
    else // No spot found for this record, mark this length (and all above) so we can skip the lookups
      MaxLengthToFind := min(MaxLengthToFind, LastFile.FileSize);

    if MoveLastFilePointer then
      LastFile := LastFile.Prev;
  end;

  TargetIdx := 0;
  Result := 0;
  while Assigned(Head) do
  begin
    if Head.FileId = -1 then
      Inc(TargetIdx, Head.FileSize)
    else
      for i := 0 to Head.FileSize -1 do
      begin
        inc(Result, TargetIdx * Head.FileId);
        inc(TargetIdx)
      end;

    Head := Head.Next;
  end;
  FileInfoList.Free;
end;

function TAdventOfCodeDay9.SolveA: Variant;
begin
  Result := DefragmentDisk(False);
end;

function TAdventOfCodeDay9.SolveB: Variant;
begin
  Result := DefragmentDisk(True);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay10'}
procedure TAdventOfCodeDay10.BeforeSolve;
var
  Grid: TAocGrid<integer>;

  procedure _Calc(aPosition: TPosition);
  var
    SeenTrailHeads: TDictionary<TPosition,integer>;
    Work: TStack<TPosition>;
    Current, Next: TPosition;
    height, nextHeight, pathCount: integer;
    dir: TAOCDirection;
  begin
    SeenTrailHeads := TDictionary<TPosition,integer>.Create;
    Work := TStack<TPosition>.Create;
    Work.Push(aPosition);

    while Work.Count > 0 do
    begin
      Current := Work.Pop;
      if not grid.TryGetValue(Current, height) then
        Continue;

      if height = 9 then
      begin
        SeenTrailHeads.TryGetValue(Current, pathCount);
        SeenTrailHeads.AddOrSetValue(Current, pathCount + 1);
        Continue;
      end;

      nextHeight := height + 1;
      for dir in [North, East, South, West] do
      begin
        Next := Current.Clone.ApplyDirection(dir);
        if grid.TryGetValue(next, height) and (height = nextHeight) then
          Work.Push(Next);
      end;
    end;

    Inc(SolutionA, SeenTrailHeads.Count);
    for pathCount in SeenTrailHeads.Values do
      Inc(SolutionB, pathCount);

    SeenTrailHeads.Free;
    Work.Free;
  end;

var
  Pair: TPair<TPosition,integer>;
begin
  SolutionA := 0;
  SolutionB := 0;
  Grid := TAocGridHelper.CreateIntegerGrid(FInput);

  for Pair in Grid do
    if Pair.Value = 0 then
      _Calc(Pair.Key);

  Grid.Free;
end;

function TAdventOfCodeDay10.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay10.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay11'}
function TAdventOfCodeDay11.ObservePebbles(const aRounds: integer): int64;

  function _FindBaseIdx(aValue: int64): integer;
  begin
    Result := 0;

    while True do
    begin
      if aValue < Base10Table[Result+1] then
        Exit;

      inc(Result);
    end;
  end;

  procedure _UpdateStoneList(aStoneList: TDictionary<int64, int64>; aStone, aCount: Int64);
  var
    CurrentCount: Int64;
  begin
    aStoneList.TryGetValue(aStone, CurrentCount);
    aStoneList.AddOrSetValue(aStone, CurrentCount + aCount);
  end;

var
  i: Integer;
  l, Base: int64;
  split: TStringDynArray;
  Stones, NewStones: TDictionary<int64, int64>;
  Pair: TPair<int64, int64>;
begin
  split := SplitString(FInput[0], ' ');
  Stones := TDictionary<int64, int64>.Create;

  for i := 0 to Length(split) -1 do
    _UpdateStoneList(Stones, split[i].ToInt64(), 1);

  NewStones := TDictionary<int64, int64>.Create(Stones.Count * 2);
  for i := 1 to aRounds do
  begin
    for Pair in Stones do
    begin
      if Pair.Key = 0 then
      begin
        _UpdateStoneList(NewStones, 1, Pair.Value);
        Continue
      end;

      l := _FindBaseIdx(Pair.Key);
      if (l and 1) = 1 then
      begin
        Base := Base10Table[1+ (l shr 1)];
        _UpdateStoneList(NewStones, Pair.Key div Base, Pair.Value);
        _UpdateStoneList(NewStones, Pair.Key mod Base, Pair.Value);
      end
      else
        _UpdateStoneList(NewStones, Pair.Key * 2024, Pair.Value);
    end;

    stones.Free;
    Stones := NewStones;
    NewStones := TDictionary<Int64, int64>.Create(Stones.Count * 2);
  end;

  Result := 0;
  for Pair in Stones do
    Inc(Result, Pair.Value);
  Stones.Free;
  NewStones.Free;
end;

function TAdventOfCodeDay11.SolveA: Variant;
begin
  Result := ObservePebbles(25);
end;

function TAdventOfCodeDay11.SolveB: Variant;
begin
  Result := ObservePebbles(75);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay12'}
procedure TAdventOfCodeDay12.BeforeSolve;
var
  CurrentArea, FenceCount, SideCount: Integer;
  Grid: TAocGrid<Char>;
  Chr1, Chr2, chr3: char;
  Seen: TAocStaticGrid<Boolean>;
  Work: TStack<TPosition>;
  Pos: TPosition;
  Dir: TAOCDirection;
  Pair: TPair<TPosition,Char>;
begin
  SolutionA := 0;
  SolutionB := 0;

  Grid := TAocGridHelper.CreateCharGrid(FInput);
  Seen := TAocStaticGrid<Boolean>.Create(Grid.MaxX, Grid.MaxY);
  Work := TStack<TPosition>.Create;

  for Pair in Grid do
  begin
    if Seen.GetValue(Pair.Key) then
      Continue;

    CurrentArea := 0;
    FenceCount := 0;
    SideCount := 0;

    Work.Push(Pair.Key);

    while Work.Count > 0 do
    begin
      Pos := Work.Pop;

      if Seen.GetValue(Pos) then
        Continue;

      Seen.SetData(Pos, True);

      Inc(CurrentArea);
      for dir in [North, East, South, West] do
      begin
        Grid.TryGetValue(Pos.Clone.ApplyDirection(dir), Chr1);
        Grid.TryGetValue(Pos.Clone.ApplyDirection(RotateDirection(dir, 1)), Chr2);

        if (chr1 <> Pair.Value) then
        begin
          Inc(FenceCount);
          if Chr2 <> Pair.Value then
            Inc(SideCount);
        end
        else
        begin
          Work.Push(Pos.Clone.ApplyDirection(Dir));
          if (chr2 = Pair.Value) and Grid.TryGetValue(Pos.Clone.ApplyDirection(dir).ApplyDirection(RotateDirection(dir, 1)), Chr3) and (Chr3 <> Pair.Value) then
            Inc(SideCount);
        end;

      end;
    end;

    inc(SolutionA, CurrentArea * FenceCount);
    Inc(SolutionB, CurrentArea * SideCount);
  end;

  Grid.Free;
  Seen.Free;
  Work.Free;
end;

function TAdventOfCodeDay12.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay12.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay13'}
function TAdventOfCodeDay13.PlayOnClawContraption(AddToTarget: int64): int64;

  function Determinant(a, b, c, d: Double): Double;
  begin
    Result := (a * d) - (b * c);
  end;

var
  i: int64;
  split: TStringDynArray;
  aX, aY, bX, bY, TargetX, TargetY, CountA, CountB: int64;
  det, detA, DetB: Double;
begin
  i := 0;
  Result := 0;

  while i <= FInput.Count do
  begin
    split := SplitString(FInput[i], ' +,');
    aX := Split[3].ToInt64;
    aY := Split[6].ToInt64;

    split := SplitString(FInput[i+1], ' +,');
    bX := Split[3].ToInt64;
    bY := Split[6].ToInt64;

    split := SplitString(FInput[i+2], ' =,');
    TargetX := AddToTarget + Split[2].ToInt64;
    TargetY := AddToTarget + Split[5].ToInt64;

    Inc(i, 4);

    // https://www.cliffsnotes.com/study-guides/algebra/algebra-ii/linear-sentences-in-two-variables/linear-equations-solutions-using-determinants-with-two-variables
    det := Determinant(aX, bX, Ay, bY);
    if det <> 0 then
    begin
      detA := Determinant(TargetX, bX, TargetY, bY);
      DetB := Determinant(aX, TargetX, Ay, TargetY);

      CountA := Trunc(DetA / det);
      CountB := Trunc(DetB / det);

      if (CountA * ax + CountB * bx = TargetX) and (CountA * aY + CountB * bY = TargetY) then
        inc(Result, CountA * 3 + CountB);
    end;
  end;
end;

function TAdventOfCodeDay13.SolveA: Variant;
begin
  Result := PlayOnClawContraption(0);
end;

function TAdventOfCodeDay13.SolveB: Variant;
begin
  Result := PlayOnClawContraption(10000000000000);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay14'}
procedure TAdventOfCodeDay14.BeforeSolve;
var
  i: Integer;
  Split: TStringDynArray;
begin
  inherited;
  SetLength(FStartPositions, FInput.Count);
  SetLength(FVelocities, FInput.Count);

  for i := 0 to FInput.Count -1 do
  begin
    Split := SplitString(FInput[i], '=, ');
    FStartPositions[i] := TPosition.Create(Split[1].ToInteger, Split[2].ToInteger);
    FVelocities[i] := TPosition.Create((Split[4].ToInteger + RoomWidth) mod RoomWidth, (Split[5].ToInteger + RoomHeigth) mod RoomHeigth);
  end;
end;

function TAdventOfCodeDay14.CalcPosition(aSeconds: integer; aStartPosition, aVelocity: TPosition): TPosition;
begin
  Result := TPosition.Create(
    (aStartPosition.x + aSeconds * aVelocity.x) mod RoomWidth,
    (aStartPosition.y + aSeconds * aVelocity.y) mod RoomHeigth);
end;

function TAdventOfCodeDay14.SolveA: Variant;
var
  i, Q1, Q2, Q3, Q4: integer;
  Position: TPosition;
begin
  Q1 :=0;
  Q2 :=0;
  Q3 :=0;
  Q4 :=0;

  for i := 0 to FInput.Count -1 do
  begin
    Position := CalcPosition(100, FStartPositions[i], FVelocities[i]);

    if (Position.X < RoomWidth shr 1) and (Position.Y < RoomHeigth shr 1) then
      Inc(Q1);
    if (Position.X < RoomWidth shr 1) and (Position.Y > RoomHeigth shr 1) then
      Inc(Q2);
    if (Position.X > RoomWidth shr 1) and (Position.Y < RoomHeigth shr 1) then
      Inc(Q3);
    if (Position.X > RoomWidth shr 1) and (Position.Y > RoomHeigth shr 1) then
      Inc(Q4);
  end;

  Result := Q1 * Q2 * Q3 * Q4;
end;

function TAdventOfCodeDay14.SolveB: Variant;
var
  i: integer;
  AllUnique: Boolean;
  Grid: TAocStaticGrid<Boolean>;
  Position: TPosition;
begin
  Result := 0;
  AllUnique := False;

  while not AllUnique do
  begin
    Grid := TAocStaticGrid<Boolean>.Create(RoomWidth, RoomHeigth);
    Inc(Result);

    for i := 0 to FInput.Count-1 do
    begin
      Position := CalcPosition(Result, FStartPositions[i], FVelocities[i]);

      AllUnique := not Grid.GetValue(Position);
      if not AllUnique then
        Break;
      Grid.SetData(Position, True);
    end;

    Grid.Free;
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay15'}
function TAdventOfCodeDay15.CharToWareHouseElement(const aChar: Char): TWarehouseElement;
begin
  Result := TWarehouseElement(IndexStr(aChar, WareHouseLabels))
end;

function TAdventOfCodeDay15.WareHouseElementToChar(const aElement: TWarehouseElement): Char;
begin
  Result := WareHouseLabels[aElement][1];
end;

function TAdventOfCodeDay15.ParseInput(out aCommands: string): TAocGrid<TWarehouseElement>;
var
  map: TStringList;
  GridLine: Boolean;
  i: integer;
begin
  Map := TStringList.Create;
  GridLine := True;
  aCommands := '';
  for i := 0 to FInput.Count-1 do
  begin
    GridLine := GridLine and (FInput[i] <> '');
    if GridLine then
      Map.Add(FInput[i])
    else
      aCommands := aCommands + FInput[i];
  end;
  Result := TAocStaticGrid<TWarehouseElement>.Create(Map, CharToWareHouseElement, WareHouseElementToChar);
  map.Free;
end;

type TUndoData = record
  Position: TPosition;
  Element: TWarehouseElement;
  class function Create(aPosition: TPosition; aElement: TWarehouseElement): TUndoData; static;
end;

class function TUndoData.Create(aPosition: TPosition; aElement: TWarehouseElement): TUndoData;
begin
  Result.Position := aPosition;
  Result.Element := aElement;
end;

procedure TAdventOfCodeDay15.MoveRobot(aWareHouse: TAocGrid<TWarehouseElement>; aRobotStartPosition: TPosition; aCommands: string);
var
  UndoStack: TStack<TUndoData>;

  function _SimpleMove(aCurrentPosition: TPosition; aDirection: TAOCDirection): boolean;
  var
    CurrentElement, NextElement: TWarehouseElement;
    TargetPosition: TPosition;
  begin
    CurrentElement := aWareHouse.GetValue(aCurrentPosition);
    TargetPosition := aCurrentPosition.Clone.ApplyDirection(aDirection);

    NextElement := aWareHouse.GetValue(TargetPosition);
    if NextElement = Wall then
      Exit(False);

    Result := (NextElement = TWarehouseElement.Empty) or _SimpleMove(TargetPosition, aDirection);

    if not Result then
      Exit(False);

    aWareHouse.SetData(aCurrentPosition, Empty);
    aWareHouse.SetData(TargetPosition, CurrentElement);
  end;

  procedure SetElementWithUndo(aPostion: TPosition; aNewElement: TWarehouseElement);
  begin
    UndoStack.Push(TUndoData.Create(aPostion, aWareHouse.GetValue(aPostion)));
    aWareHouse.SetData(aPostion, aNewElement);
  end;

  function _MoveUpDown(aCurrentPosition: TPosition; aDirection: TAocDirection): Boolean;
  const OffSets: array[Boolean] of integer = (1, -1);
  var
    CurrentElement, NeighborElement, NextElement, NextNeigborElement: TWarehouseElement;
    NeighborPosition, TargetPosition, NeighborTargetPosition: TPosition;
  begin
    CurrentElement := aWareHouse.GetValue(aCurrentPosition);
    NeighborPosition := aCurrentPosition.Clone.AddDelta(Offsets[CurrentElement = TWarehouseElement.BoxRight], 0);
    NeighborElement := aWareHouse.GetValue(NeighborPosition);
    TargetPosition := aCurrentPosition.Clone.ApplyDirection(aDirection);
    NeighborTargetPosition := NeighborPosition.Clone.ApplyDirection(aDirection);

    NextElement := aWareHouse.GetValue(TargetPosition);
    NextNeigborElement := aWareHouse.GetValue(NeighborTargetPosition);

    if (NextElement = Wall) or (NextNeigborElement = Wall) then
      Exit(False);

    Result := (NextElement = TWarehouseElement.Empty) or _MoveUpDown(TargetPosition, aDirection);

    if not Result then
      Exit;

    NextNeigborElement := aWareHouse.GetValue(NeighborTargetPosition);
    Result := (NextNeigborElement = TWarehouseElement.Empty) or _MoveUpDown(NeighborTargetPosition, aDirection);

    if not Result then
      Exit(False);


    SetElementWithUndo(aCurrentPosition, Empty);
    SetElementWithUndo(TargetPosition, CurrentElement);
    SetElementWithUndo(NeighborPosition, Empty);
    SetElementWithUndo(NeighborTargetPosition, NeighborElement);
  end;

var
  i: Integer;
  RobotPosition: TPosition;
  Dir: TAOCDirection;
  Element: TWarehouseElement;
  UndoData: TUndoData;
begin
  UndoStack := TStack<TUndoData>.Create;
  RobotPosition := aRobotStartPosition;
  aWareHouse.SetData(RobotPosition, Empty);

  for i := 1 to Length(aCommands) do
  begin
    Dir := TAOCDirection(IndexStr(aCommands[i], ['^', '>', 'v', '<']));
    Element := aWareHouse.GetValue(RobotPosition.Clone.ApplyDirection(Dir));

    if Element = Empty then
      RobotPosition := RobotPosition.Clone.ApplyDirection(Dir) // nothing there, move
    else if (Element = Box) or (Dir in [East, West]) and (Element in [BoxLeft, BoxRight]) then
    begin
      if _SimpleMove(RobotPosition.Clone.ApplyDirection(Dir), Dir) then
        RobotPosition := RobotPosition.Clone.ApplyDirection(Dir)
    end
    else if (Element in [BoxLeft, BoxRight]) then
    begin
      UndoStack.Clear;
      if _MoveUpDown(RobotPosition.Clone.ApplyDirection(Dir), Dir) then
        RobotPosition := RobotPosition.Clone.ApplyDirection(Dir)
      else
      begin
        while UndoStack.Count > 0 do
        begin
          UndoData := UndoStack.Pop;
          aWareHouse.SetData(UndoData.Position, UndoData.Element);
        end
      end
    end;
  end;

  UndoStack.Free;
end;

function TAdventOfCodeDay15.CalcGPSCoordinates(aWareHouse: TAocGrid<TWarehouseElement>; aElement: TWarehouseElement): integer;
var
  Pair: TPair<TPosition, TWarehouseElement>;
begin
  Result := 0;
  for Pair in aWareHouse do
    if Pair.Value = aElement then
      Inc(Result, Pair.Key.X + 100 * Pair.Key.Y);
end;

function TAdventOfCodeDay15.SolveA: Variant;
var
  Commands: string;
  WareHouse: TAocGrid<TWarehouseElement>;
  RobotStartPosition: TPosition;
  Pair: TPair<TPosition, TWarehouseElement>;
begin
  WareHouse := ParseInput(Commands);
  for Pair in WareHouse do
    if Pair.Value = TWarehouseElement.Robot then
    begin
      RobotStartPosition := Pair.Key;
      break;
    end;

  MoveRobot(WareHouse, RobotStartPosition, Commands);
  Result := CalcGPSCoordinates(WareHouse, Box);
  WareHouse.Free;
end;

function TAdventOfCodeDay15.SolveB: Variant;
var
  WareHouse, ExpandedWareHouse: TAocGrid<TWarehouseElement>;
  Commands: string;
  x,y: Integer;
  RobotPosition: TPosition;
  Elemenent: TWarehouseElement;
begin
  WareHouse := ParseInput(Commands);

  ExpandedWareHouse := TAocStaticGrid<TWarehouseElement>.Create(WareHouse.MaxX * 2, WareHouse.MaxY, WareHouseElementToChar);
  for x := 0 to WareHouse.MaxX-1 do
    for y := 0 to WareHouse.MaxY-1 do
    begin
      Elemenent := WareHouse.GetValue(x, y);
      case Elemenent of
        Wall, Empty:
          begin
            ExpandedWareHouse.SetData(x*2, y, Elemenent);
            ExpandedWareHouse.SetData(x*2 +1, y, Elemenent);
          end;
        Box:
          begin
            ExpandedWareHouse.SetData(x*2, y, BoxLeft);
            ExpandedWareHouse.SetData(x*2 +1, y, BoxRight);
          end;
        Robot:
          begin
            ExpandedWareHouse.SetData(x*2, y, Robot);
            ExpandedWareHouse.SetData(x*2 +1, y, Empty);
            RobotPosition := TPosition.Create(x*2, y);
          end;
        end;
    end;

  MoveRobot(ExpandedWareHouse, RobotPosition, Commands);
  Result := CalcGPSCoordinates(ExpandedWareHouse, TWarehouseElement.BoxLeft);

  WareHouse.Free;
  ExpandedWareHouse.Free;
end;

{$ENDREGION}
{$REGION 'TAdventOfCodeDay16'}
type TReindeerMazePath = class
  Parent: TReindeerMazePath;
  Position: TPosition;
  Faceing: TAOCDirection;
  Score: Integer;
  AlternativePaths: TList<TReindeerMazePath>;
  SeenPositions: TList<int64>;

  Constructor Create(aParent: TReindeerMazePath; aPosition: TPosition; aFacing: TAOCDirection; aScore: integer);
  destructor Destroy; override;
  procedure AddSeenPositionsToList(Seen: TDictionary<int64, Boolean>);
end;

procedure TReindeerMazePath.AddSeenPositionsToList(Seen: TDictionary<int64, Boolean>);
var
  Pos: int64;
  Alt: TReindeerMazePath;
begin
  for Pos in SeenPositions do
    Seen.AddOrSetValue(Pos, True);
  for Alt in AlternativePaths do
    Alt.AddSeenPositionsToList(Seen);
  if Assigned(Parent) then
    Parent.AddSeenPositionsToList(Seen);
end;

constructor TReindeerMazePath.Create(aParent: TReindeerMazePath; aPosition: TPosition; aFacing: TAOCDirection; aScore: integer);
begin
  Parent := aParent;
  Position := aPosition;
  Faceing := aFacing;
  Score := aScore;
  AlternativePaths := TList<TReindeerMazePath>.Create;
  SeenPositions := TList<int64>.Create;
end;

destructor TReindeerMazePath.Destroy;
begin
  AlternativePaths.Free;
  SeenPositions.Free;
  inherited;
end;

procedure TAdventOfCodeDay16.BeforeSolve;
var
  Paths: TList<TReindeerMazePath>;

  function _CreateOrUpdatePath(aDoCreate: Boolean; aCurrentPath: TReindeerMazePath; aPosition: TPosition; aFaceing: TAOCDirection; aScore: Integer): TReindeerMazePath;
  begin
    if aDoCreate then
    begin
      Result := TReindeerMazePath.Create(aCurrentPath, aPosition, aFaceing, aScore);
      Paths.Add(Result);
      Exit;
    end;

    Result := aCurrentPath;
    Result.Position := aPosition;
    Result.Faceing := aFaceing;
    Result.Score := aScore;
  end;

var
  i: Integer;
  DidRotate, CanRotate: boolean;
  Grid: TAocGrid<Char>;
  Pair: TPair<TPosition,Char>;
  Reindeer, Target, Next: TPosition;
  Work, NextWork: TReindeerMazePath;
  Queue: PriorityQueue<Integer, TReindeerMazePath>;
  Best: TPair<integer, TReindeerMazePath>;
  Seen: TAocGrid<TPair<integer, TReindeerMazePath>>;
  SeenPositions: TDictionary<int64,Boolean>;
begin
  Grid := TAocGridHelper.CreateCharGrid(FInput);
  Paths := TObjectList<TReindeerMazePath>.Create(True);
  Seen := TAocStaticGrid<TPair<integer, TReindeerMazePath>>.Create(Grid.MaxX, Grid.MaxY);;
  SeenPositions := TDictionary<int64,boolean>.Create;
  Queue := PriorityQueue<Integer, TReindeerMazePath>.Create();

  for Pair in Grid do
  begin
    if Pair.Value = 'E' then
      Target := Pair.Key.Clone;
    if Pair.Value = 'S' then
      Reindeer := Pair.Key.Clone;
  end;

  Work := _CreateOrUpdatePath(True, nil, Reindeer, East, 0);
  Queue.Enqueue(Work.Score, Work);

  while Queue.Count > 0 do
  begin
    Work := Queue.Dequeue;
    Work.SeenPositions.Add(Work.Position.CacheKey);

    if Work.Position.CacheKey = Target.CacheKey then
      Break;

    CanRotate := True;
    Seen.TryGetValue(Work.Position, Best);
    if (Best.Key = 0) then // never been here
    begin
      Best.Key := Work.Score;
      Best.Value := Work;
      Seen.SetData(Work.Position, Best);
    end
    else
    begin
      CanRotate := False;
      if (Work.Score = Best.Key) then // Been here with the same cost, add reference to path
      begin
        Best.Value.AlternativePaths.Add(Work);
        Continue;
      end
    end;

    // Try rotate if possible
    DidRotate := False;
    if CanRotate then
    for i in [1,3] do
    begin
      Next := Work.Position.Clone.ApplyDirection(RotateDirection(Work.Faceing, i));
      if (Grid.GetValue(next) <> '#') then
      begin
        NextWork := _CreateOrUpdatePath(True, Work, Next, RotateDirection(Work.Faceing, i), Work.Score + 1001);
        Queue.Enqueue(NextWork.Score, NextWork);
        DidRotate := True;
      end;
    end;

    // Try move forward;
    Next := Work.Position.Clone.ApplyDirection(Work.Faceing);
    if Grid.GetValue(next) <> '#' then
    begin
      NextWork := _CreateOrUpdatePath(DidRotate, Work, Next, Work.Faceing, Work.Score + 1);
      Queue.Enqueue(NextWork.Score, NextWork);
    end
  end;

  SolutionA := Work.Score;
  Work.AddSeenPositionsToList(SeenPositions);
  SolutionB := SeenPositions.Count;

  SeenPositions.Free;
  Grid.Free;
  Paths.Free;
  Seen.Free;
end;

function TAdventOfCodeDay16.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay16.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay17'}
procedure TAdventOfCodeDay17.BeforeSolve;
var
  Split: TStringDynArray;
  i: integer;
begin
  FRegisterA := SplitString(FInput[0], ' ')[2].ToInt64;
  FRegisterB := SplitString(FInput[1], ' ')[2].ToInt64;
  FRegisterC := SplitString(FInput[2], ' ')[2].ToInt64;
  FProgram := SplitString(FInput[4], ' ')[1];
  Split := SplitString(FProgram, ',');
  SetLength(FParsedProgram, Length(Split));
  for i := 0 to Length(Split) -1 do
    FParsedProgram[i] := Split[i].ToInteger;
end;

function TAdventOfCodeDay17.RunProgram(InitalARegister: int64): string;
var
  RegisterA, RegisterB, RegisterC, InstructionPointer: int64;

  function _ComboOperand: int64;
  begin
    case FParsedProgram[InstructionPointer+1] of
      0,1,2,3: result := FParsedProgram[InstructionPointer+1];
      4: result := RegisterA;
      5: result := RegisterB;
      6: result := RegisterC;
    else
      raise Exception.Create('Unknwon operand');
    end;
  end;

begin
  Result := '';
  RegisterA := InitalARegister;
  RegisterB := FRegisterB;
  RegisterC := FRegisterC;

  InstructionPointer := 0;
  while InstructionPointer < Length(FParsedProgram) do
  begin
    case FParsedProgram[InstructionPointer] of
      0: RegisterA := Trunc(RegisterA / Power(2, _ComboOperand));
      1: RegisterB := RegisterB xor FParsedProgram[InstructionPointer+1];
      2: RegisterB := _ComboOperand Mod 8;
      3: InstructionPointer := ifThen(RegisterA <> 0, FParsedProgram[InstructionPointer+1] -2, InstructionPointer);
      4: RegisterB := RegisterB xor RegisterC;
      5: Result := Result + ifthen(Result='','',',') + (_ComboOperand mod 8).ToString;
      6: RegisterB := Trunc(RegisterA / Power(2, _ComboOperand));
      7: RegisterC := Trunc(RegisterA / Power(2, _ComboOperand));
    else
      raise Exception.Create('Unknwon instruction');
    end;

    Inc(InstructionPointer, 2);
  end;
end;

function TAdventOfCodeDay17.SolveA: Variant;
begin
  Result := RunProgram(FRegisterA);
end;

function TAdventOfCodeDay17.SolveB: Variant;
var BestA: int64;

  procedure _FindA(Const aIdx: integer; CurrentRes: int64);
  var
    i, tmpA: int64;
    s: string;
  begin
    if aIdx = -1 then
    begin
      Exit;
    end;

    for i := 0 to 7 do
    begin
      tmpA := i shl (aIdx * 3) + CurrentRes;
      s := RunProgram(tmpA);
      s := Copy(s, aIdx* 2, maxInt);
      if FProgram = s then
        BestA := Min(BestA, TmpA)
      else if FProgram.EndsWith(s) then
        _FindA(aIdx - 1, tmpA);
    end;
  end;

begin
  BestA := MaxInt64;
  _FindA(Length(FParsedProgram)-1, 0);
  Result := BestA;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay18'}
procedure TAdventOfCodeDay18.BeforeSolve;
var
  i: integer;
  split: TStringDynArray;
begin
  inherited;

  FParsedInput := TList<TPosition>.Create;;
  for i := 0 to FInput.Count -1 do
  begin
    Split := SplitString(FInput[i], ',');
    FParsedInput.Add(TPosition.Create(Split[0].ToInteger,Split[1].ToInteger));
  end;
end;

procedure TAdventOfCodeDay18.AfterSolve;
begin
  inherited;
  FParsedInput.Free;
end;

function TAdventOfCodeDay18.SolveA: Variant;
Const MemoryWidth = 70 + 1;
var
  Grid, Seen: TAocGrid<Boolean>;
  Work,x,y,i,steps: integer;
  CurrentPos: TPosition;
  Dir: TAOCDirection;
  Queue: TQueue<integer>;
  IsWall: Boolean;
begin
  Grid := TAocStaticGrid<Boolean>.Create(MemoryWidth, MemoryWidth, TAocGridHelper.BoolToChar);
  Seen := TAocStaticGrid<Boolean>.Create(MemoryWidth, MemoryWidth, TAocGridHelper.BoolToChar);
  Queue := TQueue<integer>.Create;

  for i := 0 to Min(FParsedInput.Count, 1024)-1 do
    Grid.SetData(FParsedInput[i], True);

  Queue.Enqueue(0);
  while Queue.Count > 0 do
  begin
    Work := Queue.Dequeue;
    steps := Work shr 16;
    x := (Work shr 8) and 255;
    y := Work and 255;

    if (x = Grid.MaxX-1) and (y = Grid.MaxY - 1) then
    begin
      Result := steps;
      Break;
    end;

    if Seen.GetValue(x, y) then
      Continue;
    Seen.SetData(x, y, true);

    for Dir in [north, East, South, West] do
    begin
      CurrentPos := TPosition.Create(x,y).ApplyDirection(dir);
      if Grid.TryGetValue(CurrentPos, IsWall) and not IsWall then
        Queue.Enqueue(((steps + 1) shl 16) + (CurrentPos.x shl 8) + CurrentPos.Y);
    end;
  end;

  Grid.Free;
  Seen.Free;
  Queue.Free;
end;

function TAdventOfCodeDay18.SolveB: Variant;
Const MemoryWidth = 70 + 1;
var
  Grid, Reachable : TAocGrid<Boolean>;

  procedure FloodFill(aFrom: TPosition);
  var
    Todo: TStack<TPosition>;
    Work, Next: TPosition;
    Dir: TAOCDirection;
    IsWall: Boolean;
  begin
    Todo := TStack<TPosition>.Create;
    Todo.Push(aFrom);
    while Todo.Count > 0 do
    begin
      Work := Todo.Pop;

      if Reachable.GetValue(Work) then
        Continue;
      Reachable.SetData(Work, True);

      for Dir in [north, East, South, West] do
      begin
        Next := Work.Clone.ApplyDirection(dir);
        if Grid.TryGetValue(Next, IsWall) and not IsWall then
          Todo.Push(Next);
      end;
    end;

    Todo.Free;
  end;

var
  i: integer;
  CurrentPos: TPosition;
  Dir: TAOCDirection;
  DoFill, WasReachable: Boolean;
begin
  Grid := TAocStaticGrid<Boolean>.Create(MemoryWidth, MemoryWidth, TAocGridHelper.BoolToChar);
  Reachable := TAocStaticGrid<Boolean>.Create(MemoryWidth, MemoryWidth, TAocGridHelper.BoolToChar);

  for i := 0 to FParsedInput.Count-1 do
    Grid.SetData(FParsedInput[i], True);

  FloodFill(TPosition.Create(0,0));

  for i := FParsedInput.Count-1 downto 0 do
  begin
    CurrentPos := FParsedInput[i];

    Grid.SetData(CurrentPos, False);

    DoFill := False;
    for Dir in [north, East, South, West] do
    begin
      if Reachable.TryGetValue(CurrentPos.Clone.ApplyDirection(dir), WasReachable) and WasReachable then
      begin
        DoFill := True;
        Break;
      end;
    end;

    if DoFill then
      FloodFill(CurrentPos);

    if Reachable.GetValue(70,70) then
    begin
      Result := FInput[i];
      Break;
    end;
  end;

  Grid.Free;
  Reachable.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay19'}
procedure TAdventOfCodeDay19.BeforeSolve;
var
  AvailableTowels: TDictionary<String,boolean>;
  cache: TDictionary<integer,int64>;
  MaxTowelLength: integer;

  function IsPaternPossible(Const aIdx, aPaternLength: Integer; const aPatern: string): int64;
  var
    i: integer;
  begin
    if aIdx = aPaternLength then
      Exit(1);

    if cache.TryGetValue(aIdx, Result) then
      Exit;

    for i := 1 to Max(MaxTowelLength, aPaternLength - aIdx) do
      if AvailableTowels.ContainsKey(aPatern.Substring(aIdx, i)) then
        inc(Result, IsPaternPossible(aIdx + i, aPaternLength, aPatern));

    cache.Add(aIdx, Result);
  end;

var
  i, WaysToMakeTowel: int64;
  s: string;
  split: TStringDynArray;
begin
  AvailableTowels := TDictionary<string,boolean>.Create;
  MaxTowelLength := 1;
  split := SplitString(FInput[0], ',');

  for s in split do
  begin
    AvailableTowels.Add(s.Trim, true);
    MaxTowelLength := Max(MaxTowelLength, Length(s.Trim));
  end;

  cache := TDictionary<integer,int64>.Create;

  SolutionA := 0;
  SolutionB := 0;
  for i := 2 to FInput.Count -1 do
  begin
    WaysToMakeTowel := IsPaternPossible(0, Length(FInput[i]), FInput[i]);
    Inc(SolutionB, WaysToMakeTowel);
    if WaysToMakeTowel > 0 then
      Inc(SolutionA);
    cache.Clear;
  end;

  cache.Free;
  AvailableTowels.Free;
end;

function TAdventOfCodeDay19.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay19.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay20'}
procedure TAdventOfCodeDay20.BeforeSolve;
var
  Racetrack: TAocGrid<Char>;
  Pair: TPair<TPosition,Char>;
  StartPosition, EndPosition: TPosition;
  CurrentFacing: TAOCDirection;
  CurrentTime, i: integer;
begin
  Racetrack := TAocGridHelper.CreateCharGrid(FInput);
  FTimes := TAocStaticGrid<Integer>.Create(Racetrack.MaxX, Racetrack.MaxY);
  FTrackPieces := TList<TPosition>.Create;;

  for Pair in Racetrack do
  begin
    if Pair.Value = 'E' then
      StartPosition := Pair.Key.Clone;
    if Pair.Value = 'S' then
      EndPosition := Pair.Key.Clone;
  end;

  FTimes.SetData(StartPosition, 0);
  FTrackPieces.Add(StartPosition);
  CurrentFacing := North;
  while Racetrack.GetValue(StartPosition.Clone.ApplyDirection(CurrentFacing)) = '#' do
    CurrentFacing := RotateDirection(CurrentFacing, 1);

  CurrentTime := 0;
  while true do
  begin
    StartPosition := StartPosition.ApplyDirection(CurrentFacing);
    Inc(CurrentTime);
    FTimes.SetData(StartPosition, CurrentTime);
    FTrackPieces.Add(StartPosition);
    if StartPosition.CacheKey = EndPosition.CacheKey  then
      Break;

    for i := -1 to 1 do
      if Racetrack.GetValue(StartPosition.Clone.ApplyDirection(RotateDirection(CurrentFacing, i))) <> '#' then
      begin
        CurrentFacing := RotateDirection(CurrentFacing, i);
        break;
      end;
  end;

  Racetrack.Free;
end;

procedure TAdventOfCodeDay20.AfterSolve;
begin
  FTimes.Free;
  FTrackPieces.Free;
end;

function TAdventOfCodeDay20.CalcNoOfCheaths(aCheatTime: integer): integer;
var
  x,y,TimeSaved, CurrentTime, CheatTime: Integer;
  Position: TPosition;
begin
  Result := 0;
  for Position in FTrackPieces do
  begin
    CurrentTime := FTimes.GetValue(Position);
    for x := -aCheatTime to aCheatTime do
      for y := abs(x)-aCheatTime to aCheatTime-abs(x) do
      begin
        if FTimes.TryGetValue(Position.x+x, Position.y+y, CheatTime) then
        begin
          TimeSaved := CheatTime - CurrentTime - abs(x) - abs(y);
          if TimeSaved >= 100 then
            Inc(Result);
        end
      end;
  end;
end;

function TAdventOfCodeDay20.SolveA: Variant;
begin
  Result := CalcNoOfCheaths(2);
end;

function TAdventOfCodeDay20.SolveB: Variant;
begin
  Result := CalcNoOfCheaths(20);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay21'}
procedure TAdventOfCodeDay21.BeforeSolve;
type TRobotAction = (Up, Right, Down, Left, Action);
const RobotAction: array[TRobotAction] of char = ('^', '>', 'v', '<', 'A');
var ButtonPresses: TDictionary<string, int64>;

  function FindButtonInKeypad(aKeyPad: TAocGrid<Char>; aButton: char): TPosition;
  var
    x,y: integer;
    Button: char;
  begin
     for x := 0 to aKeyPad.MaxX do
      for y := 0 to aKeyPad.MaxY do
        if aKeyPad.TryGetValue(x,y,Button) and (Button = aButton) then
          Exit(TPosition.Create(x,y));
     raise Exception.Create('Button not found ' + aButton);
  end;

  function FindPathsToDest(aPad: TAocGrid<Char>; aFrom, aTo: TPosition): TList<String>;
  Const
    HorzDirs: Array[-1..1] of Char = ('<', '.', '>');
    VertDirs: Array[-1..1] of Char = ('^', '.', 'v');
  var
    HorzDir, VertDir: integer;
    tmpFrom: TPosition;
    Path: string;
    Valid: Boolean;
    tmpChar: Char;
  begin
    Result := TList<string>.Create;

    HorzDir := Sign(aTo.X - aFrom.x);
    VertDir := Sign(aTo.Y - aFrom.y);

    Path := '';
    Valid := true;
    tmpFrom := aFrom.Clone;
    while tmpFrom.x <> aTo.x do
    begin
      Path := Path + HorzDirs[HorzDir];
      tmpFrom.x := tmpFrom.x + HorzDir;
      if aPad.TryGetValue(tmpFrom, tmpChar) and (tmpChar = 'x') then
        Valid := false;
    end;

    while tmpFrom.y <> aTo.y do
    begin
      Path := Path + VertDirs[VertDir];
      tmpFrom.y := tmpFrom.y + VertDir;
      if aPad.TryGetValue(tmpFrom, tmpChar) and (tmpChar = 'x') then
        Valid := false;
    end;

    if Valid then
      Result.Add(Path);

    Path := '';
    Valid := true;
    tmpFrom := aFrom.Clone;

    while tmpFrom.y <> aTo.y do
    begin
      Path := Path + VertDirs[VertDir];
      tmpFrom.y := tmpFrom.y + VertDir;
      if aPad.TryGetValue(tmpFrom, tmpChar) and (tmpChar = 'x') then
        Valid := false;
    end;

    while tmpFrom.x <> aTo.x do
    begin
      Path := Path + HorzDirs[HorzDir];
      tmpFrom.x := tmpFrom.x + HorzDir;
      if aPad.TryGetValue(tmpFrom, tmpChar) and (tmpChar = 'x') then
        Valid := false;
    end;

    if Valid and not Result.Contains(Path) then
      Result.Add(Path);
  end;

  function PickBestPath(aPaths: TList<string>; aRobotLevel: integer): int64;
  var
    Path: string;
    PathSteps, ButtonIndex: int64;
    PrevButton, Button: Char;
  begin
    Result := MaxInt64;
    for Path in aPaths do
    begin
      PathSteps := 0;
      PrevButton := 'A';

      for ButtonIndex := 1 to Length(Path) do
      begin
        Button := Path[ButtonIndex];
        PathSteps := PathSteps + ButtonPresses[(aRobotLevel-1).ToString+PrevButton+Button];
        PrevButton := Button;
      end;

      PathSteps := PathSteps + ButtonPresses[(aRobotLevel-1).ToString+PrevButton + 'A'];
      Result := Min(Result, PathSteps);
    end;
  end;

var
  s: string;
  StepsA, StepsB, CodePart: int64;
  PrevButton, Button: Char;
  RobotCount, i: Integer;
  NumPad, directionalPad: TAocGrid<Char>;
  NumPadPaths, DirectionalPaths: TList<string>;
  ButtonFrom, ButtonTo: TRobotAction;
  PathCache: TDictionary<string,TList<String>>;
begin
  NumPad := TAocStaticGrid<Char>.Create(3, 4, TAocGridHelper.CharToChar);
  NumPad.SetData(0,0, '7');
  NumPad.SetData(1,0, '8');
  NumPad.SetData(2,0, '9');
  NumPad.SetData(0,1, '4');
  NumPad.SetData(1,1, '5');
  NumPad.SetData(2,1, '6');
  NumPad.SetData(0,2, '1');
  NumPad.SetData(1,2, '2');
  NumPad.SetData(2,2, '3');
  NumPad.SetData(0,3, 'x');
  NumPad.SetData(1,3, '0');
  NumPad.SetData(2,3, 'A');

  DirectionalPad := TAocStaticGrid<Char>.Create(3, 2, TAocGridHelper.CharToChar);
  DirectionalPad.SetData(0,0, 'x');
  DirectionalPad.SetData(1,0, '^');
  DirectionalPad.SetData(2,0, 'A');
  DirectionalPad.SetData(0,1, '<');
  DirectionalPad.SetData(1,1, 'v');
  DirectionalPad.SetData(2,1, '>');

  PathCache := TObjectDictionary<string,TList<String>>.Create([doOwnsValues]);
  ButtonPresses := TDictionary<string, int64>.Create;

  for ButtonFrom in [Up, Right, Down, Left, Action]  do
    for ButtonTo in [Up, Right, Down, Left, Action]  do
    begin
      DirectionalPaths := FindPathsToDest(directionalPad, FindButtonInKeypad(directionalPad, RobotAction[ButtonFrom]), FindButtonInKeypad(directionalPad, RobotAction[ButtonTo]));
      PathCache.Add(RobotAction[ButtonFrom]+RobotAction[ButtonTo], DirectionalPaths);
      ButtonPresses.Add('1'+RobotAction[ButtonFrom]+RobotAction[ButtonTo], Length(DirectionalPaths.First)+ 1);
    end;

  for RobotCount := 2 to 25 do
    for ButtonFrom in [Up, Right, Down, Left, Action]  do
      for ButtonTo in [Up, Right, Down, Left, Action]  do
      begin
        DirectionalPaths := PathCache[RobotAction[ButtonFrom]+RobotAction[ButtonTo]];
        ButtonPresses.Add(RobotCount.ToString+RobotAction[ButtonFrom]+RobotAction[ButtonTo], PickBestPath(DirectionalPaths, RobotCount));
      end;

  SolutionA := 0;
  SolutionB := 0;
  for s in FInput do
  begin
    PrevButton := 'A';
    StepsA := 0;
    StepsB := 0;
    CodePart := StringReplace(s, 'A', '', [rfReplaceAll]).ToInt64;

    for i := 1 to Length(s) do
    begin
      Button := s[i];
      NumPadPaths := FindPathsToDest(NumPad, FindButtonInKeypad(NumPad, PrevButton), FindButtonInKeypad(NumPad, Button));
      Inc(StepsA, PickBestPath(NumPadPaths, 3));
      Inc(StepsB, PickBestPath(NumPadPaths, 26));
      NumPadPaths.Free;
      PrevButton := Button;
    end;

    Inc(SolutionA, StepsA * CodePart);
    Inc(SolutionB, StepsB * CodePart);
  end;

  directionalPad.Free;
  NumPad.Free;
  PathCache.Free;
  ButtonPresses.Free;
end;

function TAdventOfCodeDay21.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay21.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay22'}
procedure TAdventOfCodeDay22.BeforeSolve;

  function Mix(a, b: int64): int64; inline;
  begin
    Result := a xor b;
  end;

  function Prune(aValue: int64): int64; inline;
  begin
    Result := aValue and (16777216 - 1)
  end;

var
  s: string;
  Round, Secret: int64;
  PrevDigit, CurrentDigit, sequence, BananaCount: Integer;
  Seen: array of Boolean;
  TotalBananas: array of integer;
begin
  SolutionA := 0;
  SolutionB := 0;
  SetLength(TotalBananas, 130321);

  for s in FInput do
  begin
    Secret := s.ToInt64;
    PrevDigit := Secret mod 10;
    sequence := 0;
    SetLength(Seen, 0);
    SetLength(Seen, 130321);

    for Round := 1 to 2000 do
    begin
      Secret := Prune(Mix(Secret, Secret shl 6));
      Secret := Prune(Mix(Secret, secret shr 5));
      Secret := Prune(Mix(Secret, Secret shl 11));

      CurrentDigit := Secret mod 10;

      sequence :=  (19 * sequence + CurrentDigit - PrevDigit + 9) mod 130321 ;
      if (round >= 4) and (not Seen[sequence]) then
      begin
        Seen[sequence] := True;
        BananaCount := TotalBananas[sequence];
        Inc(BananaCount, CurrentDigit);
        TotalBananas[sequence] := BananaCount;
        SolutionB := Max(SolutionB, BananaCount);
      end;

      PrevDigit := CurrentDigit;
    end;

    Inc(SolutionA, Secret);
  end;
end;

function TAdventOfCodeDay22.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay22.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay23'}
procedure TAdventOfCodeDay23.BeforeSolve;

  procedure AddComputerToList(aComputer1, aComputer2: string);
  var 
    List: TList<String>;
  begin
    if not FComputers.TryGetValue(aComputer1, List) then
    begin
      List := TList<string>.create;
      FComputers.Add(aComputer1, List);
    end;
    List.Add(aComputer2);  
  end;


var 
  s, computer1, computer2: string;
  split: TStringDynArray; 
begin
  FComputers := TObjectDictionary<string,TList<string>>.Create([doOwnsValues]);
  
  for s in FInput do
  begin
    split := SplitString(s, '-');
    computer1 := split[0];
    computer2 := split[1];
    AddComputerToList(computer1, computer2);
    AddComputerToList(computer2, computer1);
  end;
end;

procedure TAdventOfCodeDay23.AfterSolve;
begin
  FComputers.Free;
end;

function TAdventOfCodeDay23.SolveA: Variant;
var
  idxComputer2, idxComputer3: Integer;
  Computer1, Computer2, Computer3: string;
  Seen: TDictionary<string,boolean>;
  Pair: TPair<string,TList<string>>;
  sortList: TStringList;
begin
  sortList := TStringList.Create;
  sortList.Sorted := True;
  sortList.Delimiter := ',';

  Seen := TDictionary<String,boolean>.Create;
  for Pair in FComputers do
  begin
    Computer1 := Pair.Key;
    if not Computer1.StartsWith('t') then
      Continue;
    
    for idxComputer2 := 0 to Pair.Value.Count -2 do
    begin
      Computer2 := Pair.Value[idxComputer2];
      for idxComputer3 := idxComputer2 +1 to Pair.Value.Count -1 do
      begin
        Computer3 := Pair.Value[idxComputer3];
      
        if not FComputers[Computer2].Contains(Computer3) then
          Continue;

        sortList.Clear;
        sortList.Add(Computer1);
        sortList.Add(Computer2);
        sortList.Add(Computer3);
        Seen.AddOrSetValue(sortList.DelimitedText, true);
      end;
    end;
  end;

  Result := Seen.Count;

  Seen.Free;
  sortList.Free;
end;

function TAdventOfCodeDay23.SolveB: Variant;
var 
  BiggestCluster: TStringList;
    
    procedure BuildCluster(aComputerList, aCluster: TList<string>; aIdx: Integer = 0);
    var
      ToAdd, s: string;
      ToAddComputer: TList<string>;
    begin
      if BiggestCluster.Count >= (aCluster.Count + aComputerList.Count - aIdx)  then
        Exit; 
      
      if aIdx = aComputerList.Count then
      begin
        if aCluster.Count > BiggestCluster.Count then
        begin
          BiggestCluster.Clear;
          for s in aCluster do
            BiggestCluster.Add(s);
        end;

        Exit;
      end;

      BuildCluster(aComputerList, aCluster, aIdx + 1);

      ToAdd := aComputerList[aIdx];
      ToAddComputer := FComputers[ToAdd];
      for s in aCluster do
        if not ToAddComputer.Contains(s) then
          Exit;
          
      aCluster.Add(ToAdd);
      BuildCluster(aComputerList, aCluster, aIdx + 1);
      aCluster.Remove(ToAdd);
    end;

var
  Pair: TPair<string,TList<string>>;
  Cluster: TList<string>;
begin
  BiggestCluster := TStringList.Create;
  BiggestCluster.Sorted := True;
  BiggestCluster.Delimiter := ',';

  Cluster := TList<string>.Create;

  for Pair in FComputers do
  begin
    Cluster.Clear; 
    Cluster.Add(Pair.Key);
    BuildCluster(Pair.Value, Cluster);
  end;

  Result := BiggestCluster.DelimitedText;
  Cluster.Free; 
  BiggestCluster.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay24'}

{ ROperation }

class function ROperation.CreateFromString(aString: string): ROperation;
var
  Split: TStringDynArray;
begin
  split := SplitString(aString, ' ');

  Result.TargetAdress := split[4];
  Result.Adress1 := split[0];
  Result.Adress2 := split[2];

  case IndexStr(Split[1], ['XOR', 'OR', 'AND']) of
    0: Result.Operation := TOperation.tXor;
    1: Result.Operation := TOperation.tor;
    2: Result.Operation := TOperation.tAnd;
  else
    raise Exception.Create(aString);
  end;
end;

function ROperation.Equals(aOperation: TOperation; const aAdress1, aAdress2: string): Boolean;
begin
  Result := False;
  if aOperation <> Operation then
    Exit;

  if (Adress1 <> aAdress1) and (Adress2 <> aAdress1) then
    Exit;

  if (aAdress2 <> '') and (Adress1 <> aAdress2) and (Adress2 <> aAdress2) then
    Exit;

  Result := True;
end;

function ROperation.OtherAdress(const aAddress: string): string;
begin
  Result := Adress1;
  if Adress2 <> aAddress then
    Result := Adress2;
end;

procedure TAdventOfCodeDay24.BeforeSolve;
var
  i: Integer;
  s: string;
  split: TStringDynArray;
  Operation: ROperation;
begin
  FWires := TDictionary<string,boolean>.Create;
  FOperations := TDictionary<string, ROperation>.Create;

  for i := 0 to FInput.Count -1 do
  begin
    s := FInput[i];
    if s = '' then
      Break;

    split := SplitString(s, ': ');
    FWires.Add(split[0], Split[2] = '1');
  end;

  for i := i+1 to FInput.Count-1 do
  begin
    Operation := ROperation.CreateFromString(FInput[i]);
    FOperations.Add(Operation.TargetAdress, Operation);
  end;
end;

procedure TAdventOfCodeDay24.AfterSolve;
begin
  inherited;
  FOperations.Free;
  FWires.Free;
end;

function TAdventOfCodeDay24.WireName(Const aPrefix: string; aIndex: integer): string;
begin
  Result := aPrefix;
  if aIndex < 10 then
    Result := Result + '0';
  Result := Result + aIndex.ToString;
end;

function TAdventOfCodeDay24.RunProgram(aWires: TDictionary<string, Boolean>): int64;
var
  i: integer;
  Value, Value1, Value2: boolean;
  ToCalc: TQueue<ROperation>;
  Operation: ROperation;
begin
  ToCalc := TQueue<ROperation>.Create;
  for Operation in FOperations.Values do
    ToCalc.Enqueue(Operation);

  while ToCalc.Count > 0 do
  begin
    Operation := ToCalc.Dequeue;
    if not aWires.TryGetValue(Operation.Adress1, Value1) or not aWires.TryGetValue(Operation.Adress2, Value2) then
    begin
      ToCalc.Enqueue(Operation);
      Continue;
    end;

    Value := False;
    case Operation.Operation of
      tXor: Value := Value1 xor Value2;
      tOR: Value := Value1 or Value2;
      tAnd: Value := Value1 and Value2;
    end;

    aWires.Add(Operation.TargetAdress, Value);
  end;

  i := 0;
  Result := 0;
  while aWires.TryGetValue(WireName('z', i), Value) do
  begin
    if Value then
      Result := Result + int64(1) shl i;
    inc(i)
  end;

  ToCalc.Free;
end;

function TAdventOfCodeDay24.SolveA: Variant;
begin
  Result := RunProgram(FWires);
end;

type TOperations = set of TOperation;
function TAdventOfCodeDay24.SolveB: Variant;
var
  SwappedOperations: TStringList;


  procedure PrintOperations(aCurrentOperation: string; aDepth: Integer);
  const
    OperationNames: array[TOperation] of string = ('AND', 'XOR', 'OR');
  var
    Operation: ROperation;
  begin
    if (aDepth > 4) or not FOperations.TryGetValue(aCurrentOperation, Operation) then
    begin
      Writeln(aCurrentOperation.PadLeft(aDepth * 2, ' '), '...');
      Exit
    end;

    Writeln(OperationNames[Operation.Operation].PadLeft(aDepth * 2, ' '), ' = ', Operation.TargetAdress);
    PrintOperations(Operation.Adress1, aDepth + 1);
    PrintOperations(Operation.Adress2, aDepth + 1);
  end;

  function FindOperationByParams(aOperation: TOperation; const aAdress1, aAdress2: string): ROperation;
  var
    Operation: ROperation;
  begin
    for Operation in FOperations.Values do
      if Operation.Equals(aOperation, aAdress1, aAdress2) then
      begin
        Result := Operation;
        Exit;
      end;
    raise Exception.Create('Operation not found');
  end;

  procedure SwapOperations(aOperation1, aOperation2: ROperation);
  var
    Address1, Address2: string;
  begin
    Address1 := aOperation1.TargetAdress;
    Address2 := aOperation2.TargetAdress;

    SwappedOperations.Add(Address1);
    SwappedOperations.Add(Address2);

    aOperation1.TargetAdress := Address2;
    aOperation2.TargetAdress := Address1;

    FOperations.AddOrSetValue(aOperation1.TargetAdress, aOperation1);
    FOperations.AddOrSetValue(aOperation2.TargetAdress, aOperation2);
  end;

  function PickOperationByType(aOperationType: TOperations; aOperation1, aOperation2: ROperation): ROperation;
  begin
    Result := aOperation1;
    if aOperation2.Operation in aOperationType then
      Result := aOperation2;
  end;

  procedure PrintOutputs;
  var
    Wires: TDictionary<string, boolean>;
    Exptected, ProgramResult, i, j: int64;
  begin
    Wires := TDictionary<string, boolean>.Create;

    for i := 0 to 44 do
    begin
      Wires.Clear;
      for j := 0 to 44 do
      begin
        Wires.Add(WireName('x', j), false);
        Wires.Add(WireName('y', j), false);
      end;

      Wires[WireName('x', i)] := true;
      ProgramResult := RunProgram(Wires);
      Exptected := Int64(1) shl i;
      Writeln('Bitindex ', i);
      Writeln('Expected ', IntToBits(Exptected));
      Writeln('Result   ', IntToBits(ProgramResult))
    end;
    Wires.Free;
  end;

var
  CurrentBitIndex: integer;
  Operation, Operation1, Operation2, CorrectOperation, InvalidOperation, AlreadyCorrectOperation: ROperation;
  PrevXor, PrevOr, prevAnd, And4, And5, CurrentXor, CurrentOr: ROperation;
begin
//  for CurrentBitIndex := 0 to 44 do
//    PrintOperations(WireName('z', CurrentBitIndex), 0);
//  PrintOutputs;

  SwappedOperations := TStringList.Create;

{ Bit 0, we expect the following
    z00
      x00 XOR y00
}

  Operation := FOperations['z00'];
  if not Operation.Equals(tXor, 'x00', 'y00') then
  begin
    CorrectOperation := FindOperationByParams(tXor, 'x00', 'y00');
    SwapOperations(Operation, CorrectOperation);
  end;

{ Bit 1, we expect the following
  z01 =
  XOR
    XOR
      y01
      x01
    AND
      y00
      x00

}
  Operation := FOperations['z01'];
  PrevXor := FindOperationByParams(tXor, 'x01', 'y01');
  prevAnd := FindOperationByParams(tAnd, 'x00', 'y00');
  if not Operation.Equals(tXor, PrevXor.TargetAdress, prevAnd.TargetAdress) then
  begin
    if Operation.Operation <> tXor then
    begin
      CorrectOperation := FindOperationByParams(tXor, PrevXor.TargetAdress, prevAnd.TargetAdress);
      SwapOperations(Operation, CorrectOperation);
    end
    else
    begin
      Operation1 := FOperations[Operation.Adress1];
      Operation2 := FOperations[Operation.Adress2];

      InvalidOperation := PickOperationByType([tOR], Operation1, Operation2);
      AlreadyCorrectOperation := PickOperationByType([tXor, tAnd], Operation1, Operation2);
      CorrectOperation := PickOperationByType([tAnd, tXor] - [AlreadyCorrectOperation.Operation], CurrentXor, CurrentOr);

      SwapOperations(InvalidOperation, CorrectOperation);
    end;
  end;

{ Bit 2, we expect the following
z02
XOR
  OR
    AND
      y01
      x01
    AND
      XOR -> This can be retrieved from bit 1
        y01..
        x01..
      AND -> This can also be found in bit 1
        y00..
        x00..
  XOR
    y02
    x02
}

  // This might not work if bit 2 is invalid, but thats not the case in my input...
  Operation := FOperations['z02'];
  CurrentXor := FindOperationByParams(tXor, 'x02', 'x02');
  PrevOr := FOperations[Operation.OtherAdress(CurrentXor.TargetAdress)];
  PrevXor := CurrentXor;

{ Bit n, We expect the following operations
  Zn =
  XOR(1)
    XOR(2)
      Xn
      Yn
    OR(3)
      AND(4)
        Xn-1
        Yn-1
      AND(5)
        XOR(6) -> This should be the same as bit n-1, operation 2
          Yn-1
          Xn-1
        OR(7) -> This should be the same as bit n-1, operation
          Xn-2
          Yn-2
}

  CurrentBitIndex := 3;
  while FOperations.TryGetValue(WireName('z', CurrentBitIndex), Operation) do
  begin
    if SwappedOperations.Count = 8 then
      Break;

    And5 := FindOperationByParams(tAnd, PrevXor.TargetAdress, PrevOr.TargetAdress);
    And4 := FindOperationByParams(tAnd, WireName('x', CurrentBitIndex-1), WireName('y', CurrentBitIndex -1));
    CurrentOr := FindOperationByParams(tOr, And4.TargetAdress, And5.TargetAdress);
    CurrentXor := FindOperationByParams(tXor, WireName('x', CurrentBitIndex), WireName('y', CurrentBitIndex));

    if not Operation.Equals(tXor, CurrentOr.TargetAdress, CurrentXor.TargetAdress) then
    begin
      if Operation.Operation <> tXor then // The root operation is not ok
      begin
        CorrectOperation := FindOperationByParams(tXor, CurrentXor.TargetAdress, CurrentOr.TargetAdress);
        SwapOperations(Operation, CorrectOperation);
      end
      else // One of the operations of the root is not ok
      begin
        Operation1 := FOperations[Operation.Adress1];
        Operation2 := FOperations[Operation.Adress2];

        InvalidOperation := PickOperationByType([tAnd], Operation1, Operation2);
        AlreadyCorrectOperation := PickOperationByType([tOr, tXor], Operation1, Operation2);
        CorrectOperation := PickOperationByType([tOr, tXor] - [AlreadyCorrectOperation.Operation], CurrentXor, CurrentOr);

        SwapOperations(InvalidOperation, CorrectOperation);
      end;
    end
    else
    begin
      PrevOr := CurrentOr;
      PrevXor := CurrentXor;
      inc(CurrentBitIndex);
    end;
  end;

  SwappedOperations.Delimiter := ',';
  SwappedOperations.Sort;
  Result := SwappedOperations.DelimitedText;
  SwappedOperations.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay25'}
function TAdventOfCodeDay25.IntToChar(const aInt: Integer): char;
begin
  if aInt = 0 then
    Exit('.');
  Result := IntToStr(aInt)[1];
end;

function TAdventOfCodeDay25.SolveA: Variant;
var
  Keys, Locks: TList<IntegerArray>;
  Key, Lock, Template: IntegerArray;
  CanFit: Boolean;
  i, j: Int64;
  Grid: TAocGrid<Integer>;

  function GetPossible(const aCurrentX, aCurrentY: integer): TList<Integer>;
  const KnightOffsetX: array[0..7] of integer = (1, 1, -1, -1, 2, 2, -2, -2);
        KnightOffsetY: array[0..7] of integer = (2, -2, 2, -2, 1, -1, 1, -1);
  var
    i, TmpX, TmpY, x, y, i2: integer;
  begin
    if Grid.GetValue(aCurrentX, aCurrentY) <> 0 then
      Writeln('Huh ', aCurrentX, ' ', aCurrentY);

    Result := TList<Integer>.Create;

    if (aCurrentX = 5) and (aCurrentY = 6) then
      Result.Add(4)
    else if (aCurrentX = 3) and (aCurrentY = 7) then
      Result.Add(5)
    else
      for i := 1 to 9 do
        Result.Add(i);

    // Check rows
    TmpX := aCurrentX;
    while Grid.TryGetValue(Tmpx, aCurrentY, i) do
    begin
      Result.Remove(i);
      Inc(TmpX);
    end;

    TmpX := aCurrentX;
    while Grid.TryGetValue(Tmpx, aCurrentY, i) do
    begin
      Result.Remove(i);
      Dec(TmpX);
    end;

    // Check Cols
    TmpY := aCurrentY;
    while Grid.TryGetValue(aCurrentX, TmpY, i) do
    begin
      Result.Remove(i);
      Inc(TmpY);
    end;

    TmpY := aCurrentY;
    while Grid.TryGetValue(aCurrentX, TmpY, i) do
    begin
      Result.Remove(i);
      Dec(TmpY);
    end;

    // Check box
    TmpX := (aCurrentX div 3) * 3;
    TmpY := (aCurrentY div 3) * 3;
    for x := 0 to 2 do
      for y := 0 to 2 do
      begin
        i := Grid.GetValue(TmpX + x, TmpY + y);
        Result.Remove(i);
      end;

    // Check Kings move
    for x := -1 to 1 do
      for y := -1 to 1 do
      begin
        if Grid.TryGetValue(aCurrentX + x, aCurrentY + y, i) then
          Result.Remove(i);
      end;

    // Check Knight move
    for i2 := 0 to 7 do
      if Grid.TryGetValue(aCurrentX + KnightOffsetX[i2], aCurrentY + KnightOffsetY[i2], i) then
        Result.Remove(i);

    // Check Consec
    if Grid.TryGetValue(aCurrentX + 1, aCurrentY, i) and (i > 0) then
    begin
      Result.Remove(i-1);
      Result.Remove(i+1)
    end;

    if Grid.TryGetValue(aCurrentX - 1, aCurrentY, i) and (i > 0) then
    begin
      Result.Remove(i-1);
      Result.Remove(i+1)
    end;

    if Grid.TryGetValue(aCurrentX, aCurrentY + 1, i) and (i > 0) then
    begin
      Result.Remove(i-1);
      Result.Remove(i+1)
    end;

    if Grid.TryGetValue(aCurrentX, aCurrentY - 1, i) and (i > 0) then
    begin
      Result.Remove(i-1);
      Result.Remove(i+1)
    end;
  end;


  function Solve(aCurrentX, aCurrentY: integer): Boolean;
  var
    Possible: TList<integer>;
    i, NextX, NextY: integer;
    x, y: Integer;
  begin
    if aCurrentY = 9 then
      Exit(True);

    Possible := GetPossible(aCurrentX, aCurrentY);

    NextX := aCurrentX + 1;
    NextY := aCurrentY;
    if NextX = 9 then
    begin
      NextX := 0;
      NextY := NextY + 1;
    end;

    for i in Possible do
    begin
      Grid.SetData(aCurrentX, aCurrentY, i);
      Result := Solve(NextX, NextY);
      if Result and (aCurrentX = 0) and (aCurrentY = 0) then
      begin
        Grid.PrintToDebug;
        for x := 0 to 8 do
          for y := 0 to 8 do
            Grid.SetData(x,y,0);

      end
      else if Result then
        Exit;

      Grid.SetData(aCurrentX, aCurrentY, 0);
    end;

    Result := Grid.GetValue(aCurrentX, aCurrentY) <> 0;
  end;


begin

  Grid := TAocStaticGrid<Integer>.Create(9, 9, IntToChar);

  Solve(0, 0);
  Grid.PrintToDebug;


  Exit(null);


  Keys := TList<IntegerArray>.Create;
  Locks := TList<IntegerArray>.Create;

  SetLength(Template, 5);
  for i := 0 to FInput.Count -1 do
  begin
    if FInput[i] = '' then
    begin
      if FInput[i-1][1] = '.' then
        Locks.Add(Template)
      else
        Keys.Add(Template);

      SetLength(Template, 0);
      SetLength(Template, 5);

      Continue;
    end;

    for j := 0 to 4 do
      if FInput[i][j+1] = '#' then
        Template[j] := Template[j] + 1;
  end;

  if FInput[FInput.Count-1][1] = '.' then
    Locks.Add(Template)
  else
    Keys.Add(Template);

  Result := 0;
  for key in Keys do
    for lock in Locks do
    begin
      CanFit := True;
      for i := 0 to 4 do
      begin
        CanFit := CanFit and (Key[i] + Lock[i] <= 7)
      end;

      if CanFit then
        Result := Result + 1;
    end;
end;

function TAdventOfCodeDay25.SolveB: Variant;
begin
  Result := null;
end;
{$ENDREGION}

initialization

RegisterClasses([
  TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5,
  TAdventOfCodeDay6, TAdventOfCodeDay7, TAdventOfCodeDay8, TAdventOfCodeDay9, TAdventOfCodeDay10,
  TAdventOfCodeDay11,TAdventOfCodeDay12,TAdventOfCodeDay13,TAdventOfCodeDay14,TAdventOfCodeDay15,
  TAdventOfCodeDay16,TAdventOfCodeDay17,TAdventOfCodeDay18,TAdventOfCodeDay19,TAdventOfCodeDay20,
  TAdventOfCodeDay2, TAdventOfCodeDay22,TAdventOfCodeDay23,TAdventOfCodeDay24,TAdventOfCodeDay25]);

end.
