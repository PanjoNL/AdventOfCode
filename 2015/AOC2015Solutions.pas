unit AOC2015Solutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Defaults, System.Generics.Collections,
  system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils,
  system.Math, uAOCUtils, system.Types, IdHashMessageDigest, System.Character, System.Json, uAocManager;

type
  TAdventOfCodeDay1 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TCalculation = function(Const x,y,z: integer):integer of object;
  TAdventOfCodeDay2 = class(TAdventOfCode)
  private
    function CalculateWrappingPaper(Const x,y,z: integer):integer;
    function Calculateribbon(Const x,y,z: integer):integer;
    function Calculate(aCalculation: TCalculation): Integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

type
  TAdventOfCodeDay3 = class(TAdventOfCode)
  private
    function DeliverPresents(Const UseRoboSanta: Boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay4 = class(TAdventOfCode)
    function CalculateHashes(Const aExpetedStart: String): integer;
  protecteD
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TIsNiceString = function(Const aString: String): Boolean of object;
  TAdventOfCodeDay5 = class(TAdventOfCode)
  private
    function CheckStrings(aCheck: TIsNiceString): integer;
    function IsNaughtyA(Const aString: String): Boolean;
    function IsNaughtyB(Const aString: String): Boolean;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TLightEvent = procedure(Lights: TDictionary<TPoint,Integer>; Const aPoint: TPoint; Const SwitchOn: boolean) of object;
  TAdventOfCodeDay6 = class(TAdventOfCode)
  private
    function SwitchLights(Const OnToggle, OnSwitch: TLightEvent): TDictionary<TPoint, Integer>;
    procedure ToggleA(Lights: TDictionary<TPoint,Integer>; Const aPoint: TPoint; Const SwitchOn: boolean);
    procedure SwitchA(Lights: TDictionary<TPoint,Integer>; Const aPoint: TPoint; Const SwitchOn: boolean);
    procedure ToggleB(Lights: TDictionary<TPoint,Integer>; Const aPoint: TPoint; Const SwitchOn: boolean);
    procedure SwitchB(Lights: TDictionary<TPoint,Integer>; Const aPoint: TPoint; Const SwitchOn: boolean);
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay7 = class(TAdventOfCode)
  private
    function RunProgram(Const OverrideB: Word): Integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay8 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay9 = class(TAdventOfCode)
  private
    Distances: TDictionary<string, Integer>;
    Places: TList<String>;
    function FindeBestRoute(Const MaxDistance: Boolean): Integer;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay10 = class(TAdventOfCode)
  private
    SentanceA: string;
    function Talk(Const StartSentacne: String; Const Repeats: Integer): string;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay11 = class(TAdventOfCode)
  private
    FirstPassWord: string;
    function FindNewPassWord(Const OldPassWord: String): string;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay12 = class(TAdventOfCode)
  private
    function CountNumbers(const CheckForRed: Boolean): Integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay13 = class(TAdventOfCode)
  private
    PotentialHapiness: TDictionary<string, Integer>;
    Attendees: TList<String>;

    function ArrangeSeating(Const Attendees: TList<String>): Integer;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  RReindeerSpecs = record
    Speed, TravelTime, RestingTime: Integer;
  end;

  TAdventOfCodeDay14 = class(TAdventOfCode)
    ReindeerSpecs: TDictionary<String,RReindeerSpecs>;
    function CalculateDistanceTraveld(const Racetime: Integer; Specs: RReindeerSpecs): integer;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay15 = class(TAdventOfCode)
  private
    function CreateCookies(Const LimitCalories: Boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TValidator = function(const aObject: string; const Value: integer): Boolean of object;
  TAdventOfCodeDay16 = class(TAdventOfCode)
  private
    function FindAunt(const Validator: TValidator): string;
    function ValidatorA(const aObject: string; const Value: integer): Boolean;
    function ValidatorB(const aObject: string; const Value: integer): Boolean;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay17 = class(TAdventOfCode)
  private
    Combinations: TList<Integer>;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay18 = class(TAdventOfCode)
  private
    function SwitchLights(Const StuckCorners: Boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay19 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay20 = class(TAdventOfCode)
  private
    function Visit(Const infinitePresents: boolean; Const PresentsPerVisit: integer): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay21 = class(TAdventOfCode)
  private
    function SimulateGame(Const PlayerWins: Boolean): Integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay22 = class(TAdventOfCode)
  private
    function PlayGame(Const HardMode: Boolean): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay23 = class(TAdventOfCode)
  private
    function RunProgram(StartValueA: Integer): Integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay24 = class(TAdventOfCode)
  private
    function LoadSleigh(const Groups: integer): int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay25 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;


implementation

{$Region 'TAdventOfCodeDay1'}
function TAdventOfCodeDay1.SolveA: Variant;
begin
  Result := OccurrencesOfChar(FInput[0], '(') - OccurrencesOfChar(FInput[0], ')') //74
end;

function TAdventOfCodeDay1.SolveB: Variant;
var Floor: Integer;
begin
  Floor := 0;
  Result := 0;
  while Floor >= 0 do
  begin
    Floor := Floor + IfThen(FInput[0][integer(Result+1)] = '(', 1, -1);
    Inc(Result); //1795
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay2'}
function TAdventOfCodeDay2.SolveA: Variant;
begin
  Result := Calculate(CalculateWrappingPaper); //1598415
end;

function TAdventOfCodeDay2.SolveB: Variant;
begin
  Result := Calculate(Calculateribbon); //3812909
end;

function TAdventOfCodeDay2.Calculate(aCalculation: TCalculation): Integer;
var s: string;
    Split: TStringDynArray;
begin
  Result := 0;
  for s in FInput do
  begin
    Split := SplitString(s, 'x');
    Result := Result + aCalculation(StrToInt(Split[0]),StrToInt(Split[1]),StrToInt(Split[2]));
  end;
end;

function TAdventOfCodeDay2.CalculateWrappingPaper(Const x,y,z: integer):integer;
begin
  Result := 2*x*y + 2*x*z + 2*y*z + Min(Min(x*y,x*z),y*z);
end;

function TAdventOfCodeDay2.Calculateribbon(Const x,y,z: integer):integer;
begin
  Result := 2*(x+y+z-Max(Max(x,y),z)) + x*y*z;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay3'}
function TAdventOfCodeDay3.SolveA: Variant;
begin
  Result := DeliverPresents(False); //2081
end;

function TAdventOfCodeDay3.SolveB: Variant;
begin
  Result := DeliverPresents(True); //2341
end;

function TAdventOfCodeDay3.DeliverPresents(Const UseRoboSanta: Boolean): integer;
var PositionSanta, PositionRobo, Temp: TPosition;
    Seen: TDictionary<TPosition,integer>;
    c: char;
    IsSanta: Boolean;
begin
  PositionSanta := PositionSanta.Create(0,0);
  PositionRobo := PositionRobo.Create(0,0);

  IsSanta := True;
  Seen := TDictionary<TPosition,integer>.Create;
  try
  for c in FInput[0] do
    begin
      if IsSanta then
        Temp := PositionSanta
      else
        Temp := PositionRobo;

      Seen.AddOrSetValue(Temp, 1);

      Case AnsiIndexStr(c,[ '^','>','v','<']) of
        0: Temp := Temp.ApplyDirection(North);
        1: Temp := Temp.ApplyDirection(East);
        2: Temp := Temp.ApplyDirection(South);
        3: Temp := Temp.ApplyDirection(West)
      End;

      if IsSanta then
        PositionSanta := Temp
      else
        PositionRobo := Temp;

      IsSanta := (not IsSanta) or (not UseRoboSanta);
    end;
    Result := Seen.Count;
  finally
    Seen.Free;
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay4'}
function TAdventOfCodeDay4.SolveA: Variant;
begin
  Result := CalculateHashes('00000'); //282749
end;

function TAdventOfCodeDay4.SolveB: Variant;
begin
  Result := CalculateHashes('000000'); //9962624
end;

function TAdventOfCodeDay4.CalculateHashes(Const aExpetedStart: String): integer;
var
  pMD5: TIdHashMessageDigest5;
begin
  Result := 1;
  pMD5 := TIdHashMessageDigest5.Create;
  try
    while true do
    begin
      if pMD5.HashStringAsHex(FInput[0]+IntToStr(Result)).StartsWith(aExpetedStart) then
        Exit;
      inc(result);
    end;
  finally
    pMD5.Free;
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay5'}
function TAdventOfCodeDay5.SolveA: Variant;
begin
  Result := CheckStrings(IsNaughtyA); //255
end;

function TAdventOfCodeDay5.SolveB: Variant;
begin
  Result := CheckStrings(IsNaughtyB); //55
end;

function TAdventOfCodeDay5.CheckStrings(aCheck: TIsNiceString): integer;
var s: string;
begin
  Result := 0;
  for s in FInput do
    if aCheck(s) then
      Inc(Result);
end;

function TAdventOfCodeDay5.IsNaughtyA(Const aString: String): Boolean;
var i: integer;
begin
  Result := False;

  i := OccurrencesOfChar(aString, 'a') + OccurrencesOfChar(aString, 'i') + OccurrencesOfChar(aString, 'o')
     + OccurrencesOfChar(aString, 'u') + OccurrencesOfChar(aString, 'e');

  if i >= 3 then
    if (Pos('ab', aString) + Pos('cd', aString) + Pos('pq', aString) + Pos('xy', aString)) = 0  then
      for i := ord('a') to ord('z') do
      if pos(chr(i)+ chr(i), aString)> 0 then
        Exit(True);
end;

function TAdventOfCodeDay5.IsNaughtyB(Const aString: String): Boolean;
var i, j, k: integer;
    Valid: boolean;
begin
  Result := False;

  Valid := false;
  for i := 0 to Length(aString)-2 do
    if aString[i] = aString[i+2] then
    begin
      Valid := true;
      break;
    end;

  if Valid then
    for i := ord('a') to ord('z') do
    begin
      for j := ord('a') to ord('z') do
      begin
        k := Pos(chr(i)+chr(j), aString);
        if Pos(chr(i)+chr(j), aString, k+2) > 0 then
          Exit(true)
      end;
   end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay6'}
function TAdventOfCodeDay6.SolveA: Variant;
Var Lights: TDictionary<TPoint, integer>;
begin
  Lights := SwitchLights(ToggleA, SwitchA);
  Result := Lights.Count; //543903
  Lights.Free;
end;

function TAdventOfCodeDay6.SolveB: Variant;
Var Lights: TDictionary<TPoint, integer>;
    i: integer;
begin
  Lights := SwitchLights(ToggleB, SwitchB);
  Result := 0;
  for i in Lights.Values do
    Inc(Result, i); //14687245
  Lights.Free;

end;

function TAdventOfCodeDay6.SwitchLights(Const OnToggle, OnSwitch: TLightEvent): TDictionary<TPoint, Integer>;

  procedure _InternalSwitchLights(aEvent: TLightEvent; aStart, aStop: string; aSwitchOn: Boolean);
  var split: TStringDynArray;
      x, x1, x2, y, y1, y2: integer;
      Point: TPoint;
  begin
    split := SplitString(aStart, ',');
    x1 := StrToInt(Split[0]);
    y1 := StrToInt(Split[1]);
    split := SplitString(aStop, ',');
    x2 := StrToInt(Split[0]);
    y2 := StrToInt(Split[1]);

    for x := x1 to x2 do
      for y := y1 to y2 do
      begin
        Point := TPoint.Create(x,y);
        aEvent(Result, Point, aSwitchOn);
      end;
  end;

var s: String;
    Split: TStringDynArray;
begin
  Result := TDictionary<TPoint, Integer>.Create;
  for s in FInput do
  begin
    Split := SplitString(s, ' ');
    if SameText(Split[0], 'toggle' ) then
      _InternalSwitchLights(OnToggle, split[1], split[3], False{Not used})
    else if SameText(Split[0], 'turn') then
      _InternalSwitchLights(OnSwitch, split[2], split[4], Split[1] = 'on')
    else
      WriteLn('Unknown command: ', Split[0]);
  end;
end;

procedure TAdventOfCodeDay6.ToggleA(Lights: TDictionary<TPoint,Integer>; Const aPoint: TPoint; Const SwitchOn: boolean);
begin
  if Lights.ContainsKey(aPoint) then
    Lights.Remove(aPoint)
  else
    Lights.Add(aPoint, 1);
end;

procedure TAdventOfCodeDay6.SwitchA(Lights: TDictionary<TPoint,Integer>; Const aPoint: TPoint; Const SwitchOn: boolean);
begin
  if SwitchOn then
    Lights.AddOrSetValue(aPoint, 1)
  else
    Lights.Remove(aPoint);
end;

procedure TAdventOfCodeDay6.ToggleB(Lights: TDictionary<TPoint,Integer>; Const aPoint: TPoint; Const SwitchOn: boolean);
var i: integer;
begin
  Lights.TryGetValue(aPoint, i);
  Lights.AddOrSetValue(aPoint, i+2);
end;

procedure TAdventOfCodeDay6.SwitchB(Lights: TDictionary<TPoint,Integer>; Const aPoint: TPoint; Const SwitchOn: boolean);
var i: Integer;
begin
  Lights.TryGetValue(aPoint, i);
  if SwitchOn then
    Inc(i)
  else
    i := Max(0, i-1);

  Lights.AddOrSetValue(aPoint, i);
end;

{$ENDREGION}
{$Region 'TAdventOfCodeDay7'}
function TAdventOfCodeDay7.SolveA: Variant;
begin
  Result := RunProgram(0); //956
end;

function TAdventOfCodeDay7.SolveB: Variant;
begin
  Result := RunProgram(RunProgram(0)); //40149
end;

function TAdventOfCodeDay7.RunProgram(Const OverrideB: Word): Integer;
var Wires: TDictionary<String,Word>;

  function TryGetWireValue(Const aInput: string; out Value: Word): Boolean;
  var i: integer;
  begin
    Result := (TryStrToInt(aInput, i) or Wires.TryGetValue(aInput, Value));
    if Result and not Wires.ContainsKey(aInput) then
      Value := i;
  end;

Var Input: TList<String>;
    s: string;
    Split: TStringDynArray;
    wrd1, wrd2: Word;
begin
  Input := TList<string>.Create;
  Wires := TDictionary<String,Word>.Create;

  try
    for s in FInput do
      Input.Add(s);

    while Input.Count > 0 do
    begin
      for s in Input do
      begin
        Split := SplitString(s, ' ');
        if SameText(Split[1], 'AND') and TryGetWireValue(Split[0], wrd1) and TryGetWireValue(Split[2], wrd2) then
          Wires.Add(Split[4], wrd1 and wrd2)
        else if SameText(Split[1], 'OR') and TryGetWireValue(Split[0], wrd1) and TryGetWireValue(Split[2], wrd2) then
          Wires.Add(Split[4], wrd1 or wrd2)
        else if SameText(Split[1], 'LSHIFT') and TryGetWireValue(Split[0], wrd1) and TryGetWireValue(Split[2], wrd2) then
          Wires.Add(Split[4], wrd1 shl wrd2)
        else if SameText(Split[1], 'RSHIFT') and TryGetWireValue(Split[0], wrd1) and TryGetWireValue(Split[2], wrd2) then
          Wires.Add(Split[4], wrd1 shr wrd2)
        else if SameText(Split[0], 'NOT') and Wires.TryGetValue(Split[1], wrd1) then
          Wires.Add(Split[3], Not wrd1)
        else if SameText(Split[1], '->') and TryGetWireValue(Split[0], wrd1) then
        begin
          if (Split[2] = 'b') and (OverrideB > 0) then
            wrd1 := OverrideB;

          Wires.Add(Split[2], wrd1);
        end
        else
          Continue;

        Input.Remove(s);
      end;
    end;


  finally
    Result := Wires['a'];
    Input.Free;
    Wires.Free;
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay8'}
function TAdventOfCodeDay8.SolveA: Variant;
var s, temps: string;
begin
  Result := 0;
  for s in FInput do
  begin
    temps := s;
    temps := StringReplace(temps, '\\', 'X', [rfReplaceAll]);
    temps := StringReplace(temps, '\"', 'X', [rfReplaceAll]);
    temps := StringReplace(temps, '"', '', [rfReplaceAll]);
    Result := Result + Length(s) - Length(Temps) + OccurrencesOfChar(temps, '\x')*3 //1371
  end;
end;

function TAdventOfCodeDay8.SolveB: Variant;
var s: string;
    Added: integer;
begin
  Result := 0;
  for s in FInput do
  begin
    Added := Length(StringReplace(s, '\', '\\', [rfReplaceAll])) - Length(s);
    Added := Added + Length(StringReplace(s, '"', '\\', [rfReplaceAll])) - Length(s);
    Result := Result + Added + 2;
  end;

end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay9'}
procedure TAdventOfCodeDay9.BeforeSolve;
Var s: string;
    Split: TStringDynArray;
begin
  Places := TList<String>.Create;
  Distances := TDictionary<string, Integer>.Create;

  for s in FInput do
  begin
    Split := SplitString(s, ' ');
    if not Places.Contains(Split[0]) then
      Places.Add(Split[0]);
    if not Places.Contains(Split[2]) then
      Places.Add(Split[2]);

    Distances.Add(Split[0] + Split[2], Split[4].ToInteger);
    Distances.Add(Split[2] + Split[0], Split[4].ToInteger);
  end;

end;

procedure TAdventOfCodeDay9.AfterSolve;
begin
  Distances.Free;
  Places.Free;
end;

function TAdventOfCodeDay9.FindeBestRoute(Const MaxDistance: Boolean): Integer;

    procedure CheckPlaces(Const DistanceTraveld: Integer; Const CurrentPlace: String; aSeen: TList<string>);
    var Place: string;
        Distance: Integer;
    begin
      if aSeen.Count = Places.Count then
      begin
        if MaxDistance then
          Result := Max(Result, DistanceTraveld)
        else
          Result := Min(Result, DistanceTraveld);
        Exit;
      end;

      for Place in Places do
        if not aSeen.Contains(Place) then
        begin
          aSeen.Add(Place);
          Distances.TryGetValue(CurrentPlace+Place, Distance);
          CheckPlaces(DistanceTraveld + Distance, Place, aSeen);
          aSeen.Remove(Place);
        end;
    end;

var Seen: TList<String>;
begin
  Seen := TList<String>.Create;

  if MaxDistance then
    Result := 0
  else
    Result := MaxInt;

  CheckPlaces(0, '', Seen);

  Seen.Free;
end;

function TAdventOfCodeDay9.SolveA: Variant;
begin
  Result := FindeBestRoute(False);
end;

function TAdventOfCodeDay9.SolveB: Variant;
begin
  Result := FindeBestRoute(True);
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay10'}
function TAdventOfCodeDay10.SolveA: Variant;
begin
  SentanceA := Talk(FInput[0], 40);
  Result := Length(SentanceA);
end;

function TAdventOfCodeDay10.SolveB: Variant;
begin
  Result := Length(Talk(SentanceA, 10));
end;

function TAdventOfCodeDay10.Talk(Const StartSentacne: String; Const Repeats: Integer): string;
var NewSaid, Said, PrevChar: string;
  i, j, count: integer;
begin
  Said := StartSentacne;

  for i := 1 to Repeats do
  begin
    NewSaid := '';
    PrevChar := Said[1];
    Count := 0;
    for j := 1 to Length(Said) do
    begin
      if Said[j] = PrevChar then
        Inc(Count)
      else
      begin
        NewSaid := NewSaid + Count.ToString + PrevChar;
        PrevChar := Said[j];
        count := 1;
      end
    end;
    NewSaid := NewSaid + Count.ToString + PrevChar;
    Said := NewSaid;
  end;

  Result := Said;
end;

{$ENDREGION}
{$Region 'TAdventOfCodeDay11'}
function TAdventOfCodeDay11.SolveA: Variant;
begin
  FirstPassWord := FindNewPassWord(FInput[0]);
  Result := FirstPassWord;
end;

function TAdventOfCodeDay11.SolveB: Variant;
begin
  Result := FindNewPassWord(FirstPassWord);
end;

function TAdventOfCodeDay11.FindNewPassWord(Const OldPassWord: String): string;

  function PassWordValid(Const aPassWord: String): Boolean;
  var i, Doubles: Integer;
  begin
    Result := False;

    for i := 2 to Length(aPassWord)-1 do
    begin
      if ((Ord(aPassWord[i]) - Ord(aPassWord[i-1])) = 1) and ((Ord(aPassWord[i+1]) - Ord(aPassWord[i])) = 1) then
        Break;

      if i = Length(aPassWord)-1 then
        Exit;
    end;

    Doubles := 0;
    for i := Ord('a') to Ord('z') do
      if Pos(Chr(i)+Chr(i), aPassWord) > 0 then
        Inc(Doubles);
    if Doubles >= 2 then
      Result := True;
  end;

  function NextPassWord(Const aCurrent: String): string;
  Var NextChar: Char;
      Index: Integer;
  begin
    Result := aCurrent;
    Index := Length(aCurrent);

    while Index > 0 do
    begin
      NextChar := Chr(Ord(Result[Index])+1);
      if IndexStr(NextChar, ['i','o','l']) >= 0 then
        NextChar := Chr(Ord(Result[Index])+1);

      if Ord(nextchar) > Ord('z') then
      begin
        NextChar := 'a';
        Result[Index] := NextChar;
        Dec(index);
        if Index = 0 then
          Exit;
      end
      else
      begin
        Result[Index] := NextChar;
        Index := -1;
      end;
    end;
  end;

var FirstTime: Boolean;
begin
  Result := OldPassWord;
  FirstTime := True;
  while (not PassWordValid(Result)) or FirstTime do
  begin
    FirstTime := False;
    Result := NextPassWord(Result);
  end;
end;

{$ENDREGION}
{$Region 'TAdventOfCodeDay12'}
function TAdventOfCodeDay12.SolveA: Variant;
begin
  Result := CountNumbers(False);
end;

function TAdventOfCodeDay12.SolveB: Variant;
begin
  Result := CountNumbers(True);
end;

function TAdventOfCodeDay12.CountNumbers(const CheckForRed: Boolean): Integer;

   procedure Process(aJson: TJsonValue);
  var i: Integer;
      p: TJSONPair;
  begin
    if aJson is TJSONNumber then
      Result := Result + TJSONNumber(aJson).Value.ToInteger
    else if aJson is TJSONObject then
    begin
      if CheckForRed then
      for i := 0 to TJSONObject(aJson).Count - 1 do
      begin
        p := TJSONObject(aJson).Pairs[i];
        if (p.JsonValue is TJSONString) and ((p.JsonValue as TJSONString).Value = 'red') then
          Exit;
      end;

      for i := 0 to TJSONObject(aJson).Count - 1 do
      begin
        p := TJSONObject(aJson).Pairs[i];
        Process(p.JsonValue);
      end
    end
    else if aJson is TJSONArray then
      for i := 0 to TJSONArray(aJson).Count - 1 do
        Process(TJSONArray(aJson).Items[i]);
  end;

Var Json: TJSONValue;
begin
  Json := TJSONObject.ParseJSONValue(FInput[0]);
  Result := 0;
  Process(Json);
  Json.Free;
end;


{$ENDREGION}
{$Region 'TAdventOfCodeDay13'}
procedure TAdventOfCodeDay13.BeforeSolve;
var s: string;
    Split: TStringDynArray;
begin
  PotentialHapiness := TDictionary<string, Integer>.Create;
  Attendees := TList<String>.Create;

  for s in FInput do
  begin
    Split := SplitString(s, ' .');
    if not Attendees.Contains(Split[0]) then
      Attendees.Add(Split[0]);
    PotentialHapiness.Add(Split[0]+Split[10], IfThen(Split[2] = 'gain', 1, -1)*Split[3].ToInteger);
  end;
end;

procedure TAdventOfCodeDay13.AfterSolve;
begin
  PotentialHapiness.Free;
  Attendees.Free;
end;

function TAdventOfCodeDay13.ArrangeSeating(Const Attendees: TList<String>): Integer;

 procedure DetermineHappiness(Const attending: TList<string>);
  var Change, Happiness, i, j: integer;
  begin
    Happiness := 0;
    for i := 0 to attending.Count-1 do
    begin
      j := i-1;
      if i = 0 then
        j := attending.Count-1;
      PotentialHapiness.TryGetValue(attending[i]+attending[j], Change);
      Happiness := Happiness + Change;

      j := i + 1;
      if i = attending.Count-1 then
        j := 0;
      PotentialHapiness.TryGetValue(attending[i]+attending[j], Change);
      Happiness := Happiness + Change;
    end;

    Result := Max(Happiness, Result);
  end;

  procedure InternalArrengeSeating(const Attending: TList<String>);
  var s: string;
  begin
    if Attending.Count = Attendees.Count then
      DetermineHappiness(Attending)
    else
    for s in Attendees do
      if not Attending.Contains(s) then
      begin
        Attending.Add(s);
        InternalArrengeSeating(Attending);
        Attending.Remove(s);
      end;
  end;

var Attending: TList<String>;
begin
  Attending := TList<String>.Create;
  Result := 0;
  InternalArrengeSeating(Attending);
  Attending.Free;
end;

function TAdventOfCodeDay13.SolveA: Variant;
begin
  Result := ArrangeSeating(Attendees);
end;

function TAdventOfCodeDay13.SolveB: Variant;
Var ExtendedAtendees: TList<String>;
begin
  ExtendedAtendees := TList<String>.Create(Attendees);
  ExtendedAtendees.Add('Me');
  Result := ArrangeSeating(ExtendedAtendees);
  ExtendedAtendees.Free;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay14'}
procedure TAdventOfCodeDay14.BeforeSolve;
Var Spec: RReindeerSpecs;
    split: TstringDynArray;
    s: string;
begin
  ReindeerSpecs := TDictionary<String,RReindeerSpecs>.Create;

  for s in FInput do
  begin
    Split := SplitString(s, ' ');
    Spec.Speed := Split[3].ToInteger;
    Spec.TravelTime := Split[6].ToInteger;
    Spec.RestingTime := Split[13].ToInteger;
    ReindeerSpecs.Add(Split[0], Spec);
  end;
end;

procedure TAdventOfCodeDay14.AfterSolve;
begin
  ReindeerSpecs.Free;
end;

function TAdventOfCodeDay14.CalculateDistanceTraveld(const Racetime: Integer; Specs: RReindeerSpecs): integer;
var TotalTime: Integer;
begin
  TotalTime := 0;
  Result := 0;

  while TotalTime < RaceTime do
  begin
    if TotalTime + Specs.TravelTime > RaceTime then
      Result := Result + Specs.Speed * (RaceTime - TotalTime)
    else
      Result := Result + Specs.Speed * Specs.TravelTime;
    TotalTime := TotalTime + Specs.TravelTime + Specs.RestingTime;
  end;
end;

function TAdventOfCodeDay14.SolveA: Variant;
var Spec: RReindeerSpecs;
begin
  Result := 0;

  for Spec in ReindeerSpecs.Values do
    Result := Max(Result, CalculateDistanceTraveld(2503, Spec));
end;

function TAdventOfCodeDay14.SolveB: Variant;
var Points, DistanceTraveld: TDictionary<String, Integer>;
  Point, i, BestDistance, Traveld: Integer;
  Reindeer: TPair<String, RReindeerSpecs>;
  Pair: TPair<String, Integer>;
begin
  Points  := TDictionary<String, Integer>.Create;
  DistanceTraveld := TDictionary<String, Integer>.Create;

  for i := 1 to 2503 do
  begin
    BestDistance := 0;

    for Reindeer in ReindeerSpecs do
    begin
      Traveld := CalculateDistanceTraveld(i, Reindeer.Value);
      BestDistance := Max(BestDistance, Traveld);
      DistanceTraveld.AddOrSetValue(Reindeer.Key, Traveld);
    end;

    for Pair in DistanceTraveld do
      if Pair.Value = BestDistance then
      begin
        Points.TryGetValue(Pair.Key, Point);
        Points.AddOrSetValue(Pair.Key, Point+1);
      end;
  end;

  Result := 0;
  for Point in Points.Values do
    Result := Max(Point, Result);

  Points.Free;
  DistanceTraveld.Free;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay15'}
function TAdventOfCodeDay15.SolveA: Variant;
begin
  Result := CreateCookies(false);
end;

function TAdventOfCodeDay15.SolveB: Variant;
begin
  Result := CreateCookies(True)
end;

type RIngredientData = Record
 capacity, durability, flavor, texture, calories: integer;
End;

function TAdventOfCodeDay15.CreateCookies(Const LimitCalories: Boolean): integer;
var Ingredients: TList<RIngredientData>;
    IngredientData: RIngredientData;
    Split: TStringDynArray;
    s: string;
    i1, i2, i3, i4, Score, totalcapacity, totaldurability, totalflavor, totaltexture, totalcalories: integer;
begin
  Ingredients := TList<RIngredientData>.Create;
  for s in FInput do
  begin
    Split := SplitString(s.Replace(',', ''), ' ');
    IngredientData.capacity := Split[2].ToInteger;
    IngredientData.durability := Split[4].ToInteger;
    IngredientData.flavor := Split[6].ToInteger;
    IngredientData.texture := Split[8].ToInteger;
    IngredientData.calories := Split[10].ToInteger;
    Ingredients.Add(IngredientData)
  end;

  Result := 0;
  for i1 := 0 to 100 do
    for i2 := 0 to 100 do
      for i3 := 0 to 100 do
        for i4 := 0 to 100 do
        if (i1 + i2 + i3 + i4 = 100) then
        begin
          totalcapacity :=    Max(0, Ingredients[0].capacity*i1
                            + Ingredients[1].capacity*i2
                            + Ingredients[2].capacity*i3
                            + Ingredients[3].capacity*i4);
          totaldurability  := Max(0, Ingredients[0].durability*i1
                            + Ingredients[1].durability*i2
                            + Ingredients[2].durability*i3
                            + Ingredients[3].durability*i4);
          totalflavor :=      Max(0, Ingredients[0].flavor*i1
                            + Ingredients[1].flavor*i2
                            + Ingredients[2].flavor*i3
                            + Ingredients[3].flavor*i4);
          totaltexture :=     Max(0, Ingredients[0].texture*i1
                            + Ingredients[1].texture*i2
                            + Ingredients[2].texture*i3
                            + Ingredients[3].texture*i4);
          totalcalories :=  Max(0, Ingredients[0].Calories*i1
                            + Ingredients[1].Calories*i2
                            + Ingredients[2].Calories*i3
                            + Ingredients[3].Calories*i4);

          Score := totalcapacity * totaldurability * totalflavor * totaltexture;

          if (totalcalories = 500) or (not LimitCalories) then
            Result := Max(Result, Score);
        end;
  Ingredients.Free;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay16'}
function TAdventOfCodeDay16.SolveA: Variant;
begin
  Result := FindAunt(ValidatorA);
end;

function TAdventOfCodeDay16.SolveB: Variant;
begin
  Result := FindAunt(ValidatorB);//389 to high
end;

function TAdventOfCodeDay16.FindAunt(const Validator: TValidator): String;
var s: string;
    split: TStringDynArray;
    i: integer;
    Error: Boolean;
begin
  result := '';
  for s in FInput do
  begin
    Split := SplitString(s.Replace(':', '').replace(',', ''), ' ');
    Error := False;
    i := 2;
    while i < Length(split) do
    begin
      error := error or Validator(Split[i], Split[i+1].ToInteger);
      inc(i, 2);
    end;

    if not error then
      Result := split[1];
  end;
end;

function TAdventOfCodeDay16.ValidatorA(const aObject: string; const Value: integer): Boolean;
begin
  case IndexStr(aObject, ['children', 'cats', 'samoyeds', 'pomeranians', 'akitas', 'vizslas', 'goldfish', 'trees','cars','perfumes'    ]) of
    0: Result := (value <> 3);
    1: Result := (value <> 7);
    2: Result := (value <> 2);
    3: Result := (value <> 3);
    4: Result := (value <> 0);
    5: Result := (value <> 0);
    6: Result := (value <> 5);
    7: Result := (value <> 3);
    8: Result := (value <> 2);
    9: Result := (value <> 1);
  else
    Result := False;
    Assert(false, aObject + ' not found');
  end;
end;

function TAdventOfCodeDay16.ValidatorB(const aObject: string; const Value: integer): Boolean;
begin
  case IndexStr(aObject, ['cats', 'pomeranians', 'goldfish', 'trees']) of
    0: Result := (value < 7);
    1: Result := (value > 3);
    2: Result := (value > 5);
    3: Result := (value < 3);
  else
    Result := ValidatorA(aObject, Value);
  end;
end;

{$ENDREGION}
{$Region 'TAdventOfCodeDay17'}
procedure TAdventOfCodeDay17.BeforeSolve;
 var Cans: TDictionary<Integer,Integer>;

  procedure FindCans(Const TotalLiters, CanId, CansUsed: Integer);
  var Size: Integer;
  begin
    if TotalLiters = 150 then
    begin
      Combinations.Add(CansUsed);
      exit;
    end;

    if Cans.TryGetValue(CanId, size) then
    begin
      FindCans(TotalLiters, CanId+1, CansUsed);
      FindCans(TotalLiters+size, CanId+1, CansUsed+1);
    end;
  end;

var s: string;
begin
  Combinations := TList<Integer>.Create;
  Cans := TDictionary<Integer,Integer>.create;

  for s in FInput do
    Cans.Add(Cans.Count+1, s.ToInteger);

  FindCans(0, 1, 0);
  Cans.Free;
end;

procedure TAdventOfCodeDay17.AfterSolve;
begin
  Combinations.Free;
end;

function TAdventOfCodeDay17.SolveA: Variant;
begin
  Result := Combinations.Count;
end;

function TAdventOfCodeDay17.SolveB: Variant;
var MinValue, i: integer;
begin
  MinValue := MaxInt;
  for i in Combinations do
    MinValue := Min(i, MinValue);

  Result := 0;
  for i in Combinations do
    if i = minvalue then
      Inc(Result);
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay18'}
function TAdventOfCodeDay18.SolveA: Variant;
begin
  Result := SwitchLights(False);
end;

function TAdventOfCodeDay18.SolveB: Variant;
begin
  Result := SwitchLights(True);
end;

function TAdventOfCodeDay18.SwitchLights(Const StuckCorners: Boolean): integer;
var LightGrid: TDictionary<TPoint, Boolean>;

  function CountNeighbours(Const aPoint: TPoint): integer;
  const DeltaX: array[0..7] of integer = (-1,-1,-1,0,0,1,1,1);
  const DeltaY: array[0..7] of integer = (-1,0,1,-1,1,-1,0,1);
  var Point: TPoint;
      Val: Boolean;
      i: integer;
  begin
    Result := 0;
    for i := Low(DeltaX) to High(DeltaX) do
    begin
      Point := TPoint.Create(aPoint);
      Point.Offset(DeltaX[i], DeltaY[i]);
      LightGrid.TryGetValue(Point, val);
      if Val then
        Inc(Result);
    end;
  end;

  procedure StuckLights;

    procedure Switch(x,y: integer);
    var Point: TPoint;
    begin
      Point := TPoint.Create(x,y);
      LightGrid[Point] := true;
    end;

  var Width: Integer;
  begin
    Width := FInput.Count-1;
    Switch(0, 0);
    Switch(0, Width);
    Switch(Width, 0);
    Switch(Width, Width);
  end;

var PendingChanges: TList<TPoint>;
    Point: TPoint;
    i,n,x,y: Integer;
    b: boolean;
begin
  PendingChanges := TList<TPoint>.Create;
  LightGrid := TDictionary<TPoint, Boolean>.Create;

  for y := 0 to FInput.Count - 1 do
    for x := 0 to Length(FInput[0]) -1 do
    begin
      Point := TPoint.Create(x,y);
      LightGrid.Add(Point, FInput[y][x+1]= '#');
    end;

  for i := 1 to 100 do
  begin
    PendingChanges.Clear;

    if StuckCorners then
      StuckLights;

    for Point in LightGrid.Keys do
    begin
      n := CountNeighbours(Point);
      if LightGrid[Point] and (not (n in [2,3])) then
        PendingChanges.Add(Point)
      else if not LightGrid[Point] and (n = 3) then
        PendingChanges.Add(Point);
    end;

    for Point in PendingChanges do
      LightGrid[point] := not LightGrid[Point];
  end;

  if StuckCorners then
    StuckLights;

  result := 0;
  for b in LightGrid.Values do
    if b then
      Inc(result);

  PendingChanges.Free;
  LightGrid.Free;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay19'}
type RMedicinePart = record
Before, After: string
end;

function TAdventOfCodeDay19.SolveA: Variant;
Var Parts: TList<RMedicinePart>;
    PossibleMedicines: TList<String>;
    s, BaseMolecule, Medicine: string;
    Split: TStringDynArray;
    Part: RMedicinePart;
    i: integer;
begin
  Parts := TList<RMedicinePart>.Create;
  PossibleMedicines := TList<String>.Create;
  for s in FInput do
  begin
    if s = '' then
      break;
    split := SplitString(s, ' ');
    Part.Before := Split[0];
    Part.After := Split[2];
    Parts.Add(Part);
  end;

  BaseMolecule := FInput[FInput.Count-1];
  for part in Parts do
  begin
    i := 1;
    while Pos(Part.Before, BaseMolecule, i) > 0 do
    begin
      i := Pos(Part.Before, BaseMolecule, i);
      Medicine := LeftStr(BaseMolecule, Max(i-1, 0)) + Part.After + RightStr(BaseMolecule, Length(BaseMolecule)-Length(Part.Before)-i+1);

      if not PossibleMedicines.Contains(Medicine) then
        PossibleMedicines.Add(Medicine);

      inc(i);
    end;
  end;

  Result := PossibleMedicines.Count;
  PossibleMedicines.Free;
  Parts.Free;
end;

function TAdventOfCodeDay19.SolveB: Variant;
Var Parts: TList<RMedicinePart>;
    s, BaseMolecule: string;
    Split: TStringDynArray;
    Part: RMedicinePart;
    i: integer;
    Comparison: TComparison<RMedicinePart>;
begin
  //For some reason this worked (dont know why)
  Comparison :=
    function(const Left, Right: RMedicinePart): Integer
    begin
      Result := Random(100);
    end;

  Parts := TList<RMedicinePart>.Create(TComparer<RMedicinePart>.Construct(Comparison));
  for s in FInput do
  begin
    if s = '' then
      break;
    split := SplitString(s, ' ');
    Part.Before := Split[0];
    Part.After := Split[2];
    Parts.Add(Part);
  end;

  Parts.Sort;
  Result := 0;
  BaseMolecule := FInput[FInput.Count-1];
  while BaseMolecule <> 'e' do
  begin
    i := Length(BaseMolecule);
    for part in parts do
    begin
      if Pos(part.After, BaseMolecule) > 0 then
      begin
        BaseMolecule :=  BaseMolecule.Replace((Part.After), (Part.Before), []);
        Inc(Result); //207
        Break;
      end;
    end;

    if i = Length(BaseMolecule) then
      Parts.Sort;
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay20'}
function TAdventOfCodeDay20.SolveA: Variant;
begin
  result := Visit(True, 10);
end;

function TAdventOfCodeDay20.SolveB: Variant;
begin
  result := Visit(False, 11);
end;

function TAdventOfCodeDay20.Visit(Const infinitePresents: boolean; Const PresentsPerVisit: integer): integer;
var count, HousesVisited: Integer;
    Houses: array of Integer;
    i, j: Integer;
begin
  count := StrToInt(FInput[0]);

  SetLength(Houses, Count div 10 + 1);
  for i := 1 to Length(Houses)  do
  begin
    HousesVisited := 0;
    j := i;
    while (j <= Length(Houses)) and (infinitePresents or (HousesVisited < 50)) do
    begin
      Houses[j] := Houses[j] + i*PresentsPerVisit;
      Inc(j, i);
      Inc(HousesVisited);
    end;
  end;

  Result := 1;
  while Houses[integer(Result)] <= count do
    inc(Result);
end;

{$ENDREGION}
{$Region 'TAdventOfCodeDay21'}
type RItem = record
  Name: String;
  Cost, Damage, Armor: Integer;
end;

Const Weapons: array[0..4] of RItem =
(
(Name: 'Dagger';     Cost: 8;  Damage: 4; Armor: 0),
(Name: 'Shortsword'; Cost: 10; Damage: 5; Armor: 0),
(Name: 'Warhammer';  Cost: 25; Damage: 6; Armor: 0),
(Name: 'Longsword';  Cost: 40; Damage: 7; Armor: 0),
(Name: 'Greataxe';   Cost: 74; Damage: 8; Armor: 0)
);
Const Armor: array[0..4] of RItem =
(
(Name: 'Leather';    Cost: 13;  Damage: 0; Armor: 1),
(Name: 'Chainmail';  Cost: 31;  Damage: 0; Armor: 2),
(Name: 'Splintmail'; Cost: 53;  Damage: 0; Armor: 3),
(Name: 'Bandedmail'; Cost: 75;  Damage: 0; Armor: 4),
(Name: 'Platemail '; Cost: 102; Damage: 0; Armor: 5)
);
Const Rings: array[0..5] of RItem =
(
(Name: 'Damage +1';  Cost: 25;  Damage: 1; Armor: 0),
(Name: 'Damage +2';  Cost: 50;  Damage: 2; Armor: 0),
(Name: 'Damage +3';  Cost: 100; Damage: 3; Armor: 0),
(Name: 'Defense +1'; Cost: 20;  Damage: 0; Armor: 1),
(Name: 'Defense +2'; Cost: 40;  Damage: 0; Armor: 2),
(Name: 'Defense +3'; Cost: 80;  Damage: 0; Armor: 3)
);

function TAdventOfCodeDay21.SimulateGame(Const PlayerWins: Boolean): Integer;
var BossHitPoints, BossDamage, BossArmor: integer;

  function GetBossStat(Const Index: integer): integer;
  var s: string;
  begin
    s := FInput[Index];
    result := StrToInt(RightStr(s, Length(s) - (Pos(':', s))));
  end;

  function PlayGame(Const Items: TList<RItem>; out Cost: integer): Boolean;

    function CalcDamage(Const Damage, Armor: Integer): integer;
    begin
      Result := Max(Damage - Armor, 1);
    end;

  Var Damage, Armor, PlayerHitPoints, TempBossHitPoints: Integer;
      Item: RItem;
  begin
    Cost := 0;
    Damage := 0;
    Armor  := 0;
    PlayerHitPoints := 100;
    TempBossHitPoints := BossHitPoints;
    for Item in Items do
    begin
      Inc(Cost, Item.Cost);
      Inc(Damage, Item.Damage);
      Inc(Armor, Item.Armor);
    end;

    while True do
    begin
      TempBossHitPoints := TempBossHitPoints - CalcDamage(Damage, BossArmor);
      if TempBossHitPoints <= 0 then
        Exit(True); //Player won

      PlayerHitPoints := PlayerHitPoints - CalcDamage(BossDamage, Armor);
      if PlayerHitPoints <= 0 then
        Exit(False);
    end;
  end;

var Items: TList<RItem>;
    WeaponIndex, ArmorIndex, Ring1Index, Ring2Index, Cost: integer;
begin
  BossHitPoints := GetBossStat(0);
  BossDamage := GetBossStat(1);
  BossArmor := GetBossStat(2);

  Items := TList<RItem>.Create;

  if PlayerWins then
    Result := MaxInt
  else
    Result := 0;

  for WeaponIndex := Low(Weapons) to High(Weapons) do
    for ArmorIndex := Low(Armor) to High(Armor) + 1 do
      for Ring1Index := Low(Rings) to High(Rings) + 1 do
        for Ring2Index := Ring1Index to High(Rings) + 1 do
        begin
          Items.Clear;
          Items.Add(Weapons[WeaponIndex]);
          if InRange(ArmorIndex, Low(Armor), High(Armor)) then
            Items.Add(Armor[ArmorIndex]);
          if InRange(Ring1Index, Low(Rings), High(Rings)) then
            Items.Add(Rings[Ring1Index]);
          if InRange(Ring2Index, Low(Rings), High(Rings)) then
            Items.Add(Rings[Ring2Index]);

          if PlayGame(Items, Cost) = PlayerWins then
          begin
            if PlayerWins then
              Result := Min(Cost, Result)
            else
              Result := Max(Cost, Result)
          end;
        end;
  Items.Free;
end;


function TAdventOfCodeDay21.SolveA: Variant;
begin
  Result := SimulateGame(True);
end;

function TAdventOfCodeDay21.SolveB: Variant;
begin
  Result := SimulateGame(False);
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay22'}
function TAdventOfCodeDay22.SolveA: Variant;
begin
  Result := PlayGame(False);
end;

function TAdventOfCodeDay22.SolveB: Variant;
begin
  Result := PlayGame(True);
end;

function TAdventOfCodeDay22.PlayGame(Const HardMode: Boolean): integer;
var BossDamage: Integer;

  function GetBossStat(Const Index: integer): integer;
  var s: string;
  begin
    s := FInput[Index];
    result := StrToInt(RightStr(s, Length(s) - (Pos(':', s))));
  end;

  procedure Simulate2(PlayersTurn: Boolean; aPlayerHealh, aBossHealth, aTurn, aRemainingMana, ManaSpent, ShieldActive, PoisenActive, RechargeActive: Integer);
  Var PlayerArmor: Integer;
  begin
    if ManaSpent > result then
      Exit;

    if aBossHealth <= 0 then
    begin
      Result := Min(Result, ManaSpent);
      Exit;
    end;

    if HardMode and PlayersTurn then
      Dec(aPlayerHealh);

    if aPlayerHealh <= 0 then
      Exit; //Player died

    //Effects;
    PlayerArmor := 0;
    Dec(ShieldActive);
    if ShieldActive >= 0 then
      PlayerArmor := 7;

    Dec(PoisenActive);
    if PoisenActive >= 0 then
      aBossHealth := aBossHealth - 3;

    Dec(RechargeActive);
    if RechargeActive >= 0 then
      aRemainingMana := aRemainingMana + 101;

    if aBossHealth <= 0 then
    begin
      Result := Min(Result, ManaSpent);
      Exit;
    end;

    if PlayersTurn then
    begin
      if aRemainingMana >= 53 then
        Simulate2(False, aPlayerHealh, aBossHealth-4, aTurn+1, aRemainingMana-53, ManaSpent+53, ShieldActive, PoisenActive, RechargeActive);

      if aRemainingMana >= 73 then
        Simulate2(False, aPlayerHealh+2, aBossHealth-2, aTurn+1, aRemainingMana-73, ManaSpent+73, ShieldActive, PoisenActive, RechargeActive);

      if (aRemainingMana >= 113) and (ShieldActive <= 0) then
        Simulate2(False, aPlayerHealh, aBossHealth, aTurn+1, aRemainingMana-113, ManaSpent+113, 6, PoisenActive, RechargeActive);

      if (aRemainingMana >= 173) and (PoisenActive <= 0) then
        Simulate2(False, aPlayerHealh, aBossHealth, aTurn+1, aRemainingMana-173, ManaSpent+173, ShieldActive, 6, RechargeActive);

      if (aRemainingMana >= 229) and (RechargeActive <= 0) then
        Simulate2(False, aPlayerHealh, aBossHealth, aTurn+1, aRemainingMana-229, ManaSpent+229, ShieldActive, PoisenActive, 5);
    end
    else
    begin
      aPlayerHealh := aPlayerHealh - Max(BossDamage - PlayerArmor, 1);
      Simulate2(True, aPlayerHealh, aBossHealth, aTurn+1, aRemainingMana, ManaSpent, ShieldActive, PoisenActive, RechargeActive);
    end;
  end;

begin
  BossDamage := GetBossStat(1);
  Result := MaxInt;
  Simulate2(True, 50, GetBossStat(0), 0, 500, 0, 0, 0, 0);
end;


{$ENDREGION}
{$Region 'TAdventOfCodeDay23'}
function TAdventOfCodeDay23.SolveA: Variant;
begin
  Result := RunProgram(0);
end;

function TAdventOfCodeDay23.SolveB: Variant;
begin
  Result := RunProgram(1);
end;

function TAdventOfCodeDay23.RunProgram(StartValueA: Integer): Integer;
 Var RegisterA, RegisterB: Integer;

  function IsRegisterA(Const aRegister: String): Boolean;
  begin
    Result := SameText(aRegister, 'a');
  end;

  procedure SetRegister(Const aRegister: string; Const Value: Integer);
  begin
    if IsRegisterA(aRegister) then
      RegisterA := Value
    else
      RegisterB := Value;
  end;

  function GetRegister(Const aRegister: string): Integer;
  begin
    if IsRegisterA(aRegister) then
      Result := RegisterA
    else
      Result := RegisterB;
  end;

var Pointer: Integer;
    Instruction: TStringDynArray;
begin
  RegisterA := StartValueA;
  RegisterB := 0;
  Pointer := 0;

  while (Pointer >= 0) and (Pointer < FInput.Count) do
  begin
    Instruction := SplitString(FInput[Pointer].Replace('+','').Replace(',', ''), ' ');
    case IndexStr(Instruction[0], ['hlf', 'tpl', 'inc', 'jmp', 'jie', 'jio']) of
      0: SetRegister(Instruction[1], GetRegister(Instruction[1]) div 2);
      1: SetRegister(Instruction[1], GetRegister(Instruction[1]) * 3);
      2: SetRegister(Instruction[1], GetRegister(Instruction[1]) + 1);
      3: Pointer := Pointer + StrToInt(Instruction[1])-1;
      4: if not odd(GetRegister(Instruction[1])) then
          Pointer := Pointer + StrToInt(Instruction[2])-1;
      5: if (GetRegister(Instruction[1]) = 1) then
          Pointer := Pointer + StrToInt(Instruction[2])-1;
    end;
    Inc(Pointer)
  end;

  Result := RegisterB;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay24'}
function TAdventOfCodeDay24.SolveA: Variant;
begin
  Result := LoadSleigh(3);
end;

function TAdventOfCodeDay24.SolveB: Variant;
begin
  Result := LoadSleigh(4);
end;

function TAdventOfCodeDay24.LoadSleigh(const Groups: integer): int64;
var Numbers: TList<Int64>;
    s: string;
    Total, Target: int64;
    PackegesCount: Integer;

    function CanCreateTotal(id, atotal: Int64; Used: TList<Int64>): Boolean;
    begin
      Result := atotal = Target;
      if Result or (id >= Numbers.Count) then
        Exit;

      Result := CanCreateTotal(id + 1, atotal, used);

      if Result  then
        Exit;

      if not Used.Contains(Numbers[id]) then
        Result := Result or CanCreateTotal(id+1, atotal+Numbers[id], Used);
    end;

    procedure Process(PresentId, aCurrentWeight: Int64; Used: TList<Int64>);
    var i, Product: int64;
    begin
      if (aCurrentWeight > Target) or (Used.Count > PackegesCount) then
        Exit;

      if aCurrentWeight = Target then
      begin
        Product := int64(1);

        if CanCreateTotal(0, 0, Used) then
        begin
          for i in Used do
          begin
            s := s + ' ' + i.ToString;
            Product := Product * int64(i);
          end;

          if Used.Count < PackegesCount then
          begin
            Result := Product;
            PackegesCount := Used.Count;
          end
          else
            Result := Min(Result, Product)
        end
      end
      else
      begin
        if (PresentId >= Numbers.Count)  then
          Exit;

        Process(PresentId + 1, aCurrentWeight, Used);

        Used.Add(Numbers[PresentId]);
        Process(PresentId + 1, aCurrentWeight + Numbers[PresentId], Used);
        Used.Remove(Numbers[PresentId]);
      end;
    end;

var NumsUsed: TList<Int64>;
begin
  Numbers := TList<Int64>.Create;
  NumsUsed := TList<Int64>.Create;
  Total := 0;
  PackegesCount := MaxInt;
  for s in FInput do
  begin
    Inc(Total, s.ToInt64);
    Numbers.Add(s.ToInt64);
  end;

  Target := Total div Groups;

  Result := 999999999999999999;
  Process(0, 0, NumsUsed);
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay25'}
function TAdventOfCodeDay25.SolveA: Variant;
Const multi: Int64 = 252533;
      divide: Int64 = 33554393;
Var Split: TStringDynArray;
    TargetRow, TargetColumn, x, y, a: Int64;
begin
  Split := SplitString(Finput[0].Replace(',', '').Replace('.',''), ' ');
  TargetRow := Split[Length(Split) - 3].ToInt64;
  TargetColumn := Split[Length(Split) - 1].ToInt64;

  x := 1;
  y := 1;
  a := 20151125;

  while True do
  begin
    if (y = TargetRow) and (x = TargetColumn) then
      exit(a);

    if y = 1 then
    begin
      y := x + 1;
      x := 1;
    end
    else
    begin
      Dec(y);
      Inc(x);
    end;

    a := (a * Multi) mod divide;
  end;
end;

function TAdventOfCodeDay25.SolveB: Variant;
begin
  Result := 'Done!'
end;
{$ENDREGION}
initialization
  TAocManager.RegisterAocClasses(_2015, [
    TAdventOfCodeDay1,TAdventOfCodeDay2,TAdventOfCodeDay3,TAdventOfCodeDay4,TAdventOfCodeDay5,
    TAdventOfCodeDay6,TAdventOfCodeDay7,TAdventOfCodeDay8,TAdventOfCodeDay9,TAdventOfCodeDay10,
    TAdventOfCodeDay11,TAdventOfCodeDay12,TAdventOfCodeDay13,TAdventOfCodeDay14,TAdventOfCodeDay15,
    TAdventOfCodeDay16,TAdventOfCodeDay17,TAdventOfCodeDay18,TAdventOfCodeDay19,TAdventOfCodeDay20,
    TAdventOfCodeDay21,TAdventOfCodeDay22,TAdventOfCodeDay23,TAdventOfCodeDay24,TAdventOfCodeDay25]);

end.
