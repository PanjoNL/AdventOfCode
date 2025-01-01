unit uAocManager;

interface

uses
  System.Generics.Collections, uAocTypes, uAOCUtils;

type
  TAocManager = class
  private
    FImplementations: TDictionary<AocYear, AocArray>;
  public
    constructor Create;
    class procedure RegisterAocClasses(year: AocYear; Classes: AocArray);

    function GetImplementations(year: AocYear): AocArray;
  end;

var AocManager: TAocManager;

implementation

{ TAocManager }

constructor TAocManager.Create();
begin
  inherited;

  FImplementations := TDictionary<AocYear, AocArray>.Create;
end;

function TAocManager.GetImplementations(year: AocYear): AocArray;
begin
  Result := FImplementations[year];
end;

class procedure TAocManager.RegisterAocClasses(year: AocYear; Classes: AocArray);
begin
  if not Assigned(AocManager) then
    AocManager := TAocManager.Create;

  AocManager.FImplementations.Add(year, Classes);
end;

end.
