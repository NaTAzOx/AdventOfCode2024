{$mode objfpc}
{$H+}                 // Active le mode de gestion des chaînes avec mémoire dynamique
{$I-,Q-,R-}           // Désactive certaines erreurs d'entrée/sortie et de runtime
{$APPTYPE CONSOLE}    // Spécifie que le programme est une application console
{$IFDEF FPC}
    {$CODEPAGE UTF-8} // Définit l'encodage en UTF-8 pour Free Pascal Compiler
{$ENDIF}

program December13;

uses
  SysUtils, Classes, Math;

type
  TButton = record
    X, Y, Cost: Integer;
  end;

  TPrize = record
    X, Y: Integer;
  end;

function ParseValue(Line: string; Prefix: string): Integer;
var
  StartPos, EndPos: Integer;
  ValueStr: string;
begin
  if Line = '' then
    raise Exception.Create('Empty line encountered.');

  StartPos := Pos(Prefix, Line);
  if StartPos = 0 then
    raise Exception.Create('Prefix "' + Prefix + '" not found in line: ' + Line);

  StartPos := StartPos + Length(Prefix);
  EndPos := StartPos;

  // Find the end of the numeric value
  while (EndPos <= Length(Line)) and (Line[EndPos] in ['0'..'9']) do
    Inc(EndPos);

  ValueStr := Copy(Line, StartPos, EndPos - StartPos);
  Result := StrToInt(ValueStr);
end;

function CalculateCost(ButtonA, ButtonB: TButton; Prize: TPrize; MaxPress: Integer): Integer;
var
  aPress, bPress: Integer;
  MinCost: Integer;
begin
  MinCost := MaxInt;

  // Tester toutes les combinaisons de pressions sur les boutons
  for aPress := 0 to MaxPress do
    for bPress := 0 to MaxPress do
    begin
      if (aPress * ButtonA.X + bPress * ButtonB.X = Prize.X) and
         (aPress * ButtonA.Y + bPress * ButtonB.Y = Prize.Y) then
      begin
        MinCost := Min(MinCost, aPress * ButtonA.Cost + bPress * ButtonB.Cost);
      end;
    end;

  if MinCost = MaxInt then
    Result := -1 // Pas de solution
  else
    Result := MinCost;
end;

var
  Lines: TStringList;
  Line: string;
  ButtonA, ButtonB: TButton;
  Prize: TPrize;
  TokensSpent, PrizeCount, MaxPress, Cost, i, machineNumber: Integer;
  FileName: string;
begin
  FileName := 'input.txt';
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    WriteLn('Total lines: ', Lines.Count);

    TokensSpent := 0;
    PrizeCount := 0;
    MaxPress := 100;
    machineNumber := 0;

    for i := 0 to (Lines.Count div 3) - 1 do
    begin
      if i * 3 + machineNumber >= Lines.Count then
        Break;
      // Lire les configurations des boutons et des prix
      Line := Lines[i * 3 + machineNumber];
      if Line = '' then
        Continue;
      WriteLn('Processing line: ', Line);
      ButtonA.X := ParseValue(Line, 'X+');
      ButtonA.Y := ParseValue(Line, 'Y+');
      ButtonA.Cost := 3; // Coût par défaut pour A

      Line := Lines[i * 3 + 1 + machineNumber];
      if Line = '' then
        Continue;
      WriteLn('Processing line: ', Line);
      ButtonB.X := ParseValue(Line, 'X+');
      ButtonB.Y := ParseValue(Line, 'Y+');
      ButtonB.Cost := 1; // Coût par défaut pour B

      Line := Lines[i * 3 + 2 + machineNumber];
      if Line = '' then
        Continue;
      WriteLn('Processing line: ', Line);
      Prize.X := ParseValue(Line, 'X=');
      Prize.Y := ParseValue(Line, 'Y=');

      Cost := CalculateCost(ButtonA, ButtonB, Prize, MaxPress);
      if Cost <> -1 then
        TokensSpent := TokensSpent + Cost;
      machineNumber := machineNumber + 1;
    end;

    WriteLn('Total tokens spent: ', TokensSpent);
  finally
    Lines.Free;
  end;
end.
