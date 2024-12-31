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
    X, Y, Cost: Int64;
  end;

  TPrize = record
    X, Y: Int64;
  end;

const
  PrizeOffset = 10000000000000;

function ParseValue(Line: string; Prefix: string): Int64;
var
  StartPos, EndPos: Integer;
  ValueStr: string;
begin
  StartPos := Pos(Prefix, Line);
  if StartPos = 0 then
    raise Exception.Create('Prefix "' + Prefix + '" not found in line: ' + Line);

  StartPos := StartPos + Length(Prefix);
  EndPos := StartPos;

  while (EndPos <= Length(Line)) and (Line[EndPos] in ['0'..'9']) do
    Inc(EndPos);

  ValueStr := Copy(Line, StartPos, EndPos - StartPos);
  Result := StrToInt64(ValueStr);
end;

function CalculateCost(ButtonA, ButtonB: TButton; Prize: TPrize): Int64;
var
  DistanceA, DistanceB, TotalCostA, TotalCostB: Int64;
begin
  // Calcul des distances
  DistanceA := Abs(Prize.X - ButtonA.X) + Abs(Prize.Y - ButtonA.Y);
  DistanceB := Abs(Prize.X - ButtonB.X) + Abs(Prize.Y - ButtonB.Y);

  WriteLn('Debug - DistanceA: ', DistanceA, ' DistanceB: ', DistanceB);

  // Calcul des coûts
  TotalCostA := DistanceA * ButtonA.Cost;
  TotalCostB := DistanceB * ButtonB.Cost;

  WriteLn('Debug - TotalCostA: ', TotalCostA, ' TotalCostB: ', TotalCostB);

  // Retourner le coût minimum
  Result := Min(TotalCostA, TotalCostB);
  WriteLn('Debug - Result: ', Result);
end;

var
  Lines: TStringList;
  Line: string;
  ButtonA, ButtonB: TButton;
  Prize: TPrize;
  TokensSpent, PrizeCount, Cost: Int64;
  i, machineNumber: Integer;
  FileName: string;
begin
  FileName := 'input.txt';
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    WriteLn('Total lines: ', Lines.Count);

    TokensSpent := 0;
    PrizeCount := 0;
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
      Prize.X := ParseValue(Line, 'X=') + PrizeOffset;
      Prize.Y := ParseValue(Line, 'Y=') + PrizeOffset;

      WriteLn('ButtonA: X=', ButtonA.X, ' Y=', ButtonA.Y, ' Cost=', ButtonA.Cost);
      WriteLn('ButtonB: X=', ButtonB.X, ' Y=', ButtonB.Y, ' Cost=', ButtonB.Cost);
      WriteLn('Prize: X=', Prize.X, ' Y=', Prize.Y);

      Cost := CalculateCost(ButtonA, ButtonB, Prize);
      WriteLn('Cost: ', Cost);
      if Cost <> -1 then
      begin
        TokensSpent := TokensSpent + Cost;
        PrizeCount := PrizeCount + 1;
      end;
      machineNumber := machineNumber + 1;
    end;

    WriteLn('Total tokens spent: ', TokensSpent);
    WriteLn('Total prizes won: ', PrizeCount);
  finally
    Lines.Free;
  end;
end.
