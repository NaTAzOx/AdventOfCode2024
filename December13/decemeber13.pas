{$mode objfpc}
{$H+}                 // Active le mode de gestion des chaînes avec mémoire dynamique
{$I-,Q-,R-}           // Désactive certaines erreurs d'entrée/sortie et de runtime
{$APPTYPE CONSOLE}    // Spécifie que le programme est une application console
{$IFDEF FPC}
    {$CODEPAGE UTF-8} // Définit l'encodage en UTF-8 pour Free Pascal Compiler
{$ENDIF}

program ClawMachineSolver;

uses
  SysUtils, Classes, Math;

type
  TButton = record
    X, Y, Cost: Integer;
  end;

  TPrize = record
    X, Y: Integer;
  end;

function ParseValue(const Text, Prefix: string): Integer;
var
  StartPos: Integer;
  ValueStr: string;
begin
  StartPos := Pos(Prefix, Text);
  if StartPos = 0 then
    raise Exception.CreateFmt('Prefix "%s" not found in "%s"', [Prefix, Text]);

  ValueStr := Copy(Text, StartPos + Length(Prefix), MaxInt);
  ValueStr := Trim(ValueStr);

  if ValueStr = '' then
    raise Exception.CreateFmt('No value found after prefix "%s" in "%s"', [Prefix, Text]);

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

function SolveClawMachine(const FileName: string): Integer;
var
  Lines: TStringList;
  i, PrizeCount, TokensSpent, MaxPress: Integer;
  ButtonA, ButtonB: TButton;
  Prize: TPrize;
  Line: string;
  Cost: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    WriteLn('Total lines: ', Lines.Count);
    if (Lines.Count mod 3) <> 0 then
      raise Exception.Create('Invalid input file format: lines must be a multiple of 3.');

    TokensSpent := 0;
    PrizeCount := 0;
    MaxPress := 100;

    for i := 0 to (Lines.Count div 3) - 1 do
    begin
      // Lire les configurations des boutons et des prix
      Line := Lines[i * 3];
      ButtonA.X := ParseValue(Line, 'X+');
      ButtonA.Y := ParseValue(Line, 'Y+');
      ButtonA.Cost := 3; // Coût par défaut pour A

      Line := Lines[i * 3 + 1];
      ButtonB.X := ParseValue(Line, 'X+');
      ButtonB.Y := ParseValue(Line, 'Y+');
      ButtonB.Cost := 1; // Coût par défaut pour B

      Line := Lines[i * 3 + 2];
      Prize.X := ParseValue(Line, 'X=');
      Prize.Y := ParseValue(Line, 'Y=');

      Cost := CalculateCost(ButtonA, ButtonB, Prize, MaxPress);
      if Cost <> -1 then
        TokensSpent := TokensSpent + Cost;
    end;

    Result := TokensSpent;
  finally
    Lines.Free;
  end;
end;

begin
  try
    WriteLn('Total tokens spent:', SolveClawMachine('input.txt'));
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
