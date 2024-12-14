program ClawMachine;

uses
    SysUtils, Classes;

type
    TButton = record
        X, Y: Integer;
        Cost: Integer;
    end;

    TPrize = record
        X, Y: Integer;
    end;

var
    Buttons: array[1..2] of TButton;
    Prizes: array of TPrize;
    Lines: TStringList;
    Line: string;
    PrizeCount, i, j, MinTokens, Tokens: Integer;

function ExtractWord(N: Integer; const S: string; const WordDelims: TSysCharSet): string;
var
    I, J, Count: Integer;
begin
    I := 1;
    Count := 0;
    while (I <= Length(S)) and (Count < N) do
    begin
        while (I <= Length(S)) and (S[I] in WordDelims) do
            Inc(I);
        if I <= Length(S) then
            Inc(Count);
        if Count < N then
            while (I <= Length(S)) and not (S[I] in WordDelims) do
                Inc(I);
    end;
    J := I;
    while (J <= Length(S)) and not (S[J] in WordDelims) do
        Inc(J);
    ExtractWord := Copy(S, I, J - I);
end;

procedure ReadInputFile(const FileName: string);
begin
    Lines := TStringList.Create;
    try
        Lines.LoadFromFile(FileName);
        PrizeCount := StrToInt(Lines[0]);
        SetLength(Prizes, PrizeCount);

        for i := 0 to PrizeCount - 1 do
        begin
            Line := Lines[i + 1];
            Prizes[i].X := StrToInt(ExtractWord(1, Line, [' ']));
            Prizes[i].Y := StrToInt(ExtractWord(2, Line, [' ']));
        end;
    finally
        Lines.Free;
    end;
end;

begin
    // Initialize buttons
    Buttons[1].X := 1; Buttons[1].Y := 1; Buttons[1].Cost := 2;
    Buttons[2].X := 2; Buttons[2].Y := 2; Buttons[2].Cost := 3;

    // Read input file
    ReadInputFile('input.txt');

    // Process prizes
    MinTokens := MaxInt;
    for i := 0 to PrizeCount - 1 do
    begin
        for j := 1 to 2 do
        begin
            Tokens := Abs(Prizes[i].X - Buttons[j].X) + Abs(Prizes[i].Y - Buttons[j].Y) + Buttons[j].Cost;
            if Tokens < MinTokens then
                MinTokens := Tokens;
        end;
    end;

    WriteLn('Minimum tokens needed: ', MinTokens);
end.
