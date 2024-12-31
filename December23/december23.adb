with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Containers.Vectors; use Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers.Indefinite_Hashed_Maps;

procedure December23 is
   type String_Vector is array (Positive range <>) of Unbounded_String;
   package String_Vector_Pkg is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Unbounded_String);
   use String_Vector_Pkg;

   package String_Hash_Map is new Ada.Containers.Indefinite_Hashed_Maps (Key_Type => Unbounded_String, Element_Type => String_Vector);
   use String_Hash_Map;

   Connections : String_Hash_Map.Map;

   procedure Add_Connection (A, B : in Unbounded_String) is
   begin
      if not Connections.Contains (A) then
         Connections.Insert (A, (1 => B));
      else
         Connections (A).Append (B);
      end if;

      if not Connections.Contains (B) then
         Connections.Insert (B, (1 => A));
      else
         Connections (B).Append (A);
      end if;
   end Add_Connection;

   function Is_Connected (A, B : in Unbounded_String) return Boolean is
   begin
      if Connections.Contains (A) then
         for I in Connections (A)'Range loop
            if Connections (A) (I) = B then
               return True;
            end if;
         end loop;
      end if;
      return False;
   end Is_Connected;

   procedure Read_Connections (File : in File_Type) is
      Line : Unbounded_String;
      A, B : Unbounded_String;
      Dash_Pos : Positive;
   begin
      while not End_Of_File (File) loop
         Get_Line (File, Line);
         Dash_Pos := Index (Line, To_Unbounded_String ("-"));
         A := Slice (Line, 1, Dash_Pos - 1);
         B := Slice (Line, Dash_Pos + 1, Length (Line));
         Add_Connection (A, B);
      end loop;
   end Read_Connections;

   function Find_Triplets return Natural is
      Count : Natural := 0;
   begin
      for A in Connections.Iterate loop
         for B in Connections (Connections.Element (A))'Range loop
            for C in Connections (Connections.Element (A))'Range loop
               if B /= C and then Is_Connected (Connections.Element (A), Connections (Connections.Element (A)) (B)) and then Is_Connected (Connections (Connections.Element (A)) (B), Connections (Connections.Element (A)) (C)) then
                  if Connections.Element (A) (1) = 't' or else Connections (Connections.Element (A)) (B) (1) = 't' or else Connections (Connections.Element (A)) (C) (1) = 't' then
                     Count := Count + 1;
                  end if;
               end if;
            end loop;
         end loop;
      end loop;
      return Count;
   end Find_Triplets;

begin
   -- Read connections from input file
   declare
      File : File_Type;
   begin
      Open (File, In_File, "input.txt");
      Read_Connections (File);
      Close (File);
   end;

   -- Find and count triplets
   Put_Line ("Number of triplets containing at least one computer with a name starting with 't': " & Natural'Image (Find_Triplets));
end December23;