Imports System.IO

Module December9

    Function ReadFile(path As String) As String
        Dim line As String = ""

        If File.Exists(path) Then
            Using sr As StreamReader = File.OpenText(path)
                line = sr.ReadLine()
            End Using
        End If

        Return line
    End Function

    Sub VisualizeStorage()
        Dim path As String
        Dim line As String

        ' Prompt the user to select a file
        path = InputBox("Enter the path to the text file:", "Select File")
        If path = "" Then Exit Sub

        ' Read the file
        line = ReadFile(path)
        If line = "" Then
            MsgBox("No line read from the file.")
            Exit Sub
        End If

        ' Process the single line
        ProcessSingleLine(line)
    End Sub

    Sub ProcessSingleLine(line As String)
        Dim parsedArray() As Integer

        ' Parse the input line into pairs of numbers
        parsedArray = ParseInput(line)

        ' Vérification si parsedArray a bien été initialisé et contient des éléments
        If parsedArray Is Nothing OrElse parsedArray.Length = 0 Then
            MsgBox("Erreur lors de l'analyse de la ligne du fichier.")
            Exit Sub
        End If

        ' Compact the disk and calculate the checksum
        Dim checksum As Long
        checksum = CompactAndCalculateChecksum(parsedArray)
        MsgBox("Checksum: " & checksum)
    End Sub

    Function ParseInput(line As String) As Integer()
        Dim elements() As Char
        Dim i As Integer
        Dim parsedArray() As Integer

        ' Split the line into individual characters
        elements = line.ToCharArray()

        ' Initialize the parsed array
        ReDim parsedArray(elements.Length - 1)

        ' Convert the characters to numbers
        For i = 0 To elements.Length - 1
            parsedArray(i) = CInt(elements(i).ToString())
        Next i

        Return parsedArray
    End Function

    Function CompactAndCalculateChecksum(arr() As Integer) As Long
        Dim i As Integer
        Dim j As Integer
        Dim checksum As Long
        Dim disk() As String
        Dim fileID As Integer
        Dim fileLength As Integer
        Dim freeSpaceLength As Integer
        Dim position As Integer

        ' Initialize the disk array
        ReDim disk(UBound(arr))

        ' Fill the disk array with file IDs and free spaces
        fileID = 0
        For i = 0 To UBound(arr) Step 2
            If i + 1 > UBound(arr) Then
                Exit For
            End If

            fileLength = arr(i)
            freeSpaceLength = arr(i + 1)

            ' Fill the file blocks
            For j = 1 To fileLength
                If position > UBound(disk) Then
                    Exit For
                End If
                disk(position) = CStr(fileID)
                position = position + 1
            Next j

            ' Fill the free space blocks
            For j = 1 To freeSpaceLength
                If position > UBound(disk) Then
                    Exit For
                End If
                disk(position) = "."
                position = position + 1
            Next j

            fileID = fileID + 1
        Next i

        ' Compact the disk
        Dim compactedDisk() As String
        ReDim compactedDisk(UBound(disk))
        Dim compactedPosition As Integer = 0

        For i = 0 To UBound(disk)
            If disk(i) <> "." Then
                compactedDisk(compactedPosition) = disk(i)
                compactedPosition += 1
            End If
        Next i

        ' Calculate the checksum
        checksum = 0
        For i = 0 To compactedPosition - 1
            If compactedDisk(i) <> "" Then
                checksum = checksum + i * CInt(compactedDisk(i))
            End If
        Next i

        Return checksum
    End Function

    Sub Main()
        VisualizeStorage()
    End Sub

End Module