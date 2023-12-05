Imports Excel = Microsoft.Office.Interop.Excel
Imports Word = Microsoft.Office.Interop.Word
Imports System
Imports System.IO

Public Class Calculate1
    Dim myResizer As Resizer '  1. Create a form-level reference to the Resize class
    'Public Ans As String

    Private Sub Start1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim rs As New Resizer

        rs.FindAllControls(My.Forms.SIMCAT159)
        rs.ResizeAllControls(My.Forms.SIMCAT159)
        My.Forms.SIMCAT159.Show()

        rs.FindAllControls(My.Forms.GPlotting1)
        rs.ResizeAllControls(My.Forms.GPlotting1)

        rs.FindAllControls(My.Forms.Results)
        rs.ResizeAllControls(My.Forms.Results)

        My.Forms.Calculate1.Hide()

        'Dim strFolderPath As String
        'Dim Direct As String = System.Environment.CurrentDirectory
        'Const FolderName = "Background Results"
        'strFolderPath = Direct & "\" & FolderName
        'If Dir(strFolderPath, vbDirectory) = "" Then
        'MkDir(strFolderPath)
        'End If

    End Sub

    Private Sub releaseobject(ByVal obj As Object)
        Try
            System.Runtime.InteropServices.Marshal.FinalReleaseComObject(obj)
            obj = Nothing
        Catch ex As Exception
            obj = Nothing
        Finally
            GC.Collect()
        End Try
    End Sub

End Class