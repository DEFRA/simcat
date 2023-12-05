Imports Excel = Microsoft.Office.Interop.Excel

Public Class SIMCAT159
    Public Simode As Integer
    Public DatDirectory As String

    Private Sub Quit_Click(sender As Object, e As EventArgs) Handles QuitSIMCAT.Click
        My.Forms.SIMCAT159.Close()
        My.Forms.Calculate1.Close()
        My.Forms.GPlotting1.Close()
        My.Forms.Results.Close()
        Me.Close()
    End Sub

    Private Sub Selectdata_Click(sender As Object, e As EventArgs) Handles Selectdata.Click
        Dim Datafilename As String = "   "
        Dim Directory As String
        Dim Remove As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT data file"
            Directory = System.Environment.CurrentDirectory
            FileDialog.Filter = "Text files (*.dat)|*.dat|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Datafilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Datafilename)
        DatDirectory = StripFilename(Datafilename)
        Dim RemoveExists As String
        Remove = DatDirectory & "TEMP"
        RemoveExists = Dir(Remove, vbDirectory)
        If RemoveExists = "" Then
        Else
            My.Computer.FileSystem.DeleteDirectory(Remove, FileIO.DeleteDirectoryOption.DeleteAllContents)
        End If

        Simode = 0

        If SIMCATGapFilling1.Checked Then Simode = 1
        If SIMCATGapFilling2.Checked Then Simode = 2
        If SIMCATGapFilling3.Checked Then Simode = 3
        If SIMCATGapFilling4.Checked Then Simode = 4
        If SIMCATSequence5.Checked Then Simode = 5
        If SIMCATSequence6.Checked Then Simode = 6
        If SIMCATTargets7.Checked Then Simode = 7
        If SIMCATTargets8.Checked Then Simode = 8
        If SIMCATTargets9.Checked Then Simode = 9
        If Simode > 6 Then
            If UseGapFill1.Checked Then Simode = -Simode
        End If
        If Simode = 0 Then
            If UseGapFill1.Checked Then Simode = 4
        End If

        Dim RunMode As String = "SINGLE"



        If (Datafilename <> "   ") Then
            If RadioButton161.Checked Then
                FileOpen(1, "RUN161.TMP", OpenMode.Output)
                PrintLine(1, RunMode)
                PrintLine(1, Directory)
                PrintLine(1, Datafilename)
                PrintLine(1, Simode)
                FileClose(1)
                Process.Start("simcatf161.exe", 3)
            End If
            If RadioButton158.Checked Then
                FileOpen(1, "RUN158.TMP", OpenMode.Output)
                PrintLine(1, RunMode)
                PrintLine(1, Directory)
                PrintLine(1, Datafilename)
                PrintLine(1, Simode)
                FileClose(1)
                Process.Start("simcatf158.exe", 3)
            End If
            If RadioButton157.Checked Then
                FileOpen(1, "RUN157.TMP", OpenMode.Output)
                PrintLine(1, RunMode)
                PrintLine(1, Directory)
                PrintLine(1, Datafilename)
                PrintLine(1, Simode)
                FileClose(1)
                FileClose(1)
                Process.Start("simcatf157.exe", 3)
            End If
            If RadioButton148.Checked Then
                FileOpen(1, "RUNDAT.TMP", OpenMode.Output)
                PrintLine(1, RunMode)
                PrintLine(1, Directory)
                PrintLine(1, Datafilename)
                PrintLine(1, Simode)
                FileClose(1)
                Process.Start("simcatf148.exe", 3)
            End If
        End If

    End Sub

    Function StripFilename(sPathFile As String) As String
        Dim filesystem As Object
        filesystem = CreateObject("Scripting.FilesystemObject")
        StripFilename = filesystem.GetParentFolderName(sPathFile) & "\"
        Exit Function
    End Function

    Private Sub BatchRun_Click(sender As Object, e As EventArgs) Handles Batch2.Click
        Dim Simfilename As String = "   "
        Dim Direct As String = System.Environment.CurrentDirectory
        Dim RunMode As String = "-"
        Dim DatCount As Integer = 0
        Dim CountDAT As Integer = 0

        Simode = 0
        If SIMCATGapFilling1.Checked Then Simode = 1
        If SIMCATGapFilling2.Checked Then Simode = 2
        If SIMCATGapFilling3.Checked Then Simode = 3
        If SIMCATGapFilling4.Checked Then Simode = 4
        If SIMCATSequence5.Checked Then Simode = 5
        If SIMCATSequence6.Checked Then Simode = 6
        If SIMCATTargets7.Checked Then Simode = 7
        If SIMCATTargets8.Checked Then Simode = 8
        If SIMCATTargets9.Checked Then Simode = 9
        If Simode > 6 Then
            If UseGapFill1.Checked Then Simode = -Simode
        End If

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select one of your DAT files"
            FileDialog.Filter = "Text files (*.dat)|*.dat|All files (*.*)|*.*"
            DatCount = IO.Directory.GetFiles(Direct, "*.DAT").Length
            FileOpen(1, "RUNDAT.TMP", OpenMode.Output)
            PrintLine(1, RunMode)
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
            Direct = StripFilename(Simfilename)
        End Using

        Dim ik As Integer = 0
        Dim directory = Direct

        For Each filename As String In IO.Directory.GetFiles(directory, "*.DAT", IO.SearchOption.TopDirectoryOnly)
            PrintLine(1, Direct)
            PrintLine(1, filename)
            PrintLine(1, Simode)
            ik = ik + 1
            ik = ik + 1
            ik = ik + 1
        Next
        FileClose(1)

        If ik > 0 Then
            If RadioButton161.Checked Then
                If My.Computer.FileSystem.FileExists("C:\SIM160\RUN161.TMP") Then
                    My.Computer.FileSystem.DeleteFile("C:\SIM160\RUN161.TMP")
                End If
                My.Computer.FileSystem.RenameFile("C:\SIM160\RUNDAT.TMP", "RUN161.TMP")
                Batch2.Visible = False
                Process.Start("simcatf161.exe", 3)
            End If
            If RadioButton157.Checked Then
                If My.Computer.FileSystem.FileExists("C:\SIM160\RUN157.TMP") Then
                    'My.Computer.FileSystem.DeleteFile("C:\SIM160\RUN157.TMP")
                End If
                'My.Computer.FileSystem.RenameFile("C:\SIM160\RUNDAT.TMP", "RUN157.TMP")
                Process.Start("simcatf157.exe", 3)
            End If
            If RadioButton158.Checked Then
                If My.Computer.FileSystem.FileExists("C:\SIM160\RUN158.TMP") Then
                    'My.Computer.FileSystem.DeleteFile("C:\SIM160\RUN158.TMP")
                End If
                'My.Computer.FileSystem.RenameFile("C:\SIM160\RUNDAT.TMP", "RUN158.TMP")
                Process.Start("simcatf158.exe", 3)
            End If
        End If

        My.Forms.SIMCAT159.Visible = True

    End Sub
    Private Sub SIMCATPlot1_Click(sender As Object, e As EventArgs) Handles SIMCATPlot1.Click

        My.Forms.SIMCAT159.Close()
        My.Forms.GPlotting1.Visible = True
        My.Forms.GPlotting1.SelectOutputFileButton1.PerformClick()

    End Sub

    Private Sub SIMCATresults_Click(sender As Object, e As EventArgs) Handles SIMCATresults.Click

        My.Forms.SIMCAT159.Close()
        My.Forms.Results.Visible = True
        My.Forms.Results.SelectOutputFile1.PerformClick()

    End Sub

    Function EDec(Number)
        EDec = Format(Number, "###0000")
        If Number < 9999.9 Then EDec = Format(Number, "#####0")
        'If Number < 999.9 Then EDec = Format(Number, "#####0.0")
        If Number < 99.9 Then EDec = Format(Number, "#####0.0")
        If Number < 9.9 Then EDec = Format(Number, "#####0.00")
        If Number < 0.999 Then EDec = Format(Number, "#####0.00")
        If Number < 0.0999 Then EDec = Format(Number, "###0.000")
        If Number < 0.00999 Then EDec = Format(Number, "##0.0000")
    End Function
    Function EDec6(Number)
        EDec6 = Format(Number, "##########0")
        If Number < 999.9 Then EDec6 = Format(Number, "########0.0")
        If Number < 99.9 Then EDec6 = Format(Number, "#######0.00")
        If Number < 9.9 Then EDec6 = Format(Number, "######0.000")
        If Number < 0.999 Then EDec6 = Format(Number, "######0.000")
        If Number < 0.0999 Then EDec6 = Format(Number, "#####0.0000")
        If Number < 0.00999 Then EDec6 = Format(Number, "####0.00000")
    End Function
    Function EDec4(Number)
        EDec4 = Format(Number, "#####0.0000")
        If Number < 0.0 Then EDec4 = Format(Number, "####0.0000")
    End Function
    Function EDec1(Number)
        EDec1 = Format(Number, "#####0.0")
    End Function
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles RunClassic1.Click
        Process.Start("Simcatclassic.exe", 3) ' classic front end
    End Sub
    Private Sub EditData1_Click(sender As Object, e As EventArgs) Handles EditData1.Click
        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Edit a SIMCAT data file"
            Directory = System.Environment.CurrentDirectory
            FileDialog.Filter = "Text files (*.dat)|*.dat|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub Button1_Click_2(sender As Object, e As EventArgs) Handles ViewOUT1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT large output file"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.out)|*.out|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub ViewSCN11_Click(sender As Object, e As EventArgs) Handles ViewSCN1.Click
        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file copy of the screen"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.scn)|*.SCN|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub
    Private Sub ViewSUM1_Click(sender As Object, e As EventArgs) Handles ViewCAL1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for calibration"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.cal)|*.CAL|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub Button1_Click_1(sender As Object, e As EventArgs) Handles ViewERR1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file listing errors"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.err)|*.ERR|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub ViewEFF1_Click(sender As Object, e As EventArgs) Handles ViewEFF1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for discharges"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.eff)|*.EFF|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub ViewTGT1_Click(sender As Object, e As EventArgs) Handles ViewTGT1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for river targets"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.tgt)|*.TGT|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub ViewFLO1_Click(sender As Object, e As EventArgs) Handles ViewFLO1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for river flows"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.flo)|*.FLO|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub ViewLOD1_Click(sender As Object, e As EventArgs) Handles ViewLOD1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for loads"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.lod)|*.LOD|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub ViewINP1_Click(sender As Object, e As EventArgs) Handles ViewINP1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for lists of input data"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.inp)|*.INP|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub Button1_Click_3(sender As Object, e As EventArgs) Handles ViewADL1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for discharge apportionment"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.adl)|*.ADL|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub ViewACL1_Click(sender As Object, e As EventArgs) Handles ViewACL1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for catchment apportionment"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.acl)|*.ACL|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub ViweWFD1_Click(sender As Object, e As EventArgs) Handles ViewWFD1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for catchment apportionment"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.wfd)|*.WFD|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub SIMCAT158_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        FormBorderStyle = FormBorderStyle.Sizable
        MaximizeBox = False
        MinimizeBox = True
    End Sub
    'Disables close
    'Protected Overrides ReadOnly Property CreateParams() As CreateParams
    'Get
    'Dim cp As CreateParams = MyBase.CreateParams
    'Const CS_NOCLOSE As Integer = &H200
    '       cp.ClassStyle = cp.ClassStyle Or CS_NOCLOSE
    'Return cp
    'End Get
    'End Property

    'Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click

    'End Sub

    Private Sub Button1_Click_4(sender As Object, e As EventArgs) Handles ViewADC1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for discharge apportionment"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.adc)|*.ADC|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2WOK.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for loads"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.wok)|*.WOK|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles RunAgain3.Click
        'Dim Remove As String

        'Dim RemoveExists As String
        'Remove = DatDirectory & "TEMP"
        'RemoveExists = Dir(Remove, vbDirectory)
        'If RemoveExists = "" Then
        'Else
        'My.Computer.FileSystem.DeleteDirectory(Remove, FileIO.DeleteDirectoryOption.DeleteAllContents)
        'End If

        RunAgain3.BackColor = Color.Lavender
        RunAgain3.Text = "  "
        'RunAgain3.Visible = False

        If RadioButton161.Checked Then
            RunAgain3.Text = "161"
            Process.Start("simcatf161.exe", 3)
        End If
        If RadioButton158.Checked Then
            Process.Start("simcatf158.exe", 3)
        End If
        If RadioButton157.Checked Then
            Process.Start("simcatf157.exe", 3)
        End If
        If RadioButton148.Checked Then
            Process.Start("simcatf148.exe", 3)
        End If

        'My.Forms.SIMCAT160.Visible = True

        Call Delay(12)
        RunAgain3.Visible = True
        RunAgain3.BackColor = Color.Lavender
        RunAgain3.Text = "run last DAT again"
        RunAgain3.Visible = True


        'My.Forms.SIMCAT160.Visible = True

    End Sub

    Function FileExists(FilePath As String) As Boolean
        Dim TestStr As String
        TestStr = ""
        On Error Resume Next
        TestStr = Dir(FilePath)
        On Error GoTo 0
        If TestStr = "" Then
            FileExists = False
        Else
            FileExists = True
        End If
    End Function

    Sub Delay(ByVal dblSecs As Double)

        Const OneSec As Double = 1.0# / (1440.0# * 60.0#)
        Dim dblWaitTil As Date
        Now.AddSeconds(OneSec)
        dblWaitTil = Now.AddSeconds(OneSec).AddSeconds(dblSecs)
        Do Until Now > dblWaitTil
            Application.DoEvents() ' Allow windows messages to be processed
        Loop

    End Sub

    Private Sub Button3_Click_1(sender As Object, e As EventArgs) Handles Button3WAK.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for loads"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.wak)|*.WAK|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If
    End Sub

    Private Sub RadioButton157_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton157.CheckedChanged
        If RadioButton157.Checked Then
            RadioButton148.Checked = False
            RadioButton158.Checked = False
            RadioButton161.Checked = False
            ViewADC1.Visible = False
            ViewADL1.Visible = False
            ViewACL1.Visible = False
            ViewGAP1.Visible = False
            ViewCAL1.Visible = False
            ViewEFF1.Visible = True
            ViewERR1.Visible = True
            ViewFLO1.Visible = True
            ViewINP1.Visible = True
            ViewLOD1.Visible = True
            ViewMON1.Visible = True
            ViewOUT1.Visible = True
            ViewSCN1.Visible = True
            ViewTGT1.Visible = True
            ViewWFD1.Visible = True
        End If
    End Sub

    Private Sub RadioButton158_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton158.CheckedChanged
        If RadioButton158.Checked Then
            RadioButton148.Checked = False
            RadioButton157.Checked = False
            RadioButton161.Checked = False
            ViewADC1.Visible = False
            ViewACL1.Visible = True
            ViewADL1.Visible = True
            ViewCAL1.Visible = True
            ViewEFF1.Visible = True
            ViewERR1.Visible = True
            ViewGAP1.Visible = True
            ViewFLO1.Visible = True
            ViewINP1.Visible = True
            ViewLOD1.Visible = True
            ViewMON1.Visible = True
            ViewOUT1.Visible = True
            ViewSCN1.Visible = True
            ViewTGT1.Visible = True
            ViewWFD1.Visible = True
        End If
    End Sub

    Private Sub RadioButton161_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton161.CheckedChanged
        If RadioButton161.Checked Then
            RadioButton148.Checked = False
            RadioButton157.Checked = False
            RadioButton158.Checked = False
            ViewACL1.Visible = True
            ViewADC1.Visible = True
            ViewADL1.Visible = True
            ViewCAL1.Visible = True
            ViewEFF1.Visible = True
            ViewERR1.Visible = True
            ViewFLO1.Visible = True
            ViewGAP1.Visible = True
            ViewINP1.Visible = True
            ViewLOD1.Visible = True
            ViewMON1.Visible = True
            ViewOUT1.Visible = True
            ViewSCN1.Visible = True
            ViewTGT1.Visible = True
            ViewWFD1.Visible = True
        End If
    End Sub


    Private Sub ViewMON1_Click(sender As Object, e As EventArgs) Handles ViewMON1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for loads"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.lod)|*.MON|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub GAPFill2_Click(sender As Object, e As EventArgs) Handles GAPfill2.Click

        If SIMCATGapFilling1.Visible = False Then
            SIMCATGapFilling1.Visible = True
            SIMCATGapFilling2.Visible = True
            SIMCATGapFilling3.Visible = True
            SIMCATGapFilling4.Visible = True
            SIMCATSequence5.Visible = True
            SIMCATSequence6.Visible = True
            UseGapFill1.Visible = True
        Else
            SIMCATGapFilling1.Visible = False
            SIMCATGapFilling1.Checked = False
            SIMCATGapFilling2.Visible = False
            SIMCATGapFilling2.Checked = False
            SIMCATGapFilling3.Visible = False
            SIMCATGapFilling3.Checked = False
            SIMCATGapFilling4.Visible = False
            SIMCATGapFilling4.Checked = False
            SIMCATSequence5.Visible = False
            SIMCATSequence6.Visible = False
            SIMCATSequence5.Checked = False
            SIMCATSequence6.Checked = False
            UseGapFill1.Visible = False
            UseGapFill1.Checked = False
            'UseGapFill2.Checked = False
            'UseGapFill3.Checked = False
            'UseGapFill4.Checked = False
        End If

    End Sub

    Private Sub NatGap2_Click(sender As Object, e As EventArgs) Handles ViewGAP1.Click
        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for loads"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.lod)|*.GAP|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub SIMCATGapFilling3_CheckedChanged(sender As Object, e As EventArgs) Handles SIMCATGapFilling3.CheckedChanged
        If SIMCATGapFilling3.Checked Then
            ViewEFF1.Visible = False
            ViewTGT1.Visible = False
            ViewWFD1.Visible = False
            ViewADL1.Visible = False
            ViewADC1.Visible = False
            ViewACL1.Visible = False
            ViewFLO1.Visible = False
            ViewLOD1.Visible = False
            ViewMON1.Visible = False
        Else
            ViewEFF1.Visible = True
            ViewTGT1.Visible = True
            ViewWFD1.Visible = True
            ViewADL1.Visible = True
            ViewADC1.Visible = True
            ViewACL1.Visible = True
            ViewFLO1.Visible = True
            ViewLOD1.Visible = True
            ViewMON1.Visible = True
        End If
    End Sub

    Private Sub SIMCATSequence5_CheckedChanged(sender As Object, e As EventArgs) Handles SIMCATSequence5.CheckedChanged
        If SIMCATSequence5.Checked Then
            ViewEFF1.Visible = False
            ViewTGT1.Visible = False
            ViewWFD1.Visible = False
            ViewADL1.Visible = False
            ViewADC1.Visible = False
            ViewACL1.Visible = False
            ViewFLO1.Visible = False
            ViewLOD1.Visible = False
            ViewMON1.Visible = False
        Else
            ViewEFF1.Visible = True
            ViewTGT1.Visible = True
            ViewWFD1.Visible = True
            ViewADL1.Visible = True
            ViewADC1.Visible = True
            ViewACL1.Visible = True
            ViewFLO1.Visible = True
            ViewLOD1.Visible = True
            ViewMON1.Visible = True
        End If
    End Sub

    Private Sub SIMCATGapFilling2_CheckedChanged(sender As Object, e As EventArgs) Handles SIMCATGapFilling2.CheckedChanged
        If SIMCATGapFilling1.Checked Then
            ViewCAL1.Visible = True
            ViewEFF1.Visible = True
            ViewTGT1.Visible = True
            ViewWFD1.Visible = True
            ViewADL1.Visible = True
            ViewADC1.Visible = True
            ViewACL1.Visible = True
            ViewFLO1.Visible = True
            ViewLOD1.Visible = True
            ViewMON1.Visible = True
        Else
            ViewCAL1.Visible = True
            ViewEFF1.Visible = True
            ViewTGT1.Visible = True
            ViewWFD1.Visible = True
            ViewADL1.Visible = True
            ViewADC1.Visible = True
            ViewACL1.Visible = True
            ViewFLO1.Visible = True
            ViewLOD1.Visible = True
            ViewMON1.Visible = True
        End If
    End Sub

    Private Sub SIMCATGapFilling1_CheckedChanged(sender As Object, e As EventArgs) Handles SIMCATGapFilling1.CheckedChanged
        If SIMCATGapFilling1.Checked Then
            ViewCAL1.Visible = False
            ViewEFF1.Visible = False
            ViewTGT1.Visible = False
            ViewWFD1.Visible = False
            ViewADL1.Visible = False
            ViewADC1.Visible = False
            ViewACL1.Visible = False
            ViewFLO1.Visible = False
            ViewLOD1.Visible = False
            ViewMON1.Visible = False
        Else
            ViewCAL1.Visible = True
            ViewEFF1.Visible = True
            ViewTGT1.Visible = True
            ViewWFD1.Visible = True
            ViewADL1.Visible = True
            ViewADC1.Visible = True
            ViewACL1.Visible = True
            ViewFLO1.Visible = True
            ViewLOD1.Visible = True
            ViewMON1.Visible = True
        End If
    End Sub

    Private Sub Button1_Click_5(sender As Object, e As EventArgs) Handles Button5CSV.Click
        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT CSV file"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.wak)|*.CSV|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Sub FSODeleteFolder()

        'Dim Directory As String
        'Dim FSO As New FileSystemObject
        'Directory = System.Environment.CurrentDirectory + "/TEMP/"
        'Set FSO = CreateObject("Scripting.FileSystemObject")
        'FSO.DeleteFolder(Directory, False)

    End Sub

    Private Sub Button6FOLDER_Click(sender As Object, e As EventArgs) Handles Button6FOLDER.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT CSV file"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            'FileDialog.Filter = "Text files (*.wak)|*.CSV|All files (*.*)|*.*"
            FileDialog.Filter = "Text files (*.*)|*.*|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub Button1_Click_6(sender As Object, e As EventArgs)

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your manual ..."
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.docx|*.DOCX|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If
    End Sub

    Private Sub Button1_Click_7(sender As Object, e As EventArgs) Handles ManualButton1.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your manual ..."
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.docx|*.DOCX|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub Button1_Click_8(sender As Object, e As EventArgs) Handles Button1WUK.Click

        Dim Simfilename As String = "   "
        Dim Directory As String

        Using FileDialog As New OpenFileDialog
            FileDialog.Title = "Select your SIMCAT output file for loads"
            Directory = System.Environment.CurrentDirectory + "/Results/"
            FileDialog.Filter = "Text files (*.wuk)|*.WUK|All files (*.*)|*.*"
            If FileDialog.ShowDialog() = DialogResult.OK Then
                Simfilename = FileDialog.FileName
            End If
        End Using

        Directory = StripFilename(Simfilename)

        If (Simfilename <> "   ") Then
            Process.Start(Simfilename)
        End If

    End Sub

    Private Sub UseGapFill1_CheckedChanged(sender As Object, e As EventArgs) Handles UseGapFill1.CheckedChanged
        If UseGapFill1.Checked Then
            SIMCATGapFilling1.Checked = False
            SIMCATGapFilling2.Checked = False
            SIMCATGapFilling3.Checked = False
            SIMCATGapFilling4.Checked = False
            SIMCATSequence5.Checked = False
            SIMCATSequence6.Checked = False
            SIMCATGapFilling1.Visible = False
            SIMCATGapFilling2.Visible = False
            SIMCATGapFilling3.Visible = False
            SIMCATGapFilling4.Visible = False
            SIMCATSequence5.Visible = False
            SIMCATSequence6.Visible = False
        Else
            SIMCATGapFilling1.Checked = False
            SIMCATGapFilling2.Checked = False
            SIMCATGapFilling3.Checked = False
            SIMCATGapFilling4.Checked = False
            SIMCATSequence5.Checked = False
            SIMCATSequence6.Checked = False
            SIMCATGapFilling1.Visible = True
            SIMCATGapFilling2.Visible = True
            SIMCATGapFilling3.Visible = True
            SIMCATGapFilling4.Visible = True
            SIMCATSequence5.Visible = True
            SIMCATSequence6.Visible = True
        End If

    End Sub

    Private Sub SIMCATTargets8_CheckedChanged(sender As Object, e As EventArgs) Handles SIMCATTargets8.CheckedChanged
        If SIMCATTargets8.Checked Then
            SIMCATGapFilling1.Checked = False
            SIMCATGapFilling2.Checked = False
            SIMCATGapFilling3.Checked = False
            SIMCATGapFilling4.Checked = False
            SIMCATSequence5.Checked = False
            SIMCATSequence6.Checked = False
            SIMCATGapFilling1.Visible = False
            SIMCATGapFilling2.Visible = False
            SIMCATGapFilling3.Visible = False
            SIMCATGapFilling4.Visible = False
            SIMCATSequence5.Visible = False
            SIMCATSequence6.Visible = False
        End If
    End Sub

    Private Sub SIMCATTargets7_CheckedChanged(sender As Object, e As EventArgs) Handles SIMCATTargets7.CheckedChanged
        If SIMCATTargets7.Checked Then
            SIMCATGapFilling1.Checked = False
            SIMCATGapFilling2.Checked = False
            SIMCATGapFilling3.Checked = False
            SIMCATGapFilling4.Checked = False
            SIMCATSequence5.Checked = False
            SIMCATSequence6.Checked = False
            SIMCATGapFilling1.Visible = False
            SIMCATGapFilling2.Visible = False
            SIMCATGapFilling3.Visible = False
            SIMCATGapFilling4.Visible = False
            SIMCATSequence5.Visible = False
            SIMCATSequence6.Visible = False
        End If
    End Sub

    Private Sub SIMCATTargets9_CheckedChanged(sender As Object, e As EventArgs) Handles SIMCATTargets9.CheckedChanged
        If SIMCATTargets9.Checked Then
            SIMCATGapFilling1.Checked = False
            SIMCATGapFilling2.Checked = False
            SIMCATGapFilling3.Checked = False
            SIMCATGapFilling4.Checked = False
            SIMCATSequence5.Checked = False
            SIMCATSequence6.Checked = False
            SIMCATGapFilling1.Visible = False
            SIMCATGapFilling2.Visible = False
            SIMCATGapFilling3.Visible = False
            SIMCATGapFilling4.Visible = False
            SIMCATSequence5.Visible = False
            SIMCATSequence6.Visible = False
        End If
    End Sub

    Private Sub Button1_Click_9(sender As Object, e As EventArgs) Handles Classic157.Click

        Dim Directory As String

        Directory = System.Environment.CurrentDirectory
        Process.Start(Directory + "/Simcat 157 Master/Simcat157.exe", 3)

    End Sub


    Private Sub RadioButton1_CheckedChanged_1(sender As Object, e As EventArgs)

        Dim Directory As String
        Directory = System.Environment.CurrentDirectory
        Process.Start(Directory + "/Simcat 160 Master/Simcat160.exe", 3)

    End Sub


    Private Sub RadioButton2_CheckedChanged_2(sender As Object, e As EventArgs)

        Dim Directory As String

        Directory = System.Environment.CurrentDirectory
        Process.Start(Directory + "/Simcat 160 Master/Simcat160.exe", 3)

    End Sub

    Private Sub Button1_Click_11(sender As Object, e As EventArgs) Handles Button1.Click

        Dim Directory As String

        Directory = System.Environment.CurrentDirectory
        Process.Start(Directory + "/Simcat 148 Master/Simcat148.exe", 3)

    End Sub
End Class