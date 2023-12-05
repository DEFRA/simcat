Imports Excel = Microsoft.Office.Interop.Excel
Imports System
Imports System.IO
Imports System.Data.OleDb

Public Class GPlotting1
    Public numreaches, irqs As Integer
    Public startreechcode, lastreechcode, endreechcode As Integer
    Dim next_reach(9999), next_reach_master(9999) As Integer
    Public reechnumbercode(9999) As Integer
    Public LastfilenamePLOT As String
    Public Property CheckOnClick As Boolean

    Private Sub Quit_Click(sender As Object, e As EventArgs) Handles GPLOTQuit1.Click
        My.Forms.SIMCAT159.Close()
        My.Forms.Calculate1.Close()
        My.Forms.GPlotting1.Close()
        Me.Close()
    End Sub

    Private Sub BacktoSIMCAT_Click(sender As Object, e As EventArgs) Handles BacktoSIMCAT1.Click
        My.Forms.GPlotting1.Close()
        My.Forms.SIMCAT159.Visible = True
    End Sub
    Public Sub Produce_a_graph1(RunSequence)

        Dim xlApp As Excel.Application
        Dim xlWorkBook As Excel.Workbook
        Dim xlWorkSheet As Excel.Worksheet
        Dim range As Excel.Range

        Dim Directory As String
        Dim Obj As Object

        Dim plot_point_end, plot_point_start As Integer
        Dim namey1, namey2, namey3, namey4, namey5, namey6 As String
        Dim namey1d, namey2d, namey3d As String
        Dim featurename, featurenamelast, reachname As String
        Dim inamey1, inamey1d As Single
        Dim pnamey1, pnamey2, pnamey3 As Single
        Dim inamey2, inamey2d As Single
        Dim inamey3, inamey3d As Single
        Dim mrqs, nrqs, guide, starting As Integer
        Dim numfeatures, usedfeatures As Integer
        Dim reachnumber, rstart, rnext, kreechcode As Integer
        Dim reachlength, plotlength, lastlength, featlength As Single
        Dim feattype, featlast As Integer
        Dim ipstart, iplast, ip, cell1, ic2 As Integer
        Dim dequal03 As Integer
        Dim frakshun As Single
        Dim poynt, poyntprevious As Integer
        Dim ExcelfilenamePLOT As String

        Me.GraphDisplay.Series(0).Points.Clear() : Me.GraphDisplay.Series(1).Points.Clear() : Me.GraphDisplay.Series(2).Points.Clear()
        Me.GraphDisplay.Series(3).Points.Clear() : Me.GraphDisplay.Series(4).Points.Clear() : Me.GraphDisplay.Series(5).Points.Clear()
        Me.GraphDisplay.Series(6).Points.Clear() : Me.GraphDisplay.Series(7).Points.Clear()

        If RunSequence = 0 Then

            LastfilenamePLOT = "========" : ExcelfilenamePLOT = "++++++++"

            Using FileDialog As New OpenFileDialog
                Heading1.Visible = False
                Determinand1.Visible = False
                UnitsLabel1.Visible = False
                LoadsUnitsLabel1.Visible = False
                Load1.Visible = False

                featurecounter.Visible = False : reachcounter.Visible = False : runninglength.Visible = False

                Me.GraphDisplay.ChartAreas(0).AxisY.Title = "concentration"
                Me.GraphDisplay.ChartAreas(0).AxisX.Title = "sections of 100 metres)"

                Me.GraphDisplay.ChartAreas(0).AxisX.Interval = 1
                Me.GraphDisplay.ChartAreas(0).AxisX.LineWidth = 2
                Me.GraphDisplay.ChartAreas(0).AxisY.LineWidth = 2
                Me.GraphDisplay.ChartAreas(0).AxisX.MajorGrid.LineColor = Color.LightGray
                Me.GraphDisplay.ChartAreas(0).AxisY.MajorGrid.LineColor = Color.LightGray

                FileDialog.Title = "Select your data file (and therefore your determinand)"
                'FileDialog.Filter = "Micosoft Excel|*+G*|All Files|*.*"
                FileDialog.Filter = "... (*.CSV)|*+G*|All files (*.*)|*.*"
                If FileDialog.ShowDialog() = DialogResult.OK Then ' this displays the box

                    ChooseDeterminand.Visible = False

                    xlApp = New Excel.Application

                    Directory = System.Environment.CurrentDirectory
                    ExcelfilenamePLOT = FileDialog.FileName
                    LastfilenamePLOT = FileDialog.FileName
                    xlWorkBook = xlApp.Workbooks.Open(ExcelfilenamePLOT)
                    xlWorkSheet = xlWorkBook.Worksheets(1)
                    range = xlWorkSheet.UsedRange

                    'FileOpen(5, "Details.TMP", OpenMode.Output)
                    'PrintLine(5, "======================================================================================")

                    Obj = CType(range.Cells(2, 4), Excel.Range) : mrqs = Obj.value ' the type of standard of the determinand
                    Obj = CType(range.Cells(2, 5), Excel.Range) : numreaches = Obj.value ' the number of reaches

                    If mrqs = 1 Then
                        Me.GraphDisplay.ChartAreas(0).AxisY.Title = "mean concentration"
                        Me.Statistic2.Text = "90-percentile"
                        Me.Statistic3.Text = "95-percentile"
                        Statistic1.Checked = True
                    End If
                    If mrqs = 3 Then
                        Statistic2.Checked = True
                        Me.GraphDisplay.ChartAreas(0).AxisY.Title = "90-percentile concentration"
                        Me.Statistic3.Text = "95-percentile"
                    End If
                    If mrqs = 2 Then
                        Statistic3.Checked = True
                        Me.GraphDisplay.ChartAreas(0).AxisY.Title = "95-percentile concentration"
                        Me.Statistic2.Text = "90-percentile"
                    End If
                    If mrqs = 5 Then
                        Statistic2.Checked = True
                        Me.GraphDisplay.ChartAreas(0).AxisY.Title = "10-percentile concentration"
                        Me.Statistic2.Text = "10-percentile"
                    End If
                    If mrqs = 4 Then
                        Statistic3.Checked = True
                        Me.GraphDisplay.ChartAreas(0).AxisY.Title = "5-percentile concentration"
                        Me.Statistic3.Text = "5-percentile"
                    End If
                    If mrqs = 6 Then
                        Statistic4.Checked = True
                        Me.Statistic2.Text = "90-percentile"
                        Me.Statistic3.Text = "95-percentile"
                    End If
                    nrqs = mrqs : irqs = mrqs ' store a master copy

                    startreechcode = 0 : lastreechcode = 0
                    If startreechcode <= 0 Then ' -------------------------------------------------------------------------------------------------
                        ReachCheckListBox1.CheckOnClick = True
                        ReachCheckListBox1.Items.Clear()

                        For ip = 3 To numreaches + 2 ' read the data on all the reaches - set up the sequence of reaches ===========
                            Obj = CType(range.Cells(ip, 3), Excel.Range) : kreechcode = Obj.value ' .......... the reach code number
                            reechnumbercode(ip - 2) = kreechcode
                            If ip = 3 Then startreechcode = kreechcode : lastreechcode = kreechcode
                            Obj = CType(range.Cells(ip, 4), Excel.Range) 'the reach code number of the next reach
                            next_reach(kreechcode) = Obj.value 'the reach code number of the next reach
                            next_reach_master(kreechcode) = Obj.value 'the reach code number of the next reach
                            Obj = CType(range.Cells(ip, 2), Excel.Range) : reachname = Obj.text ' the name of the reach

                            If ip = 3 Then
                                ReachCheckListBox1.Items.Add(Obj.value, CheckState.Unchecked) ' UN added
                                lastreechcode = kreechcode
                                If next_reach(kreechcode) = 0 Then Exit For
                            Else
                                If ip < numreaches + 2 Then
                                    If next_reach(kreechcode) > 0 Then
                                        ReachCheckListBox1.Items.Add(Obj.value, CheckState.Unchecked)
                                    Else
                                        lastreechcode = kreechcode
                                        ReachCheckListBox1.Items.Add(Obj.value, CheckState.Unchecked) 'Un added
                                        Exit For
                                    End If
                                Else
                                    ReachCheckListBox1.Items.Add(Obj.value, CheckState.Unchecked) ' Un added
                                    lastreechcode = kreechcode
                                    Exit For
                                End If
                            End If
                        Next ' For ip = 3 To numreaches + 2 ===========================================================

                        ChooseReachesPlot.Visible = True
                        Obj = CType(range.Cells(1, 1), Excel.Range) ' the heading from the CSV file
                        Heading1.Text = Obj.value : Heading1.Visible = True
                        Obj = CType(range.Cells(2, 1), Excel.Range) ' the name of the determinand
                        Determinand1.Text = Obj.value : Determinand1.Visible = True ' the name of the determinand
                        Obj = CType(range.Cells(2, 2), Excel.Range) ' the units
                        UnitsLabel1.Text = Obj.value : UnitsLabel1.Visible = True ' the units
                        Obj = CType(range.Cells(2, 6), Excel.Range) ' the units
                        LoadsUnitsLabel1.Text = Obj.value : LoadsUnitsLabel1.Visible = False ' the units

                    Else
                        ReachCheckListBox1.Items.Clear()
                        For ip = 3 To numreaches + 2 ' read the data on all the reaches - set up the sequence of reaches ===========
                            Obj = CType(range.Cells(ip, 3), Excel.Range) : kreechcode = Obj.value ' .......... the reach code number
                            reechnumbercode(ip - 2) = kreechcode
                            Obj = CType(range.Cells(ip, 4), Excel.Range) 'the reach code number of the next reach
                            next_reach(kreechcode) = Obj.value 'the reach code number of the next reach
                            Obj = CType(range.Cells(ip, 2), Excel.Range) : reachname = Obj.text ' the name of the reach
                            If kreechcode = startreechcode Or kreechcode = lastreechcode Then
                                ReachCheckListBox1.Items.Add(Obj.value, CheckState.Checked)
                            Else
                                ReachCheckListBox1.Items.Add(Obj.value, CheckState.Unchecked)
                            End If
                            If kreechcode = lastreechcode Then next_reach(kreechcode) = 0
                        Next '=========================================================================================================

                    End If ' If startreechcode <= 0 -----------------------------------------------------------------------------------
                End If ' If FileDialog.ShowDialog() = DialogResult.OK Then

            End Using
            ReachCheckListBox1.Visible = True
            ReachCheckListBox1.Enabled = True
            ReachCheckListBox1.CheckOnClick = True
            Return
        Else 'if RunSequence = 0 or 1
            ExcelfilenamePLOT = LastfilenamePLOT
        End If

        xlApp = New Excel.Application
        xlWorkBook = xlApp.Workbooks.Open(ExcelfilenamePLOT)
        xlWorkSheet = xlWorkBook.Worksheets(1)
        range = xlWorkSheet.UsedRange

        guide = 0

        'PrintLine(5, "numreaches = ", numreaches)
        'PrintLine(5, "--------------------------------------------------------------------------------------")

        For ip = 1 To numreaches
            next_reach(ip) = next_reach_master(ip)
        Next
        For ip = 1 To numreaches
            If ReachCheckListBox1.GetItemChecked(ip - 1) = True Then
                If guide = 0 Then
                    startreechcode = reechnumbercode(ip)
                    lastreechcode = reechnumbercode(ip)
                    guide = 1
                Else
                    lastreechcode = reechnumbercode(ip)
                    next_reach(lastreechcode) = 0
                    guide = 2
                End If
            End If
        Next

        If Statistic1.Checked = True And mrqs <> 1 Then mrqs = 0
        If Statistic2.Checked = True And mrqs <> 2 Then mrqs = 0
        If Statistic3.Checked = True And mrqs <> 3 Then mrqs = 0
        If Statistic2.Checked = True And mrqs <> 4 Then mrqs = 0
        If Statistic3.Checked = True And mrqs <> 5 Then mrqs = 0
        If Statistic4.Checked = True And mrqs <> 6 Then mrqs = 0

        If Statistic1.Checked = True Then mrqs = irqs
        If Statistic2.Checked = True Then mrqs = irqs
        If Statistic3.Checked = True Then mrqs = irqs
        If Statistic4.Checked = True Then mrqs = irqs

        'Obj = CType(range.Cells(1, 1), Excel.Range) ' the heading from the CSV file
        'Heading1.Text = Obj.value : Heading1.Visible = True
        'Obj = CType(range.Cells(2, 1), Excel.Range) ' the name of the determinand
        'Determinand1.Text = Obj.value : Determinand1.Visible = True ' the name of the determinand

        Obj = CType(range.Cells(numreaches + 3, 3), Excel.Range) : numfeatures = Obj.value : usedfeatures = 0
        Me.GraphDisplay.Series("mean").Color = Color.Purple
        Me.GraphDisplay.Series("reaches").Color = Color.Transparent
        Me.GraphDisplay.Series("confidence").Color = Color.DeepPink
        Me.GraphDisplay.Series("confidence").BorderWidth = 1
        Me.GraphDisplay.Series("upper confidence").Color = Color.DeepPink
        Me.GraphDisplay.Series("upper confidence").BorderWidth = 1
        Me.GraphDisplay.Series("upper confidence").LabelBackColor = Color.LightBlue
        Me.GraphDisplay.Series("target").Color = Color.DarkOrange
        Me.GraphDisplay.Series("target").BorderDashStyle = DataVisualization.Charting.ChartDashStyle.Dash
        'Me.Chart1.Series("observed").BorderColor = Color.MediumSeaGreen
        Me.GraphDisplay.Series("observed").BorderWidth = 30
        Me.GraphDisplay.Series("names").Color = Color.Transparent
        Me.GraphDisplay.Series("names").BorderColor = Color.Transparent

        cell1 = 7 ' set a default as a plot of mean concentration
        UnitsLabel1.Visible = True
        LoadsUnitsLabel1.Visible = False
        Load1.Text = "mean concentration" 'Load1.Visible = True
        If Statistic1.Checked = True Then
            Load1.Text = "mean concentration" : cell1 = 7 : nrqs = 1 ' mean
            Me.GraphDisplay.ChartAreas(0).AxisY.Title = "mean concentration"
        End If
        If Statistic2.Checked = True Then
            Load1.Text = "90-percentile concentration" : cell1 = 10 : nrqs = 3 ' 90-percentile
            Me.GraphDisplay.ChartAreas(0).AxisY.Title = "90-percentile concentration"
        End If
        If Statistic3.Checked = True Then
            Load1.Text = "95-percentile concentration" : cell1 = 13 : nrqs = 2 ' 95-percentile
            Me.GraphDisplay.ChartAreas(0).AxisY.Title = "95-percentile concentration"
        End If
        If Statistic4.Checked = True Then
            Load1.Text = "99-percentile concentration" : cell1 = 16 : nrqs = 6 ' 99-percentile
            Me.GraphDisplay.ChartAreas(0).AxisY.Title = "99-percentile concentration"
        End If

        If LoadStatistic5.Checked = True Then
            Load1.Text = "mean load" : cell1 = 19 ' mean load
            UnitsLabel1.Visible = False
            LoadsUnitsLabel1.Visible = True
            Me.GraphDisplay.ChartAreas(0).AxisY.Title = "mean load"
            Me.GraphDisplay.Series("mean").Color = Color.SaddleBrown
            Me.GraphDisplay.Series("reaches").Color = Color.Transparent
            Me.GraphDisplay.Series("confidence").Color = Color.SaddleBrown
            Me.GraphDisplay.Series("upper confidence").Color = Color.SaddleBrown
        End If

        If startreechcode = lastreechcode Then
            next_reach(startreechcode) = 0
        End If

        rstart = startreechcode
        rnext = next_reach(rstart)

        featurenamelast = 0 : featlength = 0 : plotlength = 0.0 : plot_point_end = 0 : plot_point_start = 0
        namey1 = 0 : namey2 = 0 : namey3 = 0 : namey5 = 0 : namey3 = 6
        poyntprevious = -99

        starting = 0

        For iii = 1 To 6
            For ireach = 1 To numreaches '=============== altered from rstart to 1 on 26/02/19 =============================================
                lastlength = 0.0
                kreechcode = reechnumbercode(ireach)

                If reechnumbercode(ireach) = startreechcode Then ' for the next reach
                    starting = 99999
                Else
                    If starting = 99999 Then
                        rstart = rnext ' set the next reach number
                        rnext = next_reach(rstart) ' set the reach to be run after this reach
                        featurecounter.Text = rnext : reachcounter.Text = ireach
                    Else
                        starting = 0
                        GoTo LastLine ' end of reaches being plotted
                    End If
                End If

                feattype = 0 : featlast = 0 : featurename = "notreadyet"
                ipstart = numreaches + 4 : iplast = numreaches + 4 + 2 * numfeatures + 2 * numreaches

                For ip = ipstart To iplast ' loop through all the features ========================================================================
                    Obj = CType(range.Cells(ip, 3), Excel.Range)
                    reachnumber = Obj.value

                    ipstart = ipstart + 1
                    If reachnumber = rstart Then
                        If feattype = 999 Then Exit For
                        featlast = feattype
                        Obj = CType(range.Cells(ip, 4), Excel.Range) : feattype = Obj.value
                        If feattype = 5 Or feattype = 60 Or feattype = 61 Or feattype = 62 Then feattype = 3
                        featurenamelast = featurename
                        Obj = CType(range.Cells(ip, 1), Excel.Range) : featurename = Obj.text
                        Obj = CType(range.Cells(ip, 2), Excel.Range) : reachname = Obj.text ' the name of the reach
                        Obj = CType(range.Cells(ip, 5), Excel.Range) : featlength = 10.0 * Obj.value
                        Obj = CType(range.Cells(ip, 6), Excel.Range) : reachlength = 10.0 * Obj.value
                        Obj = CType(range.Cells(ip, 7), Excel.Range) : runninglength.Text = 10.0 * (0.1 * Int(10.0 * (Obj.value + 0.05)))
                        plotlength = plotlength + featlength - lastlength

                        Me.GraphDisplay.ChartAreas(0).AxisX.Interval = 1.0
                        If plotlength > 20.1 Then Me.GraphDisplay.ChartAreas(0).AxisX.Interval = 2
                        If plotlength > 100.1 Then Me.GraphDisplay.ChartAreas(0).AxisX.Interval = 5
                        If plotlength > 200.1 Then Me.GraphDisplay.ChartAreas(0).AxisX.Interval = 10
                        If plotlength > 400.1 Then Me.GraphDisplay.ChartAreas(0).AxisX.Interval = 20

                        lastlength = featlength : usedfeatures = usedfeatures + 1
                        featurecounter.Text = usedfeatures
                        plot_point_start = plot_point_end : plot_point_end = Int(plotlength)

                        Obj = CType(range.Cells(ip - 1, cell1 + 1), Excel.Range) : namey1 = Obj.value : inamey1 = Obj.value 'concentration
                        Obj = CType(range.Cells(ip - 1, cell1 + 2), Excel.Range) : namey2 = Obj.value : inamey2 = Obj.value 'lower confidence
                        Obj = CType(range.Cells(ip - 1, cell1 + 3), Excel.Range) : namey3 = Obj.value : inamey3 = Obj.value 'upper confidence
                        Obj = CType(range.Cells(ip, cell1 + 1), Excel.Range) : namey1d = Obj.value : inamey1d = Obj.value ' d/s mean concentration
                        Obj = CType(range.Cells(ip, cell1 + 2), Excel.Range) : namey2d = Obj.value : inamey2d = Obj.value ' d/s lower confidence
                        Obj = CType(range.Cells(ip, cell1 + 3), Excel.Range) : namey3d = Obj.value : inamey3d = Obj.value ' d/s upper confidence
                        pnamey1 = inamey1d / inamey1 : pnamey2 = inamey2d / inamey2 : pnamey3 = inamey3d / inamey3

                        If plot_point_end >= plot_point_start Then
                            If plot_point_end > 0 Then

                                If feattype = 1 Then ' ================================================================================
                                    ic2 = 109 ' the standard is a mean
                                    If nrqs = 3 Then ic2 = 112 ' 90-percentile or 90-percentile
                                    If nrqs = 2 Then ic2 = 115 ' 95-percentile or 05-percentile
                                    If nrqs = 6 Then ic2 = 118 ' 99-percentile or 01-percentile
                                    Obj = CType(range.Cells(ip - 1, ic2 + 1), Excel.Range) : namey5 = Obj.value ' confidence limit
                                    Obj = CType(range.Cells(ip - 1, ic2 + 2), Excel.Range) : namey6 = Obj.value ' confifence limit
                                    If featurename = "d/s of above" Then
                                        Me.GraphDisplay.Series("observed").BorderWidth = 30
                                        Me.GraphDisplay.Series("observed").Points.AddXY(plot_point_end, namey5, namey6)
                                    End If
                                End If 'If feattype = 1 ===============================================================================

                                For poynt = plot_point_start To plot_point_end
                                    If poynt - plot_point_start > 0 Then
                                        frakshun = (poynt - plot_point_start) / (plot_point_end - plot_point_start)
                                        namey1 = inamey1 + inamey1 * (pnamey1 - 1.0) * frakshun
                                        namey2 = inamey2 + inamey2 * (pnamey2 - 1.0) * frakshun
                                        namey3 = inamey3 + inamey3 * (pnamey3 - 1.0) * frakshun
                                    End If

                                    If poyntprevious = poynt And featurename = "Start of Reach" And featurenamelast = "notreadyet" Then
                                        Me.GraphDisplay.Series("reaches").Points.AddXY(poynt, namey1)
                                    Else
                                        Me.GraphDisplay.Series("mean").Points.AddXY(poynt, namey1)
                                        Me.GraphDisplay.Series("confidence").Points.AddXY(poynt, namey2)
                                        Me.GraphDisplay.Series("upper confidence").Points.AddXY(poynt, namey3)
                                        Me.GraphDisplay.Series("reaches").Points.AddXY(poynt, namey1)
                                    End If

                                    If poynt = poyntprevious Then dequal03 = dequal03 + 1
                                    If cell1 < 17 Or cell1 = 999 Then ' target ========================================================
                                        If cell1 = 7 And mrqs = 1 Or cell1 = 10 And mrqs = 3 Or cell1 = 13 And mrqs = 2 Or cell1 = 16 And mrqs = 6 Then
                                            Obj = CType(range.Cells(ip - 1, 36), Excel.Range) : namey4 = Obj.value
                                            If namey4 > 0.0001 Then
                                                Me.GraphDisplay.Series("target").Points.AddXY(poynt, namey4)
                                            End If
                                        End If
                                    End If ' target ===================================================================================
                                    poyntprevious = poynt
                                Next

                                If plot_point_start = plot_point_end Then
                                    If featurename = "d/s of above" Then

                                        If feattype = 3 And featlast = 3 And pnamey1 > -1.0 Then
                                            GraphDisplay.Series("upper confidence").Points.Item(plot_point_end + dequal03).Label = featurenamelast
                                            feattype = 0 '################################
                                            featlast = 0 '################################
                                        End If
                                        featurenamelast = "      " '#####################
                                        feattype = 0 '################################
                                        featlast = 0 '################################
                                    End If
                                End If ' If plot_point_start = plot_point_end

                            End If
                        End If

                    Else
                        If feattype = 999 Then Exit For
                    End If ' If reachnumber = rstart Then

                Next ' For ip = ipstart To iplast - loop on number of features ================================================

                feattype = 0
                If rnext = 0 Then Exit For

LastLine: ' end of reaches being plotted 
                dequal03 = 0
                featurenamelast = "....." '#####################
                featurename = "      " '######################

            Next ' For ireach = 1 To numreaches =================== loop on reaches ==================================

            For ip = 3 To numreaches + 2 ' ----------------------------------------------------------------------------
                Obj = CType(range.Cells(ip, 4), Excel.Range)
                next_reach(kreechcode) = Obj.value 'the reach code number of the next reach
            Next ' For ip = 3 To numreaches + 2 ------------------------------------------------------------------------

            If rnext = 0 Then Exit For

        Next ' For iii = 1 To 6 ========================================================================================

        'PrintLine(5, "Closing file -------------------------------------------------------------------------")
        'PrintLine(5, "======================================================================================")
        'FileClose(5)

exitsub:
        If Not xlWorkSheet Is Nothing Then
            xlWorkSheet = Nothing
        End If
        If Not xlWorkBook Is Nothing Then
            xlWorkBook.Close()
            xlWorkBook = Nothing
        End If
        If Not xlApp Is Nothing Then
            xlApp.Quit()
            xlApp = Nothing
        End If
        If Not range Is Nothing Then
            range = Nothing
        End If

        Dim proc As System.Diagnostics.Process
        For Each proc In System.Diagnostics.Process.GetProcessesByName("EXCEL")
            proc.Kill()
        Next

        Exit Sub
Error_handler:
        Resume exitsub

    End Sub

    Public Sub Produce_a_graph1a(RunSequence)

        Dim plot_point_end, plot_point_start As Integer
        Dim namey1, namey2, namey3, namey4, namey5, namey6 As String
        Dim namey1d, namey2d, namey3d As String
        Dim featurename, featurenamelast As String
        Dim nameofreach As String
        Dim inamey1, inamey1d, pnamey1 As Single
        Dim inamey2, inamey2d, pnamey2 As Single
        Dim inamey3, inamey3d, pnamey3 As Single
        Dim mrqs, nrqs, guide, starting As Integer
        Dim numfeatures, usedfeatures As Integer
        Dim reachnumber, rstart, rnext, kreechcode As Integer
        Dim reachlength, plotlength, lastlength, featlength As Single
        Dim feattype, featlast As Integer
        Dim ipstart, iplast, ip, cell1, ic2 As Integer
        Dim dequal03 As Integer
        Dim frakshun As Single
        Dim poynt, poyntprevious As Integer
        Dim FilenamePLOT As String

        Dim des As String = "      "
        Dim des1 As String = "      "
        Dim intx As Integer

        If RunSequence = 0 Then

            LastfilenamePLOT = "========"
            FilenamePLOT = "++++++++"

            Using FileDialog As New OpenFileDialog
                Heading1.Visible = False
                Determinand1.Visible = False
                UnitsLabel1.Visible = False
                LoadsUnitsLabel1.Visible = False
                Load1.Visible = False
                featurecounter.Visible = False : reachcounter.Visible = False : runninglength.Visible = False

                Me.GraphDisplay.Series(0).Points.Clear() : Me.GraphDisplay.Series(1).Points.Clear() : Me.GraphDisplay.Series(2).Points.Clear()
                Me.GraphDisplay.Series(3).Points.Clear() : Me.GraphDisplay.Series(4).Points.Clear() : Me.GraphDisplay.Series(5).Points.Clear()
                Me.GraphDisplay.Series(6).Points.Clear() : Me.GraphDisplay.Series(7).Points.Clear()

                Me.GraphDisplay.ChartAreas(0).AxisY.Title = "concentration"
                Me.GraphDisplay.ChartAreas(0).AxisX.Title = "distance"

                Me.GraphDisplay.ChartAreas(0).AxisX.Interval = 1
                Me.GraphDisplay.ChartAreas(0).AxisX.LineWidth = 2
                Me.GraphDisplay.ChartAreas(0).AxisY.LineWidth = 2
                Me.GraphDisplay.ChartAreas(0).AxisX.MajorGrid.LineColor = Color.LightGray
                Me.GraphDisplay.ChartAreas(0).AxisY.MajorGrid.LineColor = Color.LightGray

                FileDialog.Title = "Select your data file (and therefore your determinand)"
                FileDialog.Filter = "Micosoft Excel|*+G*|All Files|*.*"

                If FileDialog.ShowDialog() = DialogResult.OK Then ' this displays the box

                    ChooseDeterminand.Visible = False
                    FilenamePLOT = FileDialog.FileName
                    LastfilenamePLOT = FileDialog.FileName
                    '                    If LastfilenamePLOT <> FilenamePLOT Then startreechcode = 0


                    FileOpen(1, FilenamePLOT, OpenMode.Input)
                    Input(1, Heading1.Text)
                    LineInput(1)
                    Input(1, Determinand1.Text) : Input(1, des1) : Input(1, intx) : Input(1, mrqs) : Input(1, numreaches)
                    If mrqs = 1 Then
                        Me.GraphDisplay.ChartAreas(0).AxisY.Title = "mean concentration"
                        Me.Statistic2.Text = "90-percentile"
                        Me.Statistic3.Text = "95-percentile"
                        Statistic1.Checked = True
                    End If
                    If mrqs = 3 Then
                        Statistic2.Checked = True
                        Me.GraphDisplay.ChartAreas(0).AxisY.Title = "90-percentile concentration"
                        Me.Statistic3.Text = "95-percentile"
                    End If
                    If mrqs = 2 Then
                        Statistic3.Checked = True
                        Me.GraphDisplay.ChartAreas(0).AxisY.Title = "95-percentile concentration"
                        Me.Statistic2.Text = "90-percentile"
                    End If
                    If mrqs = 5 Then
                        Statistic2.Checked = True
                        Me.GraphDisplay.ChartAreas(0).AxisY.Title = "10-percentile concentration"
                        Me.Statistic2.Text = "10-percentile"
                    End If
                    If mrqs = 4 Then
                        Statistic3.Checked = True
                        Me.GraphDisplay.ChartAreas(0).AxisY.Title = "5-percentile concentration"
                        Me.Statistic3.Text = "5-percentile"
                    End If
                    If mrqs = 6 Then
                        Statistic4.Checked = True
                        Me.Statistic2.Text = "90-percentile"
                        Me.Statistic3.Text = "95-percentile"
                    End If
                    nrqs = mrqs : irqs = mrqs ' store a master copy


                    startreechcode = 0 : lastreechcode = 0 : nameofreach = "   "
                    If startreechcode <= 0 Then ' -------------------------------------------------------------------------------------------------
                        ReachCheckListBox1.CheckOnClick = True
                        ReachCheckListBox1.Items.Clear()

                        For ip = 3 To numreaches + 2 ' read the data on all the reaches - set up the sequence of reaches ===========
                            LineInput(1)
                            Input(1, des) : Input(1, nameofreach) : Input(1, kreechcode)
                            reechnumbercode(ip - 2) = kreechcode
                            If ip = 3 Then startreechcode = kreechcode : lastreechcode = kreechcode
                            Input(1, next_reach(kreechcode))
                            next_reach_master(kreechcode) = next_reach(kreechcode) 'the reach code number of the next reach

                            If ip = 3 Then
                                ReachCheckListBox1.Items.Add(nameofreach, CheckState.Checked)
                                lastreechcode = kreechcode
                                If next_reach(kreechcode) = 0 Then Exit For
                            Else
                                If ip < numreaches + 2 Then
                                    If next_reach(kreechcode) > 0 Then
                                        ReachCheckListBox1.Items.Add(nameofreach, CheckState.Unchecked)
                                    Else
                                        lastreechcode = kreechcode
                                        ReachCheckListBox1.Items.Add(nameofreach, CheckState.Checked)
                                        Exit For
                                    End If
                                Else
                                    ReachCheckListBox1.Items.Add(nameofreach, CheckState.Checked)
                                    lastreechcode = kreechcode
                                    Exit For
                                End If
                            End If
                        Next '=====================================================================================================

                        ChooseReachesPlot.Visible = True

                    Else
                        ReachCheckListBox1.Items.Clear()
                        For ip = 3 To numreaches + 2 ' read the data on all the reaches - set up the sequence of reaches ===========
                            LineInput(1)
                            Input(1, des) : Input(1, nameofreach) : Input(1, kreechcode)
                            reechnumbercode(ip - 2) = kreechcode
                            Input(1, next_reach(kreechcode))
                            If kreechcode = startreechcode Or kreechcode = lastreechcode Then
                                ReachCheckListBox1.Items.Add(nameofreach, CheckState.Checked)
                            Else
                                ReachCheckListBox1.Items.Add(nameofreach, CheckState.Unchecked)
                            End If
                            If kreechcode = lastreechcode Then next_reach(kreechcode) = 0
                        Next '=========================================================================================================

                    End If ' If startreechcode <= 0 -----------------------------------------------------------------------------------
                End If
            End Using
            ReachCheckListBox1.Visible = True
            ReachCheckListBox1.Enabled = True
            ReachCheckListBox1.CheckOnClick = True
            Return
        Else 'if RunSequence = 0 or 1
            FilenamePLOT = LastfilenamePLOT
        End If ' If FileDialog.ShowDialog() = DialogResult.OK Then


        guide = 0
        For ip = 1 To numreaches
            next_reach(ip) = next_reach_master(ip)
        Next
        For ip = 1 To numreaches
            If ReachCheckListBox1.GetItemChecked(ip - 1) = True Then
                If guide = 0 Then
                    startreechcode = reechnumbercode(ip)
                    lastreechcode = reechnumbercode(ip)
                    guide = 1
                Else
                    lastreechcode = reechnumbercode(ip)
                    next_reach(lastreechcode) = 0
                    guide = 2
                    'Exit For
                End If
            End If
        Next
        If Statistic1.Checked = True And mrqs <> 1 Then mrqs = 0
        If Statistic2.Checked = True And mrqs <> 2 Then mrqs = 0
        If Statistic3.Checked = True And mrqs <> 3 Then mrqs = 0
        If Statistic2.Checked = True And mrqs <> 4 Then mrqs = 0
        If Statistic3.Checked = True And mrqs <> 5 Then mrqs = 0
        If Statistic4.Checked = True And mrqs <> 6 Then mrqs = 0

        If Statistic1.Checked = True Then mrqs = irqs
        If Statistic2.Checked = True Then mrqs = irqs
        If Statistic3.Checked = True Then mrqs = irqs
        If Statistic4.Checked = True Then mrqs = irqs

        LineInput(1)
        Input(1, des) : Input(1, des1) : Input(1, numfeatures)
        'Input(1, des) : Input(1, des1)  'Input(1, numfeatures) ': Input(1, mrqs) : Input(1, numreaches)

        Heading1.Visible = True
        Determinand1.Visible = True
        If LoadStatistic5.Checked = False Then
            UnitsLabel1.Visible = True
        Else
            LoadsUnitsLabel1.Visible = True
        End If

        usedfeatures = 0
        Me.GraphDisplay.Series("mean").Color = Color.Purple
        Me.GraphDisplay.Series("reaches").Color = Color.Transparent
        Me.GraphDisplay.Series("confidence").Color = Color.DeepPink
        Me.GraphDisplay.Series("confidence").BorderWidth = 2
        Me.GraphDisplay.Series("upper confidence").Color = Color.DeepPink
        Me.GraphDisplay.Series("upper confidence").BorderWidth = 2
        Me.GraphDisplay.Series("upper confidence").LabelBackColor = Color.LightBlue
        Me.GraphDisplay.Series("target").Color = Color.DarkOrange
        Me.GraphDisplay.Series("target").BorderDashStyle = DataVisualization.Charting.ChartDashStyle.Dash
        Me.GraphDisplay.Series("observed").BorderWidth = 30
        Me.GraphDisplay.Series("names").Color = Color.Transparent
        Me.GraphDisplay.Series("names").BorderColor = Color.Transparent

        cell1 = 7 ' set a default as a plot of mean concentration
        Load1.Text = "mean concentration" 'Load1.Visible = True
        If Statistic1.Checked = True Then
            Load1.Text = "mean concentration" : cell1 = 7 : nrqs = 1 ' mean
            Me.GraphDisplay.ChartAreas(0).AxisY.Title = "mean concentration"
        End If
        If Statistic2.Checked = True Then
            Load1.Text = "90-percentile concentration" : cell1 = 10 : nrqs = 3 ' 90-percentile
            If mrqs = 3 Then Me.GraphDisplay.ChartAreas(0).AxisY.Title = "90-percentile concentration"
        End If
        If Statistic3.Checked = True Then
            Load1.Text = "95-percentile concentration" : cell1 = 13 : nrqs = 2 ' 95-percentile
            If mrqs = 2 Then Me.GraphDisplay.ChartAreas(0).AxisY.Title = "95-percentile concentration"
        End If
        If Statistic4.Checked = True Then
            Load1.Text = "99-percentile concentration" : cell1 = 16 : nrqs = 6 ' 99-percentile
            Me.GraphDisplay.ChartAreas(0).AxisY.Title = "99-percentile concentration"
        End If

        If LoadStatistic5.Checked = True Then
            Load1.Text = "mean load" : cell1 = 19 ' mean load
            Me.GraphDisplay.Series("mean").Color = Color.SaddleBrown
            Me.GraphDisplay.Series("reaches").Color = Color.Transparent
            Me.GraphDisplay.Series("confidence").Color = Color.SaddleBrown
            Me.GraphDisplay.Series("upper confidence").Color = Color.SaddleBrown
        End If

        If startreechcode = lastreechcode Then
            next_reach(startreechcode) = 0
        End If

        rstart = startreechcode : rnext = next_reach(rstart)
        featurenamelast = "    " : featlength = 0 : plotlength = 0.0 : plot_point_end = 0 : plot_point_start = 0
        namey1 = 0 : namey2 = 0 : namey3 = 0 : namey5 = 0 : namey6 = 0
        poyntprevious = -99

        'MsgBox("startreechcode = " & startreechcode & " rnext = " & rnext)

        starting = 0
        For ireach = 1 To numreaches '=============== altered from rstart to 1 on 26/02/19 =============================================
            lastlength = 0.0

            If reechnumbercode(ireach) = startreechcode Then ' for the next reach
                starting = 99999
            Else
                If starting = 99999 Then
                    rstart = rnext ' set the next reach number
                    rnext = next_reach(rstart) ' set the reach after this reach
                    featurecounter.Text = rnext : reachcounter.Text = ireach
                    ireach = rstart
                Else
                    starting = 0
                    GoTo LastLine
                End If
            End If

            'MsgBox("ireach = " & ireach & " rnext = " & rnext & " rstart = " & rstart)

            feattype = 0 : featlast = 0 : featurename = " " : nameofreach = " "
            ipstart = numreaches + 4 : iplast = numreaches + 4 + 2 * numfeatures + 2 * numreaches
            'MsgBox("03 ireach = " & rstart & "  rstart = " & rstart & "  rnext = " & rnext & "  featurecounter.Text = " & featurecounter.Text & "  starting = " & starting & "  ipstart = " & ipstart & "  iplast = " & iplast)
            'MsgBox("04 feattype = " & feattype & "  plotlength = " & plotlength & "  starting = " & starting)
            'Dim feattest As Integer = 777

            Dim xlength, x00 As Single
            Dim c08, c09, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19 As Single
            Dim c36, c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56 As Single
            Dim d08, d09, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19 As Single
            Dim d36, d45, d46, d47, d48, d49, d50, d51, d52, d53, d54, d55, d56 As Single

            'MsgBox("ireach = " & ireach & " ipstart = " & ipstart & " iplast = " & iplast)

            For ip = ipstart To iplast '=============================================================================================
                LineInput(1)

                d08 = c08 : d09 = c09 : d10 = c10 : d11 = c11 : d12 = c12 : d13 = c13 : d14 = c14 : d15 = c15 : d16 = c16
                d17 = c17 : d18 = c18 : d19 = c19
                d36 = c36 : d45 = c45 : d46 = c46 : d47 = c47 : d48 = c48 : d49 = c49 : d50 = c50 : d51 = c51 : d52 = c52
                d53 = c53 : d54 = c54 : d55 = c55 : d56 = c56

                Input(1, featurename) : Input(1, nameofreach) : Input(1, reachnumber)
                Input(1, feattype) : Input(1, featlength) : Input(1, reachlength) : Input(1, xlength)
                Input(1, c08) : Input(1, c09) : Input(1, c10) : Input(1, c11) : Input(1, c12) : Input(1, c13)
                Input(1, c14) : Input(1, c15) : Input(1, c16) : Input(1, c17) : Input(1, c18)
                Input(1, x00) : Input(1, x00) : Input(1, x00)
                Input(1, x00) : Input(1, x00) : Input(1, x00) : Input(1, x00) : Input(1, x00) : Input(1, x00) : Input(1, x00)
                Input(1, x00) : Input(1, x00) : Input(1, x00)
                Input(1, x00) : Input(1, x00) : Input(1, x00) : Input(1, x00) : Input(1, c36)
                Input(1, x00) : Input(1, x00) : Input(1, x00) : Input(1, x00) : Input(1, x00) : Input(1, x00) : Input(1, x00)
                Input(1, x00)

                'MsgBox(featurename & "  " & nameofreach)

                If feattype = 1 Then
                    Input(1, c45) : Input(1, c46) : Input(1, c47) : Input(1, c48) : Input(1, c49) : Input(1, c50)
                    Input(1, c51) : Input(1, c52) : Input(1, c53) : Input(1, c54) : Input(1, c55) : Input(1, c56)
                End If

                runninglength.Text = 0.1 * Int(10.0 * (xlength + 0.05))
                plotlength = plotlength + featlength - lastlength

                'If reechnumbercode(ireach) = 26 Then
                'If reachnumber = rstart And reechnumbercode(ireach) <> endreechcode Then ' ########################################
                'If reachnumber = rstart Then ' ########################################
                'If reechnumbercode(ireach) <> rstart Then ' ########################################
                'If reechnumbercode(ireach) = 26 And reachnumber = 1 Then ' ########################## Yes
                'If reechnumbercode(ireach) = numreaches And reachnumber = 1 Then ' ################## No!
                'If reechnumbercode(ireach) > 1 And reachnumber = 1 Then ' ########################### No!
                'If feattype = 999 Then ' ############################################################ No!
                'If ip = ipstart Then ' ############################################################## No!
                'feattype = 0 ' ###################################################################### No!
                'feattest = feattype
                'Obj = CType(range.Cells(ip, 4), Excel.Range) : feattype = Obj.value
                'MsgBox("feattype = " & "   " & feattype)
                'MsgBox("ip " & ip & " ireach " & ireach & " rstart " & rstart & " feattype " & feattype & " reachnumber " & reachnumber)
                'MsgBox("ip " & ip & "  feattype " & feattype & " feattest " & feattest)
                'End If
                'MsgBox("05 reachnumber = " & reachnumber & "  rstart = " & rstart & "  starting = " & starting & "  feattype = " & feattype)

                ipstart = ipstart + 1
                If reachnumber = rstart Then
                    If feattype = 999 Then Exit For
                    featlast = feattype
                    If feattype = 5 Or feattype = 60 Or feattype = 61 Or feattype = 62 Then feattype = 3
                    featurenamelast = featurename

                    'Obj = CType(range.Cells(ip, 1), Excel.Range) : featurename = Obj.text
                    'Obj = CType(range.Cells(ip, 5), Excel.Range) : featlength = Obj.value
                    'Obj = CType(range.Cells(ip, 6), Excel.Range) : reachlength = Obj.value
                    'Obj = CType(range.Cells(ip, 7), Excel.Range) : runninglength.Text = 0.1 * Int(10.0 * (Obj.value + 0.05))
                    'plotlength = plotlength + featlength - lastlength

                    Me.GraphDisplay.ChartAreas(0).AxisX.Interval = 1.0
                    If plotlength > 20.1 Then Me.GraphDisplay.ChartAreas(0).AxisX.Interval = 2
                    If plotlength > 100.1 Then Me.GraphDisplay.ChartAreas(0).AxisX.Interval = 5
                    If plotlength > 200.1 Then Me.GraphDisplay.ChartAreas(0).AxisX.Interval = 10
                    If plotlength > 400.1 Then Me.GraphDisplay.ChartAreas(0).AxisX.Interval = 20

                    lastlength = featlength : usedfeatures = usedfeatures + 1
                    featurecounter.Text = usedfeatures
                    plot_point_start = plot_point_end : plot_point_end = Int(plotlength)

                    If nrqs = 1 Then
                        namey1 = d08 : inamey1 = d08 : namey2 = d09 : inamey2 = d09 : namey3 = d10 : inamey3 = d10
                        namey1d = c08 : inamey1d = c08 : namey2d = c09 : inamey2d = c09 : namey3d = c10 : inamey3d = c10
                    End If
                    If nrqs = 3 Then
                        namey1 = d11 : inamey1 = d11 : namey2 = d12 : inamey2 = d12 : namey3 = d13 : inamey3 = d13
                        namey1d = c11 : inamey1d = c11 : namey2d = c12 : inamey2d = c12 : namey3d = c13 : inamey3d = c13
                    End If
                    If nrqs = 2 Then
                        namey1 = d14 : inamey1 = d14 : namey2 = d15 : inamey2 = d15 : namey3 = d16 : inamey3 = d16
                        namey1d = c14 : inamey1d = c14 : namey2d = c15 : inamey2d = c15 : namey3d = c16 : inamey3d = c16
                    End If
                    If nrqs = 6 Then
                        namey1 = d17 : inamey1 = d17 : namey2 = d18 : inamey2 = d18 : namey3 = d19 : inamey3 = d19
                        namey1d = c17 : inamey1d = c17 : namey2d = c18 : inamey2d = c18 : namey3d = c19 : inamey3d = c19
                    End If

                    pnamey1 = inamey1d / inamey1 : pnamey2 = inamey2d / inamey2 : pnamey3 = inamey3d / inamey3

                    If plot_point_end >= plot_point_start Then

                        If feattype = 1 Then ' And featlast = 1 Then ' =========================================================
                            If nrqs = 1 Then
                                ic2 = 101 : namey5 = d46 : namey6 = d47 ' the standard is a mean
                            End If
                            If nrqs = 3 Then
                                ic2 = 104 : namey5 = d49 : namey6 = d50 ' 90-percentile or 10-percentile
                            End If
                            If nrqs = 2 Then
                                ic2 = 107 : namey5 = d52 : namey6 = d53 ' 95-percentile or 05-percentile
                            End If
                            If nrqs = 6 Then
                                ic2 = 110 : namey5 = d55 : namey6 = d56 ' 99-percentile or 01-percentile
                            End If

                            If featurename = "d/s of above" Then
                                Me.GraphDisplay.Series("observed").BorderWidth = 30
                                Me.GraphDisplay.Series("observed").Points.AddXY(plot_point_end, namey5, namey6)
                                'Me.Chart1.Series("observed").Points.AddXY(plot_point_end, namey6, namey5)
                            End If
                        End If 'If feattype = 1 ===============================================================================

                        For poynt = plot_point_start To plot_point_end
                            If poynt - plot_point_start > 0 Then
                                frakshun = (poynt - plot_point_start) / (plot_point_end - plot_point_start)
                                namey1 = inamey1 + inamey1 * (pnamey1 - 1.0) * frakshun
                                namey2 = inamey2 + inamey2 * (pnamey2 - 1.0) * frakshun
                                namey3 = inamey3 + inamey3 * (pnamey3 - 1.0) * frakshun
                            End If

                            If poyntprevious = poynt And featurename = "Start of Reach" And featurenamelast = "notreadyet" Then
                                'MsgBox("bbb  " & featurename & "    " & featurenamelast & "   " & poyntprevious & "    " & poynt)
                                'MsgBox("bbb  " & poynt & "    " & namey1)
                                'Me.Chart1.Series("confidence").Points.AddXY(poynt, namey2)
                                'Me.Chart1.Series("confidence").Points.AddXY(poynt, namey2)
                                'Me.Chart1.Series("upper confidence").Points.AddXY(poynt, namey3)
                                'Me.Chart1.Series("mean").Points.AddXY(poynt, namey1)
                                Me.GraphDisplay.Series("reaches").Points.AddXY(poynt, namey1)
                                'MsgBox("bbb  " & featurename & "    " & featurenamelast & "   " & poyntprevious & "    " & poynt)
                            Else
                                Me.GraphDisplay.Series("mean").Points.AddXY(poynt, namey1)
                                Me.GraphDisplay.Series("confidence").Points.AddXY(poynt, namey2)
                                Me.GraphDisplay.Series("upper confidence").Points.AddXY(poynt, namey3)
                                'Me.Chart1.Series("reaches").Points.AddXY(poynt, namey1)
                                'MsgBox("ppp  " & featurename & "    " & featurenamelast & "   " & poyntprevious & "    " & poynt)
                            End If

                            'MsgBox(featurename & "    " & featurenamelast & "   " & poyntprevious & "    " & poynt)
                            If poynt = poyntprevious Then dequal03 = dequal03 + 1
                            If cell1 < 17 Or cell1 = 999 Then ' target ========================================================
                                If cell1 = 7 And mrqs = 1 Or cell1 = 10 And mrqs = 3 Or cell1 = 13 And mrqs = 2 Or cell1 = 16 And mrqs = 6 Then

                                    'Obj = CType(range.Cells(ip - 1, 36), Excel.Range)
                                    namey4 = c36
                                    If namey4 > 0.0001 Then
                                        Me.GraphDisplay.Series("target").Points.AddXY(poynt, namey4)
                                    End If
                                End If
                            End If ' target ===================================================================================
                            poyntprevious = poynt
                        Next

                        If plot_point_start = plot_point_end Then
                            If featurenamelast <> "d/s of above" Then
                                'MsgBox("last = " & featurenamelast & "  feattype = " & feattype & "  featlast = " & featlast)
                                If feattype = 3 And featlast = 3 And pnamey1 > -1.0 Then
                                    'Me.Chart1.Series("upper confidence").Points.Item(plot_point_end + dequal03).Label = featurenamelast
                                    feattype = 0 '################################
                                    featlast = 0 '################################
                                End If
                                featurenamelast = "      " '#####################
                                feattype = 0 '################################
                                featlast = 0 '################################
                            End If
                        End If ' If plot_point_start = plot_point_end

                    End If

                Else
                    If feattype = 999 Then Exit For
                End If ' If reachnumber = rstart Then

            Next ' loop on number of features ================================================================

            feattype = 0
            startreechcode = rnext
            If reachnumber = 0 Then Exit For
            If rnext = 0 Then Exit For
LastLine:
            dequal03 = 0
            featurenamelast = "....." '#####################
            featurename = "      " '######################
        Next ' loop on reaches

        FileClose(1)
        'My.Forms.GPlotting1.Visible = True

    End Sub

    Private Sub SelectOutputFileButton1_Click(sender As Object, e As EventArgs) Handles SelectOutputFileButton1.Click

        ReachCheckListBox1.Visible = False
        ChooseDeterminand.Visible = False 'True
        SelectOutputFileButton1.Visible = False
        Call produce_a_graph1(0)
        SelectDATAandRUN1.Visible = True

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles SelectDATAandRUN1.Click

        Statistic1.Visible = False
        Statistic2.Visible = False
        Statistic3.Visible = False
        Statistic4.Visible = False
        LoadStatistic5.Visible = False
        ReachCheckListBox1.Visible = False
        SelectDATAandRUN1.Visible = False
        ChooseReachesPlot.Visible = False
        ChooseDeterminand.Visible = False
        GPLOTQuit1.Visible = False
        BacktoSIMCAT1.Visible = False
        Call produce_a_graph1(1)
        SelectDATAandRUN1.Visible = True
        ChooseDeterminand.Visible = True
        Statistic1.Visible = True
        Statistic2.Visible = True
        Statistic3.Visible = True
        Statistic4.Visible = True
        LoadStatistic5.Visible = True
        ChooseReachesPlot.Visible = True
        ChooseDeterminand.Visible = True
        GPLOTQuit1.Visible = True
        BacktoSIMCAT1.Visible = True

    End Sub

    Private Sub Chooseplotdata1_Click(sender As Object, e As EventArgs) Handles ChooseDeterminand.Click

        ReachCheckListBox1.Visible = False
        ChooseDeterminand.Visible = False 'True
        SelectOutputFileButton1.Visible = False
        Call produce_a_graph1(0)
        SelectDATAandRUN1.Visible = True

    End Sub

    Private Sub RadioButton1_CheckedChanged(sender As Object, e As EventArgs) Handles Statistic1.CheckedChanged
        If Statistic1.Checked = True Then
            Statistic2.Checked = False
            Statistic3.Checked = False
            Statistic4.Checked = False
            LoadStatistic5.Checked = False
        End If
        'Me.Chart1.Series(0).Points.Clear()
    End Sub

    Private Sub RadioButton2_CheckedChanged(sender As Object, e As EventArgs) Handles Statistic2.CheckedChanged
        If Statistic2.Checked = True Then
            Statistic1.Checked = False
            Statistic3.Checked = False
            Statistic4.Checked = False
            LoadStatistic5.Checked = False
        End If
        'Me.Chart1.Series(0).Points.Clear()
        'Me.Chart1.Series(1).Points.Clear()
        'Me.Chart1.Series(2).Points.Clear()
        'Me.Chart1.Series(3).Points.Clear()
        'Me.Chart1.Series(4).Points.Clear()
        'Me.Chart1.Series(5).Points.Clear()
    End Sub

    Private Sub RadioButton3_CheckedChanged(sender As Object, e As EventArgs) Handles Statistic3.CheckedChanged
        If Statistic3.Checked = True Then
            Statistic2.Checked = False
            Statistic1.Checked = False
            Statistic4.Checked = False
            LoadStatistic5.Checked = False
        End If
        'Me.Chart1.Series(0).Points.Clear()
        'Me.Chart1.Series(1).Points.Clear()
        'Me.Chart1.Series(2).Points.Clear()
        'Me.Chart1.Series(3).Points.Clear()
        'Me.Chart1.Series(4).Points.Clear()
        'Me.Chart1.Series(5).Points.Clear()
    End Sub

    Private Sub RadioButton4_CheckedChanged(sender As Object, e As EventArgs) Handles Statistic4.CheckedChanged
        If Statistic4.Checked = True Then
            Statistic2.Checked = False
            Statistic3.Checked = False
            Statistic1.Checked = False
            LoadStatistic5.Checked = False
        End If
    End Sub

    Private Sub Button1_Click_1(sender As Object, e As EventArgs) Handles ClearButton1.Click
        If SelectDATAandRUN1.Visible = True Then
            'ReachCheckListBox1.Visible = False
            SelectDATAandRUN1.Visible = False
            ChooseReachesPlot.Visible = False
            ChooseDeterminand.Visible = False
            Me.GPLOTQuit1.Visible = False
            BacktoSIMCAT1.Visible = False
            Statistic1.Visible = False
            Statistic2.Visible = False
            Statistic3.Visible = False
            Statistic4.Visible = False
            LoadStatistic5.Visible = False
            GraphDisplay.Size = New System.Drawing.Size(1250, 630)
        Else
            'ReachCheckListBox1.Visible = True
            SelectDATAandRUN1.Visible = True
            ChooseReachesPlot.Visible = True
            ChooseDeterminand.Visible = True
            Me.GPLOTQuit1.Visible = True
            BacktoSIMCAT1.Visible = True
            Statistic1.Visible = True
            Statistic2.Visible = True
            Statistic3.Visible = True
            Statistic4.Visible = True
            LoadStatistic5.Visible = True
            GraphDisplay.Size = New System.Drawing.Size(1091, 630)
            'GraphDisplay.Size = Size()
        End If
    End Sub

    Private Sub RadioButton5_CheckedChanged(sender As Object, e As EventArgs) Handles LoadStatistic5.CheckedChanged
        If LoadStatistic5.Checked = True Then
            Statistic2.Checked = False
            Statistic3.Checked = False
            Statistic4.Checked = False
            Statistic1.Checked = False
        End If
    End Sub

    Private Sub ChooseReaches_Click(sender As Object, e As EventArgs) Handles ChooseReachesPlot.Click
        ReachCheckListBox1.Visible = True
    End Sub

    Private Sub ReleaseObjectxxx(ByVal obj As Object)
        Try
            System.Runtime.InteropServices.Marshal.FinalReleaseComObject(obj)
            obj = Nothing
        Catch ex As Exception
            obj = Nothing
        Finally
            GC.Collect()

            Try
                'Dim MSExcelControl() As Process
                Dim iID As Integer
                Dim lastOpen As DateTime
                Dim obj1(10) As Process
                obj1 = Process.GetProcessesByName("EXCEL")
                lastOpen = obj1(0).StartTime
                For Each p As Process In obj1

                    If lastOpen < p.StartTime Then
                        iID = p.Id
                        Exit For
                    End If

                Next

                For Each p As Process In obj1

                    If p.Id = iID Then
                        p.Kill()
                        Exit For
                    End If

                Next

            Catch ex As Exception

            End Try

        End Try
    End Sub

    'Sub ExcelClose()

    '   Clipboard.Clear()
    'With Myexcel

    '.application.displayalerts = False
    '.activeworkbook.close()
    '.application.displayalerts = True
    '.application.quit()

    'End With
    '   Myexcel = Nothing
    'End

    'End Sub

    Private Sub ReleaseObject(ByVal obj As Object)
        Try
            System.Runtime.InteropServices.Marshal.ReleaseComObject(obj)
            obj = Nothing
        Catch ex As Exception
            obj = Nothing
        Finally
            GC.Collect()
        End Try
    End Sub

End Class