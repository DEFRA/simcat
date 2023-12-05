Imports Excel = Microsoft.Office.Interop.Excel
Imports Word = Microsoft.Office.Interop.Word
Imports System
Imports System.IO
Imports System.Data.OleDb
Public Class Results
    Public numrepreaches As Integer
    Public startreach, endreach As Integer
    Dim next_reach(9999) As Integer
    Dim this_reach(9999) As Integer
    Public reportnumber
    Public LastfilenameREPORT As String '00000000000000000000000000000000000000000

    Public Property CheckOnClick As Boolean
    Private Sub ReturntoSIMCAT_Click(sender As Object, e As EventArgs) Handles ReturntoSIMCAT.Click
        My.Forms.Results.Close()
        My.Forms.SIMCAT159.Visible = True
    End Sub
    Private Sub ResultsQuit_Click(sender As Object, e As EventArgs) Handles ResultsQuit.Click
        My.Forms.Results.Close()
        My.Forms.SIMCAT159.Close()
        My.Forms.Calculate1.Close()
        Me.Close()
    End Sub
    Private Sub Results_Click(sender As Object, e As EventArgs)
        My.Forms.SIMCAT159.Close()
        startreach = 0
        endreach = 0
    End Sub
    Sub Produce_a_Report(RunSequence)

        Dim xlApp As Excel.Application
        Dim xlWorkBook As Excel.Workbook
        Dim xlWorkSheet As Excel.Worksheet
        Dim range As Excel.Range
        Dim Directory As String

        Dim Obj As Object
        Dim summary_statisic, lower_limit, upper_limit, standard, confidence As Single
        Dim observed, observed_lower_limit, observed_upper_limit, observed_confidence As Single
        Dim change_needed, change_needed_lower, change_needed_upper As Integer
        Dim mrqs As Integer
        Dim numfeatures, usedfeatures, countedfeatures, totalfeatures As Integer
        Dim reachnumber, rstart, rnextcode, numreaches As Integer
        Dim firstreach, lastreach As Integer
        Dim reachlength As Single
        Dim featurename, reachname As String
        Dim featuretype As Integer
        Dim determinandname As String
        Dim runninglength As Single
        Dim igo, ip1, ip2 As Integer
        Dim ip, ic, cell1, ic2 As Integer
        Dim highconf, goodconf, modconf, poorconf, badconf As Single
        Dim prop1, prop2, prop3 As Single
        Dim reachfeatures(200) As Integer
        Dim next_reach_master(999) As Integer 'the reach code number of the next reach
        Dim allreaches As Integer = 1
        Dim ireach As Integer
        Dim ExcelfilenameREPORT As String '00000000000000000000000000000000000

        RunReportButton1.Visible = False
        SelectOutputFile1.Visible = False

        If RunSequence = 0 Then 'value set in the call statement
            Using FileDialog As New OpenFileDialog

                FileDialog.Title = "Select your data file (and therefore your determinand)"
                FileDialog.Filter = "Micosoft Excel|*+W*|All Files|*.*"
                If FileDialog.ShowDialog() = DialogResult.OK Then ' this displays the box

                    xlApp = New Excel.Application
                    Directory = System.Environment.CurrentDirectory
                    ExcelfilenameREPORT = FileDialog.FileName
                    LastfilenameREPORT = FileDialog.FileName

                    xlWorkBook = xlApp.Workbooks.Open(ExcelfilenameREPORT)
                    xlWorkSheet = xlWorkBook.Worksheets(1)
                    range = xlWorkSheet.UsedRange
                    Obj = CType(range.Cells(2, 5), Excel.Range) : numreaches = Obj.value
                    If numreaches = 0 Then Return ' mo data specified 

                    If startreach > 0 Then
                        ReachCheckListBox2.Items.Clear()
                        startreach = 0 ' initialise the number of the starting reach
                        endreach = 0 ' initialise the number of the final reach to be dealt with
                    End If
                    For ip = 1 To 999
                        next_reach(ip) = 0 ' initialise the numbers of the next reach after each reach 
                        'next_reach_master(ip) = -999
                    Next

                    numrepreaches = 0 ' initialise the total number of reaches =============
                    For ip = 3 To 9999 'read the data on all the reaches - =================
                        Obj = CType(range.Cells(ip, 2), Excel.Range) ' name of reach
                        ReachCheckListBox2.Items.Add(Obj.value, CheckState.Unchecked)
                        Obj = CType(range.Cells(ip, 4), Excel.Range)
                        next_reach(ip - 2) = Obj.value
                        If next_reach(ip - 2) = 0 Then
                            numrepreaches = ip - 2
                            Exit For
                        End If
                    Next '=============================================================================================
                End If ' If FileDialog.ShowDialog() = DialogResult.OK Then
            End Using
            ReachCheckListBox2.Visible = True
            'ReachCheckListBox2.Items.Clear()
            ReachCheckListBox2.Enabled = True
            ReachCheckListBox2.CheckOnClick = True
            ChooseReachesResults.Visible = True
            Return
        Else 'then RunSequence = 1
            ExcelfilenameREPORT = LastfilenameREPORT
        End If 'if RunSequence = 0


        xlApp = New Excel.Application
        xlWorkBook = xlApp.Workbooks.Open(ExcelfilenameREPORT)
        xlWorkSheet = xlWorkBook.Worksheets(1)
        range = xlWorkSheet.UsedRange

        firstreach = 0 : lastreach = 0
        startreach = 0 : endreach = 0
        RunReportButton1.Visible = True
        Obj = CType(range.Cells(2, 5), Excel.Range) : numreaches = Obj.value

        If startreach <= 0 Then
            For ip = 1 To numrepreaches 'read the data on all the reaches - ===========================================
                Obj = CType(range.Cells(ip + 2, 2), Excel.Range) ' name of the reach
                reachname = Obj.text
                If firstreach = 0 Then
                    If ReachCheckListBox2.GetItemChecked(ip - 1) = True Then
                        Obj = CType(range.Cells(ip + 2, 3), Excel.Range) ' the number of the reach
                        this_reach(ip) = Obj.value ' code number of this starting reach
                        firstreach = Obj.value ' code number of this starting reach
                        Obj = CType(range.Cells(ip + 2, 4), Excel.Range) ' the code number of the next reach
                        next_reach(ip) = Obj.value ' code number of the next reach
                        next_reach_master(this_reach(ip)) = Obj.value ' code number of the next reach
                        startreach = ip ' added
                    End If
                Else ' for subsequent reaches
                    Obj = CType(range.Cells(ip + 2, 3), Excel.Range) ' the code number of the reach
                    this_reach(ip) = Obj.value ' code number of this reach
                    Obj = CType(range.Cells(ip + 2, 4), Excel.Range) ' the code number of the next reach
                    next_reach(ip) = Obj.value '  code number of the next reach
                    next_reach_master(this_reach(ip)) = Obj.value ' set this for the code number of the reach
                    If next_reach_master(this_reach(ip)) = 0 Then
                        endreach = ip ' list number of the last reach 
                        Exit For
                    End If

                    If ReachCheckListBox2.GetItemChecked(ip - 1) = True Then
                        lastreach = ip
                        Obj = CType(range.Cells(ip + 2, 3), Excel.Range) ' code number of reach
                        this_reach(ip) = Obj.value
                        next_reach_master(this_reach(ip)) = Obj.value
                    End If

                End If
                If firstreach > 0 And lastreach > 0 Then
                    Exit For
                End If
            Next '=====================================================================================================
        End If

        If startreach = 0 Then
            startreach = 1
            lastreach = numrepreaches
        End If

        endreach = lastreach
        startreach = 0
        endreach = 0

        For ireach = 0 To numreaches - 1
            If startreach = 0 Then
                If ReachCheckListBox2.GetItemChecked(ireach) = True Then
                    startreach = ireach + 1
                    If ireach = numreaches - 1 Then endreach = startreach
                End If
            Else
                If endreach = 0 Then
                    If ReachCheckListBox2.GetItemChecked(ireach) = True Then endreach = ireach + 1
                End If
            End If
        Next ' For ireach = 0 To numreaches - 1

        ReturntoSIMCAT.Visible = True
        ResultsQuit.Visible = True
        RunReportButton1.Visible = True

        ResultsWAIT2.Visible = True : ResultsWAIT2.BringToFront()
        SelectOutputFile1.Visible = False
        ChooseReachesResults.Visible = False
        MeanResultsRadioButton1.Visible = False : LoadResultsRadioButton1.Visible = False
        ComplianceResultsRadioButton1.Visible = False : ClassificationResultsRadioButton1.Visible = False
        MeanApportionRadioButton1.Visible = False

        If startreach > 0 And endreach <= 0 Then endreach = startreach
        If startreach <= 0 And endreach > 0 Then startreach = endreach

        Dim oWord As Word.Application
        Dim oDoc As Word.Document
        Dim oTable As Word.Table
        Dim oPara1 As Word.Paragraph, oPara3 As Word.Paragraph

        oWord = CreateObject("Word.Application")
        oWord.Visible = True
        oWord.ShowWindowsInTaskbar = False
        oDoc = oWord.Documents.Add

        oPara1 = oDoc.Content.Paragraphs.Add
        oPara1.Range.Font.Bold = False : oPara1.Range.Font.Size = 11
        oPara1.Range.Text = "SIMCAT 161" & "            Calculations: " + Format(Now, "dd MMMM yyyy") + " at " + Format(TimeValue(Now), "hh:mm")
        oPara1.Format.SpaceAfter = 0
        oPara1.Range.InsertParagraphAfter()
        oPara1.Range.InsertParagraphAfter()

        Obj = CType(range.Cells(1, 1), Excel.Range) 'the heading from the CSV file
        Obj = CType(range.Cells(2, 1), Excel.Range) : determinandname = Obj.text
        Obj = CType(range.Cells(2, 4), Excel.Range) : mrqs = Obj.value 'the type of target of the determinand ...
        Obj = CType(range.Cells(2, 5), Excel.Range) : numreaches = Obj.value 'the number of reaches ...

        For ip = numreaches + 4 To 9999 'read the data on all the features =========================================
            Obj = CType(range.Cells(ip, 3), Excel.Range) ' the number of the reach of the next feature============== 
            If Obj.Value = 0 Then ' the last feature has been found ================================================
                numfeatures = ip - 4 - numreaches ' 
                Exit For
            End If
        Next
        '============================================================================================================

        'FileOpen(5, "Details.TMP", OpenMode.Output)
        'PrintLine(5, "======================================================================================")
        'PrintLine(5, "startreach = ", startreach, "endreach = ", endreach, " in the list")
        'FileClose(5)

        If allreaches = 0 Then ' no reaches are requested - do the first reach ======================================
            ip2 = numreaches + 4 : totalfeatures = 0
            rstart = startreach : rnextcode = next_reach(rstart)
            For ireach = rstart To endreach 'scan the reaches =======================================================
                If ireach = rstart Then
                    countedfeatures = 0 : igo = 0
                    For ip = ip2 To numfeatures + numreaches + 4 '===================================================
                        Obj = CType(range.Cells(ip, 3), Excel.Range) : reachnumber = Obj.value
                        If reachnumber = rstart Then
                            ip2 = ip2 + 1
                            countedfeatures = countedfeatures + 1
                            igo = 99
                        Else
                            If igo = 99 Then
                                Exit For
                            End If
                        End If
                    Next
                    reachfeatures(ireach) = countedfeatures
                    totalfeatures = totalfeatures + reachfeatures(ireach)
                Else
                    If ireach <> rstart Then
                        rstart = rnextcode : rnextcode = next_reach(rstart)
                        If ireach = rstart Then ireach = ireach - 1
                    End If
                End If
            Next ' scan the reaches ===================================================================================
        End If ' If allreaches = 0 ==================================================================================== 

        allreaches = 1

        If allreaches = 1 Then ' ======================================================================================
            ip2 = numreaches + 4 ' set up for reading data on features ================================================
            totalfeatures = 0 ' initialise the total number of features ===============================================
            For ireach = startreach To endreach ' loop through all the reaches in the list ============================
                reachfeatures(ireach) = 0 ' initialise the number of features for this reach ==========================
                For ip = ip2 To numfeatures + numreaches + 3 ' loop through the list of features ======================
                    Obj = CType(range.Cells(ip, 3), Excel.Range) : reachnumber = Obj.value ' the reach code number ====
                    If reachnumber = this_reach(ireach) Then
                        reachfeatures(reachnumber) = reachfeatures(reachnumber) + 1
                        totalfeatures = totalfeatures + 1
                    End If

                Next 'For ip = ip2 To numfeatures + numreaches + 3 ' loop through the list of features ================

            Next 'For ireach = startreach To endreach ... loop through all the reaches in the list ====================

        End If ' If allreaches = 1 ====================================================================================




        If MeanResultsRadioButton1.Checked = True Then ' write report on mean concentrations ==========================
            rstart = startreach
            If allreaches = 0 Then rnextcode = next_reach(startreach)
            If allreaches = 1 Then rnextcode = next_reach(startreach)

            oPara3 = oDoc.Content.Paragraphs.Add
            oPara3.Range.Text = "Annual Mean Concentrations for " & determinandname
            oPara3.Range.Font.Bold = True : oPara3.Range.Font.Size = 11
            oPara3.Format.SpaceAfter = 0
            oPara3.Range.InsertParagraphAfter()
            oPara3.Range.InsertParagraphAfter()

            oTable = oDoc.Tables.Add(oDoc.Bookmarks.Item("\endofdoc").Range, totalfeatures + 1, 4)
            oTable.Range.ParagraphFormat.SpaceAfter = 2
            oTable.Range.Font.Bold = False
            oTable.Range.Font.Size = 10 : oTable.Range.Font.Color = 10
            oTable.Borders.Enable = True
            oTable.Columns.Item(1).Width = oWord.InchesToPoints(2.3) 'change the width of the columns ...
            oTable.Columns.Item(2).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(3).Width = oWord.InchesToPoints(0.6)
            oTable.Columns.Item(4).Width = oWord.InchesToPoints(1.0)
            oTable.Cell(1, 1).Range.Text = "LOCATION" ' table headings ================================================
            oTable.Cell(1, 2).Range.Text = "Distance"
            oTable.Cell(1, 3).Range.Text = "Mean"
            oTable.Cell(1, 4).Range.Text = "Confidence"
            next_reach(endreach) = 0

            usedfeatures = 0
            For ireach = startreach To endreach ' process the requested sequence of reaches ===========================
                If this_reach(ireach) <> this_reach(startreach) Then ' after the first reach
                    rstart = rnextcode
                    If allreaches = 0 Then rnextcode = next_reach(startreach)
                    If allreaches = 1 Then rnextcode = this_reach(rnextcode)
                    If rstart = endreach Then rnextcode = 0
                End If

                ip1 = numreaches + 4
                For ip = ip1 To numfeatures + numreaches + 4 '========== from first to last features modelled =========
                    Obj = CType(range.Cells(ip, 3), Excel.Range) : reachnumber = Obj.value
                    If (reachnumber = this_reach(ireach)) Then
                        Obj = CType(range.Cells(ip, 1), Excel.Range) : featurename = Obj.text

                        Obj = CType(range.Cells(ip, 2), Excel.Range) : reachname = Obj.text
                        Obj = CType(range.Cells(ip, 4), Excel.Range) : featuretype = Obj.value
                        Obj = CType(range.Cells(ip, 6), Excel.Range) : reachlength = Obj.value
                        Obj = CType(range.Cells(ip, 7), Excel.Range) : runninglength = 0.1 * Int(10.0 * (Obj.value + 0.05))
                        Obj = CType(range.Cells(ip, 8), Excel.Range) : summary_statisic = Obj.value
                        Obj = CType(range.Cells(ip, 9), Excel.Range) : lower_limit = Obj.value
                        Obj = CType(range.Cells(ip, 10), Excel.Range) : upper_limit = Obj.value
                        usedfeatures = usedfeatures + 1

                        ic = usedfeatures + 1
                        If featurename = "Start of Reach" Or featurename = "End of Reach" Then
                            oTable.Cell(ic, 1).Range.Text = featurename + " " + reachname
                        Else
                            oTable.Cell(ic, 1).Range.Text = featurename
                        End If

                        oTable.Cell(ic, 2).Range.Text = Format(runninglength, "0.00")
                        oTable.Cell(ic, 3).Range.Text = EDec(summary_statisic)
                        oTable.Cell(ic, 4).Range.Text = "(" + EDec1(lower_limit) & " - " + EDec1(upper_limit) + ")"
                    End If

                Next ' loop on number of features =====================================================================

            Next ' loop on reaches ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        End If




        If LoadResultsRadioButton1.Checked = True Then ' write WORD report on mean loads ==============================
            rstart = startreach
            If allreaches = 0 Then rnextcode = next_reach(startreach)
            If allreaches = 1 Then rnextcode = next_reach(startreach)

            oPara3 = oDoc.Content.Paragraphs.Add
            oPara3.Range.Text = "Annual Mean Loads for " & determinandname
            oPara3.Range.Font.Bold = True : oPara3.Range.Font.Size = 11
            oPara3.Format.SpaceAfter = 0
            oPara3.Range.InsertParagraphAfter()
            oPara3.Range.InsertParagraphAfter()

            oTable = oDoc.Tables.Add(oDoc.Bookmarks.Item("\endofdoc").Range, totalfeatures + 1, 4)
            oTable.Range.ParagraphFormat.SpaceAfter = 2
            oTable.Range.Font.Bold = False
            oTable.Range.Font.Size = 10 : oTable.Range.Font.Color = 10
            oTable.Borders.Enable = True
            oTable.Columns.Item(1).Width = oWord.InchesToPoints(2.3) 'change the width of the columns ...
            oTable.Columns.Item(2).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(3).Width = oWord.InchesToPoints(0.8)
            oTable.Columns.Item(4).Width = oWord.InchesToPoints(1.2)
            oTable.Cell(1, 1).Range.Text = "LOCATION" ' table headings ================================================
            oTable.Cell(1, 2).Range.Text = "Distance"
            oTable.Cell(1, 3).Range.Text = "Mean Load"
            oTable.Cell(1, 4).Range.Text = "Confidence"
            next_reach(endreach) = 0

            usedfeatures = 0
            For ireach = startreach To endreach ' process all the reaches =============================================

                If this_reach(ireach) <> this_reach(startreach) Then ' after the first reach
                    rstart = rnextcode
                    If allreaches = 0 Then rnextcode = next_reach(startreach)
                    If allreaches = 1 Then rnextcode = this_reach(rnextcode)
                    If rstart = endreach Then rnextcode = 0
                End If

                ip1 = numreaches + 4
                For ip = ip1 To numfeatures + numreaches + 4 '=========================================================
                    Obj = CType(range.Cells(ip, 3), Excel.Range) : reachnumber = Obj.value
                    If (reachnumber = this_reach(ireach)) Then
                        Obj = CType(range.Cells(ip, 1), Excel.Range) : featurename = Obj.text
                        Obj = CType(range.Cells(ip, 2), Excel.Range) : reachname = Obj.text
                        Obj = CType(range.Cells(ip, 4), Excel.Range) : featuretype = Obj.value
                        Obj = CType(range.Cells(ip, 6), Excel.Range) : reachlength = Obj.value
                        Obj = CType(range.Cells(ip, 7), Excel.Range) : runninglength = 0.1 * Int(10.0 * (Obj.value + 0.05))
                        Obj = CType(range.Cells(ip, 20), Excel.Range) : summary_statisic = Obj.value
                        Obj = CType(range.Cells(ip, 21), Excel.Range) : lower_limit = Obj.value
                        Obj = CType(range.Cells(ip, 22), Excel.Range) : upper_limit = Obj.value

                        usedfeatures = usedfeatures + 1

                        ic = usedfeatures + 1
                        If featurename = "Start of Reach" Or featurename = "End of Reach" Then
                            oTable.Cell(ic, 1).Range.Text = featurename + " " + reachname
                        Else
                            oTable.Cell(ic, 1).Range.Text = featurename
                        End If

                        oTable.Cell(ic, 2).Range.Text = Format(runninglength, "0.00")
                        oTable.Cell(ic, 3).Range.Text = EDec(summary_statisic)
                        oTable.Cell(ic, 4).Range.Text = "(" + EDec1(lower_limit) & " - " + EDec1(upper_limit) + ")"
                    End If

                Next ' loop on number of features =====================================================================

            Next ' loop on reaches ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        End If





        If ComplianceResultsRadioButton1.Checked = True Then ' write WORD report on compliance ========================
            rstart = startreach
            If allreaches = 0 Then rnextcode = next_reach(startreach)
            If allreaches = 1 Then rnextcode = next_reach(startreach)

            oPara3 = oDoc.Content.Paragraphs.Add
            oPara3.Range.Text = "Compliance with Standards for " & determinandname
            oPara3.Range.Font.Bold = True : oPara3.Range.Font.Size = 11
            oPara3.Format.SpaceAfter = 0
            oPara3.Range.InsertParagraphAfter()
            oPara3.Range.InsertParagraphAfter()

            oTable = oDoc.Tables.Add(oDoc.Bookmarks.Item("\endofdoc").Range, totalfeatures + 1, 6)
            oTable.Range.ParagraphFormat.SpaceAfter = 2
            oTable.Range.Font.Bold = False
            oTable.Range.Font.Size = 10 : oTable.Range.Font.Color = 10
            oTable.Borders.Enable = True
            oTable.Columns.Item(1).Width = oWord.InchesToPoints(2.3) 'change the width of the columns ...
            oTable.Columns.Item(2).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(3).Width = oWord.InchesToPoints(0.6)
            oTable.Columns.Item(4).Width = oWord.InchesToPoints(1.0)
            oTable.Columns.Item(5).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(6).Width = oWord.InchesToPoints(1.01)
            oTable.Cell(1, 1).Range.Text = "LOCATION" ' table headings ==================================================
            oTable.Cell(1, 2).Range.Text = "Distance"
            If mrqs = 1 Then oTable.Cell(1, 3).Range.Text = "Mean"
            If mrqs = 3 Then oTable.Cell(1, 3).Range.Text = "90-%ile"
            If mrqs = 2 Then oTable.Cell(1, 3).Range.Text = "95-%ile"
            If mrqs = 6 Then oTable.Cell(1, 3).Range.Text = "99-%ile"
            If mrqs = 5 Then oTable.Cell(1, 3).Range.Text = "10-%ile"
            oTable.Cell(1, 4).Range.Text = "Confidence"
            oTable.Cell(1, 5).Range.Text = "Standard"
            oTable.Cell(1, 6).Range.Text = "Confidence (%)"

            For ireach = startreach To endreach 'numreaches ' process all the reaches =====================================

                If this_reach(ireach) <> this_reach(startreach) Then ' after the first reach
                    rstart = rnextcode
                    If allreaches = 0 Then rnextcode = next_reach(startreach)
                    If allreaches = 1 Then rnextcode = this_reach(rnextcode)
                    If rstart = endreach Then rnextcode = 0
                End If

                ip1 = numreaches + 4
                For ip = ip1 To numfeatures + numreaches + 4 '==========================================================
                    Obj = CType(range.Cells(ip, 3), Excel.Range) : reachnumber = Obj.value
                    If (reachnumber = this_reach(ireach)) Then
                        Obj = CType(range.Cells(ip, 1), Excel.Range) : featurename = Obj.text
                        Obj = CType(range.Cells(ip, 2), Excel.Range) : reachname = Obj.text
                        Obj = CType(range.Cells(ip, 4), Excel.Range) : featuretype = Obj.value
                        Obj = CType(range.Cells(ip, 6), Excel.Range) : reachlength = Obj.value
                        Obj = CType(range.Cells(ip, 7), Excel.Range) : runninglength = 0.1 * Int(10.0 * (Obj.value + 0.05))
                        If mrqs = 1 Then cell1 = 7
                        If mrqs = 3 Then cell1 = 10
                        If mrqs = 2 Then cell1 = 13
                        If mrqs = 6 Then cell1 = 16
                        If mrqs = 5 Then cell1 = 10
                        Obj = CType(range.Cells(ip, cell1 + 1), Excel.Range) : summary_statisic = Obj.value
                        Obj = CType(range.Cells(ip, cell1 + 2), Excel.Range) : lower_limit = Obj.value
                        Obj = CType(range.Cells(ip, cell1 + 3), Excel.Range) : upper_limit = Obj.value
                        Obj = CType(range.Cells(ip, 36), Excel.Range) : standard = Obj.value
                        Obj = CType(range.Cells(ip, 37), Excel.Range) : confidence = Obj.value
                        usedfeatures = usedfeatures + 1

                        ic = usedfeatures + 1
                        If featurename = "Start of Reach" Or featurename = "End of Reach" Then
                            oTable.Cell(ic, 1).Range.Text = featurename + " " + reachname
                        Else
                            oTable.Cell(ic, 1).Range.Text = featurename
                        End If
                        oTable.Cell(ic, 2).Range.Text = Format(runninglength, "0.00")
                        oTable.Cell(ic, 3).Range.Text = EDec(summary_statisic)
                        oTable.Cell(ic, 4).Range.Text = "(" + EDec1(lower_limit) & " - " + EDec1(upper_limit) + ")"
                    End If

                    If standard > 0.000001 Then
                        oTable.Cell(ic, 5).Range.Text = EDec(standard)
                        oTable.Cell(ic, 6).Range.Text = EDec(confidence)
                    End If

                    If cell1 < -17 Then ' observed
                        ic2 = 38
                        If mrqs = 3 Then ic2 = 41
                        If mrqs = 2 Then ic2 = 44
                        If mrqs = 6 Then ic2 = 47
                    End If

                Next ' loop on number of features =====================================================================

            Next ' loop on reaches ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        End If




        If ClassificationResultsRadioButton1.Checked = True Then ' write WORD report on classification ================
            rstart = startreach
            If allreaches = 0 Then rnextcode = next_reach(startreach)
            If allreaches = 1 Then rnextcode = next_reach(startreach)

            oPara3 = oDoc.Content.Paragraphs.Add
            oPara3.Range.Text = "Classification for " & determinandname
            oPara3.Range.Font.Bold = True : oPara3.Range.Font.Size = 11
            oPara3.Format.SpaceAfter = 0
            oPara3.Range.InsertParagraphAfter()
            oPara3.Range.InsertParagraphAfter()

            oTable = oDoc.Tables.Add(oDoc.Bookmarks.Item("\endofdoc").Range, totalfeatures + 1, 7)
            oTable.Range.ParagraphFormat.SpaceAfter = 2
            oTable.Range.Font.Bold = False
            oTable.Range.Font.Size = 10 : oTable.Range.Font.Color = 10
            oTable.Borders.Enable = True
            oTable.Columns.Item(1).Width = oWord.InchesToPoints(2.3) 'change the width of the columns ...
            oTable.Columns.Item(2).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(3).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(4).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(5).Width = oWord.InchesToPoints(0.75)
            oTable.Columns.Item(6).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(7).Width = oWord.InchesToPoints(0.7)
            oTable.Cell(1, 1).Range.Text = "LOCATION" ' table headings ====================================================
            oTable.Cell(1, 2).Range.Text = "Distance"
            oTable.Cell(1, 3).Range.Text = "High"
            oTable.Cell(1, 4).Range.Text = "Good"
            oTable.Cell(1, 5).Range.Text = "Moderate"
            oTable.Cell(1, 6).Range.Text = "Poor"
            oTable.Cell(1, 7).Range.Text = "Bad"

            For ireach = startreach To endreach 'numreaches ' process all the reaches =====================================

                If this_reach(ireach) <> this_reach(startreach) Then ' after the first reach
                    rstart = rnextcode
                    If allreaches = 0 Then rnextcode = next_reach(startreach)
                    If allreaches = 1 Then rnextcode = this_reach(rnextcode)
                    If rstart = endreach Then rnextcode = 0
                End If

                ip1 = numreaches + 4
                For ip = ip1 To numfeatures + numreaches + 4 '==========================================================
                    Obj = CType(range.Cells(ip, 3), Excel.Range) : reachnumber = Obj.value
                    If (reachnumber = this_reach(ireach)) Then
                        Obj = CType(range.Cells(ip, 1), Excel.Range) : featurename = Obj.text
                        Obj = CType(range.Cells(ip, 2), Excel.Range) : reachname = Obj.text
                        Obj = CType(range.Cells(ip, 4), Excel.Range) : featuretype = Obj.value
                        Obj = CType(range.Cells(ip, 6), Excel.Range) : reachlength = Obj.value
                        Obj = CType(range.Cells(ip, 7), Excel.Range) : runninglength = 0.1 * Int(10.0 * (Obj.value + 0.05))
                        Obj = CType(range.Cells(ip, 38), Excel.Range) : highconf = Obj.value
                        Obj = CType(range.Cells(ip, 39), Excel.Range) : goodconf = Obj.value
                        Obj = CType(range.Cells(ip, 40), Excel.Range) : modconf = Obj.value
                        Obj = CType(range.Cells(ip, 41), Excel.Range) : poorconf = Obj.value
                        Obj = CType(range.Cells(ip, 42), Excel.Range) : badconf = Obj.value
                        usedfeatures = usedfeatures + 1

                        ic = usedfeatures + 1
                        If featurename = "Start of Reach" Or featurename = "End of Reach" Then
                            oTable.Cell(ic, 1).Range.Text = featurename + " " + reachname
                        Else
                            oTable.Cell(ic, 1).Range.Text = featurename
                        End If

                        oTable.Cell(ic, 2).Range.Text = Format(runninglength, "0.00")
                        oTable.Cell(ic, 3).Range.Text = Format(highconf, "0.00")
                        oTable.Cell(ic, 4).Range.Text = Format(goodconf, "0.00")
                        oTable.Cell(ic, 5).Range.Text = Format(modconf, "0.00")
                        oTable.Cell(ic, 6).Range.Text = Format(poorconf, "0.00")
                        oTable.Cell(ic, 7).Range.Text = Format(badconf, "0.00")
                    End If

                Next ' loop on number of features =====================================================================

            Next ' loop on reaches ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        End If




        If MeanApportionRadioButton1.Checked = True Then ' write WORD report on apportionment of mean concentration ===
            rstart = startreach
            If allreaches = 0 Then rnextcode = next_reach(startreach)
            If allreaches = 1 Then rnextcode = next_reach(startreach)

            oPara3 = oDoc.Content.Paragraphs.Add
            oPara3.Range.Text = "Mean concentration added to river for: " & determinandname
            oPara3.Range.Font.Bold = True : oPara3.Range.Font.Size = 11
            oPara3.Format.SpaceAfter = 0
            oPara3.Range.InsertParagraphAfter()
            oPara3.Range.InsertParagraphAfter()

            oTable = oDoc.Tables.Add(oDoc.Bookmarks.Item("\endofdoc").Range, totalfeatures + 1, 5)
            oTable.Range.ParagraphFormat.SpaceAfter = 2
            oTable.Range.Font.Bold = False
            oTable.Range.Font.Size = 10 : oTable.Range.Font.Color = 10
            oTable.Borders.Enable = True
            oTable.Columns.Item(1).Width = oWord.InchesToPoints(2.3) 'change the width of the columns ...
            oTable.Columns.Item(2).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(3).Width = oWord.InchesToPoints(0.6)
            oTable.Columns.Item(4).Width = oWord.InchesToPoints(0.8)
            oTable.Columns.Item(5).Width = oWord.InchesToPoints(0.8)
            oTable.Cell(1, 1).Range.Text = "LOCATION" ' table headings ==================================================
            oTable.Cell(1, 2).Range.Text = "Distance"
            oTable.Cell(1, 3).Range.Text = "Mean"
            oTable.Cell(1, 4).Range.Text = "Discharges"
            oTable.Cell(1, 5).Range.Text = "Diffuse"

            For ireach = startreach To endreach 'numreaches ' process all the reaches =====================================

                If this_reach(ireach) <> this_reach(startreach) Then ' after the first reach
                    rstart = rnextcode
                    If allreaches = 0 Then rnextcode = next_reach(startreach)
                    If allreaches = 1 Then rnextcode = this_reach(rnextcode)
                    If rstart = endreach Then rnextcode = 0
                End If

                ip1 = numreaches + 4
                For ip = ip1 To numfeatures + numreaches + 4 '==========================================================
                    Obj = CType(range.Cells(ip, 3), Excel.Range) : reachnumber = Obj.value
                    If (reachnumber = this_reach(ireach)) Then
                        Obj = CType(range.Cells(ip, 1), Excel.Range) : featurename = Obj.text
                        Obj = CType(range.Cells(ip, 2), Excel.Range) : reachname = Obj.text
                        Obj = CType(range.Cells(ip, 4), Excel.Range) : featuretype = Obj.value
                        Obj = CType(range.Cells(ip, 6), Excel.Range) : reachlength = Obj.value
                        Obj = CType(range.Cells(ip, 7), Excel.Range) : runninglength = 0.1 * Int(10.0 * (Obj.value + 0.05))
                        Obj = CType(range.Cells(ip, 8), Excel.Range) : summary_statisic = Obj.value
                        Obj = CType(range.Cells(ip, 76), Excel.Range) : prop1 = Obj.value
                        Obj = CType(range.Cells(ip, 108), Excel.Range) : prop2 = Obj.value
                        Obj = CType(range.Cells(ip, 107), Excel.Range) : prop3 = Obj.value

                        'prop2 = prop2 + prop3

                        '==========================================================================
                        'STORED CONTRIBUTIONS OF LOAD =============================================
                        '==========================================================================
                        '43  76  ( 1) = 'Mean from all discharges (3, 12, 5, 39, 60, 61 and 62)'
                        '44  77  ( 2) = 'Added by sewage effluents                      (3)'
                        '45  78  ( 3) = 'Intermittent discharges of sewage             (12)'
                        '46  79  ( 4) = 'Added by industrial discharges                 (5)'
                        '47  80  ( 5) = 'Added by mine waters                          (39)'
                        '48  81  ( 6) = 'Diffuse pollution from livestock              (25)'
                        '49  82  ( 7) = 'Diffuse pollution from arable                 (27)'
                        '50  83  ( 8) = 'Highway runoff                                (29)'
                        '51  84  ( 9) = 'Urban runoff                                  (31)'
                        '52  85  (10) = 'Atmosphere deposition                         (33)'
                        '53  86  (11) = 'Natural background                            (35)'
                        '54  87  (12) = 'Septic tanks                                  (37)'
                        '55  88  (13) = 'Aggregate CSOs                                (40)'
                        '56  89  (14) = 'Aggregated STWs                               (42)'
                        '57  90  (15) = 'Diffuse mines                                 (46)'
                        '58  91  (16) = 'Birds, boats and angling                      (48)'
                        '59  92  (17) = 'Boundaries and tributaries              (2 and 10)'
                        '60  93  (18) = 'Diffuse input                                 (13)'
                        '61  94  (19) = 'Diffuse input                                 (15)'
                        '62  95  (20) = 'Reach diffuse                                     '
                        '63  96  (21) = 'Gap filling of flows                              '
                        '64  97  (22) = 'Gap filling of quality                            '
                        '65  98  (23) = 'User-named diffuse input                      (50)'
                        '66  99  (24) = 'User-named diffuse input                      (52)'
                        '67 100  (25) = 'User-named diffuse input                      (54)'
                        '68 101  (26) = 'User-named diffuse input                      (56)'
                        '69 103  (27) = 'User-named diffuse input                      (58)'
                        '70 104  (28) = 'Other point sources                           (60)'
                        '71 105  (29) = 'Private wastewaters                           (61)'
                        '72 106  (30) = 'Fish farms                                    (62)'
                        '73 107  (31) = 'Blank                                             '
                        '74 108  (32) = 'Total                                             '
                        '75 109  (33) = 'Total diffuse                                     '
                        '===========================================================================
                        usedfeatures = usedfeatures + 1

                        ic = usedfeatures + 1
                        If featurename = "Start of Reach" Or featurename = "End of Reach" Then
                            oTable.Cell(ic, 1).Range.Text = featurename + " " + reachname
                        Else
                            oTable.Cell(ic, 1).Range.Text = featurename
                        End If

                        oTable.Cell(ic, 2).Range.Text = Format(runninglength, "0.00")
                        oTable.Cell(ic, 3).Range.Text = EDec(summary_statisic)
                        oTable.Cell(ic, 4).Range.Text = EDec(prop1)
                        oTable.Cell(ic, 5).Range.Text = EDec(prop2)
                    End If

                Next ' loop on number of features =====================================================================

            Next ' loop on reaches ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        End If ' apportionment of mean concentration ==================================================================




        If LoadApportionRadioButton1.Checked = True Then ' write WORD report on apportionment of mean load ============
            rstart = startreach
            If allreaches = 0 Then rnextcode = next_reach(startreach)
            If allreaches = 1 Then rnextcode = next_reach(startreach)

            oPara3 = oDoc.Content.Paragraphs.Add
            oPara3.Range.Text = "Load added to river for: " & determinandname
            oPara3.Range.Font.Bold = True : oPara3.Range.Font.Size = 11
            oPara3.Format.SpaceAfter = 0
            oPara3.Range.InsertParagraphAfter()
            oPara3.Range.InsertParagraphAfter()

            oTable = oDoc.Tables.Add(oDoc.Bookmarks.Item("\endofdoc").Range, totalfeatures + 1, 5)
            oTable.Range.ParagraphFormat.SpaceAfter = 2
            oTable.Range.Font.Bold = False
            oTable.Range.Font.Size = 10 : oTable.Range.Font.Color = 10
            oTable.Borders.Enable = True
            oTable.Columns.Item(1).Width = oWord.InchesToPoints(2.3) 'change the width of the columns ...
            oTable.Columns.Item(2).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(3).Width = oWord.InchesToPoints(0.6)
            oTable.Columns.Item(4).Width = oWord.InchesToPoints(0.8)
            oTable.Columns.Item(5).Width = oWord.InchesToPoints(0.8)
            oTable.Cell(1, 1).Range.Text = "LOCATION" ' table headings ==================================================
            oTable.Cell(1, 2).Range.Text = "Distance"
            oTable.Cell(1, 3).Range.Text = "Mean load"
            oTable.Cell(1, 4).Range.Text = "Discharges"
            oTable.Cell(1, 5).Range.Text = "Diffuse"

            For ireach = startreach To endreach 'numreaches ' process all the reaches =====================================

                If this_reach(ireach) <> this_reach(startreach) Then ' after the first reach
                    rstart = rnextcode
                    If allreaches = 0 Then rnextcode = next_reach(startreach)
                    If allreaches = 1 Then rnextcode = this_reach(rnextcode)
                    If rstart = endreach Then rnextcode = 0
                End If

                ip1 = numreaches + 4
                For ip = ip1 To numfeatures + numreaches + 4 '==========================================================
                    Obj = CType(range.Cells(ip, 3), Excel.Range) : reachnumber = Obj.value
                    If (reachnumber = this_reach(ireach)) Then
                        Obj = CType(range.Cells(ip, 1), Excel.Range) : featurename = Obj.text
                        Obj = CType(range.Cells(ip, 2), Excel.Range) : reachname = Obj.text
                        Obj = CType(range.Cells(ip, 4), Excel.Range) : featuretype = Obj.value
                        Obj = CType(range.Cells(ip, 6), Excel.Range) : reachlength = Obj.value
                        Obj = CType(range.Cells(ip, 7), Excel.Range) : runninglength = 0.1 * Int(10.0 * (Obj.value + 0.05))
                        Obj = CType(range.Cells(ip, 20), Excel.Range) : summary_statisic = Obj.value
                        Obj = CType(range.Cells(ip, 43), Excel.Range) : prop1 = Obj.value
                        Obj = CType(range.Cells(ip, 75), Excel.Range) : prop2 = Obj.value
                        Obj = CType(range.Cells(ip, 76), Excel.Range) : prop3 = Obj.value
                        prop2 = prop2 + prop3
                        '==========================================================================
                        'STORED CONTRIBUTIONS OF LOAD =============================================
                        '==========================================================================
                        '43  72  ( 1) = 'Mean from all discharges (3, 12, 5, 39, 60, 61 and 62)'
                        '44  73  ( 2) = 'Added by sewage effluents                      (3)'
                        '45  74  ( 3) = 'Intermittent discharges of sewage             (12)'
                        '46  75  ( 4) = 'Added by industrial discharges                 (5)'
                        '47  76  ( 5) = 'Added by mine waters                          (39)'
                        '48  77  ( 6) = 'Diffuse pollution from livestock              (25)'
                        '49  78  ( 7) = 'Diffuse pollution from arable                 (27)'
                        '50  79  ( 8) = 'Highway runoff                                (29)'
                        '51  80  ( 9) = 'Urban runoff                                  (31)'
                        '52  81  (10) = 'Atmosphere deposition                         (33)'
                        '53  82  (11) = 'Natural background                            (35)'
                        '54  83  (12) = 'Septic tanks                                  (37)'
                        '55  84  (13) = 'Aggregate CSOs                                (40)'
                        '56  85  (14) = 'Aggregated STWs                               (42)'
                        '57  86  (15) = 'Diffuse mines                                 (46)'
                        '58  87  (16) = 'Birds, boats and angling                      (48)'
                        '59  88  (17) = 'Boundaries and tributaries              (2 and 10)'
                        '60  89  (18) = 'Diffuse input                                 (13)'
                        '61  90  (19) = 'Diffuse input                                 (15)'
                        '62  91  (20) = 'Reach diffuse                                     '
                        '63  92  (21) = 'Gap filling of flows                              '
                        '64  93  (22) = 'Gap filling of quality                            '
                        '65  94  (23) = 'User-named diffuse input                      (50)'
                        '66  95  (24) = 'User-named diffuse input                      (52)'
                        '67  96  (25) = 'User-named diffuse input                      (54)'
                        '68  97  (26) = 'User-named diffuse input                      (56)'
                        '69  98  (27) = 'User-named diffuse input                      (58)'
                        '70  99  (28) = 'Other point sources                           (60)'
                        '71 100  (29) = 'Private wastewaters                           (61)'
                        '===========================================================================
                        usedfeatures = usedfeatures + 1

                        ic = usedfeatures + 1
                        If featurename = "Start of Reach" Or featurename = "End of Reach" Then
                            oTable.Cell(ic, 1).Range.Text = featurename + " " + reachname
                            oTable.Cell(ic, 1).Range.Text = featurename + " " + reachname
                        Else
                            oTable.Cell(ic, 1).Range.Text = featurename
                            End If
                            oTable.Cell(ic, 2).Range.Text = Format(runninglength, "0.00")
                            oTable.Cell(ic, 3).Range.Text = EDec(summary_statisic)
                            oTable.Cell(ic, 4).Range.Text = EDec(prop1)
                            oTable.Cell(ic, 5).Range.Text = EDec(prop2)
                        End If

                Next ' loop on number of features =====================================================================

            Next ' loop on reaches ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        End If ' apportionment of mean load ===========================================================================




        If CalibrationResultsRadioButton4.Checked = True Then ' write WORD report on observed and calibrated ==========
            rstart = startreach
            If allreaches = 0 Then rnextcode = next_reach(startreach)
            If allreaches = 1 Then rnextcode = next_reach(startreach)

            oPara3 = oDoc.Content.Paragraphs.Add
            oPara3.Range.Text = "Calibration for " & determinandname
            oPara3.Range.Font.Bold = True : oPara3.Range.Font.Size = 11
            oPara3.Format.SpaceAfter = 0
            oPara3.Range.InsertParagraphAfter()
            oPara3.Range.InsertParagraphAfter()

            oTable = oDoc.Tables.Add(oDoc.Bookmarks.Item("\endofdoc").Range, totalfeatures + 1, 7)
            oTable.Range.ParagraphFormat.SpaceAfter = 2
            oTable.Range.Font.Bold = False
            oTable.Range.Font.Size = 10 : oTable.Range.Font.Color = 10
            oTable.Borders.Enable = True
            oTable.Columns.Item(1).Width = oWord.InchesToPoints(2.2) 'change the width of the columns ...
            oTable.Columns.Item(2).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(3).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(4).Width = oWord.InchesToPoints(1.0)
            oTable.Columns.Item(5).Width = oWord.InchesToPoints(0.7)
            'oTable.Columns.Item(6).Width = oWord.InchesToPoints(1.0)
            oTable.Columns.Item(6).Width = oWord.InchesToPoints(0.7)
            oTable.Columns.Item(7).Width = oWord.InchesToPoints(0.8)
            oTable.Cell(1, 1).Range.Text = "LOCATION" ' table headings ================================================
            oTable.Cell(1, 2).Range.Text = "Distance"
            oTable.Cell(1, 3).Range.Text = "SIMCAT Mean"
            oTable.Cell(1, 4).Range.Text = "Confidence"
            oTable.Cell(1, 5).Range.Text = "Observed Mean"
            'oTable.Cell(1, 6).Range.Text = "Confidence"
            oTable.Cell(1, 6).Range.Text = "% Change Required"
            oTable.Cell(1, 7).Range.Text = "Range"

            For ireach = startreach To endreach ' process all the reaches =============================================

                If this_reach(ireach) <> this_reach(startreach) Then ' after the first reach
                    rstart = rnextcode
                    If allreaches = 0 Then rnextcode = next_reach(startreach)
                    If allreaches = 1 Then rnextcode = this_reach(rnextcode)
                    If rstart = endreach Then rnextcode = 0
                End If

                ip1 = numreaches + 4
                For ip = ip1 To numfeatures + numreaches + 4 '=========================================================
                    Obj = CType(range.Cells(ip, 3), Excel.Range) : reachnumber = Obj.value
                    If (reachnumber = this_reach(ireach)) Then
                        Obj = CType(range.Cells(ip, 1), Excel.Range) : featurename = Obj.text
                        Obj = CType(range.Cells(ip, 2), Excel.Range) : reachname = Obj.text
                        Obj = CType(range.Cells(ip, 4), Excel.Range) : featuretype = Obj.value
                        Obj = CType(range.Cells(ip, 6), Excel.Range) : reachlength = Obj.value
                        Obj = CType(range.Cells(ip, 7), Excel.Range) : runninglength = 0.1 * Int(10.0 * (Obj.value + 0.05))

                        If featuretype = 1 Then

                            cell1 = 7
                            Obj = CType(range.Cells(ip, cell1 + 1), Excel.Range) : summary_statisic = Obj.value
                            Obj = CType(range.Cells(ip, cell1 + 2), Excel.Range) : lower_limit = Obj.value
                            Obj = CType(range.Cells(ip, cell1 + 3), Excel.Range) : upper_limit = Obj.value
                            Obj = CType(range.Cells(ip, 109), Excel.Range) : observed = Obj.value
                            Obj = CType(range.Cells(ip, 110), Excel.Range) : observed_lower_limit = Obj.value
                            Obj = CType(range.Cells(ip, 111), Excel.Range) : observed_upper_limit = Obj.value
                            Obj = CType(range.Cells(ip, 121), Excel.Range) : change_needed = Obj.value
                            Obj = CType(range.Cells(ip, 122), Excel.Range) : change_needed_lower = Obj.value
                            Obj = CType(range.Cells(ip, 123), Excel.Range) : change_needed_upper = Obj.value
                            usedfeatures = usedfeatures + 1
                            If featuretype = 8886 Then
                                usedfeatures = usedfeatures - 1
                            Else
                                ic = usedfeatures + 1
                                If featurename = "Start of Reach" Or featurename = "End of Reach" Then
                                    oTable.Cell(ic, 1).Range.Text = featurename + " " + reachname
                                Else
                                    oTable.Cell(ic, 1).Range.Text = featurename
                                End If
                                oTable.Cell(ic, 2).Range.Text = Format(runninglength, "0.00")
                                oTable.Cell(ic, 3).Range.Text = EDec(summary_statisic)
                                oTable.Cell(ic, 4).Range.Text = "(" + EDec1(lower_limit) & " - " + EDec1(upper_limit) + ")"
                                If observed > 0.000001 Then
                                    oTable.Cell(ic, 5).Range.Text = EDec(observed)
                                    'oTable.Cell(ic, 6).Range.Text = "(" + EDec1(observed_lower_limit) & " - " + EDec1(observed_upper_limit) + ")"
                                    oTable.Cell(ic, 6).Range.Text = change_needed
                                    oTable.Cell(ic, 7).Range.Text = "(" & change_needed_lower & " to " & change_needed_upper & ")"
                                End If
                            End If
                        Else
                            cell1 = 7
                            Obj = CType(range.Cells(ip, cell1 + 1), Excel.Range) : summary_statisic = Obj.value
                            Obj = CType(range.Cells(ip, cell1 + 2), Excel.Range) : lower_limit = Obj.value
                            Obj = CType(range.Cells(ip, cell1 + 3), Excel.Range) : upper_limit = Obj.value
                            Obj = CType(range.Cells(ip, 45), Excel.Range) : observed = Obj.value
                            Obj = CType(range.Cells(ip, 46), Excel.Range) : observed_lower_limit = Obj.value
                            Obj = CType(range.Cells(ip, 47), Excel.Range) : observed_upper_limit = Obj.value
                            usedfeatures = usedfeatures + 1

                            ic = usedfeatures + 1
                            If featurename = "Start of Reach" Or featurename = "End of Reach" Then
                                oTable.Cell(ic, 1).Range.Text = featurename + " " + reachname
                            Else
                                oTable.Cell(ic, 1).Range.Text = featurename
                            End If
                            oTable.Cell(ic, 2).Range.Text = Format(runninglength, "0.00")
                            oTable.Cell(ic, 3).Range.Text = EDec(summary_statisic)
                            oTable.Cell(ic, 4).Range.Text = "(" + EDec1(lower_limit) & " - " + EDec1(upper_limit) + ")"
                        End If
                    End If

                Next ' loop on number of features ================================================================

            Next ' loop on reaches +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        End If ' observed and calculated river quality ===========================================================




        ResultsWAIT2.Visible = False  'ReachesCheckListBox2.Visible = False
        MeanResultsRadioButton1.Visible = True : ComplianceResultsRadioButton1.Visible = True
        LoadResultsRadioButton1.Visible = True : ClassificationResultsRadioButton1.Visible = True
        MeanApportionRadioButton1.Visible = True

        ChooseReachesResults.Visible = False
        SelectOutputFile1.Visible = True : ReturntoSIMCAT.Visible = True : ResultsQuit.Visible = True

        oDoc.SpellingChecked = True : oDoc.GrammarChecked = True : oDoc.Saved = True

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
    Private Sub ChooseReaches_Click(sender As Object, e As EventArgs) Handles ChooseReachesResults.Click
        If ReachCheckListBox2.Visible = True Then
            ReachCheckListBox2.Visible = False
        Else
            ReachCheckListBox2.Visible = True
        End If
    End Sub
    Private Sub RESULTSRun1_Click(sender As Object, e As EventArgs) Handles SelectOutputFile1.Click
        LastfilenameREPORT = "xxxx" '0000000000000000000000000000000000000000000
        Call Produce_a_Report(0)
        RunReportButton1.Visible = True
    End Sub
    Private Sub RunReportButton1_Click(sender As Object, e As EventArgs) Handles RunReportButton1.Click
        RunReportButton1.Visible = True
        Call Produce_a_Report(1)
    End Sub
    Private Sub Releaseobject(ByVal obj As Object)
        Try
            System.Runtime.InteropServices.Marshal.FinalReleaseComObject(obj)
            obj = Nothing
        Catch ex As Exception
            obj = Nothing
        Finally
            GC.Collect()
        End Try
    End Sub
    Private Sub MeanResultsRadioButton1_CheckedChanged(sender As Object, e As EventArgs) Handles MeanResultsRadioButton1.CheckedChanged
        If MeanResultsRadioButton1.Checked = True Then
            ComplianceResultsRadioButton1.Checked = False
            LoadResultsRadioButton1.Checked = False
            ClassificationResultsRadioButton1.Checked = False
            MeanApportionRadioButton1.Checked = False
            reportnumber = 1
        End If
    End Sub
    Private Sub ComplianceResultsRadioButton1_CheckedChanged(sender As Object, e As EventArgs) Handles ComplianceResultsRadioButton1.CheckedChanged
        If ComplianceResultsRadioButton1.Checked = True Then
            LoadResultsRadioButton1.Checked = False
            MeanResultsRadioButton1.Checked = False
            ClassificationResultsRadioButton1.Checked = False
            MeanApportionRadioButton1.Checked = False
            reportnumber = 2
        End If
    End Sub
    Private Sub ClassificationResultsRadioButton1_CheckedChanged(sender As Object, e As EventArgs) Handles ClassificationResultsRadioButton1.CheckedChanged
        If ClassificationResultsRadioButton1.Checked = True Then
            LoadResultsRadioButton1.Checked = False
            MeanResultsRadioButton1.Checked = False
            ComplianceResultsRadioButton1.Checked = False
            MeanApportionRadioButton1.Checked = False
            reportnumber = 3
        End If
    End Sub
    Private Sub LoadResultsRadioButton1_CheckedChanged(sender As Object, e As EventArgs) Handles LoadResultsRadioButton1.CheckedChanged
        If LoadResultsRadioButton1.Checked = True Then
            ComplianceResultsRadioButton1.Checked = False
            MeanResultsRadioButton1.Checked = False
            ClassificationResultsRadioButton1.Checked = False
            MeanApportionRadioButton1.Checked = False
            reportnumber = 4
        End If
    End Sub
    Private Sub MeanApportionRadioButton1_CheckedChanged(sender As Object, e As EventArgs) Handles MeanApportionRadioButton1.CheckedChanged
        If MeanApportionRadioButton1.Checked = True Then
            LoadResultsRadioButton1.Checked = False
            ComplianceResultsRadioButton1.Checked = False
            MeanResultsRadioButton1.Checked = False
            ClassificationResultsRadioButton1.Checked = False
            reportnumber = 5
        End If
    End Sub
    Private Sub CalibrationResultsRadioButton1_CheckedChanged(sender As Object, e As EventArgs) Handles CalibrationResultsRadioButton4.CheckedChanged
        'If CalibrationResultsRadioButton1.Checked = True Then
        'ComplianceResultsRadioButton1.Checked = False
        'MeanResultsRadioButton1.Checked = False
        'ClassificationResultsRadioButton1.Checked = False
        'End If
        'reportnumber = 6
    End Sub
    Function EDec(Number)
        EDec = Format(Number, "###0000")
        If Number < 9999.9 Then EDec = Format(Number, "#####0")
        If Number < 99.9 Then EDec = Format(Number, "#####0.0")
        If Number < 9.9 Then EDec = Format(Number, "#####0.00")
        If Number < 0.999 Then EDec = Format(Number, "#####0.00")
        If Number < 0.0999 Then EDec = Format(Number, "###0.000")
        If Number < 0.00999 Then EDec = Format(Number, "##0.0000")
    End Function

    Function EDec1(Number)
        EDec1 = Format(Number, "###0000")
        If Number < 9999.9 Then EDec1 = Format(Number, "#####0")
        If Number < 99.9 Then EDec1 = Format(Number, "#####0.0")
        If Number < 9.9 Then EDec1 = Format(Number, "#####0.00")
        If Number < 0.999 Then EDec1 = Format(Number, "#####0.00")
        If Number < 0.0999 Then EDec1 = Format(Number, "###0.000")
        'If Number < 0.00999 Then EDec1 = Format(Number, "##0.000")
    End Function

    Private Sub Results_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub Wait(ByVal seconds As Integer)
        For i As Integer = 0 To seconds * 20
            System.Threading.Thread.Sleep(10)
            Application.DoEvents()
        Next
    End Sub

End Class