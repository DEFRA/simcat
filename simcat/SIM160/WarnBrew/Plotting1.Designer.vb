<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class GPlotting1
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Dim ChartArea1 As System.Windows.Forms.DataVisualization.Charting.ChartArea = New System.Windows.Forms.DataVisualization.Charting.ChartArea()
        Dim Legend1 As System.Windows.Forms.DataVisualization.Charting.Legend = New System.Windows.Forms.DataVisualization.Charting.Legend()
        Dim Series1 As System.Windows.Forms.DataVisualization.Charting.Series = New System.Windows.Forms.DataVisualization.Charting.Series()
        Dim Series2 As System.Windows.Forms.DataVisualization.Charting.Series = New System.Windows.Forms.DataVisualization.Charting.Series()
        Dim Series3 As System.Windows.Forms.DataVisualization.Charting.Series = New System.Windows.Forms.DataVisualization.Charting.Series()
        Dim Series4 As System.Windows.Forms.DataVisualization.Charting.Series = New System.Windows.Forms.DataVisualization.Charting.Series()
        Dim Series5 As System.Windows.Forms.DataVisualization.Charting.Series = New System.Windows.Forms.DataVisualization.Charting.Series()
        Dim Series6 As System.Windows.Forms.DataVisualization.Charting.Series = New System.Windows.Forms.DataVisualization.Charting.Series()
        Dim DataPoint1 As System.Windows.Forms.DataVisualization.Charting.DataPoint = New System.Windows.Forms.DataVisualization.Charting.DataPoint(0R, 0R)
        Dim Series7 As System.Windows.Forms.DataVisualization.Charting.Series = New System.Windows.Forms.DataVisualization.Charting.Series()
        Dim Series8 As System.Windows.Forms.DataVisualization.Charting.Series = New System.Windows.Forms.DataVisualization.Charting.Series()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(GPlotting1))
        Me.GPLOTQuit1 = New System.Windows.Forms.Button()
        Me.GraphDisplay = New System.Windows.Forms.DataVisualization.Charting.Chart()
        Me.ChooseDeterminand = New System.Windows.Forms.Button()
        Me.Statistic1 = New System.Windows.Forms.RadioButton()
        Me.Statistic2 = New System.Windows.Forms.RadioButton()
        Me.Statistic3 = New System.Windows.Forms.RadioButton()
        Me.Statistic4 = New System.Windows.Forms.RadioButton()
        Me.LoadStatistic5 = New System.Windows.Forms.RadioButton()
        Me.Heading1 = New System.Windows.Forms.Label()
        Me.Determinand1 = New System.Windows.Forms.Label()
        Me.Load1 = New System.Windows.Forms.Label()
        Me.featurecounter = New System.Windows.Forms.Button()
        Me.reachcounter = New System.Windows.Forms.Button()
        Me.runninglength = New System.Windows.Forms.Button()
        Me.BacktoSIMCAT1 = New System.Windows.Forms.Button()
        Me.ChooseReachesPlot = New System.Windows.Forms.Button()
        Me.ReachCheckListBox1 = New System.Windows.Forms.CheckedListBox()
        Me.SelectDATAandRUN1 = New System.Windows.Forms.Button()
        Me.SelectOutputFileButton1 = New System.Windows.Forms.Button()
        Me.UnitsLabel1 = New System.Windows.Forms.Label()
        Me.LoadsUnitsLabel1 = New System.Windows.Forms.Label()
        Me.ClearButton1 = New System.Windows.Forms.Button()
        CType(Me.GraphDisplay, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'GPLOTQuit1
        '
        Me.GPLOTQuit1.BackColor = System.Drawing.Color.Crimson
        Me.GPLOTQuit1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.None
        Me.GPLOTQuit1.FlatAppearance.BorderSize = 3
        Me.GPLOTQuit1.Font = New System.Drawing.Font("Microsoft Sans Serif", 26.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.GPLOTQuit1.ForeColor = System.Drawing.Color.Snow
        Me.GPLOTQuit1.Location = New System.Drawing.Point(3530, 1519)
        Me.GPLOTQuit1.Margin = New System.Windows.Forms.Padding(0)
        Me.GPLOTQuit1.Name = "GPLOTQuit1"
        Me.GPLOTQuit1.Size = New System.Drawing.Size(462, 137)
        Me.GPLOTQuit1.TabIndex = 181
        Me.GPLOTQuit1.Text = "quit"
        Me.GPLOTQuit1.UseVisualStyleBackColor = False
        '
        'GraphDisplay
        '
        Me.GraphDisplay.BackColor = System.Drawing.Color.Gainsboro
        Me.GraphDisplay.BackHatchStyle = System.Windows.Forms.DataVisualization.Charting.ChartHatchStyle.BackwardDiagonal
        Me.GraphDisplay.BackSecondaryColor = System.Drawing.Color.Yellow
        Me.GraphDisplay.BorderlineDashStyle = System.Windows.Forms.DataVisualization.Charting.ChartDashStyle.Dash
        ChartArea1.AxisX.IsMarginVisible = False
        ChartArea1.AxisX.IsStartedFromZero = False
        ChartArea1.IsSameFontSizeForAllAxes = True
        ChartArea1.Name = "ChartArea1"
        Me.GraphDisplay.ChartAreas.Add(ChartArea1)
        Legend1.Name = "Legend1"
        Me.GraphDisplay.Legends.Add(Legend1)
        Me.GraphDisplay.Location = New System.Drawing.Point(44, 76)
        Me.GraphDisplay.Margin = New System.Windows.Forms.Padding(2)
        Me.GraphDisplay.Name = "GraphDisplay"
        Series1.BackImageTransparentColor = System.Drawing.Color.Transparent
        Series1.BorderDashStyle = System.Windows.Forms.DataVisualization.Charting.ChartDashStyle.Dot
        Series1.BorderWidth = 3
        Series1.ChartArea = "ChartArea1"
        Series1.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.StepLine
        Series1.Color = System.Drawing.Color.DarkOrange
        Series1.Legend = "Legend1"
        Series1.Name = "target"
        Series1.ShadowColor = System.Drawing.Color.White
        Series1.XValueType = System.Windows.Forms.DataVisualization.Charting.ChartValueType.[Double]
        Series2.BorderColor = System.Drawing.Color.Transparent
        Series2.BorderWidth = 20
        Series2.ChartArea = "ChartArea1"
        Series2.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Candlestick
        Series2.Color = System.Drawing.Color.PaleGreen
        Series2.CustomProperties = "DrawSideBySide=True, IsXAxisQuantitative=False"
        Series2.Legend = "Legend1"
        Series2.Name = "observed"
        Series2.ShadowColor = System.Drawing.Color.Empty
        Series2.XValueType = System.Windows.Forms.DataVisualization.Charting.ChartValueType.[Single]
        Series2.YValuesPerPoint = 4
        Series3.BorderColor = System.Drawing.Color.Lime
        Series3.ChartArea = "ChartArea1"
        Series3.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line
        Series3.Color = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer))
        Series3.LabelBorderColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(128, Byte), Integer))
        Series3.Legend = "Legend1"
        Series3.Name = "confidence"
        Series3.XValueType = System.Windows.Forms.DataVisualization.Charting.ChartValueType.[Double]
        Series4.ChartArea = "ChartArea1"
        Series4.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line
        Series4.Color = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer))
        Series4.Font = New System.Drawing.Font("Microsoft Sans Serif", 6.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Series4.IsVisibleInLegend = False
        Series4.LabelBackColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Series4.LabelBorderColor = System.Drawing.Color.White
        Series4.Legend = "Legend1"
        Series4.Name = "upper confidence"
        Series4.XValueType = System.Windows.Forms.DataVisualization.Charting.ChartValueType.[Double]
        Series4.YValuesPerPoint = 2
        Series5.BorderWidth = 8
        Series5.ChartArea = "ChartArea1"
        Series5.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line
        Series5.Color = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer))
        Series5.LabelAngle = 2
        Series5.LabelBackColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Series5.LabelBorderColor = System.Drawing.Color.Blue
        Series5.Legend = "Legend1"
        Series5.Name = "mean"
        Series5.ShadowColor = System.Drawing.Color.Transparent
        Series5.SmartLabelStyle.CalloutBackColor = System.Drawing.Color.LightGray
        Series5.SmartLabelStyle.IsMarkerOverlappingAllowed = True
        Series5.XValueType = System.Windows.Forms.DataVisualization.Charting.ChartValueType.[Single]
        Series6.ChartArea = "ChartArea1"
        Series6.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line
        Series6.Color = System.Drawing.Color.White
        Series6.Enabled = False
        Series6.IsVisibleInLegend = False
        Series6.LabelBorderColor = System.Drawing.Color.Transparent
        Series6.LabelForeColor = System.Drawing.Color.Red
        Series6.Legend = "Legend1"
        Series6.Name = "names"
        DataPoint1.LabelBorderColor = System.Drawing.Color.Red
        Series6.Points.Add(DataPoint1)
        Series6.ShadowColor = System.Drawing.Color.White
        Series6.YValueType = System.Windows.Forms.DataVisualization.Charting.ChartValueType.[Single]
        Series7.ChartArea = "ChartArea1"
        Series7.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line
        Series7.Color = System.Drawing.Color.White
        Series7.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Series7.IsValueShownAsLabel = True
        Series7.IsVisibleInLegend = False
        Series7.LabelBackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(255, Byte), Integer))
        Series7.Legend = "Legend1"
        Series7.Name = "discharges"
        Series7.ShadowColor = System.Drawing.Color.White
        Series8.BorderWidth = 8
        Series8.ChartArea = "ChartArea1"
        Series8.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line
        Series8.Color = System.Drawing.Color.White
        Series8.Enabled = False
        Series8.Legend = "Legend1"
        Series8.Name = "reaches"
        Series8.ShadowColor = System.Drawing.Color.White
        Series8.XValueType = System.Windows.Forms.DataVisualization.Charting.ChartValueType.[Single]
        Me.GraphDisplay.Series.Add(Series1)
        Me.GraphDisplay.Series.Add(Series2)
        Me.GraphDisplay.Series.Add(Series3)
        Me.GraphDisplay.Series.Add(Series4)
        Me.GraphDisplay.Series.Add(Series5)
        Me.GraphDisplay.Series.Add(Series6)
        Me.GraphDisplay.Series.Add(Series7)
        Me.GraphDisplay.Series.Add(Series8)
        Me.GraphDisplay.Size = New System.Drawing.Size(3456, 1758)
        Me.GraphDisplay.TabIndex = 182
        Me.GraphDisplay.Text = "Chart1"
        '
        'ChooseDeterminand
        '
        Me.ChooseDeterminand.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ChooseDeterminand.Location = New System.Drawing.Point(3530, 995)
        Me.ChooseDeterminand.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.ChooseDeterminand.Name = "ChooseDeterminand"
        Me.ChooseDeterminand.Size = New System.Drawing.Size(462, 170)
        Me.ChooseDeterminand.TabIndex = 183
        Me.ChooseDeterminand.Text = "choose determinand"
        Me.ChooseDeterminand.UseVisualStyleBackColor = True
        '
        'Statistic1
        '
        Me.Statistic1.AutoSize = True
        Me.Statistic1.Checked = True
        Me.Statistic1.Location = New System.Drawing.Point(3530, 131)
        Me.Statistic1.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.Statistic1.Name = "Statistic1"
        Me.Statistic1.Size = New System.Drawing.Size(141, 41)
        Me.Statistic1.TabIndex = 185
        Me.Statistic1.TabStop = True
        Me.Statistic1.Text = "mean"
        Me.Statistic1.UseVisualStyleBackColor = True
        '
        'Statistic2
        '
        Me.Statistic2.AutoSize = True
        Me.Statistic2.Location = New System.Drawing.Point(3530, 187)
        Me.Statistic2.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.Statistic2.Name = "Statistic2"
        Me.Statistic2.Size = New System.Drawing.Size(244, 41)
        Me.Statistic2.TabIndex = 186
        Me.Statistic2.Text = "90-percentile"
        Me.Statistic2.UseVisualStyleBackColor = True
        '
        'Statistic3
        '
        Me.Statistic3.AutoSize = True
        Me.Statistic3.Location = New System.Drawing.Point(3530, 242)
        Me.Statistic3.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.Statistic3.Name = "Statistic3"
        Me.Statistic3.Size = New System.Drawing.Size(244, 41)
        Me.Statistic3.TabIndex = 187
        Me.Statistic3.Text = "95-percentile"
        Me.Statistic3.UseVisualStyleBackColor = True
        '
        'Statistic4
        '
        Me.Statistic4.AutoSize = True
        Me.Statistic4.Location = New System.Drawing.Point(3530, 298)
        Me.Statistic4.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.Statistic4.Name = "Statistic4"
        Me.Statistic4.Size = New System.Drawing.Size(244, 41)
        Me.Statistic4.TabIndex = 188
        Me.Statistic4.Text = "99-percentile"
        Me.Statistic4.UseVisualStyleBackColor = True
        '
        'LoadStatistic5
        '
        Me.LoadStatistic5.AutoSize = True
        Me.LoadStatistic5.Location = New System.Drawing.Point(3530, 392)
        Me.LoadStatistic5.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.LoadStatistic5.Name = "LoadStatistic5"
        Me.LoadStatistic5.Size = New System.Drawing.Size(211, 41)
        Me.LoadStatistic5.TabIndex = 189
        Me.LoadStatistic5.Text = "mean load"
        Me.LoadStatistic5.UseVisualStyleBackColor = True
        '
        'Heading1
        '
        Me.Heading1.AutoSize = True
        Me.Heading1.Font = New System.Drawing.Font("Microsoft Sans Serif", 10.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Heading1.Location = New System.Drawing.Point(74, 26)
        Me.Heading1.Margin = New System.Windows.Forms.Padding(6, 0, 6, 0)
        Me.Heading1.Name = "Heading1"
        Me.Heading1.Size = New System.Drawing.Size(161, 46)
        Me.Heading1.TabIndex = 190
        Me.Heading1.Text = "heading"
        Me.Heading1.Visible = False
        '
        'Determinand1
        '
        Me.Determinand1.AutoSize = True
        Me.Determinand1.Font = New System.Drawing.Font("Microsoft Sans Serif", 10.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Determinand1.ForeColor = System.Drawing.Color.DarkGreen
        Me.Determinand1.Location = New System.Drawing.Point(1526, 22)
        Me.Determinand1.Margin = New System.Windows.Forms.Padding(6, 0, 6, 0)
        Me.Determinand1.Name = "Determinand1"
        Me.Determinand1.Size = New System.Drawing.Size(240, 46)
        Me.Determinand1.TabIndex = 191
        Me.Determinand1.Text = "determinand"
        Me.Determinand1.Visible = False
        '
        'Load1
        '
        Me.Load1.AutoSize = True
        Me.Load1.Font = New System.Drawing.Font("Microsoft Sans Serif", 10.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Load1.ForeColor = System.Drawing.Color.DarkRed
        Me.Load1.Location = New System.Drawing.Point(1999, 22)
        Me.Load1.Margin = New System.Windows.Forms.Padding(6, 0, 6, 0)
        Me.Load1.Name = "Load1"
        Me.Load1.Size = New System.Drawing.Size(96, 46)
        Me.Load1.TabIndex = 192
        Me.Load1.Text = "load"
        Me.Load1.Visible = False
        '
        'featurecounter
        '
        Me.featurecounter.BackColor = System.Drawing.Color.Yellow
        Me.featurecounter.Font = New System.Drawing.Font("Microsoft Sans Serif", 16.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.featurecounter.ForeColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.featurecounter.Location = New System.Drawing.Point(3530, 564)
        Me.featurecounter.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.featurecounter.Name = "featurecounter"
        Me.featurecounter.Size = New System.Drawing.Size(401, 81)
        Me.featurecounter.TabIndex = 193
        Me.featurecounter.Text = "0"
        Me.featurecounter.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.featurecounter.UseVisualStyleBackColor = False
        Me.featurecounter.Visible = False
        '
        'reachcounter
        '
        Me.reachcounter.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(128, Byte), Integer))
        Me.reachcounter.Font = New System.Drawing.Font("Microsoft Sans Serif", 16.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.reachcounter.ForeColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.reachcounter.Location = New System.Drawing.Point(3530, 472)
        Me.reachcounter.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.reachcounter.Name = "reachcounter"
        Me.reachcounter.Size = New System.Drawing.Size(401, 81)
        Me.reachcounter.TabIndex = 194
        Me.reachcounter.Text = "0"
        Me.reachcounter.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.reachcounter.UseVisualStyleBackColor = False
        Me.reachcounter.Visible = False
        '
        'runninglength
        '
        Me.runninglength.BackColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(128, Byte), Integer))
        Me.runninglength.Font = New System.Drawing.Font("Microsoft Sans Serif", 16.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.runninglength.ForeColor = System.Drawing.Color.LimeGreen
        Me.runninglength.Location = New System.Drawing.Point(3530, 657)
        Me.runninglength.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.runninglength.Name = "runninglength"
        Me.runninglength.Size = New System.Drawing.Size(401, 81)
        Me.runninglength.TabIndex = 195
        Me.runninglength.Text = "0"
        Me.runninglength.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.runninglength.UseVisualStyleBackColor = False
        Me.runninglength.Visible = False
        '
        'BacktoSIMCAT1
        '
        Me.BacktoSIMCAT1.BackColor = System.Drawing.Color.Blue
        Me.BacktoSIMCAT1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.None
        Me.BacktoSIMCAT1.FlatAppearance.BorderSize = 3
        Me.BacktoSIMCAT1.Font = New System.Drawing.Font("Microsoft Sans Serif", 26.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.BacktoSIMCAT1.ForeColor = System.Drawing.Color.Snow
        Me.BacktoSIMCAT1.Location = New System.Drawing.Point(3530, 1365)
        Me.BacktoSIMCAT1.Margin = New System.Windows.Forms.Padding(0)
        Me.BacktoSIMCAT1.Name = "BacktoSIMCAT1"
        Me.BacktoSIMCAT1.Size = New System.Drawing.Size(462, 137)
        Me.BacktoSIMCAT1.TabIndex = 196
        Me.BacktoSIMCAT1.Text = "return"
        Me.BacktoSIMCAT1.UseVisualStyleBackColor = False
        '
        'ChooseReachesPlot
        '
        Me.ChooseReachesPlot.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(128, Byte), Integer))
        Me.ChooseReachesPlot.Font = New System.Drawing.Font("Microsoft Sans Serif", 16.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ChooseReachesPlot.ForeColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.ChooseReachesPlot.Location = New System.Drawing.Point(3530, 812)
        Me.ChooseReachesPlot.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.ChooseReachesPlot.Name = "ChooseReachesPlot"
        Me.ChooseReachesPlot.Size = New System.Drawing.Size(462, 170)
        Me.ChooseReachesPlot.TabIndex = 197
        Me.ChooseReachesPlot.Text = "select reaches"
        Me.ChooseReachesPlot.UseVisualStyleBackColor = False
        Me.ChooseReachesPlot.Visible = False
        '
        'ReachCheckListBox1
        '
        Me.ReachCheckListBox1.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.ReachCheckListBox1.FormattingEnabled = True
        Me.ReachCheckListBox1.Location = New System.Drawing.Point(897, 268)
        Me.ReachCheckListBox1.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.ReachCheckListBox1.MultiColumn = True
        Me.ReachCheckListBox1.Name = "ReachCheckListBox1"
        Me.ReachCheckListBox1.Size = New System.Drawing.Size(941, 784)
        Me.ReachCheckListBox1.TabIndex = 1
        Me.ReachCheckListBox1.Visible = False
        '
        'SelectDATAandRUN1
        '
        Me.SelectDATAandRUN1.BackColor = System.Drawing.Color.MediumOrchid
        Me.SelectDATAandRUN1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.None
        Me.SelectDATAandRUN1.FlatAppearance.BorderSize = 3
        Me.SelectDATAandRUN1.Font = New System.Drawing.Font("Microsoft Sans Serif", 26.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.SelectDATAandRUN1.ForeColor = System.Drawing.Color.Snow
        Me.SelectDATAandRUN1.Location = New System.Drawing.Point(3530, 1204)
        Me.SelectDATAandRUN1.Margin = New System.Windows.Forms.Padding(0)
        Me.SelectDATAandRUN1.Name = "SelectDATAandRUN1"
        Me.SelectDATAandRUN1.Size = New System.Drawing.Size(456, 137)
        Me.SelectDATAandRUN1.TabIndex = 199
        Me.SelectDATAandRUN1.Text = "run"
        Me.SelectDATAandRUN1.UseVisualStyleBackColor = False
        '
        'SelectOutputFileButton1
        '
        Me.SelectOutputFileButton1.BackColor = System.Drawing.SystemColors.ActiveCaption
        Me.SelectOutputFileButton1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.None
        Me.SelectOutputFileButton1.FlatAppearance.BorderSize = 3
        Me.SelectOutputFileButton1.Font = New System.Drawing.Font("Microsoft Sans Serif", 26.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.SelectOutputFileButton1.ForeColor = System.Drawing.Color.Snow
        Me.SelectOutputFileButton1.Location = New System.Drawing.Point(3530, 1678)
        Me.SelectOutputFileButton1.Margin = New System.Windows.Forms.Padding(0)
        Me.SelectOutputFileButton1.Name = "SelectOutputFileButton1"
        Me.SelectOutputFileButton1.Size = New System.Drawing.Size(84, 80)
        Me.SelectOutputFileButton1.TabIndex = 201
        Me.SelectOutputFileButton1.UseVisualStyleBackColor = False
        '
        'UnitsLabel1
        '
        Me.UnitsLabel1.AutoSize = True
        Me.UnitsLabel1.Font = New System.Drawing.Font("Microsoft Sans Serif", 10.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.UnitsLabel1.ForeColor = System.Drawing.Color.Crimson
        Me.UnitsLabel1.Location = New System.Drawing.Point(2620, 24)
        Me.UnitsLabel1.Margin = New System.Windows.Forms.Padding(6, 0, 6, 0)
        Me.UnitsLabel1.Name = "UnitsLabel1"
        Me.UnitsLabel1.Size = New System.Drawing.Size(104, 46)
        Me.UnitsLabel1.TabIndex = 202
        Me.UnitsLabel1.Text = "units"
        Me.UnitsLabel1.Visible = False
        '
        'LoadsUnitsLabel1
        '
        Me.LoadsUnitsLabel1.AutoSize = True
        Me.LoadsUnitsLabel1.Font = New System.Drawing.Font("Microsoft Sans Serif", 10.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.LoadsUnitsLabel1.ForeColor = System.Drawing.Color.Crimson
        Me.LoadsUnitsLabel1.Location = New System.Drawing.Point(2823, 22)
        Me.LoadsUnitsLabel1.Margin = New System.Windows.Forms.Padding(6, 0, 6, 0)
        Me.LoadsUnitsLabel1.Name = "LoadsUnitsLabel1"
        Me.LoadsUnitsLabel1.Size = New System.Drawing.Size(104, 46)
        Me.LoadsUnitsLabel1.TabIndex = 203
        Me.LoadsUnitsLabel1.Text = "units"
        Me.LoadsUnitsLabel1.Visible = False
        '
        'ClearButton1
        '
        Me.ClearButton1.BackColor = System.Drawing.SystemColors.ActiveCaption
        Me.ClearButton1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.None
        Me.ClearButton1.FlatAppearance.BorderSize = 3
        Me.ClearButton1.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ClearButton1.ForeColor = System.Drawing.Color.Snow
        Me.ClearButton1.Location = New System.Drawing.Point(3901, 1754)
        Me.ClearButton1.Margin = New System.Windows.Forms.Padding(0)
        Me.ClearButton1.Name = "ClearButton1"
        Me.ClearButton1.Size = New System.Drawing.Size(84, 80)
        Me.ClearButton1.TabIndex = 204
        Me.ClearButton1.Text = "X"
        Me.ClearButton1.UseVisualStyleBackColor = False
        '
        'GPlotting1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(19.0!, 37.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.SystemColors.ActiveCaption
        Me.ClientSize = New System.Drawing.Size(3844, 1920)
        Me.Controls.Add(Me.ClearButton1)
        Me.Controls.Add(Me.LoadsUnitsLabel1)
        Me.Controls.Add(Me.UnitsLabel1)
        Me.Controls.Add(Me.SelectOutputFileButton1)
        Me.Controls.Add(Me.SelectDATAandRUN1)
        Me.Controls.Add(Me.ReachCheckListBox1)
        Me.Controls.Add(Me.ChooseReachesPlot)
        Me.Controls.Add(Me.BacktoSIMCAT1)
        Me.Controls.Add(Me.runninglength)
        Me.Controls.Add(Me.reachcounter)
        Me.Controls.Add(Me.featurecounter)
        Me.Controls.Add(Me.Load1)
        Me.Controls.Add(Me.Determinand1)
        Me.Controls.Add(Me.Heading1)
        Me.Controls.Add(Me.LoadStatistic5)
        Me.Controls.Add(Me.Statistic4)
        Me.Controls.Add(Me.Statistic3)
        Me.Controls.Add(Me.Statistic2)
        Me.Controls.Add(Me.Statistic1)
        Me.Controls.Add(Me.ChooseDeterminand)
        Me.Controls.Add(Me.GraphDisplay)
        Me.Controls.Add(Me.GPLOTQuit1)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.Name = "GPlotting1"
        Me.Padding = New System.Windows.Forms.Padding(2, 0, 0, 0)
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        CType(Me.GraphDisplay, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents GPLOTQuit1 As Button
    Friend WithEvents ChooseDeterminand As Button
    Friend WithEvents Statistic1 As RadioButton
    Friend WithEvents Statistic2 As RadioButton
    Friend WithEvents Statistic3 As RadioButton
    Friend WithEvents Statistic4 As RadioButton
    Friend WithEvents LoadStatistic5 As RadioButton
    Friend WithEvents Heading1 As Label
    Friend WithEvents Determinand1 As Label
    Friend WithEvents Load1 As Label
    Friend WithEvents featurecounter As Button
    Friend WithEvents reachcounter As Button
    Friend WithEvents runninglength As Button
    Friend WithEvents BacktoSIMCAT1 As Button
    Friend WithEvents ChooseReachesPlot As Button
    Friend WithEvents ReachCheckListBox1 As CheckedListBox
    Friend WithEvents SelectDATAandRUN1 As Button
    Friend WithEvents GraphDisplay As DataVisualization.Charting.Chart
    Friend WithEvents SelectOutputFileButton1 As Button
    Friend WithEvents UnitsLabel1 As Label
    Friend WithEvents LoadsUnitsLabel1 As Label
    Friend WithEvents ClearButton1 As Button
End Class
