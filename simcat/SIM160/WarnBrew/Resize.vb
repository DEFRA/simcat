'-------------------------------------------------------------------------------
' Resizer
' This class is used to dynamically resize and reposition all controls on a form.
' Container controls are processed recursively so that all controls on the form
' are handled.
'
' Usage:
'  Resizing functionality requires only three lines of code on a form:
'
'  1. Create a form-level reference to the Resize class:
'     Dim myResizer as Resizer
'
'  2. In the Form_Load event, call the  Resizer class FIndAllControls method:
'     myResizer.FindAllControls(Me)
'
'  3. In the Form_Resize event, call the  Resizer class ResizeAllControls method:
'     myResizer.ResizeAllControls(Me)
'
'-------------------------------------------------------------------------------
' Place the following in the 
'-------------------------------------------------------------------------------
'      Dim rs As New Resizer
'      rs.FindAllControls(Me)
'      rs.ResizeAllControls(Me)
'-------------------------------------------------------------------------------

Public Class Resizer
    Public fheight As Single
    Public fwidth As Single

    '----------------------------------------------------------
    ' ControlInfo
    ' Structure of original state of all processed controls
    '----------------------------------------------------------
    Private Structure ControlInfo
        Public name As String
        Public parentName As String
        Public leftOffsetPercent As Double
        Public topOffsetPercent As Double
        Public heightPercent As Double
        Public originalHeight As Integer
        Public originalWidth As Integer
        Public widthPercent As Double
        Public originalFontSize As Single
    End Structure

    '-------------------------------------------------------------------------
    ' Dictionary of (control name, control info) for all processed controls
    '-------------------------------------------------------------------------
    Private ctrlDict As Dictionary(Of String, ControlInfo) = New Dictionary(Of String, ControlInfo)

    '----------------------------------------------------------------------------------------
    ' FindAllControls
    ' Recursive function to process all controls contained in the initially passed
    ' control container and store it in the Control dictionary
    '----------------------------------------------------------------------------------------
    Public Sub FindAllControls(thisCtrl As Control)

        Dim intH As Single = Screen.PrimaryScreen.Bounds.Height
        Dim intW As Single = Screen.PrimaryScreen.Bounds.Width

        fheight = 1.0 : fwidth = 1.0

        If intH <> 720 And intW <> 1280 Then ' commented out in 5.0
            intH = 0.5 * (intH + 720) : intW = 0.5 * (intW + 1280) ' commented out in 5.0

            'fheight = intH / 720 : fwidth = intW / 1280  'included in 5.0
        End If ' commented out in 5.0

        thisCtrl.Height = thisCtrl.Height * fheight
        thisCtrl.Width = thisCtrl.Width * fwidth

        'If the current control has a parent, store all original relative position and size information in the dictionary.
        'Recursively call FindAllControls for each control contained in the current Control

        For Each ctl As Control In thisCtrl.Controls
            Try
                If Not IsNothing(ctl.Parent) Then
                    Dim parentHeight = ctl.Parent.Height
                    Dim parentWidth = ctl.Parent.Width

                    'MsgBox("parent height: " & parentHeight & vbCrLf & "parent width: " & parentWidth)

                    Dim c As New ControlInfo
                    c.name = ctl.Name
                    c.parentName = ctl.Parent.Name
                        c.topOffsetPercent = Convert.ToDouble(ctl.Top) / Convert.ToDouble(parentHeight)
                        c.leftOffsetPercent = Convert.ToDouble(ctl.Left) / Convert.ToDouble(parentWidth)
                        c.heightPercent = Convert.ToDouble(ctl.Height) / Convert.ToDouble(parentHeight)
                        c.widthPercent = Convert.ToDouble(ctl.Width) / Convert.ToDouble(parentWidth)
                        c.originalFontSize = ctl.Font.Size
                        c.originalHeight = ctl.Height
                        c.originalWidth = ctl.Width
                        ctrlDict.Add(c.name, c)

                End If

            Catch ex As Exception
                Debug.Print(ex.Message)
            End Try

            If ctl.Controls.Count > 0 Then
                FindAllControls(ctl)
            End If

        Next '-- For Each

    End Sub

    'ResizeAllControls
    'Recursive function to resize and reposition all controls contained in the Control dictionary
    Public Sub ResizeAllControls(thisCtrl As Control)

        Dim fontRatioW As Single
        Dim fontRatioH As Single
        Dim fontRatio As Single
        Dim f As Font

        '-- Resize and reposition all controls in the passed control
        For Each ctl As Control In thisCtrl.Controls
            Try
                If Not IsNothing(ctl.Parent) Then
                    Dim parentHeight = ctl.Parent.Height
                    Dim parentWidth = ctl.Parent.Width

                    Dim c As New ControlInfo

                    Dim ret As Boolean = False
                    Try
                        '-- Get the current control's info from the control info dictionary
                        ret = ctrlDict.TryGetValue(ctl.Name, c)

                        '-- If found, adjust the current control based on control relative
                        '-- size and position information stored in the dictionary
                        If (ret) Then
                            '-- Size
                            ctl.Width = Int(parentWidth * c.widthPercent) * fwidth
                            ctl.Height = Int(parentHeight * c.heightPercent) * fheight

                            '-- Position
                            ctl.Top = Int(parentHeight * c.topOffsetPercent) * fheight
                            ctl.Left = Int(parentWidth * c.leftOffsetPercent) * fwidth

                            '-- Font
                            f = ctl.Font
                            fontRatioW = (ctl.Width / c.originalWidth) * fwidth
                            fontRatioH = (ctl.Height / c.originalHeight) * fheight
                            'fontRatio = ((fontRatioW + fontRatioH) / 2) * 1.0 '-- average change in control Height and Width
                            'fontRatio = fontRatioW  '-- average change in control Height and Width
                            fontRatio = ((fontRatioW + 1.0) / 2) * 1.0 '-- average change in control Height and Width
                            ctl.Font = New Font(f.FontFamily, c.originalFontSize * fontRatio, f.Style)
                            'MsgBox("fontRatio: " & fontRatio)

                        End If
                    Catch
                    End Try
                End If
            Catch ex As Exception
            End Try

            '-- Recursive call for controls contained in the current control
            If ctl.Controls.Count > 0 Then
                ResizeAllControls(ctl)
            End If

        Next '-- For Each
    End Sub

End Class