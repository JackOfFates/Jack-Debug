﻿Imports System.Collections.ObjectModel
Imports System.ComponentModel
Imports System.Net.Security
Imports System.Reflection
Imports System.Threading
Imports System.Windows
Imports System.Windows.Controls
Imports System.Windows.Controls.Primitives
Imports System.Windows.Input
Imports System.Windows.Media
Imports System.Windows.Media.Animation
Imports System.Xml.Serialization
Imports ControlzEx.Theming
Imports JackDebug.WPF.Collections
Imports JackDebug.WPF.Values
Imports MahApps.Metro.Controls
Imports MicroSerializationLibrary
Imports MicroSerializationLibrary.Serialization
Imports Windows.Networking.XboxLive

Public Class DebugWindow
    Inherits MetroWindow

#Region "UI"

    Private isUserChangingLower As Boolean = False,
            isUserChangingUpper As Boolean = False,
            Range As Integer

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

        ThemeManager.Current.ChangeTheme(Me, "Dark.Red")
        SetRange()
    End Sub

    Private Sub SetRange()
        If isUserChangingLower Or isUserChangingUpper Then Return
        Range = If(isAtBeginning And isAtEnd, MaxRange, Math.Min(Upper - Lower, MaxRange))
    End Sub

    Private Sub DebugWindow_Loaded(sender As Object, e As RoutedEventArgs) Handles Me.Loaded
        _PlotWidth = Plot.ActualWidth
        _PlotHeight = Plot.ActualHeight
        Enabled = False
    End Sub

    Private Sub DebugWindow_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing
        Enabled = False
        BackgroundWorker.Dispose()
        For i As Integer = 0 To DebugWatcher.Watchers.Count - 1
            DebugWatcher.Watchers(i).Dispose()
        Next
        DebugWatcher.Watchers.Clear()
    End Sub

    Private Sub Slider_Loaded(sender As Object, e As RoutedEventArgs) Handles Slider.Loaded
        Slider_LowerValueChanged(Nothing, Nothing)
    End Sub

    Private Sub Plot_SizeChanged(sender As Object, e As SizeChangedEventArgs) Handles Plot.SizeChanged
        _PlotWidth = Plot.ActualWidth
        _PlotHeight = Plot.ActualHeight
        ClearPoints()
    End Sub

    Private Sub ToggleEnabled_Toggled(sender As Object, e As RoutedEventArgs) Handles ToggleEnabled.Toggled
        Enabled = Not Enabled
    End Sub

    Private Sub Slider_LowerValueChanged(sender As Object, e As RoutedPropertyChangedEventArgs(Of Double)) Handles Slider.LowerValueChanged
        If IsMaxWidth Then
            e.Handled = True
            Range = MaxRange
            Lower = Upper - MaxRange
        End If
        _Lower = Slider.LowerValue
    End Sub

    Private Sub Slider_UpperValueChanged(sender As Object, e As RoutedPropertyChangedEventArgs(Of Double)) Handles Slider.UpperValueChanged
        If IsMaxWidth Then
            e.Handled = True
            Lower = Upper - Range
        End If
        _Upper = Slider.UpperValue
    End Sub

    Private Sub Slider_LowerThumbDragStarted(sender As Object, e As DragStartedEventArgs) Handles Slider.LowerThumbDragStarted
        SetRange()
        isUserChangingLower = True
    End Sub

    Private Sub Slider_LowerThumbDragCompleted(sender As Object, e As DragCompletedEventArgs) Handles Slider.LowerThumbDragCompleted
        isUserChangingLower = False
        SetRange()
    End Sub

    Private Sub Slider_UpperThumbDragStarted(sender As Object, e As DragStartedEventArgs) Handles Slider.UpperThumbDragStarted
        SetRange()
        isUserChangingUpper = True
        KeepAtEnd = False
    End Sub

    Private Sub Slider_UpperThumbDragCompleted(sender As Object, e As DragCompletedEventArgs) Handles Slider.UpperThumbDragCompleted
        isUserChangingUpper = False
        SetRange()
    End Sub

    Private Sub Slider_LowerThumbDragDelta(sender As Object, e As DragDeltaEventArgs) Handles Slider.LowerThumbDragDelta
        SetRange()
    End Sub

    Private Sub Slider_UpperThumbDragDelta(sender As Object, e As DragDeltaEventArgs) Handles Slider.UpperThumbDragDelta
        SetRange()
    End Sub

    Private Sub Slider_CentralThumbDragCompleted(sender As Object, e As DragCompletedEventArgs) Handles Slider.CentralThumbDragCompleted
        isUserChangingUpper = False
        isUserChangingLower = False
        SetRange()
    End Sub

    Private Sub Slider_CentralThumbDragStarted(sender As Object, e As DragStartedEventArgs) Handles Slider.CentralThumbDragStarted
        SetRange()
        isUserChangingUpper = True
        isUserChangingLower = True
        KeepAtEnd = False
    End Sub

#End Region

#Region "Caches"

    Private ts As TimeSpan = TimeSpan.FromTicks(1000)
    Private WithEvents CurrentTimeline As ValueTimeline
    Private CurrentWatcher As DebugWatcher
    Public ConcurrentIterations As New List(Of String)
    Private CurrentWatchers As New Dictionary(Of String, DebugWatcher)

#End Region

#Region "Drawing"

    Public Sub ClearPoints()
        InvokeUI(Sub() Points.Clear())
        DrawIndex = 0
        _PlotWidth = Plot.ActualWidth
        _PlotHeight = Plot.ActualHeight
    End Sub

    Public Sub DrawTimeline(cw As String)
        If CurrentTimeline Is Nothing Then Return

        Maximum = CurrentTimeline.Maximum - 1

        If KeepAtEnd Then
            Upper = Maximum
        End If

        If isAtEnd Then
            If IsMaxWidth AndAlso Not isUserChangingLower Then
                Range = MaxRange
                Lower = Upper - Range
            ElseIf Not isUserChangingLower Then
                Lower = Upper - Range
            End If
        End If

        If isAtBeginning AndAlso Not isUserChangingLower Then
            If IsMaxWidth Then
                Range = MaxRange
                Lower = Upper - Range
            Else
                Lower = 0
            End If
            'ElseIf IsMaxWidth AndAlso isUserChangingLower Then
            '    Upper = Math.Min(Lower + Range, MaxRange)
        End If

        If DrawIndex = Upper Then Return

        Dim splice As ValueTimelineSplice = CurrentTimeline.GetValuesWithin(Lower, Upper)
        If splice.isGraphable Then
            If CurrentTimeline.HighestValue Is Nothing Then Return
            If CurrentTimeline.LowestValue Is Nothing Then Return
            InvokeUI(
                Sub()
                    ClearPoints()
                    Dim l As Integer = splice.Values.Length - 1
                    Dim pointWidth As Double = PlotWidth / l

                    Dim Flags As TypeFlags = splice.Values.Last().Flags

                    CreatePoint(DrawIndex, 0)
                    CreatePoint(DrawIndex, PlotHeight)
                    CreatePoint(DrawIndex, 0)
                    For i As Integer = 0 To splice.Values.Length - 1
                        Dim v As DebugValue = splice.Values(i)
                        Dim InterpolatedValue As Double

                        If Flags.isBoolean Then
                            If v.Value Then
                                InterpolatedValue = PlotHeight
                            Else
                                InterpolatedValue = 0
                            End If
                        ElseIf Flags.isDrawingRectangle Then
                        ElseIf Flags.isShapesRect Then
                        ElseIf Flags.isDrawingPoint Then
                        ElseIf Flags.isWindowsPoint Then
                        ElseIf Flags.isNumeric Then
                            InterpolatedValue = Interpolate(v.Value, CurrentTimeline.LowestValue, CurrentTimeline.HighestValue, 0, PlotHeight)
                        End If

                        CreatePoint(DrawIndex, InterpolatedValue)
                        CreatePoint(DrawIndex + pointWidth, InterpolatedValue)

                        DrawIndex += pointWidth
                    Next

                    CreatePoint(DrawIndex, 0)

                    If Flags.isBoolean Then
                        LowLabel.Text = "False"
                        HighLabel.Text = "True"
                    Else
                        LowLabel.Text = CurrentTimeline.LowestValue
                        HighLabel.Text = CurrentTimeline.HighestValue
                    End If
                End Sub)


        End If

    End Sub

    Public Sub CreatePoint(x As Double, y As Double)
        InvokeUI(Sub() Points.Add(New Point(x, PlotHeight - y)))
    End Sub

#End Region

#Region "Shared Properties"
    Public Shared Property MaxRange As Integer = 1000
    Public Shared Property MaxRecursiveThreads As Integer = 10
    Public Shared Property AnimationDuration As TimeSpan = TimeSpan.FromSeconds(0.75)
    Public Shared Property DefaultBackground As Color = Color.FromArgb(255, 37, 37, 37)
    Public Shared Property ValueChangedAnimation As New ColorAnimation(Colors.LimeGreen, DefaultBackground, AnimationDuration)

#End Region

#Region "Properties"

    Public Property KeepAtEnd As Boolean = False
    Private Property DrawIndex As Double = 0
    Private Property SelectedArrayIndex As Integer = 0

    Public ReadOnly Property isAtEnd As Boolean
        Get
            If Not isUserChangingUpper AndAlso Upper >= Maximum * 0.95 Then
                KeepAtEnd = True
            End If
            Return Upper = Maximum
        End Get
    End Property

    Public ReadOnly Property isAtBeginning As Boolean
        Get
            Return Lower = 0
        End Get
    End Property

    Public Property Enabled As Boolean
        Get
            Return _Enabled
        End Get
        Set(value As Boolean)
            _Enabled = value
            For i As Integer = 0 To DebugWatcher.Watchers.Count - 1
                Dim w As DebugWatcher = DebugWatcher.Watchers(i)
                w.isEnabled = value
            Next
            If value Then
                ForceInvoke(Sub()
                                Do While BackgroundWorker IsNot Nothing
                                    Thread.Sleep(1)
                                Loop
                            End Sub)
                BackgroundWorker = New Timer(New TimerCallback(AddressOf BackgroundWorker_DoWork), Nothing, 0, Timeout.Infinite)
            End If
        End Set
    End Property
    Private _Enabled As Boolean = False

    Public Property Realtime As Boolean
        Get
            Return _Realtime
        End Get
        Set(value As Boolean)
            _Realtime = value
        End Set
    End Property
    Private _Realtime As Boolean = True

    Public ReadOnly Property PlotHeight As Double
        Get
            Return _PlotHeight
        End Get
    End Property
    Private _PlotHeight As Double = 1

    Public ReadOnly Property PlotWidth As Double
        Get
            Return _PlotWidth
        End Get
    End Property
    Private _PlotWidth As Double = 1

    Public Property Upper As Integer
        Get
            Return _Upper
        End Get
        Set(value As Integer)
            Application.Current.Dispatcher.Invoke(Sub() Slider.UpperValue = value)
        End Set
    End Property
    Private _Upper As Integer

    Public Property Lower As Integer
        Get
            Return _Lower
        End Get
        Set(value As Integer)
            Application.Current.Dispatcher.Invoke(Sub() Slider.LowerValue = value)
        End Set
    End Property
    Private _Lower As Integer

    Public Property Maximum As Integer
        Get
            Return _Maximum
        End Get
        Set(value As Integer)
            _Maximum = value
            Application.Current.Dispatcher.Invoke(Sub() Slider.Maximum = value)
        End Set
    End Property
    Private _Maximum As Integer

    Public ReadOnly Property WatcherCount As Integer
        Get
            Return DebugWatcher.Watchers.Count
        End Get
    End Property
    Private _WatcherCount As Integer = 0

    Public ReadOnly Property TimelineDifference As Integer
        Get
            Return _TimelineDifference
        End Get
    End Property
    Private _TimelineDifference As Integer = 0

    Public ReadOnly Property IsMaxWidth As Boolean
        Get
            Return Upper - Lower > MaxRange
        End Get
    End Property

#End Region

#Region "Tree View"

    Private WatcherItems As New Dictionary(Of String, TreeViewItem)
    Private TreeItems As New Dictionary(Of String, TreeViewItem)

    Private Function CreateTreeItem(Type As Type, Header As String, Optional ToolTip As String = Nothing) As TreeViewItem
        Dim NewValue As TreeViewItem = Nothing
#Disable Warning BC42358
        InvokeUI(Sub()
                     NewValue = New TreeViewItem
                     If ToolTip = Nothing Then ToolTip = Type.Name
                     With NewValue
                         .ToolTip = ToolTip
                         .Header = Header
                         .Background = New SolidColorBrush(DefaultBackground)
                     End With
                 End Sub)
#Enable Warning BC42358

        Return NewValue
    End Function

#End Region

#Region "Animations"

    Public Async Sub ValueChangedAnim(TreeViewItem As TreeViewItem, Indexies As List(Of Integer))
#Disable Warning BC42358
        InvokeUI(
            Sub()
                If Indexies IsNot Nothing AndAlso Indexies.Count > 0 Then
                    For i As Integer = 0 To Indexies.Count - 1
                        If TreeViewItem.Items.Count >= i Then
                            Try
                                Dim aTVI As TreeViewItem = TreeViewItem.Items(i)
                                aTVI.Background.BeginAnimation(SolidColorBrush.ColorProperty, ValueChangedAnimation)
                            Catch ex As Exception

                            End Try
                        End If
                    Next
                End If
                TreeViewItem.Background.BeginAnimation(SolidColorBrush.ColorProperty, ValueChangedAnimation)
            End Sub)
#Enable Warning BC42358
    End Sub

#End Region

#Region "Functions"

    Private Sub SetTimeline(cw As DebugWatcher, GUID As String, SelectedArrayIndex As Integer)
        ClearPoints()
        CurrentWatcher = cw
        SelectedArrayIndex = SelectedArrayIndex
        CurrentTimeline = CurrentWatcher.Timelines(GUID)
    End Sub

    Private Function SubItemSelected(TVI As TreeViewItem) As Boolean
        For i As Integer = 0 To TVI.Items.Count - 1
            Dim item As TreeViewItem = TVI.Items(i)
            If item.IsSelected Then
                Return True
            End If
        Next
        Return False
    End Function

#End Region

#Region "Background Worker"

    Private WithEvents BackgroundWorker As Timer
    ''' <summary>
    ''' Tree View Item Updates
    ''' </summary>
    Private Sub BackgroundWorker_DoWork(state As Object)
        Do While Enabled
            Dim nCount As Integer = WatcherCount
            If nCount <> _WatcherCount Then
                For i As Integer = 0 To DebugWatcher.Watchers.Count - 1
                    If Not Enabled Then Exit For
                    Dim w As DebugWatcher = DebugWatcher.Watchers(i)
                    Dim t As Type = w.AttachedObject.GetType
                    If Not CurrentWatchers.ContainsKey(w.GUID) AndAlso Not IsImmutable(t) Then
                        CurrentWatchers.Add(w.GUID, w)
                        Dim NewWatcher As TreeViewItem = CreateTreeItem(t, w.Name)
#Disable Warning BC42358
                        InvokeUI(
                            Sub()
                                If Not TreeItems.ContainsKey(w.GUID) Then
                                    Watchers.Items.Add(NewWatcher)
                                    WatcherItems.Add(w.GUID, NewWatcher)

                                    AddHandler w.ValueCalculated, AddressOf ValueCalculated
                                    AddHandler w.ValueChanged, AddressOf ValueChanged
                                End If
                            End Sub)
#Enable Warning BC42358
                    End If
                Next
                _WatcherCount = nCount
            End If
            If NotNothing(CurrentWatcher) Then
                DrawTimeline(CurrentWatcher.GUID)
            End If
            Thread.Sleep(1)
        Loop
        BackgroundWorker = Nothing
    End Sub

#End Region

    ''' <summary>
    ''' Detects main & child Item Value Changes
    ''' </summary>
    ''' <param name="DebugWatcher"></param>
    ''' <param name="ChangedValue"></param>
    ''' <param name="ArrayIndexies"></param>
    Private Sub ValueChanged(DebugWatcher As DebugWatcher, ChangedValue As DebugValue, ArrayIndexies As List(Of Integer))
        InvokeUI(Sub()
                     If TreeItems.ContainsKey(ChangedValue.GUID) Then
                         Dim TVI As TreeViewItem = TreeItems(ChangedValue.GUID)
                         ValueChangedAnim(TVI, ArrayIndexies)
                     End If
                     SyncLock (ChangedValue.AllChangedGuids)
                         For i As Integer = 0 To ChangedValue.AllChangedGuids.Count - 1
                             Dim guid As String = ChangedValue.AllChangedGuids(i)
                             If TreeItems.ContainsKey(guid) Then
                                 Dim TVI As TreeViewItem = TreeItems(guid)
                                 ValueChangedAnim(TVI, New List(Of Integer))
                             End If
                         Next
                     End SyncLock
                 End Sub)
    End Sub

    ''' <summary>
    ''' Fires when the watcher has a new value.
    ''' </summary>
    ''' <param name="Watcher">Current Watcher</param>
    ''' <param name="Value">Current Value</param>
    Private Async Sub ValueCalculated(Watcher As DebugWatcher, Value As DebugValue, Optional Parents As Object() = Nothing)
        If Value Is Nothing Then Return

        If NotNothing(Parents) AndAlso Parents.Contains(Value.Value) Then
            Return
        End If
        Dim TimelineDiff As Integer = 0

        If TreeItems.ContainsKey(Value.GUID) Then
            If CurrentTimeline IsNot Nothing AndAlso Value.GUID = CurrentTimeline.GUID Then TimelineDiff += 1
            ''' TODO: 
            ''' Split into seperate methods and/or functions & Cleanup in general.
            Dim TVI As TreeViewItem = TreeItems(Value.GUID)
            If Value.Flags.isArray Or Value.Flags.isList Or Value.Flags.isDictionary Then
                If Value.Flags.isDictionary AndAlso Value.KeyList IsNot Nothing Then
                    ''' DICTIONARY FUNCTION
                    Dim ArrayType As Type = Nothing
                    For i As Integer = i To Value.KeyList.Count - 1
                        Dim Index As Integer = i
                        If Index > Value.Value.Length - 1 Then Exit For
                        Dim ArrayItemKey As Object = Value.KeyList(Index)
                        Dim ArrayItemValue As Object = Value.ValueList(Index)
                        Dim ValueToString As String = If(IsNothing(ArrayItemValue), "NULL", ArrayItemValue.ToString())
                        If ArrayType Is Nothing Then ArrayType = ArrayItemValue.GetType()
                        Dim Header As String = "[" & Index & "]" & ArrayItemKey.ToString() & ": " & ValueToString
                        If Index > TVI.Items.Count - 1 Then
                            Dim aTVI As TreeViewItem = CreateTreeItem(ArrayType, Header)
                            AddHandler aTVI.Selected, Sub() SetTimeline(Watcher, Value.GUID, Index)
#Disable Warning BC42358
                            InvokeUI(
                                Sub()
                                    If Index > Value.Value.Count - 1 Then Return
                                    Try
                                        TVI.Items.Add(aTVI)
                                    Catch ex As Exception
                                        '''TODO: Not sure why this thows an out of bounds exception..
                                    End Try
                                End Sub)
#Enable Warning BC42358
                        Else
#Disable Warning BC42358
                            InvokeUI(
                                Sub()
                                    If Index > Value.Value.Count - 1 Then Return
                                    Try
                                        TVI.Items(Index).Header = Header
                                    Catch ex As Exception
                                        '''TODO: Not sure why this thows an out of bounds exception..
                                    End Try
                                End Sub)
#Enable Warning BC42358
                        End If
                    Next
                ElseIf Value.Flags.isList AndAlso Value.Value IsNot Nothing Then
                    ''' LIST FUNCTION
                    Dim ArrayType As Type = Nothing
                    Dim L As Integer = Value.Length - 1
                    For i As Integer = i To L
                        If i > Value.Value.Count - 1 Then Exit For
                        Dim Index As Integer = i
                        Dim ArrayItem As Object = Value.Value(Index)
                        Dim ValueToString As String = If(IsNothing(ArrayItem), "NULL", ArrayItem.ToString())
                        If ArrayType Is Nothing Then ArrayType = ArrayItem.GetType()
                        Dim Header As String = "[" & Index & "] " & ArrayType.Name & ": " & ValueToString
                        If Index > TVI.Items.Count - 1 Then
                            Dim aTVI As TreeViewItem = CreateTreeItem(ArrayType, Header)

                            AddHandler aTVI.Selected, Sub() SetTimeline(Watcher, Value.GUID, Index)
#Disable Warning BC42358
                            InvokeUI(
                                Sub()
                                    If Index > Value.Value.Count - 1 Then Return
                                    Try
                                        TVI.Items.Add(aTVI)
                                    Catch ex As Exception
                                        '''TODO: Not sure why this thows an out of bounds exception..
                                    End Try
                                End Sub)
#Enable Warning BC42358
                        Else
#Disable Warning BC42358
                            InvokeUI(
                                Sub()
                                    If Index > Value.Value.Count - 1 Then Return
                                    Try
                                        TVI.Items(Index).Header = Header
                                    Catch ex As Exception
                                        '''TODO: Check for out of bounds if nessesary. Haven't seen it happen, yet..
                                    End Try
                                End Sub)
#Enable Warning BC42358
                        End If
                    Next
                ElseIf Value.Flags.isArray AndAlso Value.Value IsNot Nothing Then
                    ''' ARRAY FUNCTION
                    Dim ArrayType As Type = Nothing
                    For i As Integer = 0 To Value.Length - 1
                        If Value.Length - 1 < i Then Exit For
                        Dim Index As Integer = i
                        If Index < 0 Then Exit For
                        Try
                            Dim ArrayItem As Object = Value.Value(Index)
                            If ArrayType Is Nothing Then ArrayType = ArrayItem.GetType()
                            Dim ValueToString As String = If(IsNothing(ArrayItem), "NULL", ArrayItem.ToString())
                            Dim Header As String = "[" & Index & "] " & ArrayType.Name & ": " & ValueToString
                            Dim ItemCount1 As Integer = 0

#Disable Warning BC42358
                            InvokeUI(Sub() ItemCount1 = TVI.Items.Count - 1)
#Enable Warning BC42358
                            If Index > ItemCount1 Then
                                Dim aTVI As TreeViewItem = CreateTreeItem(ArrayItem.GetType, Header)
                                AddHandler aTVI.Selected, Sub() SetTimeline(Watcher, Value.GUID, Index)
#Disable Warning BC42358
                                InvokeUI(
                                    Sub()
                                        If Index > Value.Value.Length - 1 Then Return
                                        Try
                                            TVI.Items.Add(aTVI)
                                        Catch ex As Exception
                                            '''TODO: Not sure why this thows an out of bounds exception..
                                        End Try
                                    End Sub)
#Enable Warning BC42358
                            Else
#Disable Warning BC42358
                                InvokeUI(
                                    Sub()
                                        If Index > Value.Value.length - 1 Then Return
                                        Try
                                            TVI.Items(Index).Header = Header
                                        Catch ex As Exception
                                            '''TODO: Check for out of bounds if nessesary. Haven't seen it happen, yet..
                                        End Try
                                    End Sub)
#Enable Warning BC42358
                            End If
                        Catch
                        End Try
                    Next
                End If
                Dim ItemCount2 As Integer = 0
#Disable Warning BC42358
                InvokeUI(Sub() ItemCount2 = TVI.Items.Count - 1)
#Enable Warning BC42358
                If ItemCount2 > Value.Length - 1 Then
                    Dim Length As Integer = 0
                    For x As Integer = ItemCount2 To Length Step -1
                        Dim Index As Integer = x
                        Dim [Continue] As Boolean = False
#Disable Warning BC42358
                        InvokeUI(
                                Sub()
                                    If TVI.Items.Count > 0 AndAlso TVI.Items.Count - 1 <= Index Then
                                        Try
                                            TVI.Items.RemoveAt(Index)
                                        Catch ex As Exception

                                        End Try
                                    Else
                                        [Continue] = True
                                    End If
                                End Sub)
#Enable Warning BC42358
                        If [Continue] Then Continue For
                    Next
                End If
            ElseIf Value.Flags.isSystem Then
                Dim ValueToString As String = If(IsNothing(Value.Value), "NULL", Value.Value.ToString())

#Disable Warning BC42358
                InvokeUI(Sub() TVI.Header = Value.Name & "(" & ValueToString & ")")
#Enable Warning BC42358
            Else
                'IterateValues(Watcher, TVI, Value, New Object() {Value.Value})
            End If
        Else
            ''' TODO: 
            ''' Split into seperate methods and/or functions & Cleanup in general.
            If Value.Flags.isArray Or Value.Flags.isList Or Value.Flags.isDictionary Then

                ''' LIST, DICTIONARY, AND ARRAY HEADER ITEM
                Dim TVI As TreeViewItem = CreateTreeItem(Value.Type, Value.Name)
                AddHandler TVI.Selected, Sub() If Not SubItemSelected(TVI) Then SetTimeline(Watcher, Value.GUID, -1)

                Dim ArrayType As Type = Nothing
                Dim index As Integer = 0
                Dim L As Integer = Value.Length - 1
                For i As Integer = 0 To L
                    index = i
                    If Value.Flags.isDictionary Then
                        If index > Value.Value.Count - 1 Then Exit For
                    ElseIf Value.Flags.isList Then
                        If index > Value.Value.Count - 1 Then Exit For
                    ElseIf Value.Flags.isArray Then
                        If index > Value.Value.Length - 1 Then Exit For
                    End If

                    Dim ArrayItem As Object = Value.Value(index)
                    If ArrayType Is Nothing Then ArrayType = ArrayItem.GetType()
                    Dim ValueToString As String = If(IsNothing(ArrayItem), "NULL", ArrayItem.ToString())
                    Dim Header As String = "[" & index & "] " & ArrayType.Name & ": " & ValueToString
                    Dim aTVI As TreeViewItem = CreateTreeItem(ArrayItem.GetType, ArrayItem.GetType().Name)
                    AddHandler aTVI.Selected, Sub() SetTimeline(Watcher, Value.GUID, index)
#Disable Warning BC42358
                    InvokeUI(Sub()
                                 If Value.Flags.isDictionary Then
                                     If index > Value.Value.Count - 1 Then Return
                                 ElseIf Value.Flags.isList Then
                                     If index > Value.Value.Count - 1 Then Return
                                 ElseIf Value.Flags.isArray Then
                                     If index > Value.Value.Length - 1 Then Return
                                 End If
                                 Try
                                     TVI.Items.Add(aTVI)
                                 Catch ex As Exception
                                     '''TODO: Fix yet ANOTHER out of bounds exception... not sure why.
                                 End Try
                             End Sub)
#Enable Warning BC42358
                Next
                If TreeItems.ContainsKey(Value.GUID) Then
                    TVI = Nothing
                    _TimelineDifference = TimelineDiff
                    Return
                Else
                    TreeItems.Add(Value.GUID, TVI)
#Disable Warning BC42358
                    InvokeUI(
                        Sub() WatcherItems(Watcher.GUID).Items.Add(TVI))
#Enable Warning BC42358
                End If
            Else
                Dim ValueToString As String = If(IsNothing(Value.Value), "NULL", Value.Value.ToString())
                Dim Header As String = Value.Name & "(" & ValueToString & ")"
                Dim TVI As TreeViewItem = CreateTreeItem(Value.Type, Header)
                AddHandler TVI.Selected, Sub() If Not SubItemSelected(TVI) Then SetTimeline(Watcher, Value.GUID, -1)
                If TreeItems.ContainsKey(Value.GUID) Then
                    TVI = Nothing
                    _TimelineDifference = TimelineDiff
                    Return
                Else
                    TreeItems.Add(Value.GUID, TVI)

#Disable Warning BC42358
                    InvokeUI(Sub()
                                 Try
                                     WatcherItems(Watcher.GUID).Items.Add(TVI)
                                 Catch ex As Exception
                                     '''TODO: Fix yet ANOTHER out of bounds exception... not sure why.
                                 End Try
                             End Sub)
#Enable Warning BC42358
                End If

                If Value IsNot Nothing AndAlso NotNothing(Value.Children) AndAlso Value.Children.Length > 0 Then
                    'IterateValues(Watcher, TVI, Value, New Object() {Value.Value})
                End If
            End If

        End If

        _TimelineDifference = TimelineDiff
    End Sub

    ''' <summary>
    ''' Iterate a Fields Child Values & Update UI
    ''' </summary>
    ''' <param name="cw"></param>
    ''' <param name="TVI"></param>
    ''' <param name="CurrentValue"></param>
    ''' <param name="Parent"></param>
    ''' <returns></returns>
    Private Function IterateValues(cw As DebugWatcher, TVI As TreeViewItem, CurrentValue As DebugValue, Parents As Object())
        If Not ConcurrentIterations.Contains(CurrentValue.GUID) AndAlso ConcurrentIterations.Count < MaxRecursiveThreads Then
            SyncLock (ConcurrentIterations)
                ConcurrentIterations.Add(CurrentValue.GUID)
            End SyncLock
            Dim T As New Thread(
                    Sub()
                        If CurrentValue.Children.Length > 0 Then
                            Dim Index As Integer = 0
                            Try
                                For i As Integer = 0 To CurrentValue.Children.Length - 1
                                    Index = i
                                    Dim ArrayItem As DebugValue = CurrentValue.Children.Values(Index)
                                    Dim safeValue As Object = If(ArrayItem.Value Is Nothing, Nothing, ArrayItem.ValueChanged)
                                    ValueCalculated(cw, ArrayItem, Parents.AddJoin(safeValue))
                                Next
                            Catch ex As Exception
                                '''Just in case, it will fill up stops in the thread pool if i allow the count to leak too high. 10 Max until i get efficiency under control.
                            End Try
                            SyncLock (ConcurrentIterations)
                                ConcurrentIterations.Remove(CurrentValue.GUID)
                            End SyncLock
                        End If

                    End Sub)
            T.Start()


        End If
    End Function

End Class
