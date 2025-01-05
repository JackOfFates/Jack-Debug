﻿Imports JackDebug.WPF.Values
Imports MicroSerializationLibrary.Serialization

Namespace Collections
    Public Class ValueTimeline
        Implements IDisposable

#Region "Properties"
        Public ReadOnly Property GUID As String
            Get
                Return _GUID
            End Get
        End Property
        Private _GUID As String

        Public ReadOnly Property Maximum As Integer
            Get
                Return _Maximum
            End Get
        End Property
        Private _Maximum As Integer = -1

        Public ReadOnly Property HighestValue As DebugValue
            Get
                Return _HighestValue
            End Get
        End Property
        Private _HighestValue As DebugValue

        Public ReadOnly Property LowestValue As DebugValue
            Get
                Return _LowestValue
            End Get
        End Property
        Private _LowestValue As DebugValue

        Public ReadOnly Property LastValue As DebugValue
            Get
                If Values.Length > 0 Then
                    Return Values.Last()
                Else
                    Return Nothing
                End If
            End Get
        End Property

        Public Property IsEnabled As Boolean = True

        Public Property Values As New List(Of DebugValue)
        Public Property Keys As New List(Of Long)

#End Region

#Region "Events"

        Public Event ValueChanged(sender As ValueTimeline, ChangedValue As DebugValue, ArrayIndexies As List(Of Integer))

        Public Sub OnValueChangedEvent(ChangedValue As DebugValue, Optional ArrayIndexies As List(Of Integer) = Nothing)
            RaiseEvent ValueChanged(Me, ChangedValue, ArrayIndexies)
        End Sub

#End Region

#Region "IDisposable"
        Private disposedValue As Boolean
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then
                    _LowestValue.Dispose()
                    _LowestValue = Nothing
                    _HighestValue.Dispose()
                    _HighestValue = Nothing
                    Dim count As Integer = Values.Count - 1
                    For i As Integer = count To 0 Step -1
                        Values(i).Dispose()
                        Values(i) = Nothing
                    Next
                    Keys.Clear()
                    Values.Clear()
                    Values = Nothing
                    Keys = Nothing
                End If

                disposedValue = True
            End If
        End Sub
        Public Sub Dispose() Implements IDisposable.Dispose
            Dispose(disposing:=True)
            GC.SuppressFinalize(Me)
        End Sub
#End Region

        Public Function GetValuesWithin(StartIndex As Integer, EndIndex As Integer) As ValueTimelineSplice
            Dim LowValue As DebugValue = Nothing
            Dim HighValue As DebugValue = Nothing
            Dim OutputValues As New List(Of DebugValue)
            For i As Integer = StartIndex To EndIndex
                If Keys.Length - 1 >= StartIndex AndAlso Keys.Length - 1 >= EndIndex Then
                    Dim Value As DebugValue = Values(i)
                    If Value.Flags.isGraphable() Then
                        If LowValue Is Nothing OrElse Value.Value < LowValue.Value Then LowValue = Value
                        If HighValue Is Nothing OrElse Value.Value > HighValue.Value Then HighValue = Value
                    End If
                    OutputValues.Add(Value)
                End If
            Next
            Return New ValueTimelineSplice(OutputValues, LowValue, HighValue)
        End Function

        Public Function GetValuesWithin(StartTime As Long, EndTime As Long) As List(Of DebugValue)
            Dim Output As New List(Of DebugValue)
            For i As Integer = 0 To Keys.Length - 1
                If Values.Length - 1 >= i Then
                    Dim Value As DebugValue = Values(i)
                    If Value.Timecode >= StartTime AndAlso Value.Timecode <= EndTime Then
                        Output.Add(Value)
                    End If
                End If
            Next
            Return Output
        End Function

        Public Function GetValuesWithin(StartTime As DateTime, EndTime As DateTime)
            Return GetValuesWithin(StartTime.Ticks, EndTime.Ticks)
        End Function

        Public Sub AddValue(DebugValue As DebugValue)
            Keys.Add(DebugValue.Timecode)
            Values.Add(DebugValue)
            _Maximum = Keys.Length
            If DebugValue.ValueChanged Then OnValueChangedEvent(DebugValue, DebugValue.ChangedIndexies)
            If DebugValue.Flags.isGraphable() Then
                If _HighestValue Is Nothing OrElse (_HighestValue.Value < DebugValue.Value) Then
                    _HighestValue = DebugValue
                End If
                If _LowestValue Is Nothing OrElse (_LowestValue.Value > DebugValue.Value) Then
                    _LowestValue = DebugValue
                End If
            End If
        End Sub

        Public Function Clone() As ValueTimeline
            Dim c As ValueTimeline = CloneObject(Of ValueTimeline)
            Return c
        End Function

        Public Sub New(GUID As String)
            _GUID = GUID
        End Sub

    End Class

End Namespace