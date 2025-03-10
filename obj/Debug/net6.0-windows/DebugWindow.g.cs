﻿#pragma checksum "..\..\..\DebugWindow.xaml" "{ff1816ec-aa5e-4d10-87f7-6f4963833460}" "AE606E01292077B5A72EE42C9CA1EF2013FE1C6D"
//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:4.0.30319.42000
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

using JackDebug;
using MahApps.Metro.Controls;
using System;
using System.Diagnostics;
using System.Windows;
using System.Windows.Automation;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Controls.Ribbon;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Ink;
using System.Windows.Input;
using System.Windows.Markup;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Media.Effects;
using System.Windows.Media.Imaging;
using System.Windows.Media.Media3D;
using System.Windows.Media.TextFormatting;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Windows.Shell;




/// <summary>
/// DebugWindow
/// </summary>
public partial class DebugWindow : MahApps.Metro.Controls.MetroWindow, System.Windows.Markup.IComponentConnector {
    
    
    #line 19 "..\..\..\DebugWindow.xaml"
    [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
    internal System.Windows.Controls.TreeView Watchers;
    
    #line default
    #line hidden
    
    
    #line 20 "..\..\..\DebugWindow.xaml"
    [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
    internal System.Windows.Controls.Grid Plot;
    
    #line default
    #line hidden
    
    
    #line 21 "..\..\..\DebugWindow.xaml"
    [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
    internal System.Windows.Shapes.Polygon p;
    
    #line default
    #line hidden
    
    
    #line 23 "..\..\..\DebugWindow.xaml"
    [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
    internal System.Windows.Media.PointCollection Points;
    
    #line default
    #line hidden
    
    
    #line 28 "..\..\..\DebugWindow.xaml"
    [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
    internal System.Windows.Controls.TextBlock LowLabel;
    
    #line default
    #line hidden
    
    
    #line 29 "..\..\..\DebugWindow.xaml"
    [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
    internal System.Windows.Controls.TextBlock HighLabel;
    
    #line default
    #line hidden
    
    
    #line 31 "..\..\..\DebugWindow.xaml"
    [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
    internal System.Windows.Controls.Grid TimelineGrid;
    
    #line default
    #line hidden
    
    
    #line 32 "..\..\..\DebugWindow.xaml"
    [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
    internal MahApps.Metro.Controls.RangeSlider Slider;
    
    #line default
    #line hidden
    
    
    #line 34 "..\..\..\DebugWindow.xaml"
    [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
    internal MahApps.Metro.Controls.ToggleSwitch ToggleEnabled;
    
    #line default
    #line hidden
    
    private bool _contentLoaded;
    
    /// <summary>
    /// InitializeComponent
    /// </summary>
    [System.Diagnostics.DebuggerNonUserCodeAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("PresentationBuildTasks", "9.0.0.0")]
    public void InitializeComponent() {
        if (_contentLoaded) {
            return;
        }
        _contentLoaded = true;
        System.Uri resourceLocater = new System.Uri("/JackDebug;component/debugwindow.xaml", System.UriKind.Relative);
        
        #line 1 "..\..\..\DebugWindow.xaml"
        System.Windows.Application.LoadComponent(this, resourceLocater);
        
        #line default
        #line hidden
    }
    
    [System.Diagnostics.DebuggerNonUserCodeAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("PresentationBuildTasks", "9.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Never)]
    [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Design", "CA1033:InterfaceMethodsShouldBeCallableByChildTypes")]
    [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")]
    [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily")]
    void System.Windows.Markup.IComponentConnector.Connect(int connectionId, object target) {
            switch (connectionId)
            {
            case 1:
        this.Watchers = ((System.Windows.Controls.TreeView)(target));
        return;
            case 2:
        this.Plot = ((System.Windows.Controls.Grid)(target));
        return;
            case 3:
        this.p = ((System.Windows.Shapes.Polygon)(target));
        return;
            case 4:
        this.Points = ((System.Windows.Media.PointCollection)(target));
        return;
            case 5:
        this.LowLabel = ((System.Windows.Controls.TextBlock)(target));
        return;
            case 6:
        this.HighLabel = ((System.Windows.Controls.TextBlock)(target));
        return;
            case 7:
        this.TimelineGrid = ((System.Windows.Controls.Grid)(target));
        return;
            case 8:
        this.Slider = ((MahApps.Metro.Controls.RangeSlider)(target));
        return;
            case 9:
        this.ToggleEnabled = ((MahApps.Metro.Controls.ToggleSwitch)(target));
        return;
            }
        this._contentLoaded = true;
    }
}

