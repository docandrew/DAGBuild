with Ada.Real_Time;

with SDL;
with SDL.TTFs;
with SDL.Video.Palettes;
with SDL.Video.Windows;

with DAGBuild.GUI.Themes; use DAGBuild.GUI.Themes;

--@TODO load settings from a file, persist changes in window size, position
package DAGBuild.GUI.Settings is
    
    -- Initial position of the window
    Init_Pos : SDL.Natural_Coordinates :=
    (
        X => SDL.Video.Windows.Centered_Window_Position,
        Y => SDL.Video.Windows.Centered_Window_Position
    );

    -- Initial Size of the window
    Init_Size : SDL.Positive_Sizes := 
    (
        Width  => 640,
        Height => 480
    );

    Font_Name       : String := "Muli-Medium.ttf";    -- var-width
    --Font_Name       : String := "FiraCode-Regular.ttf"; -- fixed-width
    --Font_Name       : String := "Fira Code Medium Nerd Font Complete Mono.ttf";
    Font_Size       : SDL.TTFs.Point_Sizes := 14;

    Double_Click_Threshold  : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(500);
    Cursor_Blink_Rate       : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(400);

    -- Length of time required for the mouse to sit still before it's considered "hovering"
    Hover_Tooltip_Time      : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(1200);

    -- Use same elements that VSCode does for easy theme use, note that we use
    -- colors that might not correspond necessarily to what they're intended for
    -- in VSCode.
    type Color_Scheme is
    record
		Focus_Border                                : SDL.Video.Palettes.Colour;
		Foreground                                  : SDL.Video.Palettes.Colour;
		Widget_Shadow                               : SDL.Video.Palettes.Colour;
		Selection_Background                        : SDL.Video.Palettes.Colour;
        TextBlockQuote_Background                   : SDL.Video.Palettes.Colour;
		TextLink_Foreground                         : SDL.Video.Palettes.Colour;
		TextLink_ActiveForeground                   : SDL.Video.Palettes.Colour;
		TextPreformat_Foreground                    : SDL.Video.Palettes.Colour;
		Button_Background                           : SDL.Video.Palettes.Colour;
		Button_Foreground                           : SDL.Video.Palettes.Colour;
		Button_HoverBackground                      : SDL.Video.Palettes.Colour;
        Dropdown_Background                         : SDL.Video.Palettes.Colour;
		Dropdown_Foreground                         : SDL.Video.Palettes.Colour;
		Dropdown_Border                             : SDL.Video.Palettes.Colour;
        Input_Background                            : SDL.Video.Palettes.Colour;
		Input_Border                                : SDL.Video.Palettes.Colour;
		Input_Foreground                            : SDL.Video.Palettes.Colour;
		Input_PlaceholderForeground                 : SDL.Video.Palettes.Colour;
		InputOption_ActiveBorder                    : SDL.Video.Palettes.Colour;
		InputValidation_errorBackground             : SDL.Video.Palettes.Colour;
		InputValidation_errorBorder                 : SDL.Video.Palettes.Colour;
		InputValidation_InfoBackground              : SDL.Video.Palettes.Colour;
		InputValidation_InfoBorder                  : SDL.Video.Palettes.Colour;
		InputValidation_WarningBackground           : SDL.Video.Palettes.Colour;
		InputValidation_WarningBorder               : SDL.Video.Palettes.Colour;
		Scrollbar_Shadow                            : SDL.Video.Palettes.Colour;
		ScrollbarSlider_Background                  : SDL.Video.Palettes.Colour;
		ScrollbarSlider_HoverBackground             : SDL.Video.Palettes.Colour;
		ScrollbarSlider_ActiveBackground            : SDL.Video.Palettes.Colour;
		Badge_Background                            : SDL.Video.Palettes.Colour;
		Badge_Foreground                            : SDL.Video.Palettes.Colour;
		ProgressBar_Background                      : SDL.Video.Palettes.Colour;
		List_ActiveSelectionBackground              : SDL.Video.Palettes.Colour;
		List_ActiveSelectionForeground              : SDL.Video.Palettes.Colour;
		List_FocusBackground                        : SDL.Video.Palettes.Colour;
		List_FocusForeground                        : SDL.Video.Palettes.Colour;
		List_HighlightForeground                    : SDL.Video.Palettes.Colour;
		List_HoverBackground                        : SDL.Video.Palettes.Colour;
		List_HoverForeground                        : SDL.Video.Palettes.Colour;
		List_InactiveSelectionBackground            : SDL.Video.Palettes.Colour;
		List_InactiveSelectionForeground            : SDL.Video.Palettes.Colour;
		List_InvalidItemForeground                  : SDL.Video.Palettes.Colour;
		ActivityBar_Background                      : SDL.Video.Palettes.Colour;
		ActivityBar_Foreground                      : SDL.Video.Palettes.Colour;
		ActivityBar_Border                          : SDL.Video.Palettes.Colour;
		ActivityBarBadge_Background                 : SDL.Video.Palettes.Colour;
		ActivityBarBadge_Foreground                 : SDL.Video.Palettes.Colour;
		SideBar_Background                          : SDL.Video.Palettes.Colour;
		SideBar_Border                              : SDL.Video.Palettes.Colour;
		SideBarTitle_Foreground                     : SDL.Video.Palettes.Colour;
		SideBarSectionHeader_Background             : SDL.Video.Palettes.Colour;
		SideBarSectionHeader_Foreground             : SDL.Video.Palettes.Colour;
		EditorGroup_Border                          : SDL.Video.Palettes.Colour;
		EditorGroup_Background                      : SDL.Video.Palettes.Colour;
		EditorGroupHeader_NoTabsBackground          : SDL.Video.Palettes.Colour;
		EditorGroupHeader_tabsBackground            : SDL.Video.Palettes.Colour;
		EditorGroupHeader_tabsBorder                : SDL.Video.Palettes.Colour;
		Tab_ActiveBackground                        : SDL.Video.Palettes.Colour;
		Tab_ActiveForeground                        : SDL.Video.Palettes.Colour;
		Tab_Border                                  : SDL.Video.Palettes.Colour;
		Tab_ActiveBorder                            : SDL.Video.Palettes.Colour;
		Tab_UnfocusedActiveBorder                   : SDL.Video.Palettes.Colour;
		Tab_InactiveBackground                      : SDL.Video.Palettes.Colour;
		Tab_InactiveForeground                      : SDL.Video.Palettes.Colour;
		Tab_UnfocusedActiveForeground               : SDL.Video.Palettes.Colour;
		Tab_UnfocusedInactiveForeground             : SDL.Video.Palettes.Colour;
		Editor_Background                           : SDL.Video.Palettes.Colour;
		Editor_Foreground                           : SDL.Video.Palettes.Colour;
		EditorLineNumber_Foreground                 : SDL.Video.Palettes.Colour;
		EditorLineNumber_ActiveForeground           : SDL.Video.Palettes.Colour;
		EditorCursor_Foreground                     : SDL.Video.Palettes.Colour;
		Editor_SelectionBackground                  : SDL.Video.Palettes.Colour;
		Editor_InactiveSelectionBackground          : SDL.Video.Palettes.Colour;
		Editor_SelectionHighlightBackground         : SDL.Video.Palettes.Colour;
		Editor_SelectionHighlightBorder             : SDL.Video.Palettes.Colour;
		Editor_WordHighlightBackground              : SDL.Video.Palettes.Colour;
		Editor_WordHighlightStrongBackground        : SDL.Video.Palettes.Colour;
		Editor_FindMatchBackground                  : SDL.Video.Palettes.Colour;
		Editor_FindMatchBorder                      : SDL.Video.Palettes.Colour;
		Editor_FindMatchHighlightBackground         : SDL.Video.Palettes.Colour;
		Editor_FindMatchHighlightBorder             : SDL.Video.Palettes.Colour;
		Editor_FindRangeHighlightBackground         : SDL.Video.Palettes.Colour;
		Editor_FindRangeHighlightBorder             : SDL.Video.Palettes.Colour;
		Editor_LineHighlightBackground              : SDL.Video.Palettes.Colour;
		EditorLink_ActiveForeground                 : SDL.Video.Palettes.Colour;
		Editor_RangeHighlightBackground             : SDL.Video.Palettes.Colour;
		EditorWhitespace_Foreground                 : SDL.Video.Palettes.Colour;
		EditorIndentGuide_Background                : SDL.Video.Palettes.Colour;
		EditorIndentGuide_ActiveBackground          : SDL.Video.Palettes.Colour;
		EditorRuler_Foreground                      : SDL.Video.Palettes.Colour;
		EditorCodeLens_Foreground                   : SDL.Video.Palettes.Colour;
		EditorBracketMatch_Background               : SDL.Video.Palettes.Colour;
		EditorBracketMatch_Border                   : SDL.Video.Palettes.Colour;
		EditorOverviewRuler_Border                  : SDL.Video.Palettes.Colour;
		EditorOverviewRuler_ModifiedForeground      : SDL.Video.Palettes.Colour;
		EditorOverviewRuler_AddedForeground         : SDL.Video.Palettes.Colour;
		EditorOverviewRuler_DeletedForeground       : SDL.Video.Palettes.Colour;
		EditorOverviewRuler_errorForeground         : SDL.Video.Palettes.Colour;
		EditorOverviewRuler_WarningForeground       : SDL.Video.Palettes.Colour;
		EditorError_Foreground                      : SDL.Video.Palettes.Colour;
		EditorWarning_Foreground                    : SDL.Video.Palettes.Colour;
		EditorGutter_ModifiedBackground             : SDL.Video.Palettes.Colour;
		EditorGutter_AddedBackground                : SDL.Video.Palettes.Colour;
		EditorGutter_DeletedBackground              : SDL.Video.Palettes.Colour;
		DiffEditor_InsertedTextBackground           : SDL.Video.Palettes.Colour;
		DiffEditor_RemovedTextBackground            : SDL.Video.Palettes.Colour;
        EditorWidget_Background                     : SDL.Video.Palettes.Colour;
        EditorSuggestWidget_Foreground              : SDL.Video.Palettes.Colour;
		EditorSuggestWidget_Background              : SDL.Video.Palettes.Colour;
		EditorSuggestWidget_Border                  : SDL.Video.Palettes.Colour;
		EditorSuggestWidget_HighlightForeground     : SDL.Video.Palettes.Colour;
		EditorSuggestWidget_SelectedBackground      : SDL.Video.Palettes.Colour;
		EditorHoverWidget_Background                : SDL.Video.Palettes.Colour;
		EditorHoverWidget_Border                    : SDL.Video.Palettes.Colour;
		DebugExceptionWidget_Border                 : SDL.Video.Palettes.Colour;
		DebugExceptionWidget_Background             : SDL.Video.Palettes.Colour;
		EditorMarkerNavigation_Background           : SDL.Video.Palettes.Colour;
        PeekView_Border                             : SDL.Video.Palettes.Colour;
		PeekViewEditor_Background                   : SDL.Video.Palettes.Colour;
		PeekViewEditor_MatchHighlightBackground     : SDL.Video.Palettes.Colour;
		PeekViewResult_Background                   : SDL.Video.Palettes.Colour;
		PeekViewResult_FileForeground               : SDL.Video.Palettes.Colour;
		PeekViewResult_MatchHighlightBackground     : SDL.Video.Palettes.Colour;
		PeekViewTitle_Background                    : SDL.Video.Palettes.Colour;
		PeekViewTitleDescription_Foreground         : SDL.Video.Palettes.Colour;
		PeekViewTitleLabel_Foreground               : SDL.Video.Palettes.Colour;
		Panel_Background                            : SDL.Video.Palettes.Colour;
		Panel_Border                                : SDL.Video.Palettes.Colour;
		PanelTitle_ActiveBorder                     : SDL.Video.Palettes.Colour;
		PanelTitle_ActiveForeground                 : SDL.Video.Palettes.Colour;
		PanelTitle_InactiveForeground               : SDL.Video.Palettes.Colour;
		StatusBar_Background                        : SDL.Video.Palettes.Colour;
		StatusBar_Foreground                        : SDL.Video.Palettes.Colour;
		StatusBar_Border                            : SDL.Video.Palettes.Colour;
		StatusBar_DebuggingBackground               : SDL.Video.Palettes.Colour;
		StatusBar_DebuggingForeground               : SDL.Video.Palettes.Colour;
		StatusBar_NoFolderBackground                : SDL.Video.Palettes.Colour;
		StatusBarItem_ActiveBackground              : SDL.Video.Palettes.Colour;
		StatusBarItem_HoverBackground               : SDL.Video.Palettes.Colour;
		StatusBarItem_ProminentBackground           : SDL.Video.Palettes.Colour;
		StatusBarItem_ProminentHoverBackground      : SDL.Video.Palettes.Colour;
		TitleBar_ActiveBackground                   : SDL.Video.Palettes.Colour;
		TitleBar_ActiveForeground                   : SDL.Video.Palettes.Colour;
		TitleBar_InactiveBackground                 : SDL.Video.Palettes.Colour;
		TitleBar_InactiveForeground                 : SDL.Video.Palettes.Colour;
		TitleBar_Border                             : SDL.Video.Palettes.Colour;
        ExtensionButton_ProminentForeground         : SDL.Video.Palettes.Colour;
		ExtensionButton_ProminentBackground         : SDL.Video.Palettes.Colour;
		ExtensionButton_ProminentHoverBackground    : SDL.Video.Palettes.Colour;
        PickerGroup_Border                          : SDL.Video.Palettes.Colour;
		PickerGroup_Foreground                      : SDL.Video.Palettes.Colour;
        DebugToolBar_Background                     : SDL.Video.Palettes.Colour;
        WalkThrough_embeddedEditorBackground        : SDL.Video.Palettes.Colour;
        GitDecoration_ModifiedResourceForeground    : SDL.Video.Palettes.Colour;
		GitDecoration_DeletedResourceForeground     : SDL.Video.Palettes.Colour;
		GitDecoration_UntrackedResourceForeground   : SDL.Video.Palettes.Colour;
		GitDecoration_IgnoredResourceForeground     : SDL.Video.Palettes.Colour;
		GitDecoration_conflictingResourceForeground : SDL.Video.Palettes.Colour;
		GitDecoration_SubmoduleResourceForeground   : SDL.Video.Palettes.Colour;
		Settings_HeaderForeground                   : SDL.Video.Palettes.Colour;
		Settings_ModifiedItemIndicator              : SDL.Video.Palettes.Colour;
        Terminal_Background                         : SDL.Video.Palettes.Colour;
		Terminal_Foreground                         : SDL.Video.Palettes.Colour;
		Terminal_ANSIBlack                          : SDL.Video.Palettes.Colour;
		Terminal_ANSIRed                            : SDL.Video.Palettes.Colour;
		Terminal_ANSIGreen                          : SDL.Video.Palettes.Colour;
		Terminal_ANSIYellow                         : SDL.Video.Palettes.Colour;
		Terminal_ANSIBlue                           : SDL.Video.Palettes.Colour;
		Terminal_ANSIMagenta                        : SDL.Video.Palettes.Colour;
		Terminal_ANSICyan                           : SDL.Video.Palettes.Colour;
		Terminal_ANSIWhite                          : SDL.Video.Palettes.Colour;
		Terminal_ANSIBrightBlack                    : SDL.Video.Palettes.Colour;
		Terminal_ANSIBrightRed                      : SDL.Video.Palettes.Colour;
		Terminal_ANSIBrightGreen                    : SDL.Video.Palettes.Colour;
		Terminal_ANSIBrightYellow                   : SDL.Video.Palettes.Colour;
		Terminal_ANSIBrightBlue                     : SDL.Video.Palettes.Colour;
		Terminal_ANSIBrightMagenta                  : SDL.Video.Palettes.Colour;
		Terminal_ANSIBrightCyan                     : SDL.Video.Palettes.Colour;
		Terminal_ANSIBrightWhite                    : SDL.Video.Palettes.Colour;
    end record;

    Default_Dark : Color_Scheme := Color_Scheme'(
		Focus_Border                                => Hex_Color (16#505867#),
		Foreground                                  => Hex_Color (16#707a8c#),
		Widget_Shadow                               => Hex_Color (16#141925#),
		Selection_Background                        => Hex_Color (16#2a3546#),
        TextBlockQuote_Background                   => Hex_Color (16#232834#),
		TextLink_Foreground                         => Hex_Color (16#ffcc66#),
		TextLink_ActiveForeground                   => Hex_Color (16#ffcc66#),
		TextPreformat_Foreground                    => Hex_Color (16#cbccc6#),
		Button_Foreground                           => Hex_Color (16#cbccc6#),
		Button_Background                           => Hex_Color (16#3c526a#),
		Button_HoverBackground                      => Hex_Color (16#707a8c#),
        Dropdown_Background                         => Hex_Color (16#232834#),
		Dropdown_Foreground                         => Hex_Color (16#707a8c#),
		Dropdown_Border                             => Hex_Color (16#373e4c#),
        Input_Background                            => Hex_Color (16#232834#),
		Input_Border                                => Hex_Color (16#373e4c#),
		Input_Foreground                            => Hex_Color (16#cbccc6#),
		Input_PlaceholderForeground                 => Hex_Color (16#586070#),
		InputOption_ActiveBorder                    => Hex_Color (16#ffcc66#),
		InputValidation_errorBackground             => Hex_Color (16#1f2430#),
		InputValidation_errorBorder                 => Hex_Color (16#ff3333#),
		InputValidation_InfoBackground              => Hex_Color (16#1f2430#),
		InputValidation_InfoBorder                  => Hex_Color (16#5ccfe6#),
		InputValidation_WarningBackground           => Hex_Color (16#1f2430#),
		InputValidation_WarningBorder               => Hex_Color (16#ffd580#),
		Scrollbar_Shadow                            => Hex_Color (16#191e2a#),
		ScrollbarSlider_Background                  => Hex_Color (16#5c6773#),
		ScrollbarSlider_HoverBackground             => Hex_Color (16#707a8c#),
		ScrollbarSlider_ActiveBackground            => Hex_Color (16#cbccc6#),
		Badge_Background                            => Hex_Color (16#ffcc66#),
		Badge_Foreground                            => Hex_Color (16#1f2430#),
		ProgressBar_Background                      => Hex_Color (16#ffcc66#),
		List_ActiveSelectionBackground              => Hex_Color (16#191e2a#),
		List_ActiveSelectionForeground              => Hex_Color (16#707a8c#),
		List_FocusBackground                        => Hex_Color (16#191e2a#),
		List_FocusForeground                        => Hex_Color (16#707a8c#),
		List_HighlightForeground                    => Hex_Color (16#ffcc66#),
		List_HoverBackground                        => Hex_Color (16#191e2a#),
		List_HoverForeground                        => Hex_Color (16#707a8c#),
		List_InactiveSelectionBackground            => Hex_Color (16#191e2a#),
		List_InactiveSelectionForeground            => Hex_Color (16#707a8c#),
		List_InvalidItemForeground                  => Hex_Color (16#586070#),
		ActivityBar_Background                      => Hex_Color (16#1f2430#),
		ActivityBar_Foreground                      => Hex_Color (16#707a8ccc#),
		ActivityBar_Border                          => Hex_Color (16#1f2430#),
		ActivityBarBadge_Background                 => Hex_Color (16#ffcc66#),
		ActivityBarBadge_Foreground                 => Hex_Color (16#1f2430#),
		SideBar_Background                          => Hex_Color (16#1f2430#),
		SideBar_Border                              => Hex_Color (16#1f2430#),
		SideBarTitle_Foreground                     => Hex_Color (16#707a8c#),
		SideBarSectionHeader_Background             => Hex_Color (16#1f2430#),
		SideBarSectionHeader_Foreground             => Hex_Color (16#707a8c#),
		EditorGroup_Border                          => Hex_Color (16#191e2a#),
		EditorGroup_Background                      => Hex_Color (16#232834#),
		EditorGroupHeader_NoTabsBackground          => Hex_Color (16#1f2430#),
		EditorGroupHeader_tabsBackground            => Hex_Color (16#1f2430#),
		EditorGroupHeader_tabsBorder                => Hex_Color (16#1f2430#),
		Tab_ActiveBackground                        => Hex_Color (16#1f2430#),
		Tab_ActiveForeground                        => Hex_Color (16#cbccc6#),
		Tab_Border                                  => Hex_Color (16#1f2430#),
		Tab_ActiveBorder                            => Hex_Color (16#ffcc66#),
		Tab_UnfocusedActiveBorder                   => Hex_Color (16#707a8c#),
		Tab_InactiveBackground                      => Hex_Color (16#1f2430#),
		Tab_InactiveForeground                      => Hex_Color (16#707a8c#),
		Tab_UnfocusedActiveForeground               => Hex_Color (16#707a8c#),
		Tab_UnfocusedInactiveForeground             => Hex_Color (16#707a8c#),
		Editor_Background                           => Hex_Color (16#1f2430#),
		Editor_Foreground                           => Hex_Color (16#cbccc6#),
		EditorLineNumber_Foreground                 => Hex_Color (16#707a8c66#),
		EditorLineNumber_ActiveForeground           => Hex_Color (16#707a8ccc#),
		EditorCursor_Foreground                     => Hex_Color (16#ffcc66#),
		Editor_SelectionBackground                  => Hex_Color (16#2a3546#),
		Editor_InactiveSelectionBackground          => Hex_Color (16#262f3e#),
		Editor_SelectionHighlightBackground         => Hex_Color (16#262f3e#),
		Editor_SelectionHighlightBorder             => Hex_Color (16#313e52#),
		Editor_WordHighlightBackground              => Hex_Color (16#262f3e#),
		Editor_WordHighlightStrongBackground        => Hex_Color (16#ffcc6633#),
		Editor_FindMatchBackground                  => Hex_Color (16#ffcc660d#),
		Editor_FindMatchBorder                      => Hex_Color (16#ffcc66#),
		Editor_FindMatchHighlightBackground         => Hex_Color (16#ffcc660d#),
		Editor_FindMatchHighlightBorder             => Hex_Color (16#ffcc6659#),
		Editor_FindRangeHighlightBackground         => Hex_Color (16#262f3e#),
		Editor_FindRangeHighlightBorder             => Hex_Color (16#1f243000#),
		Editor_LineHighlightBackground              => Hex_Color (16#191e2a#),
		EditorLink_ActiveForeground                 => Hex_Color (16#ffcc66#),
		Editor_RangeHighlightBackground             => Hex_Color (16#191e2a#),
		EditorWhitespace_Foreground                 => Hex_Color (16#707a8c66#),
		EditorIndentGuide_Background                => Hex_Color (16#707a8c4d#),
		EditorIndentGuide_ActiveBackground          => Hex_Color (16#707a8cb3#),
		EditorRuler_Foreground                      => Hex_Color (16#707a8c4d#),
		EditorCodeLens_Foreground                   => Hex_Color (16#5c6773#),
		EditorBracketMatch_Background               => Hex_Color (16#707a8c4d#),
		EditorBracketMatch_Border                   => Hex_Color (16#707a8c99#),
		EditorOverviewRuler_Border                  => Hex_Color (16#191e2a#),
		EditorOverviewRuler_ModifiedForeground      => Hex_Color (16#77a8d999#),
		EditorOverviewRuler_AddedForeground         => Hex_Color (16#a6cc7099#),
		EditorOverviewRuler_DeletedForeground       => Hex_Color (16#f2798399#),
		EditorOverviewRuler_errorForeground         => Hex_Color (16#ff3333#),
		EditorOverviewRuler_WarningForeground       => Hex_Color (16#ffcc66#),
		EditorError_Foreground                      => Hex_Color (16#ff3333#),
		EditorWarning_Foreground                    => Hex_Color (16#ffcc66#),
		EditorGutter_ModifiedBackground             => Hex_Color (16#77a8d999#),
		EditorGutter_AddedBackground                => Hex_Color (16#a6cc7099#),
		EditorGutter_DeletedBackground              => Hex_Color (16#f2798399#),
		DiffEditor_InsertedTextBackground           => Hex_Color (16#bae67e26#),
		DiffEditor_RemovedTextBackground            => Hex_Color (16#f29e7426#),
        EditorWidget_Background                     => Hex_Color (16#232834#),
        EditorSuggestWidget_Foreground              => Hex_Color (16#cbccc6#),
		EditorSuggestWidget_Background              => Hex_Color (16#23283499#),
		EditorSuggestWidget_Border                  => Hex_Color (16#ffd580#),
		EditorSuggestWidget_HighlightForeground     => Hex_Color (16#ffcc66#),
		EditorSuggestWidget_SelectedBackground      => Hex_Color (16#191e2a#),
		EditorHoverWidget_Background                => Hex_Color (16#232834#),
		EditorHoverWidget_Border                    => Hex_Color (16#101521#),
		DebugExceptionWidget_Border                 => Hex_Color (16#191e2a#),
		DebugExceptionWidget_Background             => Hex_Color (16#232834#),
		EditorMarkerNavigation_Background           => Hex_Color (16#232834#),
        PeekView_Border                             => Hex_Color (16#191e2a#),
		PeekViewEditor_Background                   => Hex_Color (16#232834#),
		PeekViewEditor_MatchHighlightBackground     => Hex_Color (16#ffcc6633#),
		PeekViewResult_Background                   => Hex_Color (16#232834#),
		PeekViewResult_FileForeground               => Hex_Color (16#707a8c#),
		PeekViewResult_MatchHighlightBackground     => Hex_Color (16#ffcc6633#),
		PeekViewTitle_Background                    => Hex_Color (16#232834#),
		PeekViewTitleDescription_Foreground         => Hex_Color (16#707a8c#),
		PeekViewTitleLabel_Foreground               => Hex_Color (16#707a8c#),
		Panel_Background                            => Hex_Color (16#1f2430#),
		Panel_Border                                => Hex_Color (16#191e2a#),
		PanelTitle_ActiveBorder                     => Hex_Color (16#ffcc66#),
		PanelTitle_ActiveForeground                 => Hex_Color (16#cbccc6#),
		PanelTitle_InactiveForeground               => Hex_Color (16#707a8c#),
		StatusBar_Background                        => Hex_Color (16#1f2430#),
		StatusBar_Foreground                        => Hex_Color (16#707a8c#),
		StatusBar_Border                            => Hex_Color (16#1f2430#),
		StatusBar_DebuggingBackground               => Hex_Color (16#f29e74#),
		StatusBar_DebuggingForeground               => Hex_Color (16#1f2430#),
		StatusBar_NoFolderBackground                => Hex_Color (16#232834#),
		StatusBarItem_ActiveBackground              => Hex_Color (16#00000050#),
		StatusBarItem_HoverBackground               => Hex_Color (16#00000030#),
		StatusBarItem_ProminentBackground           => Hex_Color (16#191e2a#),
		StatusBarItem_ProminentHoverBackground      => Hex_Color (16#00000030#),
		TitleBar_ActiveBackground                   => Hex_Color (16#1f2430#),
		TitleBar_ActiveForeground                   => Hex_Color (16#cbccc6#),
		TitleBar_InactiveBackground                 => Hex_Color (16#1f2430#),
		TitleBar_InactiveForeground                 => Hex_Color (16#707a8c#),
		TitleBar_Border                             => Hex_Color (16#1f2430#),
        ExtensionButton_ProminentForeground         => Hex_Color (16#1f2430#),
		ExtensionButton_ProminentBackground         => Hex_Color (16#ffcc66#),
		ExtensionButton_ProminentHoverBackground    => Hex_Color (16#fac761#),
        PickerGroup_Border                          => Hex_Color (16#191e2a#),
		PickerGroup_Foreground                      => Hex_Color (16#484f5e#),
        DebugToolBar_Background                     => Hex_Color (16#232834#),
        WalkThrough_EmbeddedEditorBackground        => Hex_Color (16#232834#),
        GitDecoration_ModifiedResourceForeground    => Hex_Color (16#77a8d9b3#),
		GitDecoration_DeletedResourceForeground     => Hex_Color (16#f27983b3#),
		GitDecoration_UntrackedResourceForeground   => Hex_Color (16#a6cc70b3#),
		GitDecoration_IgnoredResourceForeground     => Hex_Color (16#484f5e#),
		GitDecoration_conflictingResourceForeground => Hex_Color (16#FFFFFFFF#),
		GitDecoration_SubmoduleResourceForeground   => Hex_Color (16#d4bfffb3#),
		Settings_HeaderForeground                   => Hex_Color (16#cbccc6#),
		Settings_ModifiedItemIndicator              => Hex_Color (16#77a8d9#),
        Terminal_Background                         => Hex_Color (16#1f2430#),
		Terminal_Foreground                         => Hex_Color (16#cbccc6#),
		Terminal_ANSIBlack                          => Hex_Color (16#191e2a#),
		Terminal_ANSIRed                            => Hex_Color (16#ed8274#),
		Terminal_ANSIGreen                          => Hex_Color (16#a6cc70#),
		Terminal_ANSIYellow                         => Hex_Color (16#fad07b#),
		Terminal_ANSIBlue                           => Hex_Color (16#6dcbfa#),
		Terminal_ANSIMagenta                        => Hex_Color (16#cfbafa#),
		Terminal_ANSICyan                           => Hex_Color (16#90e1c6#),
		Terminal_ANSIWhite                          => Hex_Color (16#c7c7c7#),
		Terminal_ANSIBrightBlack                    => Hex_Color (16#686868#),
		Terminal_ANSIBrightRed                      => Hex_Color (16#f28779#),
		Terminal_ANSIBrightGreen                    => Hex_Color (16#bae67e#),
		Terminal_ANSIBrightYellow                   => Hex_Color (16#ffd580#),
		Terminal_ANSIBrightBlue                     => Hex_Color (16#73d0ff#),
		Terminal_ANSIBrightMagenta                  => Hex_Color (16#d4bfff#),
		Terminal_ANSIBrightCyan                     => Hex_Color (16#95e6cb#),
		Terminal_ANSIBrightWhite                    => Hex_Color (16#ffffff#)
    );

    Default_Light : Color_Scheme := Color_Scheme'(
        Focus_Border                                => Hex_Color (16#bdc2c8#),
        Foreground                                  => Hex_Color (16#959da6#),
        Widget_Shadow                               => Hex_Color (16#56606940#),
        Selection_Background                        => Hex_Color (16#e8eef4fd#),
        TextBlockQuote_Background                   => Hex_Color (16#ffffff#),
        TextLink_Foreground                         => Hex_Color (16#ff9940#),
        TextLink_ActiveForeground                   => Hex_Color (16#ff9940#),
        TextPreformat_Foreground                    => Hex_Color (16#6c7680#),
        Button_Background                           => Hex_Color (16#ff9940#),
        Button_Foreground                           => Hex_Color (16#fafafa#),
        Button_HoverBackground                      => Hex_Color (16#f9943b#),
        Dropdown_Background                         => Hex_Color (16#ffffff#),
        Dropdown_Foreground                         => Hex_Color (16#959da6#),
        Dropdown_Border                             => Hex_Color (16#dcdee1#),
        Input_Background                            => Hex_Color (16#ffffff#),
        Input_Border                                => Hex_Color (16#dcdee1#),
        Input_Foreground                            => Hex_Color (16#6c7680#),
        Input_PlaceholderForeground                 => Hex_Color (16#b3b9bf#),
        InputOption_ActiveBorder                    => Hex_Color (16#ff9940#),
        InputValidation_errorBackground             => Hex_Color (16#fafafa#),
        InputValidation_errorBorder                 => Hex_Color (16#f51818#),
        InputValidation_InfoBackground              => Hex_Color (16#fafafa#),
        InputValidation_InfoBorder                  => Hex_Color (16#55b4d4#),
        InputValidation_WarningBackground           => Hex_Color (16#fafafa#),
        InputValidation_WarningBorder               => Hex_Color (16#f2ae49#),
        Scrollbar_Shadow                            => Hex_Color (16#959da61a#),
        ScrollbarSlider_Background                  => Hex_Color (16#959da667#),
        ScrollbarSlider_HoverBackground             => Hex_Color (16#959da699#),
        ScrollbarSlider_ActiveBackground            => Hex_Color (16#959da6b3#),
        Badge_Background                            => Hex_Color (16#ff9940#),
        Badge_Foreground                            => Hex_Color (16#fafafa#),
        ProgressBar_Background                      => Hex_Color (16#ff9940#),
        List_ActiveSelectionBackground              => Hex_Color (16#959da61a#),
        List_ActiveSelectionForeground              => Hex_Color (16#959da6#),
        List_FocusBackground                        => Hex_Color (16#959da61a#),
        List_FocusForeground                        => Hex_Color (16#959da6#),
        List_HighlightForeground                    => Hex_Color (16#ff9940#),
        List_HoverBackground                        => Hex_Color (16#959da61a#),
        List_HoverForeground                        => Hex_Color (16#959da6#),
        List_InactiveSelectionBackground            => Hex_Color (16#959da61a#),
        List_InactiveSelectionForeground            => Hex_Color (16#959da6#),
        List_InvalidItemForeground                  => Hex_Color (16#b3b9bf#),
        ActivityBar_Background                      => Hex_Color (16#fafafa#),
        ActivityBar_Foreground                      => Hex_Color (16#959da6cc#),
        ActivityBar_Border                          => Hex_Color (16#fafafa#),
        ActivityBarBadge_Background                 => Hex_Color (16#ff9940#),
        ActivityBarBadge_Foreground                 => Hex_Color (16#fafafa#),
        SideBar_Background                          => Hex_Color (16#fafafa#),
        SideBar_Border                              => Hex_Color (16#fafafa#),
        SideBarTitle_Foreground                     => Hex_Color (16#959da6#),
        SideBarSectionHeader_Background             => Hex_Color (16#fafafa#),
        SideBarSectionHeader_Foreground             => Hex_Color (16#959da6#),
        EditorGroup_Border                          => Hex_Color (16#959da61a#),
        EditorGroup_Background                      => Hex_Color (16#ffffff#),
        EditorGroupHeader_NoTabsBackground          => Hex_Color (16#fafafa#),
        EditorGroupHeader_tabsBackground            => Hex_Color (16#fafafa#),
        EditorGroupHeader_tabsBorder                => Hex_Color (16#fafafa#),
        Tab_ActiveBackground                        => Hex_Color (16#fafafa#),
        Tab_ActiveForeground                        => Hex_Color (16#6c7680#),
        Tab_Border                                  => Hex_Color (16#fafafa#),
        Tab_ActiveBorder                            => Hex_Color (16#ff9940#),
        Tab_UnfocusedActiveBorder                   => Hex_Color (16#959da6#),
        Tab_InactiveBackground                      => Hex_Color (16#fafafa#),
        Tab_InactiveForeground                      => Hex_Color (16#959da6#),
        Tab_UnfocusedActiveForeground               => Hex_Color (16#959da6#),
        Tab_UnfocusedInactiveForeground             => Hex_Color (16#959da6#),
        Editor_Background                           => Hex_Color (16#fafafa#),
        Editor_Foreground                           => Hex_Color (16#6c7680#),
        EditorLineNumber_Foreground                 => Hex_Color (16#959da666#),
        EditorLineNumber_ActiveForeground           => Hex_Color (16#959da6cc#),
        EditorCursor_Foreground                     => Hex_Color (16#ff9940#),
        Editor_SelectionBackground                  => Hex_Color (16#e8eef4#),
        Editor_InactiveSelectionBackground          => Hex_Color (16#eff3f6#),
        Editor_SelectionHighlightBackground         => Hex_Color (16#eff3f6#),
        Editor_SelectionHighlightBorder             => Hex_Color (16#dee8f1#),
        Editor_WordHighlightBackground              => Hex_Color (16#eff3f6#),
        Editor_WordHighlightStrongBackground        => Hex_Color (16#ff994033#),
        Editor_FindMatchBackground                  => Hex_Color (16#ff99400d#),
        Editor_FindMatchBorder                      => Hex_Color (16#ff9940#),
        Editor_FindMatchHighlightBackground         => Hex_Color (16#ff99400d#),
        Editor_FindMatchHighlightBorder             => Hex_Color (16#ff994059#),
        Editor_FindRangeHighlightBackground         => Hex_Color (16#eff3f6#),
        Editor_FindRangeHighlightBorder             => Hex_Color (16#fafafa00#),
        Editor_LineHighlightBackground              => Hex_Color (16#959da61a#),
        EditorLink_ActiveForeground                 => Hex_Color (16#ff9940#),
        Editor_RangeHighlightBackground             => Hex_Color (16#959da61a#),
        EditorWhitespace_Foreground                 => Hex_Color (16#959da666#),
        EditorIndentGuide_Background                => Hex_Color (16#959da64d#),
        EditorIndentGuide_ActiveBackground          => Hex_Color (16#959da6b3#),
        EditorRuler_Foreground                      => Hex_Color (16#959da64d#),
        EditorCodeLens_Foreground                   => Hex_Color (16#abb0b6#),
        EditorBracketMatch_Background               => Hex_Color (16#959da64d#),
        EditorBracketMatch_Border                   => Hex_Color (16#959da699#),
        EditorOverviewRuler_Border                  => Hex_Color (16#959da61a#),
        EditorOverviewRuler_ModifiedForeground      => Hex_Color (16#709ecc99#),
        EditorOverviewRuler_AddedForeground         => Hex_Color (16#99bf4d99#),
        EditorOverviewRuler_DeletedForeground       => Hex_Color (16#f2798399#),
        EditorOverviewRuler_errorForeground         => Hex_Color (16#f51818#),
        EditorOverviewRuler_WarningForeground       => Hex_Color (16#ff9940#),
        EditorError_Foreground                      => Hex_Color (16#f51818#),
        EditorWarning_Foreground                    => Hex_Color (16#ff9940#),
        EditorGutter_ModifiedBackground             => Hex_Color (16#709ecc99#),
        EditorGutter_AddedBackground                => Hex_Color (16#99bf4d99#),
        EditorGutter_DeletedBackground              => Hex_Color (16#f2798399#),
        DiffEditor_InsertedTextBackground           => Hex_Color (16#86b30026#),
        DiffEditor_RemovedTextBackground            => Hex_Color (16#ed936626#),
        EditorWidget_Background                     => Hex_Color (16#ffffff#),
        EditorSuggestWidget_Foreground              => Hex_Color (16#ff9940#),
        EditorSuggestWidget_Background              => Hex_Color (16#ffffff#),
        EditorSuggestWidget_Border                  => Hex_Color (16#f0f0f0#),
        EditorSuggestWidget_HighlightForeground     => Hex_Color (16#ff9940#),
        EditorSuggestWidget_SelectedBackground      => Hex_Color (16#959da61a#),
        EditorHoverWidget_Background                => Hex_Color (16#ffffff#),
        EditorHoverWidget_Border                    => Hex_Color (16#f0f0f0#),
        DebugExceptionWidget_Border                 => Hex_Color (16#959da61a#),
        DebugExceptionWidget_Background             => Hex_Color (16#ffffff#),
        EditorMarkerNavigation_Background           => Hex_Color (16#ffffff#),
        PeekView_Border                             => Hex_Color (16#959da61a#),
        PeekViewEditor_Background                   => Hex_Color (16#ffffff#),
        PeekViewEditor_MatchHighlightBackground     => Hex_Color (16#ff994033#),
        PeekViewResult_Background                   => Hex_Color (16#ffffff#),
        PeekViewResult_FileForeground               => Hex_Color (16#959da6#),
        PeekViewResult_MatchHighlightBackground     => Hex_Color (16#ff994033#),
        PeekViewTitle_Background                    => Hex_Color (16#ffffff#),
        PeekViewTitleDescription_Foreground         => Hex_Color (16#959da6#),
        PeekViewTitleLabel_Foreground               => Hex_Color (16#959da6#),
        Panel_Background                            => Hex_Color (16#fafafa#),
        Panel_Border                                => Hex_Color (16#959da61a#),
        PanelTitle_ActiveBorder                     => Hex_Color (16#ff9940#),
        PanelTitle_ActiveForeground                 => Hex_Color (16#6c7680#),
        PanelTitle_InactiveForeground               => Hex_Color (16#959da6#),
        StatusBar_Background                        => Hex_Color (16#fafafa#),
        StatusBar_Foreground                        => Hex_Color (16#959da6#),
        StatusBar_Border                            => Hex_Color (16#fafafa#),
        StatusBar_DebuggingBackground               => Hex_Color (16#ed9366#),
        StatusBar_DebuggingForeground               => Hex_Color (16#fafafa#),
        StatusBar_NoFolderBackground                => Hex_Color (16#ffffff#),
        StatusBarItem_ActiveBackground              => Hex_Color (16#00000050#),
        StatusBarItem_HoverBackground               => Hex_Color (16#00000030#),
        StatusBarItem_ProminentBackground           => Hex_Color (16#959da61a#),
        StatusBarItem_ProminentHoverBackground      => Hex_Color (16#00000030#),
        TitleBar_ActiveBackground                   => Hex_Color (16#fafafa#),
        TitleBar_ActiveForeground                   => Hex_Color (16#6c7680#),
        TitleBar_InactiveBackground                 => Hex_Color (16#fafafa#),
        TitleBar_InactiveForeground                 => Hex_Color (16#959da6#),
        TitleBar_Border                             => Hex_Color (16#fafafa#),
        ExtensionButton_ProminentForeground         => Hex_Color (16#fafafa#),
        ExtensionButton_ProminentBackground         => Hex_Color (16#ff9940#),
        ExtensionButton_ProminentHoverBackground    => Hex_Color (16#f9943b#),
        PickerGroup_Border                          => Hex_Color (16#959da61a#),
        PickerGroup_Foreground                      => Hex_Color (16#c8ccd0#),
        DebugToolBar_Background                     => Hex_Color (16#ffffff#),
        WalkThrough_EmbeddedEditorBackground        => Hex_Color (16#ffffff#),
        GitDecoration_ModifiedResourceForeground    => Hex_Color (16#709eccb3#),
        GitDecoration_DeletedResourceForeground     => Hex_Color (16#f27983b3#),
        GitDecoration_UntrackedResourceForeground   => Hex_Color (16#99bf4db3#),
        GitDecoration_IgnoredResourceForeground     => Hex_Color (16#c8ccd0#),
        GitDecoration_conflictingResourceForeground => Hex_Color (16#000000#),
        GitDecoration_SubmoduleResourceForeground   => Hex_Color (16#a37accb3#),
        Settings_HeaderForeground                   => Hex_Color (16#6c7680#),
        Settings_ModifiedItemIndicator              => Hex_Color (16#709ecc#),
        Terminal_Background                         => Hex_Color (16#fafafa#),
        Terminal_Foreground                         => Hex_Color (16#6c7680#),
        Terminal_ANSIBlack                          => Hex_Color (16#000000#),
        Terminal_ANSIRed                            => Hex_Color (16#ea6c6d#),
        Terminal_ANSIGreen                          => Hex_Color (16#99bf4d#),
        Terminal_ANSIYellow                         => Hex_Color (16#eca944#),
        Terminal_ANSIBlue                           => Hex_Color (16#3199e1#),
        Terminal_ANSIMagenta                        => Hex_Color (16#9e75c7#),
        Terminal_ANSICyan                           => Hex_Color (16#46ba94#),
        Terminal_ANSIWhite                          => Hex_Color (16#c7c7c7#),
        Terminal_ANSIBrightBlack                    => Hex_Color (16#686868#),
        Terminal_ANSIBrightRed                      => Hex_Color (16#f07171#),
        Terminal_ANSIBrightGreen                    => Hex_Color (16#86b300#),
        Terminal_ANSIBrightYellow                   => Hex_Color (16#f2ae49#),
        Terminal_ANSIBrightBlue                     => Hex_Color (16#399ee6#),
        Terminal_ANSIBrightMagenta                  => Hex_Color (16#a37acc#),
        Terminal_ANSIBrightCyan                     => Hex_Color (16#4cbf99#),
        Terminal_ANSIBrightWhite                    => Hex_Color (16#d1d1d1#)
    );

end DAGBuild.GUI.Settings;