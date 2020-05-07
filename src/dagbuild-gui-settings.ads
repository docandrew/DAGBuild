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
		Button_background                           : SDL.Video.Palettes.Colour;
		Button_foreground                           : SDL.Video.Palettes.Colour;
		Button_hoverBackground                      : SDL.Video.Palettes.Colour;
        Dropdown_background                         : SDL.Video.Palettes.Colour;
		Dropdown_foreground                         : SDL.Video.Palettes.Colour;
		Dropdown_border                             : SDL.Video.Palettes.Colour;
        Input_background                            : SDL.Video.Palettes.Colour;
		Input_border                                : SDL.Video.Palettes.Colour;
		Input_foreground                            : SDL.Video.Palettes.Colour;
		Input_placeholderForeground                 : SDL.Video.Palettes.Colour;
		InputOption_activeBorder                    : SDL.Video.Palettes.Colour;
		InputValidation_errorBackground             : SDL.Video.Palettes.Colour;
		InputValidation_errorBorder                 : SDL.Video.Palettes.Colour;
		InputValidation_infoBackground              : SDL.Video.Palettes.Colour;
		InputValidation_infoBorder                  : SDL.Video.Palettes.Colour;
		InputValidation_warningBackground           : SDL.Video.Palettes.Colour;
		InputValidation_warningBorder               : SDL.Video.Palettes.Colour;
		Scrollbar_shadow                            : SDL.Video.Palettes.Colour;
		ScrollbarSlider_background                  : SDL.Video.Palettes.Colour;
		ScrollbarSlider_hoverBackground             : SDL.Video.Palettes.Colour;
		ScrollbarSlider_activeBackground            : SDL.Video.Palettes.Colour;
		Badge_background                            : SDL.Video.Palettes.Colour;
		Badge_foreground                            : SDL.Video.Palettes.Colour;
		ProgressBar_background                      : SDL.Video.Palettes.Colour;
		List_activeSelectionBackground              : SDL.Video.Palettes.Colour;
		List_activeSelectionForeground              : SDL.Video.Palettes.Colour;
		List_focusBackground                        : SDL.Video.Palettes.Colour;
		List_focusForeground                        : SDL.Video.Palettes.Colour;
		List_highlightForeground                    : SDL.Video.Palettes.Colour;
		List_hoverBackground                        : SDL.Video.Palettes.Colour;
		List_hoverForeground                        : SDL.Video.Palettes.Colour;
		List_inactiveSelectionBackground            : SDL.Video.Palettes.Colour;
		List_inactiveSelectionForeground            : SDL.Video.Palettes.Colour;
		List_invalidItemForeground                  : SDL.Video.Palettes.Colour;
		ActivityBar_background                      : SDL.Video.Palettes.Colour;
		ActivityBar_foreground                      : SDL.Video.Palettes.Colour;
		ActivityBar_border                          : SDL.Video.Palettes.Colour;
		ActivityBarBadge_background                 : SDL.Video.Palettes.Colour;
		ActivityBarBadge_foreground                 : SDL.Video.Palettes.Colour;
		SideBar_background                          : SDL.Video.Palettes.Colour;
		SideBar_border                              : SDL.Video.Palettes.Colour;
		SideBarTitle_foreground                     : SDL.Video.Palettes.Colour;
		SideBarSectionHeader_background             : SDL.Video.Palettes.Colour;
		SideBarSectionHeader_foreground             : SDL.Video.Palettes.Colour;
		EditorGroup_border                          : SDL.Video.Palettes.Colour;
		EditorGroup_background                      : SDL.Video.Palettes.Colour;
		EditorGroupHeader_noTabsBackground          : SDL.Video.Palettes.Colour;
		EditorGroupHeader_tabsBackground            : SDL.Video.Palettes.Colour;
		EditorGroupHeader_tabsBorder                : SDL.Video.Palettes.Colour;
		Tab_activeBackground                        : SDL.Video.Palettes.Colour;
		Tab_activeForeground                        : SDL.Video.Palettes.Colour;
		Tab_border                                  : SDL.Video.Palettes.Colour;
		Tab_activeBorder                            : SDL.Video.Palettes.Colour;
		Tab_unfocusedActiveBorder                   : SDL.Video.Palettes.Colour;
		Tab_inactiveBackground                      : SDL.Video.Palettes.Colour;
		Tab_inactiveForeground                      : SDL.Video.Palettes.Colour;
		Tab_unfocusedActiveForeground               : SDL.Video.Palettes.Colour;
		Tab_unfocusedInactiveForeground             : SDL.Video.Palettes.Colour;
		Editor_background                           : SDL.Video.Palettes.Colour;
		Editor_foreground                           : SDL.Video.Palettes.Colour;
		EditorLineNumber_foreground                 : SDL.Video.Palettes.Colour;
		EditorLineNumber_activeForeground           : SDL.Video.Palettes.Colour;
		EditorCursor_foreground                     : SDL.Video.Palettes.Colour;
		Editor_selectionBackground                  : SDL.Video.Palettes.Colour;
		Editor_inactiveSelectionBackground          : SDL.Video.Palettes.Colour;
		Editor_selectionHighlightBackground         : SDL.Video.Palettes.Colour;
		Editor_selectionHighlightBorder             : SDL.Video.Palettes.Colour;
		Editor_wordHighlightBackground              : SDL.Video.Palettes.Colour;
		Editor_wordHighlightStrongBackground        : SDL.Video.Palettes.Colour;
		Editor_findMatchBackground                  : SDL.Video.Palettes.Colour;
		Editor_findMatchBorder                      : SDL.Video.Palettes.Colour;
		Editor_findMatchHighlightBackground         : SDL.Video.Palettes.Colour;
		Editor_findMatchHighlightBorder             : SDL.Video.Palettes.Colour;
		Editor_findRangeHighlightBackground         : SDL.Video.Palettes.Colour;
		Editor_findRangeHighlightBorder             : SDL.Video.Palettes.Colour;
		Editor_lineHighlightBackground              : SDL.Video.Palettes.Colour;
		EditorLink_activeForeground                 : SDL.Video.Palettes.Colour;
		Editor_rangeHighlightBackground             : SDL.Video.Palettes.Colour;
		EditorWhitespace_foreground                 : SDL.Video.Palettes.Colour;
		EditorIndentGuide_background                : SDL.Video.Palettes.Colour;
		EditorIndentGuide_activeBackground          : SDL.Video.Palettes.Colour;
		EditorRuler_foreground                      : SDL.Video.Palettes.Colour;
		EditorCodeLens_foreground                   : SDL.Video.Palettes.Colour;
		EditorBracketMatch_background               : SDL.Video.Palettes.Colour;
		EditorBracketMatch_border                   : SDL.Video.Palettes.Colour;
		EditorOverviewRuler_border                  : SDL.Video.Palettes.Colour;
		EditorOverviewRuler_modifiedForeground      : SDL.Video.Palettes.Colour;
		EditorOverviewRuler_addedForeground         : SDL.Video.Palettes.Colour;
		EditorOverviewRuler_deletedForeground       : SDL.Video.Palettes.Colour;
		EditorOverviewRuler_errorForeground         : SDL.Video.Palettes.Colour;
		EditorOverviewRuler_warningForeground       : SDL.Video.Palettes.Colour;
		EditorError_foreground                      : SDL.Video.Palettes.Colour;
		EditorWarning_foreground                    : SDL.Video.Palettes.Colour;
		EditorGutter_modifiedBackground             : SDL.Video.Palettes.Colour;
		EditorGutter_addedBackground                : SDL.Video.Palettes.Colour;
		EditorGutter_deletedBackground              : SDL.Video.Palettes.Colour;
		DiffEditor_insertedTextBackground           : SDL.Video.Palettes.Colour;
		DiffEditor_removedTextBackground            : SDL.Video.Palettes.Colour;
        EditorWidget_background                     : SDL.Video.Palettes.Colour;
		EditorSuggestWidget_background              : SDL.Video.Palettes.Colour;
		EditorSuggestWidget_border                  : SDL.Video.Palettes.Colour;
		EditorSuggestWidget_highlightForeground     : SDL.Video.Palettes.Colour;
		EditorSuggestWidget_selectedBackground      : SDL.Video.Palettes.Colour;
		EditorHoverWidget_background                : SDL.Video.Palettes.Colour;
		EditorHoverWidget_border                    : SDL.Video.Palettes.Colour;
		DebugExceptionWidget_border                 : SDL.Video.Palettes.Colour;
		DebugExceptionWidget_background             : SDL.Video.Palettes.Colour;
		EditorMarkerNavigation_background           : SDL.Video.Palettes.Colour;
        PeekView_border                             : SDL.Video.Palettes.Colour;
		PeekViewEditor_background                   : SDL.Video.Palettes.Colour;
		PeekViewEditor_matchHighlightBackground     : SDL.Video.Palettes.Colour;
		PeekViewResult_background                   : SDL.Video.Palettes.Colour;
		PeekViewResult_fileForeground               : SDL.Video.Palettes.Colour;
		PeekViewResult_matchHighlightBackground     : SDL.Video.Palettes.Colour;
		PeekViewTitle_background                    : SDL.Video.Palettes.Colour;
		PeekViewTitleDescription_foreground         : SDL.Video.Palettes.Colour;
		PeekViewTitleLabel_foreground               : SDL.Video.Palettes.Colour;
		Panel_background                            : SDL.Video.Palettes.Colour;
		Panel_border                                : SDL.Video.Palettes.Colour;
		PanelTitle_activeBorder                     : SDL.Video.Palettes.Colour;
		PanelTitle_activeForeground                 : SDL.Video.Palettes.Colour;
		PanelTitle_inactiveForeground               : SDL.Video.Palettes.Colour;
		StatusBar_background                        : SDL.Video.Palettes.Colour;
		StatusBar_foreground                        : SDL.Video.Palettes.Colour;
		StatusBar_border                            : SDL.Video.Palettes.Colour;
		StatusBar_debuggingBackground               : SDL.Video.Palettes.Colour;
		StatusBar_debuggingForeground               : SDL.Video.Palettes.Colour;
		StatusBar_noFolderBackground                : SDL.Video.Palettes.Colour;
		StatusBarItem_activeBackground              : SDL.Video.Palettes.Colour;
		StatusBarItem_hoverBackground               : SDL.Video.Palettes.Colour;
		StatusBarItem_prominentBackground           : SDL.Video.Palettes.Colour;
		StatusBarItem_prominentHoverBackground      : SDL.Video.Palettes.Colour;
		TitleBar_activeBackground                   : SDL.Video.Palettes.Colour;
		TitleBar_activeForeground                   : SDL.Video.Palettes.Colour;
		TitleBar_inactiveBackground                 : SDL.Video.Palettes.Colour;
		TitleBar_inactiveForeground                 : SDL.Video.Palettes.Colour;
		TitleBar_border                             : SDL.Video.Palettes.Colour;
        ExtensionButton_prominentForeground         : SDL.Video.Palettes.Colour;
		ExtensionButton_prominentBackground         : SDL.Video.Palettes.Colour;
		ExtensionButton_prominentHoverBackground    : SDL.Video.Palettes.Colour;
        PickerGroup_border                          : SDL.Video.Palettes.Colour;
		PickerGroup_foreground                      : SDL.Video.Palettes.Colour;
        debugToolBar_background                     : SDL.Video.Palettes.Colour;
        walkThrough_embeddedEditorBackground        : SDL.Video.Palettes.Colour;
        GitDecoration_modifiedResourceForeground    : SDL.Video.Palettes.Colour;
		GitDecoration_deletedResourceForeground     : SDL.Video.Palettes.Colour;
		GitDecoration_untrackedResourceForeground   : SDL.Video.Palettes.Colour;
		GitDecoration_ignoredResourceForeground     : SDL.Video.Palettes.Colour;
		GitDecoration_conflictingResourceForeground : SDL.Video.Palettes.Colour;
		GitDecoration_submoduleResourceForeground   : SDL.Video.Palettes.Colour;
		Settings_headerForeground                   : SDL.Video.Palettes.Colour;
		Settings_modifiedItemIndicator              : SDL.Video.Palettes.Colour;
        Terminal_background                         : SDL.Video.Palettes.Colour;
		Terminal_foreground                         : SDL.Video.Palettes.Colour;
		Terminal_ansiBlack                          : SDL.Video.Palettes.Colour;
		Terminal_ansiRed                            : SDL.Video.Palettes.Colour;
		Terminal_ansiGreen                          : SDL.Video.Palettes.Colour;
		Terminal_ansiYellow                         : SDL.Video.Palettes.Colour;
		Terminal_ansiBlue                           : SDL.Video.Palettes.Colour;
		Terminal_ansiMagenta                        : SDL.Video.Palettes.Colour;
		Terminal_ansiCyan                           : SDL.Video.Palettes.Colour;
		Terminal_ansiWhite                          : SDL.Video.Palettes.Colour;
		Terminal_ansiBrightBlack                    : SDL.Video.Palettes.Colour;
		Terminal_ansiBrightRed                      : SDL.Video.Palettes.Colour;
		Terminal_ansiBrightGreen                    : SDL.Video.Palettes.Colour;
		Terminal_ansiBrightYellow                   : SDL.Video.Palettes.Colour;
		Terminal_ansiBrightBlue                     : SDL.Video.Palettes.Colour;
		Terminal_ansiBrightMagenta                  : SDL.Video.Palettes.Colour;
		Terminal_ansiBrightCyan                     : SDL.Video.Palettes.Colour;
		Terminal_ansiBrightWhite                    : SDL.Video.Palettes.Colour;
    end record;

    Default_Dark : Color_Scheme := Color_Scheme'(
		Focus_Border                                => Hex_Color(16#505867#),
		Foreground                                  => Hex_Color(16#707a8c#),
		Widget_Shadow                               => Hex_Color(16#141925#),
		Selection_Background                        => Hex_Color(16#2a3546#),
        TextBlockQuote_Background                   => Hex_Color(16#232834#),
		TextLink_Foreground                         => Hex_Color(16#ffcc66#),
		TextLink_ActiveForeground                   => Hex_Color(16#ffcc66#),
		TextPreformat_Foreground                    => Hex_Color(16#cbccc6#),
		Button_foreground                           => Hex_Color(16#cbccc6#),
		Button_background                           => Hex_Color(16#3c526a#),
		Button_hoverBackground                      => Hex_Color(16#707a8c#),
        Dropdown_background                         => Hex_Color(16#232834#),
		Dropdown_foreground                         => Hex_Color(16#707a8c#),
		Dropdown_border                             => Hex_Color(16#373e4c#),
        Input_background                            => Hex_Color(16#232834#),
		Input_border                                => Hex_Color(16#373e4c#),
		Input_foreground                            => Hex_Color(16#cbccc6#),
		Input_placeholderForeground                 => Hex_Color(16#586070#),
		InputOption_activeBorder                    => Hex_Color(16#ffcc66#),
		InputValidation_errorBackground             => Hex_Color(16#1f2430#),
		InputValidation_errorBorder                 => Hex_Color(16#ff3333#),
		InputValidation_infoBackground              => Hex_Color(16#1f2430#),
		InputValidation_infoBorder                  => Hex_Color(16#5ccfe6#),
		InputValidation_warningBackground           => Hex_Color(16#1f2430#),
		InputValidation_warningBorder               => Hex_Color(16#ffd580#),
		Scrollbar_shadow                            => Hex_Color(16#191e2a#),
		ScrollbarSlider_background                  => Hex_Color(16#5c6773#),
		ScrollbarSlider_hoverBackground             => Hex_Color(16#707a8c#),
		ScrollbarSlider_activeBackground            => Hex_Color(16#cbccc6#),
		Badge_background                            => Hex_Color(16#ffcc66#),
		Badge_foreground                            => Hex_Color(16#1f2430#),
		ProgressBar_background                      => Hex_Color(16#ffcc66#),
		List_activeSelectionBackground              => Hex_Color(16#191e2a#),
		List_activeSelectionForeground              => Hex_Color(16#707a8c#),
		List_focusBackground                        => Hex_Color(16#191e2a#),
		List_focusForeground                        => Hex_Color(16#707a8c#),
		List_highlightForeground                    => Hex_Color(16#ffcc66#),
		List_hoverBackground                        => Hex_Color(16#191e2a#),
		List_hoverForeground                        => Hex_Color(16#707a8c#),
		List_inactiveSelectionBackground            => Hex_Color(16#191e2a#),
		List_inactiveSelectionForeground            => Hex_Color(16#707a8c#),
		List_invalidItemForeground                  => Hex_Color(16#586070#),
		ActivityBar_background                      => Hex_Color(16#1f2430#),
		ActivityBar_foreground                      => Hex_Color(16#707a8ccc#),
		ActivityBar_border                          => Hex_Color(16#1f2430#),
		ActivityBarBadge_background                 => Hex_Color(16#ffcc66#),
		ActivityBarBadge_foreground                 => Hex_Color(16#1f2430#),
		SideBar_background                          => Hex_Color(16#1f2430#),
		SideBar_border                              => Hex_Color(16#1f2430#),
		SideBarTitle_foreground                     => Hex_Color(16#707a8c#),
		SideBarSectionHeader_background             => Hex_Color(16#1f2430#),
		SideBarSectionHeader_foreground             => Hex_Color(16#707a8c#),
		EditorGroup_border                          => Hex_Color(16#191e2a#),
		EditorGroup_background                      => Hex_Color(16#232834#),
		EditorGroupHeader_noTabsBackground          => Hex_Color(16#1f2430#),
		EditorGroupHeader_tabsBackground            => Hex_Color(16#1f2430#),
		EditorGroupHeader_tabsBorder                => Hex_Color(16#1f2430#),
		Tab_activeBackground                        => Hex_Color(16#1f2430#),
		Tab_activeForeground                        => Hex_Color(16#cbccc6#),
		Tab_border                                  => Hex_Color(16#1f2430#),
		Tab_activeBorder                            => Hex_Color(16#ffcc66#),
		Tab_unfocusedActiveBorder                   => Hex_Color(16#707a8c#),
		Tab_inactiveBackground                      => Hex_Color(16#1f2430#),
		Tab_inactiveForeground                      => Hex_Color(16#707a8c#),
		Tab_unfocusedActiveForeground               => Hex_Color(16#707a8c#),
		Tab_unfocusedInactiveForeground             => Hex_Color(16#707a8c#),
		Editor_background                           => Hex_Color(16#1f2430#),
		Editor_foreground                           => Hex_Color(16#cbccc6#),
		EditorLineNumber_foreground                 => Hex_Color(16#707a8c66#),
		EditorLineNumber_activeForeground           => Hex_Color(16#707a8ccc#),
		EditorCursor_foreground                     => Hex_Color(16#ffcc66#),
		Editor_selectionBackground                  => Hex_Color(16#2a3546#),
		Editor_inactiveSelectionBackground          => Hex_Color(16#262f3e#),
		Editor_selectionHighlightBackground         => Hex_Color(16#262f3e#),
		Editor_selectionHighlightBorder             => Hex_Color(16#313e52#),
		Editor_wordHighlightBackground              => Hex_Color(16#262f3e#),
		Editor_wordHighlightStrongBackground        => Hex_Color(16#ffcc6633#),
		Editor_findMatchBackground                  => Hex_Color(16#ffcc660d#),
		Editor_findMatchBorder                      => Hex_Color(16#ffcc66#),
		Editor_findMatchHighlightBackground         => Hex_Color(16#ffcc660d#),
		Editor_findMatchHighlightBorder             => Hex_Color(16#ffcc6659#),
		Editor_findRangeHighlightBackground         => Hex_Color(16#262f3e#),
		Editor_findRangeHighlightBorder             => Hex_Color(16#1f243000#),
		Editor_lineHighlightBackground              => Hex_Color(16#191e2a#),
		EditorLink_activeForeground                 => Hex_Color(16#ffcc66#),
		Editor_rangeHighlightBackground             => Hex_Color(16#191e2a#),
		EditorWhitespace_foreground                 => Hex_Color(16#707a8c66#),
		EditorIndentGuide_background                => Hex_Color(16#707a8c4d#),
		EditorIndentGuide_activeBackground          => Hex_Color(16#707a8cb3#),
		EditorRuler_foreground                      => Hex_Color(16#707a8c4d#),
		EditorCodeLens_foreground                   => Hex_Color(16#5c6773#),
		EditorBracketMatch_background               => Hex_Color(16#707a8c4d#),
		EditorBracketMatch_border                   => Hex_Color(16#707a8c99#),
		EditorOverviewRuler_border                  => Hex_Color(16#191e2a#),
		EditorOverviewRuler_modifiedForeground      => Hex_Color(16#77a8d999#),
		EditorOverviewRuler_addedForeground         => Hex_Color(16#a6cc7099#),
		EditorOverviewRuler_deletedForeground       => Hex_Color(16#f2798399#),
		EditorOverviewRuler_errorForeground         => Hex_Color(16#ff3333#),
		EditorOverviewRuler_warningForeground       => Hex_Color(16#ffcc66#),
		EditorError_foreground                      => Hex_Color(16#ff3333#),
		EditorWarning_foreground                    => Hex_Color(16#ffcc66#),
		EditorGutter_modifiedBackground             => Hex_Color(16#77a8d999#),
		EditorGutter_addedBackground                => Hex_Color(16#a6cc7099#),
		EditorGutter_deletedBackground              => Hex_Color(16#f2798399#),
		DiffEditor_insertedTextBackground           => Hex_Color(16#bae67e26#),
		DiffEditor_removedTextBackground            => Hex_Color(16#f29e7426#),
        EditorWidget_background                     => Hex_Color(16#232834#),
		EditorSuggestWidget_background              => Hex_Color(16#23283499#),
		EditorSuggestWidget_border                  => Hex_Color(16#ffd580#),
		EditorSuggestWidget_highlightForeground     => Hex_Color(16#ffcc66#),
		EditorSuggestWidget_selectedBackground      => Hex_Color(16#191e2a#),
		EditorHoverWidget_background                => Hex_Color(16#232834#),
		EditorHoverWidget_border                    => Hex_Color(16#101521#),
		DebugExceptionWidget_border                 => Hex_Color(16#191e2a#),
		DebugExceptionWidget_background             => Hex_Color(16#232834#),
		EditorMarkerNavigation_background           => Hex_Color(16#232834#),
        PeekView_border                             => Hex_Color(16#191e2a#),
		PeekViewEditor_background                   => Hex_Color(16#232834#),
		PeekViewEditor_matchHighlightBackground     => Hex_Color(16#ffcc6633#),
		PeekViewResult_background                   => Hex_Color(16#232834#),
		PeekViewResult_fileForeground               => Hex_Color(16#707a8c#),
		PeekViewResult_matchHighlightBackground     => Hex_Color(16#ffcc6633#),
		PeekViewTitle_background                    => Hex_Color(16#232834#),
		PeekViewTitleDescription_foreground         => Hex_Color(16#707a8c#),
		PeekViewTitleLabel_foreground               => Hex_Color(16#707a8c#),
		Panel_background                            => Hex_Color(16#1f2430#),
		Panel_border                                => Hex_Color(16#191e2a#),
		PanelTitle_activeBorder                     => Hex_Color(16#ffcc66#),
		PanelTitle_activeForeground                 => Hex_Color(16#cbccc6#),
		PanelTitle_inactiveForeground               => Hex_Color(16#707a8c#),
		StatusBar_background                        => Hex_Color(16#1f2430#),
		StatusBar_foreground                        => Hex_Color(16#707a8c#),
		StatusBar_border                            => Hex_Color(16#1f2430#),
		StatusBar_debuggingBackground               => Hex_Color(16#f29e74#),
		StatusBar_debuggingForeground               => Hex_Color(16#1f2430#),
		StatusBar_noFolderBackground                => Hex_Color(16#232834#),
		StatusBarItem_activeBackground              => Hex_Color(16#00000050#),
		StatusBarItem_hoverBackground               => Hex_Color(16#00000030#),
		StatusBarItem_prominentBackground           => Hex_Color(16#191e2a#),
		StatusBarItem_prominentHoverBackground      => Hex_Color(16#00000030#),
		TitleBar_activeBackground                   => Hex_Color(16#1f2430#),
		TitleBar_activeForeground                   => Hex_Color(16#cbccc6#),
		TitleBar_inactiveBackground                 => Hex_Color(16#1f2430#),
		TitleBar_inactiveForeground                 => Hex_Color(16#707a8c#),
		TitleBar_border                             => Hex_Color(16#1f2430#),
        ExtensionButton_prominentForeground         => Hex_Color(16#1f2430#),
		ExtensionButton_prominentBackground         => Hex_Color(16#ffcc66#),
		ExtensionButton_prominentHoverBackground    => Hex_Color(16#fac761#),
        PickerGroup_border                          => Hex_Color(16#191e2a#),
		PickerGroup_foreground                      => Hex_Color(16#484f5e#),
        debugToolBar_background                     => Hex_Color(16#232834#),
        walkThrough_embeddedEditorBackground        => Hex_Color(16#232834#),
        GitDecoration_modifiedResourceForeground    => Hex_Color(16#77a8d9b3#),
		GitDecoration_deletedResourceForeground     => Hex_Color(16#f27983b3#),
		GitDecoration_untrackedResourceForeground   => Hex_Color(16#a6cc70b3#),
		GitDecoration_ignoredResourceForeground     => Hex_Color(16#484f5e#),
		GitDecoration_conflictingResourceForeground => Hex_Color(16#FFFFFFFF#),
		GitDecoration_submoduleResourceForeground   => Hex_Color(16#d4bfffb3#),
		Settings_headerForeground                   => Hex_Color(16#cbccc6#),
		Settings_modifiedItemIndicator              => Hex_Color(16#77a8d9#),
        Terminal_background                         => Hex_Color(16#1f2430#),
		Terminal_foreground                         => Hex_Color(16#cbccc6#),
		Terminal_ansiBlack                          => Hex_Color(16#191e2a#),
		Terminal_ansiRed                            => Hex_Color(16#ed8274#),
		Terminal_ansiGreen                          => Hex_Color(16#a6cc70#),
		Terminal_ansiYellow                         => Hex_Color(16#fad07b#),
		Terminal_ansiBlue                           => Hex_Color(16#6dcbfa#),
		Terminal_ansiMagenta                        => Hex_Color(16#cfbafa#),
		Terminal_ansiCyan                           => Hex_Color(16#90e1c6#),
		Terminal_ansiWhite                          => Hex_Color(16#c7c7c7#),
		Terminal_ansiBrightBlack                    => Hex_Color(16#686868#),
		Terminal_ansiBrightRed                      => Hex_Color(16#f28779#),
		Terminal_ansiBrightGreen                    => Hex_Color(16#bae67e#),
		Terminal_ansiBrightYellow                   => Hex_Color(16#ffd580#),
		Terminal_ansiBrightBlue                     => Hex_Color(16#73d0ff#),
		Terminal_ansiBrightMagenta                  => Hex_Color(16#d4bfff#),
		Terminal_ansiBrightCyan                     => Hex_Color(16#95e6cb#),
		Terminal_ansiBrightWhite                    => Hex_Color(16#ffffff#)
    );

    --Font_Name       : String := "Muli-Medium.ttf";    -- var-width
    --Font_Name       : String := "FiraCode-Regular.ttf"; -- fixed-width
    Font_Name       : String := "Fira Code Medium Nerd Font Complete Mono.ttf";
    Font_Size       : SDL.TTFs.Point_Sizes := 14;

    Double_Click_Threshold  : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(500);
    Cursor_Blink_Rate       : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(400);

    -- Length of time required for the mouse to sit still before it's considered "hovering"
    Hover_Tooltip_Time      : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(1200);

end DAGBuild.GUI.Settings;