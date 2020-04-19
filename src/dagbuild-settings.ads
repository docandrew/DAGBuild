
with SDL;
with SDL.TTFs;
with SDL.Video.Palettes;
with SDL.Video.Windows;

package DAGBuild.Settings is

    Init_Pos : SDL.Natural_Coordinates :=
    (
        X => SDL.Video.Windows.Centered_Window_Position,
        Y => SDL.Video.Windows.Centered_Window_Position
    );

    Init_Size : SDL.Positive_Sizes := 
    (
        Width  => 640,
        Height => 480
    );

    -- Color defaults, overrideable in user prefs
    Dark_BG         : SDL.Video.Palettes.Colour := (Red     => 16#1F#,
                                                    Green   => 16#24#,
                                                    Blue    => 16#30#,
                                                    Alpha   => 16#FF#);

    Dark_Widget     : SDL.Video.Palettes.Colour := (Red     => 16#23#,
                                                    Green   => 16#28#,
                                                    Blue    => 16#34#,
                                                    Alpha   => 16#FF#);

    Dark_Hot        : SDL.Video.Palettes.Colour := (Red     => 16#CB#,
                                                    Green   => 16#CC#,
                                                    Blue    => 16#C6#,
                                                    Alpha   => 16#FF#);

    Dark_Active     : SDL.Video.Palettes.Colour := (Red     => 16#FF#,
                                                    Green   => 16#CC#,
                                                    Blue    => 16#66#,
                                                    Alpha   => 16#FF#);

    Dark_Text       : SDL.Video.Palettes.Colour := (Red     => 16#CB#,
                                                    Green   => 16#CC#,
                                                    Blue    => 16#C6#,
                                                    Alpha   => 16#FF#);

    Dark_Hot_Text   : SDL.Video.Palettes.Colour := (Red     => 16#BA#,
                                                    Green   => 16#E6#,
                                                    Blue    => 16#7E#,
                                                    Alpha   => 16#FF#);

    Dark_Focus      : SDL.Video.Palettes.Colour := (Red     => 16#5C#,
                                                    Green   => 16#CF#,
                                                    Blue    => 16#E6#,
                                                    Alpha   => 16#FF#);

    --Font            : String := "Fira Code Light Nerd Font Complete Mono Windows Compatible.ttf";
    Font_Name       : String := "FiraCode-Regular.ttf";
    Font_Size       : SDL.TTFs.Point_Sizes := 14;

end DAGBuild.Settings;