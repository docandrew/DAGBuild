
with SDL;
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
    Dark_BG         : SDL.Video.Palettes.Colour := (Red    => 16#23#,
                                                    Green  => 16#28#,
                                                    Blue   => 16#34#,
                                                    Alpha  => 16#FF#);

    Dark_Widget     : SDL.Video.Palettes.Colour := (Red    => 16#70#,
                                                    Green  => 16#7A#,
                                                    Blue   => 16#8C#,
                                                    Alpha  => 16#FF#);

    Dark_Active     : SDL.Video.Palettes.Colour := (Red    => 16#CB#,
                                                    Green  => 16#CC#,
                                                    Blue   => 16#C6#,
                                                    Alpha  => 16#FF#);

    Dark_Text       : SDL.Video.Palettes.Colour := (Red    => 16#CB#,
                                                    Green  => 16#CC#,
                                                    Blue   => 16#C6#,
                                                    Alpha  => 16#FF#);

end DAGBuild.Settings;