
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
    Default_Dark_BG     : constant SDL.Video.Palettes.Colour := (Red    => 16#23#, 
                                                                 Green  => 16#28#,
                                                                 Blue   => 16#34#,
                                                                 Alpha  => 16#FF#);

end DAGBuild.Settings;