
package DAGBuild.GUI.Widgets.Label is

    -- Draw a label
    -- @field Display_Length is the width in characters that this field should hold
    procedure Label (st              : in out DAGBuild.GUI.State.UIState;
                     Text            : String;
                     x               : SDL.Natural_Coordinate;
                     y               : SDL.Natural_Coordinate;
                     Display_Length  : Natural := 10;
                     BG_Color        : SDL.Video.Palettes.Colour := (0,0,0,0));

end DAGBuild.GUI.Widgets.Label;
