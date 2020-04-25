with Ada.Strings.Unbounded;

with SDL.TTFs;

with DAGBuild.GUI.State;

package DAGBuild.GUI.Widgets is

    DAG_Font        : SDL.TTFs.Fonts;

    -- Draw a button, return True if clicked
    function Button (st      : in out DAGBuild.GUI.State.UIState;
                     x       : SDL.Natural_Coordinate;
                     y       : SDL.Natural_Coordinate;
                     Label   : String := "") return Boolean;

    -- Draw a slider, return True if value was updated
    -- @param Max is the maximum value this slider can represent
    -- @param Val is the underlying value to be changed/set
    -- @return True if value was changed
    function Vertical_Slider (st  : in out DAGBuild.GUI.State.UIState;
                              x   : SDL.Natural_Coordinate;
                              y   : SDL.Natural_Coordinate;
                              Max : Integer;
                              Val : in out Integer) return Boolean;

    -- Draw a label
    -- @field Display_Length is the width in characters that this field should hold
    procedure Label (st              : in out DAGBuild.GUI.State.UIState;
                     Text            : String;
                     x               : SDL.Natural_Coordinate;
                     y               : SDL.Natural_Coordinate;
                     Display_Length  : Natural := 10);

    -- Single-Line Text Field
    -- @field Display_Length is the number of characters that this widget can display
    -- @field Max_Length is the max number of characters that this widget can hold
    -- @return True if we hit Return key in this field.
    function Text_Field (st              : in out DAGBuild.GUI.State.UIState;
                         Text            : in out Ada.Strings.Unbounded.Unbounded_String;
                         x               : SDL.Natural_Coordinate;
                         y               : SDL.Natural_Coordinate;
                         Display_Length  : Natural := 10;
                         Max_Length      : Natural := 10) return Boolean;

end DAGBuild.GUI.Widgets;
